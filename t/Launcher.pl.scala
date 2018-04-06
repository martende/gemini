#!/usr/bin/env perl

use 5.20.0;
use List::Util      qw(any all);   # List::MoreUtils in older Perl (Ubuntu 14.04 and older)
use List::MoreUtils qw(part uniq);
use Cwd             qw(cwd);
use File::Spec;                    # Cwd::abs_path resolves symlinks which is undesirable
use Capture::Tiny   qw(capture);
use Data::Dumper;
use Digest::MD5     qw(md5_hex);
use Digest::SHA     qw(sha1_hex sha256_hex sha512_hex);
use LWP::Simple     qw(getstore);  # <-- TODO: perl code requires `apt-get -y install curl libwww-perl` for little things; these packages may be unavailable in docker environment
use File::Path      qw(make_path);
use File::Basename  qw(dirname basename);
use MIME::Base64    qw(encode_base64);
use Time::HiRes     qw(sleep);
use POSIX           qw(WNOHANG);

my $D        = ($^O eq "MSWin32") ? ";" : ":";
my $ABS_FILE = File::Spec->rel2abs(__FILE__);                           # remember, there could be chdir() after require("???/Launcher.pl") and before launch()
my $ABS_0    = File::Spec->rel2abs($0);                                 # perl file which does 'require Launcher'
my $ABS_BASE = __FILE__ ne $0 ? dirname($ABS_0) : ($ENV{PWD} || cwd()); # dir of --//-- or current dir without resolving symlinks (cwd() does resolve but $PWD does not work in Win32)

$SIG{BREAK} = $SIG{INT} = 'IGNORE' if $ARGV[0] eq "--monitor"; # jvm and monitor receives Ctrl-C (windows needs it, linux inherits is doing fork)

sub readFile {
  my ($filename) = @_;
  local $/ = undef;
  open FILE, $filename or die "readFile($filename) failed: $!";
  binmode FILE;
  my $data = <FILE>;
  close FILE;
  return $data;
}

sub writeFile {
  my ($filename, $content) = @_;
  make_path(dirname($filename)) or die "$!" unless -d dirname($filename);
  open FH, ">$filename" or die "writeFile($filename) failed: $!";
  binmode FH; # do not emit \r
  print FH $content;
  close FH;
}

# public function, the monitor calls it to kill jvm
sub killpid {
  my ($pid) = @_;
  system "taskkill /T /F /PID $pid"  if $^O eq "MSWin32";   # it also kills monitor children (e.g. wget)
  system "kill -9 $pid"              if $^O ne "MSWin32";
}

# resolve java version number or java relative path to absolute path for JAVA_HOME
sub resolveJavaHome {
  my $java = shift || 8;                                            # default java version
  my $javadir;
  if ($java =~ /^(6|7|8|9)$/) {                                       # find JAVA_HOME if only version (6,7 or 8) is specified
    if ($^O ne "MSWin32" && -f "/etc/NIXOS") {
      ($javadir = `readlink -f \$(which java)`) =~ s|/bin/java\s*$||;
    }
    if (!$javadir) {
      my $jdks_base_dir = ($^O eq "MSWin32") ? "C:/Java" : "/opt";  # place of system-wide installation
      sub findJavaDir { return (glob("$jdks_base_dir/jdk1.$java.0_?"),    # glob sorts in alphanumeric order, so it peeks the newest jdk
                                glob("$jdks_base_dir/jdk1.$java.0_??"),
                                glob("$jdks_base_dir/jdk1.$java.0_???"))[-1]; }
      $javadir = findJavaDir;
      if (!$javadir && !-w $jdks_base_dir) {                        # fallback to local java installation if run under limited user
        $jdks_base_dir = $^O eq "MSWin32" ? $ENV{USERPROFILE} : $ENV{HOME};
        $javadir = findJavaDir;
      }
      if (!$javadir && $java==8) {                                  # install JDK; globally if possible
        print("JDK not found; downloading JDK$java to $jdks_base_dir\n");
        system("curl -L --header 'Cookie: oraclelicense=accept-securebackup-cookie' http://download.oracle.com/otn-pub/java/jdk/8u131-b11/d54c1d3a095b4ff2b6607d096fa80163/jdk-8u131-linux-x64.tar.gz | tar xz --directory=$jdks_base_dir");
        $javadir = findJavaDir;
      }
    }
  } else {
    $javadir = $java;
  }
  -d $javadir or die "JAVA=`$javadir' is not a directory";
  return File::Spec->rel2abs($javadir);
}

sub getstoreChecked {
  my ($url, $path) = @_;
  print("GET $url ");
  if (200 == getstore("$url",     "$path.tmp")) {
    my $binary = readFile("$path.tmp");
    if (200 == getstore("$url.sha1", "$path.sha1") && lc(substr(readFile("$path.sha1"), 0, 40)) eq sha1_hex($binary)) {
      print("ok\n");
      rename "$path.tmp", $path;
      unlink "$path.md5";
      return;
    }
    if (200 == getstore("$url.md5",  "$path.md5" ) && lc(substr(readFile("$path.md5" ), 0, 32)) eq  md5_hex($binary)) {
      print("ok\n");
      rename "$path.tmp", $path;
      unlink "$path.sha1";
      return;
    }
  }
  print("fail\n");
  unlink "$path";
  unlink "$path.tmp";
  unlink "$path.md5";
  unlink "$path.sha1";
}

# launcher must download scala-compiler.jar, .. to compile own jar; do not do it if the jars are on $CLASSPATH (that could mean nix deployment)
my @repoUrls = ( "https://repo1.maven.org/maven2",
                 "https://oss.sonatype.org/content/repositories/releases");

sub artifact {
  my ($groupId, $artifactId, $version) = split(':', shift, 3);
  ($artifactId, $version) = ($groupId, $artifactId) unless $version;

  (my $groupIdPath = $groupId) =~ s/\./\//g;
  my $home = $^O eq "MSWin32" ? $ENV{USERPROFILE} : (-w $ENV{HOME} ? $ENV{HOME} : "/var/tmp"); # under "nixbld"
  my $cacheDir = "$home/.m2/repository/$groupIdPath/$artifactId/$version";
  make_path($cacheDir) or die "make_path($cacheDir) failed: $!" unless -d $cacheDir;
  my @requiredFiles = ("$artifactId-$version.pom",
                       "$artifactId-$version.jar");
  for (my $i=0; $i<99 && any { ! -s "$cacheDir/$_" } @requiredFiles; $i++) {
    for (@requiredFiles) {
      getstoreChecked($repoUrls[$i % @repoUrls]."/$groupIdPath/$artifactId/$version/$_", "$cacheDir/$_");
    }
  }
  # TODO: validate signature
  return "$cacheDir/$artifactId-$version.jar";
}

sub makeLauncherJar {
  # outdir is always Memo.readwriteDir ($HOME/.memo | /var/tmp/.memo)
  my $outdir = ($^O eq "MSWin32" ? $ENV{USERPROFILE} : (-w $ENV{HOME} ? $ENV{HOME} : "/var/tmp")) . "/.memo";  # /var/tmp under "nixbld"
  my ($javaexe, @launcherlibs) = @_;
  my @src_perl_commented = map { /^#!/../^!#$/ ? "// $_" : $_ } split(/\n/, readFile $ABS_FILE); # keep line numbering
  my $hash = sha1_hex( join "\n", (grep { !/^\/\// } @src_perl_commented), sort(map { lc(basename $_) } @launcherlibs) );

  unless (-e "$outdir/$hash.jar") { # TODO: check for a jar file next to the source?
    writeFile "$outdir/$$.$hash.scala", join("\n", @src_perl_commented); # add PID to avoid collisions
    my @cmd = ($javaexe, "-Dscala.usejavacp=true",
                         "-cp", join($D, @launcherlibs),
                         "scala.tools.nsc.Main", (map { "-Xplugin:$_" } grep { /scalamacros.paradise/ } @launcherlibs), # fastparse+scala-2.10 needs this plugin
                                                 "-unchecked",
                                                 "-d", "$outdir/$$.$hash.jar",
                                                 "$outdir/$$.$hash.scala");
    print STDERR "Compiling $ABS_FILE ($outdir/$$.$hash.scala) -> $outdir/$hash.jar\n";
    print STDERR "JAVAEXE   $javaexe\n";
    print STDERR "LIBS      ".(join ' ', sort(map { lc(basename $_) } @launcherlibs))."\n";
    # print STDERR join("\033[46m \033[0m", @cmd)."\n\n";
    if ($^O ne "MSWin32") {
      system '/bin/sh', '-c', ("cd /tmp; " . join(' ', @cmd)); # current dir must be writable otherwise scalac throws NPE. (there is no $HOME under "nixbld")
    } else {
      system @cmd;
    }
    my $exitcode = $? >> 8;
    unlink "$outdir/$$.$hash.scala";
    exit($exitcode) if $exitcode != 0;
    rename "$outdir/$$.$hash.jar", "$outdir/$hash.jar";
  }
  return "$outdir/$hash.jar";
}

sub launch {
  my %h = @_;
  $h{VERBOSE}  = 1                                           unless exists $h{VERBOSE};  # VERBOSE  is 1 by default
  $h{MONITOR}  = 0                                           unless exists $h{MONITOR};  # MONITOR  is 0 by default
  $h{NOSERVER} = (($^O ne "MSWin32") && $< >= 30000 ? 1 : 0) unless exists $h{NOSERVER}; # NOSERVER is 0 by default unless nixbld (which cannot start background process nor create a X11 window )
  $h{SOURCES}  = [ basename($0) ]                            unless exists $h{SOURCES};
# @repoUrls = (@repoUrls, $h{ADDREPOS});

  my $TMP = ($^O eq "MSWin32") ? "c:/tmp" : (-w $ENV{HOME} ? "$ENV{HOME}/tmp" : ($ENV{TMP} ? $ENV{TMP} : "/tmp"));  # avoid using /tmp for security reasons; "nixbld1" has no writable home
  make_path($TMP) or die "make_path($TMP) failed: $!" unless -d $TMP;

  # note that SCALACARGS is string, JAVAARGS is list of strings, e.g:
  #   JAVA     => [ 8, '-Xmx256m', '-Xms64m', '-agentpath:jvmti-demo.dll' ],
  #   SCALA    => [ '2.11.1', '-feature -deprecated' ],
  if (ref($h{SCALA}) eq 'ARRAY') { $h{SCALACARGS} = $h{SCALA}[1];                         $h{SCALA} = $h{SCALA}[0]; }
  if (ref($h{JAVA} ) eq 'ARRAY') { $h{JAVAARGS}   = [ @{$h{JAVA}}[1..(@{$h{JAVA}}-1)] ];  $h{JAVA}  = $h{JAVA}[0];  } # JAVAC ARGS here?

  $h{JAVA} = resolveJavaHome($h{JAVA});                                                                               # here $h{JAVA} is either JAVA_HOME or java.exe (or avian.exe)
  my $javaexe = $h{JAVA}."/bin/".($^O eq "MSWin32" ? ($^X =~ /wperl\.exe$/i ? "javaw.exe" : "java.exe") : "java");    # use javaw.exe if wperl.exe is used
  -x $javaexe or die "JAVA=`$h{JAVA}' does not exist or not executable";
  $ENV{JAVA_HOME} = $h{JAVA}; # it only has sense for the moninor process; service()'s and run()'s argumens are already evaluated; moreover this JAVA_HOME depends on JAVA argument

  my @jvmci_options = grep /EnableJVMCI|UseJVMCICompiler/, @{$h{JAVAARGS}};
  if (@jvmci_options && (`$javaexe -version 2>&1` !~ /jvmci/)) {
    print STDERR "remove JAVAARGS options unsupported by this JVM: @jvmci_options\n";
    $h{JAVAARGS} = [ grep !/EnableJVMCI|UseJVMCICompiler/, @{$h{JAVAARGS}} ];
  }

  $h{SCALA} ||= "2.12";                           # default scala version
  $h{SCALA} = "2.10.7"    if $h{SCALA} eq "2.10"; # latest minor releases
  $h{SCALA} = "2.11.12"   if $h{SCALA} eq "2.11";
  $h{SCALA} = "2.12.5"    if $h{SCALA} eq "2.12";
  $h{SCALA} = "2.13.0-M3" if $h{SCALA} eq "2.13"; # no fastparse yet
  my $scala_version = $h{SCALA};  $scala_version =~ s/^(\d+\.\d+)(.+)$/$1/ if $h{SCALA} =~ /^2\.(10|11|12|13)\.\d+$/; # binary version, 2.10, 2.11, 2.12 if release, full version if beta

  # NOTE: size of @launcher_artifacts_resolved is hardcoded below in `launcher.jar+deps`
  my @launcher_artifacts_resolved = ( "net.java.dev.jna:jna:4.1.0",
                                      "net.java.dev.jna:jna-platform:4.1.0",
                                      "com.lihaoyi:sourcecode_$scala_version:0.1.4",
                                      "com.lihaoyi:fastparse_$scala_version:1.0.0",
                                      "com.lihaoyi:fastparse-utils_$scala_version:1.0.0",
                                      "org.scala-lang:scala-compiler:$h{SCALA}",
                                      "org.scala-lang:scala-library:$h{SCALA}",
                                      "org.scala-lang:scala-reflect:$h{SCALA}",
                                      "jline:jline:2.14.3",
           $scala_version eq '2.10' ? ( "org.scalamacros:paradise_$h{SCALA}:2.1.0",
                                        "org.scalamacros:quasiquotes_$scala_version:2.1.0" ) # for fastparse
                                    : ( "org.scala-lang.modules:scala-parser-combinators_$scala_version:1.0.5", # for scala-compiler
                                        "org.scala-lang.modules:scala-xml_$scala_version:1.0.6" )
                                    );
  my @launcherlibs = map { artifact $_ } @launcher_artifacts_resolved;
  my $launcherjar = makeLauncherJar($javaexe, @launcherlibs);

  $SIG{BREAK} = $SIG{INT} = 'IGNORE' if $^O eq "MSWin32";                 # Windows: BREAK for Ctrl-Break, INT for Ctrl-C. Linux: --service has own SIGINT handler, --run does not need it

  # todo: take -D..., -X... out of @ARGV and add it to java.exe command line
  my @cmd = ($javaexe,                "-cp",           join($D, uniq($launcherjar, @launcherlibs, ($h{CLASSPATH} ? @{$h{CLASSPATH}} : ()))),  # the only purpose of $h{CLASSPATH} is to contain the jars for javaagent; TODO: exclude jna on linux?
                                      "-Xshare:auto",
                                      "-Djava.io.tmpdir=$TMP",
                                      "-Dsun.jnu.encoding=UTF-8",
                                      "-Dfile.encoding=UTF-8",
                    $h{NOSERVER}   ? ("-Xss4m", "-Xmx2000m")                                                                           : (),  # SI-9866; less prio than ${JAVAARGS}; for my code, Xss1m is too small, 2m is ok, 4m is just in case
                    $h{JAVAARGS}   ? @{$h{JAVAARGS}}                                                                                   : (),  # any filenames in $h{JAVAARGS} and @ARGV are relative to current dir, not to --base
    "ms.webmaster.launcher.Launcher", "--base",        $ABS_BASE,                                                                             # base directory for relative names in --tee and --dep-tree (todo: -Dbase=)
                                      "--launcher",    $ABS_FILE,                                                                             # to watch Launcher's source for changes, why?
                                      "--action",      $h{ACTION},
                    $h{CLASS}      ? ("--class",       $h{CLASS}     )                                                                 : (),
                    $h{TEE}        ? ("--tee",         $h{TEE}       )                                                                 : (),
                    $h{SCALACARGS} ? ("--scalac-args", $h{SCALACARGS})                                                                 : (),
                    $h{ADDREPOS}   ? ("--add-repos",   $h{ADDREPOS}  )                                                                 : (),
                    $h{NOSERVER}   ?  "--no-server"                                                                                    : (),
                    $h{VERBOSE}    ?  "--verbose"                                                                                      : (),  # print source files and compiler times (otherwise only warnings and errors)
                                      "--dep-tree",    encode_base64(Data::Dumper->new([$h{SOURCES}])->Terse(1)->Indent(0)->Dump, ''),        # base64 preserves with '"' in json (and unicode in file names?). todo: a bigger deptree can be pass via ENV variable
                    $h{EXTRA}      ?  @{$h{EXTRA}}                                                                                     : (),
             $^O eq "MSWin32"      ? ("--perl-pid",    $$            )
                                   : ("--",            @ARGV         ));                                                                      # on Windows @ARGV is not unicode, so the unicode command line to be get via ReadProcessMemory(--perl-pid, ...)

  # print STDERR join("\033[46m \033[0m", @cmd)."\n\n";
  my @savedtmp = ($ENV{TEMP}, $ENV{TMP});
  $ENV{TEMP} = $ENV{TMP} = $TMP;

  my $jvm_exitcode;
  if ($h{MONITOR}) {
    (__FILE__ ne $0) or die "Launcher must be used as library";
    if ($^O eq "MSWin32") {
      require Win32::Process;
      sub systembg_win {
        Win32::Process::Create(my $child_proc, $_[0], join(" ", map {"\"$_\""} @_), 0, 0, ".") or die("Win32::Process::Create: " . Win32::FormatMessage(Win32::GetLastError()));
        return $child_proc;
      }
      my $javaproc = systembg_win(@cmd);                                                    # print time()." jvm(pid=" . $javaproc->GetProcessID() . ") started\n";
      my $monproc  = systembg_win($^X, $ABS_0, "--monitor", 1, $javaproc->GetProcessID());  # print time()." mon(pid=" . $monproc->GetProcessID()  . ") started\n";
      my $mon_exitcode;
      while (! $javaproc->Wait(2000)) {
        if ($monproc->GetExitCode($mon_exitcode) && $mon_exitcode!=259) {
          print "monitor(pid=" . $monproc->GetProcessID() . ") died with exitcode=$mon_exitcode, will start it again\n"; # next check for monitor liveness will be in 2 sec
          $monproc  = systembg_win $^X, $ABS_0, "--monitor", 0, $javaproc->GetProcessID();
        }
      }
      $javaproc->GetExitCode($jvm_exitcode) or die "$!";
      $monproc->Kill($mon_exitcode);
    } else {
      sub systembg_linux {
        my $cpid = fork();
        defined($cpid) or die "fork error: $!";
        if ($cpid==0) { exec @_ or die "exec error: $!"; }
        return $cpid;
      }
      my $javapid = systembg_linux(@cmd);                                                   # print "jvm(pid=$javapid) started\n";
      $SIG{BREAK} = $SIG{INT} = 'IGNORE';       # let only jvm to receive Ctrl-C (linux needs it, windows does not)
      my $monpid  = systembg_linux($^X, $ABS_0, "--monitor", 1, $javapid);                  # print "mon(pid=$monpid) started\n";
WAITMORE:                                       # wait for any of the two children to terminate, restart monitor if it terminates earlier than jvm (it happens)
      my $pidexited = waitpid(-1, 0);           # waitpid(-1, _) does not work for processes made with Win32::Process::Create(), but works for those made with open O, '| ...'
      die "waitpid()'s result ($pidexited) is not monpid($monpid) nor javapid($javapid)" unless $pidexited==$javapid || $pidexited==$monpid;
      goto JAVAEXITED if $pidexited==$javapid;
      print "monitor(pid=$monpid) died with exitcode=".($? >> 8).", will start it again in 2 sec\n";
      for (my $i=0; $i<20; $i++) {              # kind of sleep(2) interruped if java process dies
        my $wp = waitpid($javapid, WNOHANG);
        goto JAVAEXITED if $wp==$javapid;
        die "wp=$wp\n" unless $wp==0;
        Time::HiRes::sleep 0.1;
      }
      $monpid = systembg_linux $^X, $ABS_0, "--monitor", 0, $javapid;
      goto WAITMORE;
JAVAEXITED:
      $jvm_exitcode = $? >> 8;
      if (waitpid($monpid, WNOHANG) != 0) {
        print "jvm(pid=$javapid) died exitcode=$jvm_exitcode, monitor is dead\n";
      } else {
        print "jvm(pid=$javapid) died exitcode=$jvm_exitcode, kill monitor(pid=$monpid)\n";
        system "kill -9 $monpid";
        waitpid $monpid, 0;                       # clean up defunct child process
      }
    }
  } else {
    system @cmd;
    $jvm_exitcode = $? >> 8;
  }

  ($ENV{TEMP}, $ENV{TMP}) = @savedtmp;
  $SIG{BREAK} = $SIG{INT} = 'DEFAULT';
  return $jvm_exitcode;
}

sub run {
  my %h = @_;
  my $jvm_exitcode = launch ACTION=>"run", @_;
  print "<<<JVM finished with exitcode=$jvm_exitcode>>>\n" if $h{VERBOSE};
  return $jvm_exitcode;
}

sub service {
  my %h = @_;
  my $jvm_exitcode = launch ACTION=>"service", @_;
  return if $jvm_exitcode==167; # 167 = break (e.g. on initial compilation error)
  if ($jvm_exitcode!=168) {     # 168 = quick restart (e.g. on initial compilation error)
    $| = 1; # autoflush on
    print "<<<JVM finished with exitcode=$jvm_exitcode, will start again in 3 sec (press Ctrl-C to abort)>>>.";
    Time::HiRes::sleep 1.0; print ".";
    Time::HiRes::sleep 1.0; print ".";
    Time::HiRes::sleep 1.0; print "woke up\n";
    $| = 0; # autoflush off
  }

  die "assertion failed ".$ABS_FILE."!=".File::Spec->rel2abs(__FILE__) if $ABS_FILE ne File::Spec->rel2abs(__FILE__);
  die "assertion failed ".$ABS_0   ."!=".File::Spec->rel2abs($0)       if $ABS_0    ne File::Spec->rel2abs($0);
  # reread main file, it could have been changed
  delete $INC{scalar __FILE__};         # Launcher.pl.scala
  delete $INC{$ABS_FILE};               # Launcher.pl.scala
  delete $INC{$0};                      # Service.pl.scala, just in case
  delete $INC{$ABS_0};                  # Service.pl.scala, just in case
  require $ABS_0;
  die "assertion failed: unreachable";
}

return 1 if __FILE__ ne $0; # stop here if this file used as a library

# run command-line utils
my ($action, @rest) = @ARGV;
exit(print makeLauncherJar(@rest))               if $action eq "bootstrap";
exit(launch ACTION => $action, EXTRA => [@rest]) if $action =~ /^(resolve|fetch|launch|scalac|compile-deptree|compile-jointtree)$/;

print "Hi!\n";

=cut
!#

package ms.webmaster
import scala.language.reflectiveCalls
import scala.util.{Try, Failure, Success, Properties}
import scala.Console._
import java.io.{Console => _, _}
import java.util.concurrent.atomic.{AtomicReference, AtomicBoolean}
import java.util.zip.{ZipOutputStream, ZipInputStream, ZipEntry}
import org.fusesource.jansi.{WindowsAnsiOutputStream, AnsiOutputStream} // <- todo: jansi is very buggy on Windows, replace it with smtg
import sun.misc.{Signal, SignalHandler}



// TODO: java.io.File -> java.nio.file.Path
package launcher {

object LauncherUtils {
  def md5(ab: Array[Byte]): String = {
    val md = java.security.MessageDigest getInstance "MD5"
    md update ab
    md.digest.map("%02x" format _).mkString
  }


  trait SHAble[T] {
    def update2(md: java.security.MessageDigest, v: T)
  }
  implicit val paramSHAble     = new SHAble[SHAny[_]]    { def update2(md: java.security.MessageDigest, v: SHAny[_]   ) { v update1 md                           } }
  implicit val byteArraySHAble = new SHAble[Array[Byte]] { def update2(md: java.security.MessageDigest, v: Array[Byte]) { md update v                            } }
  implicit val stringSHAble    = new SHAble[String]      { def update2(md: java.security.MessageDigest, v: String     ) { md update v.getBytes("UTF-8")          } }
  implicit val longSHAble      = new SHAble[Long]        { def update2(md: java.security.MessageDigest, v: Long       ) { md update v.toString.getBytes("UTF-8") } }
  private[this] def traversableSHAble[A, T <: TraversableOnce[A]](implicit elementEncoder: SHAble[A]) = new SHAble[T] {
    def update2(md: java.security.MessageDigest, v: T) { for (it <- v) { md update md.digest; elementEncoder.update2(md, it) } }
  }
  implicit def listSHAble[A:SHAble] = traversableSHAble[A, List  [A]]
  implicit def seqSHAble [A:SHAble] = traversableSHAble[A, Seq   [A]]
/*
  def sha1[A : SHAble](items: A* /* homogeneous sequence */): String = {
    val md = java.security.MessageDigest getInstance "SHA1"
    implicitly[SHAble[Seq[A]]].update2(md, items)
    md.digest.map("%02x" format _).mkString
  }
*/
  implicit class SHAny[A : SHAble](v: A) {
    def update1(md: java.security.MessageDigest) { implicitly[SHAble[A]].update2(md, v) }
  }
  def sha1(items: SHAny[_]* /* heterogeneous sequence */): String = {
    val md = java.security.MessageDigest getInstance "SHA1"
    implicitly[SHAble[Seq[SHAny[_]]]].update2(md, items)
    md.digest.map("%02x" format _).mkString
  }


  def mkAbsFile(base: File, relPath: String) = if (new File(relPath).isAbsolute) new File(relPath) else new File(base, relPath)

  def mkClasspathString(cpfiles: List[File]): String =
    cpfiles map { f =>
      val path = f.getAbsolutePath
      require(!path.contains(File.pathSeparator), s"'$path' contains ${File.pathSeparator}, todo: implement escaping")
      path
    } mkString File.pathSeparator


  def deleteDirectory(dir: File): Traversable[File] = new Traversable[File] {
    def foreach[U](onFileDie: File=>U) {
      dir.listFiles match {
        case null  =>
        case files => files foreach {
                        case f if f.isDirectory => deleteDirectory(f) foreach onFileDie
                        case f                  => onFileDie(f); f.delete()
                      }
      }
      dir.delete()
    }
  }


  implicit class RichCollection[A, Repr](xs: scala.collection.IterableLike[A, Repr]){
    def distinctBy[B, That](f: A=>B)(implicit cbf: scala.collection.generic.CanBuildFrom[Repr, A, That]) = {
      val seen = new java.util.HashSet[B]
      for (a <- xs if seen add f(a)) yield a
    }
    // returns Some(head) if xs is nonEmpty and all elems are the same
    def allthesame              (implicit equiv: Equiv[A]): Option[A] = xs.foldLeft(xs.headOption)((o,a) => o filter (equiv.equiv(_, a)))
    def allthesameBy[B](f: A=>B)(implicit equiv: Equiv[A]): Option[A] = allthesame(Equiv by f)
  }


  def copyStream(output: OutputStream, input: InputStream): Long = {
    val buffer = new Array[Byte](0x10000)
    var count = 0L
    var n = input read buffer
    while (-1 != n) {
      output.write(buffer, 0, n)
      count += n
      n = input read buffer
    }
    count
  }

  def readFileToByteArray(file: File, skip: Long=0): Array[Byte] = {
    val fi = new FileInputStream(file)
    try {
      fi.skip(skip)
      val baos = new ByteArrayOutputStream
      copyStream(baos, fi)
      baos.toByteArray
    } finally
      fi.close()
  }


  def addFilesToZip(targetZip: File, sourceZips: Traversable[File], files: Traversable[(String, Compiler.Source)]) {
    val zout = new ZipOutputStream(new FileOutputStream(targetZip))
    val seen = new java.util.HashSet[String]
    for ((name, source) <- files) {
      if (seen add name.toUpperCase) {
        zout putNextEntry new ZipEntry(name)
        source.copyTo(zout)
        zout.closeEntry()
      } else {
        System.err.println(s"addFilesToZip: ignore duplicate name `$name'")
      }
    }
    for (source <- sourceZips) {
      val zin = new ZipInputStream(new FileInputStream(source))
      for (ze <- Iterator continually zin.getNextEntry takeWhile (_ != null)) {
        if (seen add ze.getName.toUpperCase) {
          zout putNextEntry ze
          copyStream(zout, zin)
          zout.closeEntry()
        } else if (ze.getName != "META-INF/MANIFEST.MF") {
          System.err.println(s"addFilesToZip: ignore duplicate name `${ze.getName}' (from `$source`)")
        }
      }
      zin.close()
    }
    zout.close()
  }


  // it includes directory /jre/classes
  def classpathFiles(cl: ClassLoader, withSystemJars: Boolean): List[Compiler.FileSource] = { // JarFileSource | DirFileSource
    def classpathURLs(cl: ClassLoader): List[Array[java.net.URL]] = cl match {
      case cl: java.net.URLClassLoader => cl.getURLs :: classpathURLs(cl.getParent)
      case null                        => sun.misc.Launcher.getBootstrapClassPath.getURLs :: Nil
    }
    val baseprefix = Launcher.cmdline.base.getPath.replace("\\","/")+"/"
    for (url  <- classpathURLs(cl).reverse.drop(if (withSystemJars) 0 else 2).flatten;
         file <- Try(new File(url.toURI)).toOption)
    yield Compiler.FileSource.fromFileAndRelpath(file, file.getPath.replace("\\","/") stripPrefix baseprefix)
  }


  def withCaptureStdoutAndStderr[A](onprint: Array[Byte] => Unit)(body: =>A): A = {
    val ostream = new OutputStream {
      private[this] val baos = new ByteArrayOutputStream

      def write(b: Int) { if (b == '\n'.toInt) flush() else baos.write(b) }

      override def flush() {
        if (baos.size > 0) {
          onprint(baos.toByteArray)
          baos.reset()
        }
      }

      override def close() { flush() }
    }

    val ps = new PrintStream(ostream, false, "UTF-8")
    val (backout, backerr) = (System.out, System.err)
    System.setOut(ps)
    System.setErr(ps)
    try {
      Console.withErr(ps) {
        Console.withOut(ps) {
          body
        }
      }
    } finally {
      System.setOut(backout)
      System.setErr(backerr)
      ps.close()
      ostream.close()
    }
  }


  // add tee file, where stdout and stderr will be duplicated to, without sacrifying colors on the console
  // todo: rotate logs
  def tee(file: File) {
    class TeePrintStream(parent: PrintStream, out: OutputStream, codepage: String) extends PrintStream(out, true, codepage) {
      private[this] var inwrap = false
      private[this] def wrap(f: =>Unit)(g: =>Unit): Unit = synchronized {
        if (inwrap) g
        else {
          inwrap = true
          try {f; g}
          catch {
            case _: NumberFormatException => /* ignore invalid ANSI-sequence in `org.fusesource.jansi.AnsiOutputStream` */
          //case _: ArrayIndexOutOfBoundsException => /* at org.fusesource.jansi.AnsiOutputStream.write(AnsiOutputStream.java:117) */
          }
          finally inwrap = false
        }
      }
      override def checkError                                                 = synchronized { parent.checkError || super.checkError }
      override def close()                                                    { synchronized { parent.close(); super.close() } }
      override def flush()                                                    { synchronized { parent.flush(); super.flush() } }
      override def write(x: Int)                                              { wrap {parent.write(x)} {super.write(x)} }
      override def write(x: Array[Byte], o: Int, l: Int)                      { wrap {parent.write(x, o, l)} {super.write(x, o, l)} }
      // parent can have different charset, that is why overriding `write` is not enough
      override def print(x: Char)                                             { wrap {parent.print(x)} {super.print(x)} }
      override def print(x: Array[Char])                                      { wrap {parent.print(x)} {super.print(x)} }
      override def print(x: String)                                           { wrap {parent.print(x)} {super.print(x)} }
      override def print(x: AnyRef)                                           { wrap {parent.print(x)} {super.print(x)} }
      override def format(format: String, args: AnyRef*)                      = { wrap {parent.format(   format, args:_*)} {super.format(   format, args:_*)}; this }
      override def format(l: java.util.Locale, format: String, args: AnyRef*) = { wrap {parent.format(l, format, args:_*)} {super.format(l, format, args:_*)}; this }
    }

    // \r\r -> \r, \r\n -> \n, \r -> \n
    class CRLFOutputStream(os: OutputStream) extends FilterOutputStream(os) {
      private[this] var lastb = -1
      override def write(b: Int): Unit = synchronized {
        (lastb, b) match { case (13,13) =>
                           case (13,10) => super.write(10)
                           case (13, _) => super.write(10); super.write(b)
                           case ( _,13) =>
                           case ( _, _) => super.write(b) }
        lastb = b
      }
    }

    file.getParentFile.mkdirs()
    Try(new FileOutputStream(file)) match {
      case Success(teeStream) =>
        teeStream.write(Array(0xEF.toByte, 0xBB.toByte, 0xBF.toByte)) // write UTF-8 BOF

        // both AnsiOutputStream and CRLFOutputStream are stateful, stdout and stderr must have own state
        val psOut = new TeePrintStream(System.out, new AnsiOutputStream(new CRLFOutputStream(teeStream)), "UTF-8")
        val psErr = new TeePrintStream(System.err, new AnsiOutputStream(new CRLFOutputStream(teeStream)), "UTF-8")
        System.setOut(psOut); scala.Console.setOut(psOut)
        System.setErr(psErr); scala.Console.setErr(psErr)
      case Failure(e) =>
        System.err.println(e)
    }
  }


  case class KeyEvent(down: Boolean, repeatCount: Short, vk: Short, scanCode: Short, ch: Char, controlKeyState: Int) {
    override def toString = f"KeyEvent(${if (down) "v"
                                         else "^"} $repeatCount%2d ${if (ch==0) "    "
                                                                     else if (0x20<=ch && ch<=0x7E) s" '$ch'"
                                                                     else f"${ch.toShort}%04X"} $vk%04X $scanCode%04X $controlKeyState%04X)"
    // how KeyEvent seen by System.in.read
    def toAnsi: Array[Byte] = (ch match {
      case _ if !down           => ""
      case 0x0D                 => "\n"
      case 0 => vk match {
        case 0x21 /* page up */ => "\u001B[5~"
        case 0x22 /* page dn */ => "\u001B[6~"
        case 0x23 /* end     */ => "\u001B[4~"
        case 0x24 /* home    */ => "\u001B[1~"
        case 0x25 /* left    */ => "\u001B[D"
        case 0x26 /* up      */ => "\u001B[A"
        case 0x27 /* right   */ => "\u001B[C"
        case 0x28 /* down    */ => "\u001B[B"
        case 0x2D /* insert  */ => "\u001B[2~"
        case 0x2E /* delete  */ => "\u001B[3~"
        //case 0x70 /* f1      */ => "\u001B[11~"
        //case 0x71 /* f2      */ => "\u001B[12~"
        //case 0x72 /* f3      */ => "\u001B[13~"
        //case 0x73 /* f4      */ => "\u001B[14~"
        //case 0x74 /* f5      */ => "\u001B[15~"
        //case 0x75 /* f6      */ => "\u001B[17~"
        //case 0x76 /* f7      */ => "\u001B[18~"
        //case 0x77 /* f8      */ => "\u001B[19~"
        //case 0x78 /* f9      */ => "\u001B[20~"
        //case 0x79 /* f10     */ => "\u001B[21~"
        //case 0x7A /* f11     */ => "\u001B[23~"
        //case 0x7B /* f12     */ => "\u001B[24~"
        case _                  => ""
      }
      case _                    => new String(Array[Char](ch))
    }) getBytes "UTF-8"

    //def leftAltPressed    = 0 != (controlKeyState & 0x0002) // The left ALT key is pressed.
    //def leftCtrlPressed   = 0 != (controlKeyState & 0x0008) // The left CTRL key is pressed.
    //def rightAltPressed   = 0 != (controlKeyState & 0x0001) // The right ALT key is pressed.
    //def rightCtrlPressed  = 0 != (controlKeyState & 0x0004) // The right CTRL key is pressed.
    //def shiftPressed      = 0 != (controlKeyState & 0x0010) // The SHIFT key is pressed.
  }


  trait Natives {
    def readKeyEvent: KeyEvent
    def setupSmartConsole()
    def getProcessArgsW(pid: Int): Array[String]
  }


  class LinuxNatives extends Natives {
    def readKeyEvent: KeyEvent = ???
    private[this] def isStdoutConsole = true // todo: isatty(1)
    private[this] def isStderrConsole = true // todo: isatty(2)
    //lazy val getConsoleCodepage: String = {
    //  val re = """[^.]+\.([a-zA-Z0-9_-]+)""".r // something like "en_US.UTF-8"
    //  System.getenv("LANG") match {
    //    case re(codepage) => codepage
    //    case _            => "UTF-8"
    //  }
    //}
    def setupSmartConsole() {
      // strip ANSI-codes if stdout/err goes to file/pipe
      val smartout = if (isStdoutConsole) System.out else new PrintStream(new /*Html*/AnsiOutputStream(System.out), true, "UTF-8")
      val smarterr = if (isStderrConsole) System.err else new PrintStream(new /*Html*/AnsiOutputStream(System.err), true, "UTF-8")
      System.setOut(smartout); scala.Console.setOut(smartout)
      System.setErr(smarterr); scala.Console.setErr(smarterr)
    }
    def getProcessArgsW(pid: Int): Array[String] = ???
  }


  trait WindowsNatives extends Natives {
    import com.sun.jna

    val FILE_TYPE_CHAR    = 2
    val STD_INPUT_HANDLE  = -10
    val STD_OUTPUT_HANDLE = -11
    val STD_ERROR_HANDLE  = -12

    trait NtDll extends jna.platform.win32.NtDll {
      def NtQueryInformationProcess(processHandle:            jna.platform.win32.WinNT.HANDLE,
                                    processInformationClass:  Int,
                                    processInformation:       jna.Pointer,
                                    processInformationLength: Int,
                                    returnLength:             jna.ptr.IntByReference): Int
    }
    trait Kernel32 extends jna.platform.win32.Kernel32 {
      def SetConsoleCP             (n: Int): Boolean
      def GetConsoleCP             (): Int
      def SetConsoleOutputCP       (n: Int): Boolean
      def GetConsoleOutputCP       (): Int
      def GetStdHandle             (n: Int): jna.platform.win32.WinNT.HANDLE
      def GetConsoleMode           (hFile:          jna.platform.win32.WinNT.HANDLE, lpMode:        jna.ptr.IntByReference): Boolean
      def SetConsoleMode           (hFile:          jna.platform.win32.WinNT.HANDLE, dwMode:        Int                   ): Boolean
      def SetConsoleTextAttribute  (hConsoleOutput: jna.platform.win32.WinNT.HANDLE, wAttributes:   Int                   ): Boolean
      def ReadConsoleInput         (hConsoleInput:  jna.platform.win32.WinNT.HANDLE, lpBuffer:      java.nio.Buffer, nLength: Int,          lpNumberOfEventsRead: jna.ptr.IntByReference): Boolean
      def ReadProcessMemory        (hProcess:       jna.platform.win32.WinNT.HANDLE, lpBaseAddress: jna.Pointer,     lpBuffer: jna.Pointer, nSize:                Int,                    lpNumberOfBytesRead: jna.ptr.IntByReference): Boolean
    }
    trait Shell32 extends jna.platform.win32.Shell32 {
      def CommandLineToArgvW       (cmdLine:        String,                          pNumArgs:      jna.ptr.IntByReference): jna.Pointer
    }
    val ntdll    = jna.Native.loadLibrary("ntdll",    classOf[NtDll   ], jna.win32.W32APIOptions.UNICODE_OPTIONS).asInstanceOf[NtDll   ]
    val kernel32 = jna.Native.loadLibrary("kernel32", classOf[Kernel32], jna.win32.W32APIOptions.UNICODE_OPTIONS).asInstanceOf[Kernel32]
    val shell32  = jna.Native.loadLibrary("shell32",  classOf[Shell32 ], jna.win32.W32APIOptions.UNICODE_OPTIONS).asInstanceOf[Shell32 ]

    // perl.exe cannot forward the unicode args correctly, but we can read unicode strings from its memory
    private[this] def getProcessCommandLineW(pid: Int): String = {
      val hProcess = kernel32.OpenProcess(0x0400/*PROCESS_QUERY_INFORMATION*/|0x0010/*PROCESS_VM_READ*/, false, pid)
      try {
        val pbi          = new jna.Memory(if (jna.Pointer.SIZE==4) 0x18 else 0x30)
        val returnLength = new jna.ptr.IntByReference(0)
        require(0 == ntdll.NtQueryInformationProcess(hProcess, 0/*ProcessBasicInformation*/, pbi, pbi.size.toInt, returnLength))

        val pebBaseAddress = pbi.getPointer(if (jna.Pointer.SIZE==4) 0x04 else 0x08)
        val peb            = new jna.Memory(if (jna.Pointer.SIZE==4) 0x14 else 0x28)
        require(kernel32.ReadProcessMemory(hProcess, pebBaseAddress, peb, peb.size.toInt, null))

        val processParametersBaseAddress = peb.getPointer(if (jna.Pointer.SIZE==4) 0x10 else 0x20)
        require(processParametersBaseAddress != jna.Pointer.NULL, "it does not work with 64-bit perl.exe and 32-bit java.exe")
        val processParameters = new jna.Memory(if (jna.Pointer.SIZE==4) 0x48 else 0x80)
        require(kernel32.ReadProcessMemory(hProcess, processParametersBaseAddress, processParameters, processParameters.size.toInt, null))

        val commandLineLen         = processParameters.getShort  (if (jna.Pointer.SIZE==4) 0x40 else 0x70)
        val commandLineBaseAddress = processParameters.getPointer(if (jna.Pointer.SIZE==4) 0x44 else 0x78)
        val commandLine            = new jna.Memory(commandLineLen)
        require(kernel32.ReadProcessMemory(hProcess, commandLineBaseAddress, commandLine, commandLine.size.toInt, null))

        (for (i <- 0 until commandLine.size.toInt by 2) yield commandLine.getShort(i).toChar).mkString
      } finally
        kernel32.CloseHandle(hProcess)
    }

    private[this] def splitCommandLine(commandLine: String): Array[String] = {
      val numArgs = new jna.ptr.IntByReference(0)
      val p = shell32.CommandLineToArgvW(commandLine, numArgs)
      val args = p.getStringArray(0, numArgs.getValue, true)
      require(args(0).contains("perl") && args(1).matches(".*\\.(scala|java|pl)"), args.toList)
      args.slice(2, args.length)
    }

    def getProcessArgsW(pid: Int): Array[String] = splitCommandLine(getProcessCommandLineW(pid))
  }


  class WindowsConsoleNatives extends WindowsNatives {
    import com.sun.jna.ptr.IntByReference
    import com.sun.jna.platform.win32._

    private[this] val hstdin:  WinNT.HANDLE = kernel32.GetStdHandle(STD_INPUT_HANDLE)
    locally {
      val mode = new IntByReference(0)
      val rc = kernel32.GetConsoleMode(hstdin, mode)
      kernel32.SetConsoleMode(hstdin, mode.getValue & ~0x1C) // turn off mouse input, window input and echo.
                                                             // line mode is out of control,
                                                             // it is effectively off because ReadConsoleInput is used, not ReadConsole or ReadFile
    }

    def readKeyEvent: KeyEvent = {
      val d = java.nio.ByteBuffer allocateDirect 20 order java.nio.ByteOrder.LITTLE_ENDIAN
      val read = new IntByReference(0)
      val rc = kernel32.ReadConsoleInput(hstdin, d, 1, read)
      require(rc && read.getValue == 1)
      if (d.getShort(0) == 1) {
        val ke = KeyEvent(d.getInt(4) != 0, d.getShort(8), d.getShort(10), d.getShort(12), d.getShort(14).toChar, d.getInt(16))
        //println(s"ke=$ke")
        ke
      } else
        readKeyEvent
    }


    def setupSmartConsole() {
      val hstdout: WinNT.HANDLE = kernel32.GetStdHandle(STD_OUTPUT_HANDLE)
      val hstderr: WinNT.HANDLE = kernel32.GetStdHandle(STD_ERROR_HANDLE)
      val isStdoutConsole: Boolean = kernel32.GetFileType(hstdout) == FILE_TYPE_CHAR
      val isStderrConsole: Boolean = kernel32.GetFileType(hstderr) == FILE_TYPE_CHAR

      // default "gray on black" (todo: also do in on Ctrl-C exit)
      if (isStdoutConsole)
        kernel32.SetConsoleTextAttribute(hstdout, 0x07)
      else if (isStderrConsole)
        kernel32.SetConsoleTextAttribute(hstderr, 0x07)

      kernel32.SetConsoleOutputCP(65001) // try to force UTF-8 (it works only with truetype font in console, todo: GetCurrentConsoleFontEx)
      kernel32.SetConsoleCP(65001) // try to force UTF-8
      val consoleCharset: String = kernel32.GetConsoleOutputCP() match {
                                     case 65001 => "UTF-8"
                                     case cp    => require(false, s"be careful, it was not checked with non-UTF8 console ${kernel32.GetConsoleOutputCP()}")
                                                   "CP" + cp
                                   }

      val smartin = new InputStream {
        private[this] var defer = List.empty[Byte]
        def read: Int = synchronized {
          defer match {
            case hd::rest => defer=rest; hd
            case Nil      => defer=readKeyEvent.toAnsi.toList; read
          }
        }
        override def read(b: Array[Byte], off: Int, len: Int): Int = {
          for (i <- 0 until len) {
            read match {
              case -1 if i==0 => return -1
              case -1         => return i
              case '\n'       => b(off+i)='\n'; return i+1
              case x          => b(off+i)=x.toByte
            }
          }
          len
        }
      }


      // System.out.write() (FileOutputStream(FileDescriptor.out).write inside it) prints extra garbage with "chcp 65001"
      // because kernel32.WriteFile returns number of written _characters_, then jvm interprets it as count of _bytes_ and retries write
      //
      // todo: replace WindowsAnsiOutputStream with something else, it is too slow (AnsiOutputStream is slow too)
      lazy val waos = new WindowsAnsiOutputStream(new OutputStream {
        private[this] val houtput = if (isStdoutConsole) hstdout else hstderr
        private[this] var defer   = Array.emptyByteArray

        def write(b: Int) {
          write(Array(b.toByte), 0, 1)
        }
        override def write(b: Array[Byte], off: Int, len: Int): Unit = synchronized {
          val d = defer ++ b.slice(off, off+len)
          // bytes of an utf-8 character must be written in a single WriteFile() call
          // todo: also ANSI-sequences, single 0x1B (or 0x1B following by unicode) crashes conemu.dll hook
          defer = if (consoleCharset == "UTF-8") {
                 if (d.length>0 && (d(d.length-1)&0xC0)==0xC0) d.slice(d.length-1, d.length) // defer last char    (prefix of 2+ chars sequence)
            else if (d.length>1 && (d(d.length-2)&0xE0)==0xE0
                                && (d(d.length-1)&0xC0)==0x80) d.slice(d.length-2, d.length) // defer 2 last chars (prefix of 3+ chars sequence)
            else if (d.length>2 && (d(d.length-3)&0xF0)==0xF0
                                && (d(d.length-2)&0xC0)==0x80
                                && (d(d.length-1)&0xC0)==0x80) d.slice(d.length-3, d.length) // defer 3 last chars (prefix of 4+ chars sequence)
            else if (d.length>3 && (d(d.length-4)&0xF8)==0xF8
                                && (d(d.length-3)&0xC0)==0x80
                                && (d(d.length-2)&0xC0)==0x80
                                && (d(d.length-1)&0xC0)==0x80) d.slice(d.length-4, d.length) // defer 4 last chars (prefix of 5+ chars sequence)
            else if (d.length>4 && (d(d.length-5)&0xFE)==0xFC
                                && (d(d.length-4)&0xC0)==0x80
                                && (d(d.length-3)&0xC0)==0x80
                                && (d(d.length-2)&0xC0)==0x80
                                && (d(d.length-1)&0xC0)==0x80) d.slice(d.length-5, d.length) // defer 5 last chars (prefix of 6 chars sequence)
            else                                               Array.emptyByteArray
          } else {
            Array.emptyByteArray
          }
          val bytesToWrite = d.length - defer.length
          require(bytesToWrite >= 0)
          if (bytesToWrite != 0) {
            for (i <- 0 until bytesToWrite-2 if d(i)==0xE2.toByte && d(i+1)==0x97.toByte && d(i+2)==0x8F.toByte) { d(i+1)=0x80.toByte; d(i+2)=0xA2.toByte } // u25CF BLACK CIRCLE -> u2022 BULLET (for PragmataPro)
            kernel32.WriteFile(houtput, d, bytesToWrite, new IntByReference(0), null)
          }
        }
      })


      val smartout = if (isStdoutConsole) new PrintStream(waos, true, consoleCharset)
                     else                 new PrintStream(new /*Html*/AnsiOutputStream(System.out), true, "UTF-8")

      // waos does not work with stdout redirected (issue: https://github.com/fusesource/jansi/issues/18)
      val smarterr = if (isStderrConsole) new PrintStream(if (!isStdoutConsole) new AnsiOutputStream(System.err) else waos, true, consoleCharset)
                     else                 new PrintStream(new /*Html*/AnsiOutputStream(System.err), true, "UTF-8")

      // set smart stdin/stdout/stderr
      System.setIn (smartin);  scala.Console.setIn (smartin)
      System.setOut(smartout); scala.Console.setOut(smartout)
      System.setErr(smarterr); scala.Console.setErr(smarterr)
    }
  }


  class WindowsGuiNatives extends WindowsNatives {
    def setupSmartConsole() {}
    def readKeyEvent: KeyEvent = ???
  }


  val natives: Natives = if (!Properties.isWin)           new LinuxNatives
                         else if (System.console != null) new WindowsConsoleNatives
                         else                             new WindowsGuiNatives

}
import LauncherUtils._


//class Aes(password: Array[Byte]) {
//  private[this] val key: javax.crypto.SecretKey = new javax.crypto.spec.SecretKeySpec({ val md = java.security.MessageDigest getInstance "SHA1"; md update password; md.digest take 16 }, "AES")
//  def encrypt(value:    Array[Byte]): Array[Byte] = { val ci=javax.crypto.Cipher.getInstance("AES/ECB/PKCS5Padding", "SunJCE"); ci.init(javax.crypto.Cipher.ENCRYPT_MODE, key); ci.doFinal(value)    }
//  def decrypt(encypted: Array[Byte]): Array[Byte] = { val ci=javax.crypto.Cipher.getInstance("AES/ECB/PKCS5Padding", "SunJCE"); ci.init(javax.crypto.Cipher.DECRYPT_MODE, key); ci.doFinal(encypted) }
//}


trait Memo {
  def getOrElseCreate(name: String, doCreate: File=>Unit, doOnExists: =>Unit = ()): File
  def memo[V](key: SHAny[_])(slowv: =>V): V = {
    val name = sha1(key) + ".memo"
    val fin = new FileInputStream(getOrElseCreate(name, { f => val fos = new FileOutputStream(f)
                                                               try     new ObjectOutputStream(fos) writeObject slowv
                                                               finally fos.close() }))
    try     new ObjectInputStream(fin).readObject.asInstanceOf[V]
    finally fin.close()
  }
  def memo2[V, V2](key: SHAny[_],
                   toSerialiazable:   V =>V2,
                   fromSerialiazable: V2=>V )(slowv: =>V): V = fromSerialiazable(memo[V2](key)(toSerialiazable(slowv)))
}

class FileCache(val readonlyDirs: List[File], val readwriteDir: File, val mirrorDir: Option[File]) extends Memo {
  readwriteDir.mkdirs()
  def getOrElseCreate(name: String, doCreate: File=>Unit, doOnExists: =>Unit = ()): File = {
    val rc = (readwriteDir :: readonlyDirs).distinct map (new File(_, name)) find (_.exists) match {
               case Some(target) => doOnExists
                                    target
               case None         => val target = new File(readwriteDir, name)
                                    target.getParentFile.mkdirs()
                                    val tmp = new File(target.getParentFile, s"${java.util.UUID.randomUUID.toString take 8}_${target.getName}")
                                    doCreate(tmp)
                                    require(tmp.renameTo(target) || target.exists(), s"renameTo($tmp, $target) failed")
                                    target
             }
    for (dir <- mirrorDir) {
      java.nio.file.Files.copy( rc.toPath, new File(dir, rc.getName).toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING )
    }
    rc
  }
}

object Cache extends FileCache(readonlyDirs = Option(System getenv "SCALAUNCHER_RO_MEMO").toList flatMap (_ split File.pathSeparator) filter (_.nonEmpty) map (new File(_)) filter (_.isDirectory),
                               readwriteDir = { val homeDir=new File(System getenv (if (Properties.isWin) "USERPROFILE" else "HOME"))
                                                new File(if (homeDir.exists) homeDir else new File("/var/tmp"), ".memo") // /var/tmp under "nixbld"
                                              },
                               mirrorDir    = Launcher.cmdline.memoMirrorDir)


object Compiler {
  import scala.tools.nsc.{Settings, Global}
  import scala.tools.nsc.reporters.StoreReporter
  import scala.tools.nsc.io.VirtualFile
  import scala.reflect.internal.util.{BatchSourceFile, NoPosition}
  import javax.tools.{Diagnostic, DiagnosticListener, JavaFileObject, SimpleJavaFileObject, ToolProvider}

  // serializable form of scalac message (StoreReporter#Info) or javac message (Diagnostic[_])
  private[Compiler] case class Msg(path: Option[String]/*todo? refer to exact source in Request*/, line: Long, column: Long, msg: String, severity: String) {
    def this(info: StoreReporter#Info) = this(
      path    = Try{ val p = info.pos.source.file.canonicalPath     // scala-2.10 throws here on no path
                     if (p=="<no file>") throw new RuntimeException // let scala 2.11 crash here
                     if (p startsWith "__") "/"+p.drop(2) else p    // bug: scala compiler throws assert if VirtualFile has absolute path, that is why '/' has been strippeed and here added back
                   }.toOption,
      line    = Try(info.pos.line.toLong)   getOrElse 0,
      column  = Try(info.pos.column.toLong) getOrElse 0,
      msg     = info.msg,
      severity= info.severity.toString)
    def this(diagnostic: Diagnostic[_ <: JavaFileObject]) = this(
      path    = diagnostic.getSource       match { case null                        => None
                                                   case  sjfo: SimpleJavaFileObject => Some(new File(sjfo.toUri).getPath) // strip file:/
                                                   case   jfo:       JavaFileObject => Some(jfo.toUri.toString) },        // e.g. com.sun.tools.javac.file.ZipFileIndexArchive.ZipFileIndexFileObject and "jar:file:/C:/surge/lib/rackspace-clouddns-1.7.3.jar!/org/jclouds/rackspace/clouddns/v1/CloudDNSApi.class"
      line    = diagnostic.getLineNumber   match { case Diagnostic.NOPOS=>0 case x=>x },
      column  = diagnostic.getColumnNumber match { case Diagnostic.NOPOS=>0 case x=>x },
      msg     = diagnostic.getMessage(null),
      severity= diagnostic.getKind.toString) // ERROR, MANDATORY_WARNING, NOTE, OTHER, WARNING
  }

  sealed trait Source { // surge.Brunnen?
              def relPath:        String // will be visible as __FILE__
              def size:           Long
    protected def binaryContent:  Array[Byte]
              def copyTo(os: OutputStream)
              def sha:            String
    override  def toString         = s"${getClass.getSimpleName}($relPath, $size bytes)"
  }

  object cleanupSourceJavaScala {
    private val RE1 = ("^(::)?#![\\u0000-\\uFFFF]+?!" + "#").r
    private val RE2 = """((?:^|\n)[ \t]*import[ \t]+)(`([^`]+)`[ \t]*,?)""".r       // todo: ignore inside of /**/-comments
    def apply(binary: Array[Byte]): (String, List[String]) = {
      val str  = new String(binary, "UTF-8") stripPrefix "\uFEFF"             // keep \n and preserve line numbers in error messages
      val str2 = RE1.replaceAllIn(str, _.matched.replaceAll("""[^\n]""", "")) // '!' and '#' are splited to help IntelliJ with parsing this file
      var magicImports = List.empty[String]
      val str3 = RE2.replaceAllIn(str2, { m =>
                    magicImports ::= m.group(3)
                    if (m.group(2) endsWith ",") "$1" + " "*m.group(2).length else "\n" // cut off magic imports
                 })
      (str3, magicImports.reverse)
    }
  }

  sealed trait `.java` extends Source {
    lazy val (stringContent, _) = cleanupSourceJavaScala(binaryContent)
  }
  sealed trait `.scala` extends Source {
    lazy val (stringContent, magicImports) = cleanupSourceJavaScala(binaryContent)
  }
  sealed trait `.jar` extends FileSource

  sealed trait InMemSource extends Source {
                  def size                     = binaryContent.length
                  def copyTo(os: OutputStream) { os write binaryContent }
             lazy val sha                      = sha1(relPath, binaryContent) // `relPath` goes to exception info and to __FILE__
  }
  case class ScalaInMemSource private[Compiler](relPath: String, binaryContent: Array[Byte]) extends InMemSource with `.scala`
  case class JavaInMemSource  private[Compiler](relPath: String, binaryContent: Array[Byte]) extends InMemSource with `.java`
  case class OtherInMemSource private[Compiler](relPath: String, binaryContent: Array[Byte]) extends InMemSource

  sealed trait FileSource extends Source {
                   def file:     File // = mkAbsFile(base, relPath)
                   def size                     = file.length
                   def copyTo(os: OutputStream) { val is=new FileInputStream(file); try copyStream(os, is) finally is.close() }
              lazy val binaryContent            = readFileToByteArray(file)
  }
  case class DirSource          private[Compiler](relPath: String, file: File)  extends FileSource { // directory on classpath may contain .class, .properties and other file types
             lazy val sha  = Joint.quickHash(file :: Nil) // hash on meta info, no content
  }
  case class JarFileSource      private[Compiler](relPath: String, file: File)  extends FileSource with `.jar` {
             require(!file.isDirectory && file.getName.endsWith(".jar"), file)
             lazy val sha  = sha1(readFileToByteArray(file, skip=file.length-30000 max 0))            // avoid reading huge jars reading zip directory must be enough for uniqueness checking (it has crc32 of files)
  }
  case class MyJarFileSource    private[Compiler](relPath: String, file: File)  extends FileSource with `.jar` {               // launcher.jar or compiled by it
             require(!file.isDirectory && file.getName.matches("[0-9a-f]{40}\\.jar"), file)
             lazy val sha  = file.getName stripSuffix ".jar"                                          // TODO? also include java version. scala version is included via dependedcy names
  }
  case class MavenJarFileSource private[Compiler](relPath: String, artifact: Maven.Artifact)  extends FileSource with `.jar` {
                  def file = artifact.jarFile.get
             lazy val sha  = sha1(artifact.toString, file.length)  // assume stable content
  }
  case class ScalaFileSource    private[Compiler](relPath: String, file: File)  extends FileSource with `.scala` {
    override lazy val binaryContent = readFileToByteArray(file)
             lazy val sha  = sha1(relPath, binaryContent) // relPath goest to exception info and to __FILE__
  }
  case class JavaFileSource     private[Compiler](relPath: String, file: File)  extends FileSource with `.java` {
    override lazy val binaryContent = readFileToByteArray(file)
             lazy val sha  = sha1(relPath, binaryContent) // relPath goest to exception info and to __FILE__
  }
  case class OtherFileSource    private[Compiler](relPath: String, file: File)  extends FileSource { // .xml, .domains, ..
    override lazy val binaryContent = readFileToByteArray(file)
             lazy val sha  = sha1(relPath, binaryContent)
  }
  object InMemSource {
    def apply(relPath: String, binaryContent: Array[Byte]) = relPath match {
                                                               case FileSource.reScalaPath() => ScalaInMemSource(relPath, binaryContent)
                                                               case FileSource.reJavaPath()  => JavaInMemSource(relPath, binaryContent)
                                                               case _                        => OtherInMemSource(relPath, binaryContent)
                                                             }
  }
  object FileSource {
    private[launcher] val reMyJar     = "(?:.+/)?[0-9a-f]{40}\\.jar".r
    private[launcher] val reScalaPath = ".+\\.scala(?:\\.w?pl)?(?:-wrapped)*".r
    private[launcher] val reJavaPath  =  ".+\\.java(?:\\.w?pl)?(?:-wrapped)*" .r
    private[launcher] def fromFileAndRelpath(file: File, relPath: String): FileSource = (file, file.getAbsolutePath.replace("\\", "/")) match {
      case (Maven.Artifact.Path(artifact), _) => MavenJarFileSource(relPath, artifact)
      case _              if file.isDirectory => DirSource         (relPath, file)
      case (_, reMyJar())                     => MyJarFileSource   (relPath, file)
      case (_, path) if path endsWith ".jar"  => JarFileSource     (relPath, file)
      case (_, reScalaPath())                 => ScalaFileSource   (relPath, file)
      case (_, reJavaPath())                  => JavaFileSource    (relPath, file)
      case _                                  => OtherFileSource   (relPath, file)
    }
    def apply(base: File, relPath: String): FileSource = fromFileAndRelpath(mkAbsFile(base, relPath), relPath)
  }

  //todo: case class Remote(...) extends Source /* http:// ftp:// scp:// hdfs:// */


  private[this] case class Request(outputJar: File, sources: List[Source], scalacArguments: List[String]) {
    require(outputJar.isAbsolute, s"outputJar is not absolute `$outputJar'")
  }
  private[this] trait Response
  private[this] case class ResponseOK       (messages: List[Msg]) extends Response
  private[this] case class ResponseFail     (messages: List[Msg]) extends Response
  private[this] case class ResponseException(cause:    Throwable) extends Response // `cause` is e.g FileNotFoundException


  private[this] def compileInProcess(request: Request): Response = {
    request.outputJar.getParentFile.mkdirs()

    def compileJava(scalajar: Option[File], scalamessages: List[Msg]): Response = {
      val tmpDir = new File(System getProperty "java.io.tmpdir", request.outputJar.getName + ".classes")
      tmpDir.mkdir()
      try {
        val compiler = ToolProvider.getSystemJavaCompiler
        require(compiler != null, "ToolProvider.getSystemJavaCompiler()==null; JDK not installed")

        // todo: find out how to avoid creation of .class-files on disk
        val javacMsgs = List.newBuilder[Msg]
        val success = compiler.getTask(null,                                                                          // Writer out,
                                       null,                                                                          // JavaFileManager fileManager,
                                       new DiagnosticListener[JavaFileObject] {
                                         def report(diagnostic: Diagnostic[_ <: JavaFileObject]) { javacMsgs += new Msg(diagnostic) }
                                       },                                                                             // DiagnosticListener<? super JavaFileObject> diagnosticListener,
                                       scala.collection.JavaConversions.asJavaIterable(List(//"-verbose",             // Iterable<String> options,
                                                                                              "-g",                   // * Generate all debugging info
                                                                                            //"-source",    "1.7",    // * Provide source compatibility with specified release
                                                                                              "-d",         tmpDir.getAbsolutePath,
                                                                                            // todo: classpath-of-depjars -> libprovider
                                                                                              "-classpath", mkClasspathString(request.sources.flatMap{ case _:`.java`    => None
                                                                                                                                                       case _:`.scala`   => None
                                                                                                                                                       case s:DirSource  => Some(s.file)
                                                                                                                                                       case s:`.jar`     => Some(s.file)
                                                                                                                                                       case _            => ??? } ++ scalajar))),

                                       null,                                                                          // Iterable<String> classes,
                                       scala.collection.JavaConversions.asJavaIterable(request.sources flatMap {      //for (s <- request.javaSources) // Iterable<? extends JavaFileObject> compilationUnits
                                                                                         case s:`.java` => Some(new SimpleJavaFileObject(new File(s.relPath.replaceAll("\\.java(\\.w?pl)?(-wrapped)*$", ".java")).toURI, JavaFileObject.Kind.SOURCE) {
                                                                                                                  override def getCharContent(ignoreEncodingErrors: Boolean) = s.stringContent
                                                                                                                })
                                                                                         case _         => None })
                                      ).call()
        val messages = scalamessages ++ javacMsgs.result
        if (success) {
          // outputJar = scalajar ++ .class-files
          addFilesToZip(request.outputJar, scalajar, for (file <- deleteDirectory(tmpDir).view) yield { // why view?
                                                       if (!file.getName.endsWith(".class"))
                                                         System.err.println(s"unexpected file `${file.getAbsolutePath}' in the target directory") // can be "META-INF/services/org.apache.accumulo.start.spi.KeywordExecutable"
                                                       val relpath = file.getPath.stripPrefix(tmpDir.toString).replace('\\', '/').stripPrefix("/")
                                                       (relpath, OtherFileSource(relpath, file))
                                                     })
          ResponseOK(messages)
        } else {
          ResponseFail(messages)
        }
      } finally {
        for (_ <- deleteDirectory(tmpDir)) {}
      }
    }

    val tmpJar = new File(System getProperty "java.io.tmpdir", request.outputJar.getName + ".2.jar")
    try {
      if (request.sources exists (_.isInstanceOf[`.scala`])) {
        val alsoCompileJava = request.sources exists (_.isInstanceOf[`.java`])
        val settings = new Settings
        var messages = List.empty[Msg]
        withCaptureStdoutAndStderr(printedBytes => messages ::= Msg(None, 0, 0, new String(printedBytes, "UTF-8"), "ERROR")) {
          settings.processArguments(request.scalacArguments, processAll = true) // it prints parse error
          // todo: classpath-of-depjars -> libprovider
        }
        val cp  = request.sources flatMap { case _:`.java`   => None
                                            case _:`.scala`  => None
                                            case s:DirSource => Some(s.file)
                                            case s:`.jar`    => Some(s.file)
                                            case s           => require(false, s); ??? }
        val src = request.sources flatMap { case s:`.scala`  => Some(new BatchSourceFile(new VirtualFile(if (s.relPath startsWith "/") "__"+s.relPath.drop(1) else s.relPath), s.stringContent)) // bug: scala compiler throw assert if VirtualFile's fake path is absolute, so stripPrefix "/"
                                            case s:`.java`   => Some(new BatchSourceFile(new VirtualFile(if (s.relPath startsWith "/") "__"+s.relPath.drop(1) else s.relPath), s.stringContent))
                                            case _:`.jar`    => None
                                            case _:DirSource => None
                                            case s           => require(false, s); ??? }
      //settings.plugin.value    = cp filter (_.getName startsWith "paradise_") map (_.getCanonicalPath)
        settings.classpath.value = mkClasspathString(cp)
        settings.outputDirs setSingleOutput (if (alsoCompileJava) tmpJar else request.outputJar).getAbsolutePath // todo: virtual dir?
        if (messages.nonEmpty) {
          ResponseFail(messages.reverse)
        } else {
          val reporter = new StoreReporter
          withCaptureStdoutAndStderr(printedBytes => reporter.info(NoPosition, new String(printedBytes, "UTF-8"), force = false) /* SI-6866 */) { // macroses can print to stdout/stderr
            val compiler = new Global(settings, reporter)
            new compiler.Run compileSources src
          }

          val scalamessages=reporter.infos.toList.map(new Msg(_))
          if (reporter.hasErrors) {
            ResponseFail(scalamessages)
          } else if (alsoCompileJava) { // do we still have java sources to compile...
            compileJava(scalajar=Some(tmpJar), scalamessages=scalamessages)
          } else {
            ResponseOK(scalamessages)
          }
        }
      } else {
        compileJava(scalajar=None, scalamessages=Nil)
      }
    } catch {
      case e: Throwable => ResponseException(e)
    } finally {
      tmpJar.delete()
    }
  }

  // always run 64-bit java for compiling server
  private[this] def cmdline = (if (Properties.isWin) Array("cmd.exe", "/c", "start", "/MIN",   System.getProperty("java.home").replace("_i386", "") + "\\bin\\java.exe")
                               else                  Array("setsid", "xterm", "-iconic", "-e", System.getProperty("java.home")                      + "/bin/java")) ++
                              Array("-cp", mkClasspathString(classpathFiles(this.getClass.getClassLoader, withSystemJars=false) map (_.file)), "-Xmx2000m", "-Xss4m", "ms.webmaster.launcher.Compiler", "--daemon")
  private[this] def compilerVersionDigest: String = (new Throwable).getStackTrace()(0).getFileName // Launcher.pl.scala source file name, actually sha1(content of Launcher.pl.scala)
//private[this] def compilerVersionDigest: String = Properties.versionNumberString

  // the compiling server (todo: exit on long inactivity)
  def main(args: Array[String]) {
    args.toList match {
      case "--daemon" :: Nil =>
        val listener = new java.net.ServerSocket(7779, 10, java.net.InetAddress getByName "127.0.0.1")
        while(true) {
          val socket = listener.accept()
          try {
            val ois = new ObjectInputStream(socket.getInputStream)
            if (compilerVersionDigest != ois.readObject.asInstanceOf[String])
              scala.sys.exit(1)
            val request = ois.readObject.asInstanceOf[Request]
            println(request)
            new ObjectOutputStream(socket.getOutputStream) writeObject compileInProcess(request)
          } catch {
            case ex: Throwable => ex.printStackTrace() // todo: ignore only network errors, but scala.sys.exit on OOM, etc
          } finally {
            socket.close()
            Runtime.getRuntime.gc() // force close files (jars on classpath)
          }
        }
      case _ => ???
    }
  }

  private[this] def compileOnServer(request: Request): Response = {  // todo: use Try
    def doit() = Try[Response] {
                   val socket = new java.net.Socket("127.0.0.1", 7779)
                   try {
                     val os = new ObjectOutputStream(socket.getOutputStream)
                     os writeObject compilerVersionDigest
                     os writeObject request.copy(sources = request.sources.map {
                                                             case mjfs: MavenJarFileSource => JarFileSource(mjfs.relPath, mjfs.file) // Maven.Artifact and thus MavenJarFileSource is not Serializable
                                                             case s => s
                                                           })
                     os.flush()
                     new ObjectInputStream(socket.getInputStream).readObject.asInstanceOf[Response]
                   } finally {
                     socket.close() // java.net.* can be thrown
                   }
                 }

    (doit() recoverWith { case _: java.net.ConnectException | _:java.net.SocketException =>
                            println(s"${RED_B}running ${cmdline mkString " "}$RESET")
                            Runtime.getRuntime.exec(cmdline)
                            doit()
          } recoverWith { case _: java.net.ConnectException | _:java.net.SocketException =>
                            Thread.sleep(5000)           // the compiler may be starting slowly, give it some time to start
                            doit()
              } recover { case e: Throwable =>
                            println(s"$e fall back to in-proc compilation") // it may result in StackOverflow error due to -Xss absense (todo: -Xss4m for all. not only when NOSERVER=>1)
                            compileInProcess(request)
                        }).get
  }

  class Exception extends RuntimeException("compiler error") with scala.util.control.NoStackTrace

  private[this] def compressedSourceFilenames(sources: List[Source]): List[String] = { // todo: same output in compiling server console
    val reprefix = """(.+/)[^/]+""".r  ; def getprefix(s: String) = s match { case reprefix(x) => x case _ => "" }
    val resuffix = """.+(\.[^/]+)""".r ; def getsuffix(s: String) = s match { case resuffix(x) => x case _ => "" }

    sources.groupBy{ case src:DirSource           => (4, getprefix(src.relPath))
                     case src:MavenJarFileSource  => (3, if (src.artifact.groupId==src.artifact.artifactId && src.artifact.groupId.startsWith("commons-")) "commons-" else src.artifact.groupId)
                     case src:`.jar`              => (2, getprefix(src.relPath))
                     case src:FileSource          => (1, getprefix(src.relPath))
                     case src:InMemSource         => (0, getprefix(src.relPath)) }.toList.sortBy(_._1) flatMap {
      case ((code@(0|1|2|3|4), pfx: String), srcs: List[Source]) =>
        val relpaths = srcs.map(_.relPath).sorted
        val (prefix: String, coloredFiles: List[String], suffix: String) = code match {
          case 4 =>                                                            (pfx, relpaths.map(MAGENTA+BOLD+ _.stripPrefix(pfx)                  +RESET), "/")
          case 2 => val sfx = relpaths.map(getsuffix).allthesame getOrElse ""; (pfx, relpaths.map(MAGENTA     + _.stripPrefix(pfx).stripSuffix(sfx) +RESET), sfx)
          case 1 => val sfx = relpaths.map(getsuffix).allthesame getOrElse ""; (pfx, relpaths.map(CYAN        + _.stripPrefix(pfx).stripSuffix(sfx) +RESET), sfx)
          case 0 => val sfx = relpaths.map(getsuffix).allthesame getOrElse ""; (pfx, relpaths.map(RED         + _.stripPrefix(pfx).stripSuffix(sfx) +RESET), sfx)
          case 3 => val artifacts: List[Maven.Artifact] = srcs.map(_.asInstanceOf[MavenJarFileSource].artifact).sorted
                    val D=BLACK+BOLD+":"+RESET
                    (artifacts, artifacts.map(a=>a.artifactId indexOf '-' match { case -1 => a.artifactId case i => a.artifactId take i+1 }).allthesame, artifacts.map(_.version).allthesame) match {
                      case (      _, Some(apx), _      ) if artifacts.forall(a=>a.groupId==a.artifactId) => (      MAGENTA+apx+RESET,   artifacts.map(a=> MAGENTA+a.artifactId.stripPrefix(apx)+RESET +D  +a.version      ), ""  ) // log4j:1.2.17
                      case (      _, None     , Some(v))                                                 => (pfx+D,                     artifacts.map(a=> MAGENTA+a.artifactId                 +RESET                     ), D +v) // org.bouncycastle:{bcpg-jdk15on,bcpkix-jdk15on,bcprov-jdk15on}:1.54
                      case (      _, None     , None   )                                                 => (pfx+D,                     artifacts.map(a=> MAGENTA+a.artifactId                 +RESET +D  +a.version      ), ""  ) // org.apache.httpcomponents:{httpclient:4.2.5,httpcore:4.2.4}
                      case (      _, Some(apx), Some(v))                                                 => (pfx+D+MAGENTA+apx+RESET,   artifacts.map(a=> MAGENTA+a.artifactId.stripPrefix(apx)+RESET                     ), D +v) // org.apache.curator:curator-{client,framework,recipes}:2.7.3
                      case (_::_::_, Some(apx), None   ) if artifacts.forall(_.artifactId==apx)          => (pfx+D+MAGENTA+apx+RESET+D, artifacts.map(a=>                                              RED+a.version+RESET), ""  ) // com.google.code.findbugs:jsr305:{1.3.9,3.0.0} - major version conflict?
                      case (      _, Some(apx), None   )                                                 => (pfx+D+MAGENTA+apx+RESET,   artifacts.map(a=> MAGENTA+a.artifactId.stripPrefix(apx)+RESET +D  +a.version      ), ""  ) // org.apache.commons:commons-{compress:1.4.1,lang3:3.1}
                    }
        }
        List(prefix + (coloredFiles match {
                        case List(one)=>one
                        case many     =>many.mkString(s"$BLACK$BOLD{$RESET", s"$BLACK$BOLD,$RESET", s"$BLACK$BOLD}$RESET") }) + suffix)
    }
  }

  def compileCrate(outputJar:       File,
                   sources:         List[Source],
                   scalacArguments: List[String],
                   noServer:        Boolean) {
    require (sources.exists{ case _:`.java` | _:`.scala` => true case _ => false }, "nothing to compile " + sources)

    def formatCompilerMessage(msg: Compiler.Msg) = {
      val (color, prefix) = msg.severity match { case "ERROR"   => (RED    + BOLD, "!")
                                                 case "WARNING" => (YELLOW + BOLD, "-")
                                                 case "INFO"    => (BLACK  + BOLD, " ")
                                                 case _         => (CYAN   + BOLD, " ") /* just in case, should not happen */ }
      s"$color$prefix$RESET ${msg.path getOrElse "nopath" replace("\\","/")}:${msg.line},${msg.column}".stripSuffix(" nopath:0,0") + " " + color + msg.msg.replaceAll("\\s+", " ") + RESET
    }

    val request = Request(outputJar, sources, scalacArguments)
    val response = if (noServer) compileInProcess(request) else compileOnServer(request)
    response match {
      case ResponseOK(messages)                            => for (m <- messages) System.err.println(/*" "*19 +*/ formatCompilerMessage(m))
      case ResponseFail(messages)                          => for (m <- messages) System.err.println(/*" "*19 +*/ formatCompilerMessage(m))                                                    ; throw new Compiler.Exception
      case ResponseException(cause: FileNotFoundException) => System.err.println(s"$RED_B$cause$RESET")                                                                                        ; throw new Compiler.Exception
      case ResponseException(cause: StackOverflowError)    => System.err.println(s"$RED_B$cause$RESET")                                                                                        ; throw new Compiler.Exception
      case ResponseException(cause)                        => System.err.println(s"${RED_B}ResponseException($RESET"); cause.printStackTrace(System.err); System.err.println(s"$RED_B)$RESET") ; throw new Compiler.Exception
    }
  }

  def compileTreeToJar(deptree:         DepTree[Source],
                       scalacArguments: List[String],
                       noServer:        Boolean,
                       verbose:         Boolean,
                       mainArtifacts:   Set[Maven.Artifact] // just to decide what to print with different verbose levels
                      ): List[FileSource] = {
    if (verbose)
      System.err.println( s"$MAGENTA_B*** compile ${if (noServer) "in proc" else "on server"}$RESET" + scalacArguments.map(" "+MAGENTA_B+_+RESET).mkString)

    deptree.flatten foreach { case fs: FileSource if !fs.file.exists => throw new FileNotFoundException(s"$fs file not found") // otherwise compiler silently ignores .jar
                              case _                                 => }
    val seennames = new java.util.HashSet[String]

    val reals: List[FileSource] = deptree.fold[List[/*jar+xml*/ FileSource]] {
      (folders: List[List[FileSource]], files: List[Source]) =>
        // other kinds of sources are not for the scala/java/(cpp) compiler (but they can be A.watched and B.added to classpath)
        val depfiles:   List[FileSource] = folders.flatten distinctBy (_.sha) sortBy (_.sha) /* determenism */
        val sources:    List[Source]     = files.filter{ case _:DirSource | _:`.java` | _:`.scala` | _:`.jar` => true
                                                         case _:OtherInMemSource | _:OtherFileSource          => false } distinctBy (_.sha) filterNot (s => depfiles.exists(_.sha == s.sha)) sortBy (_.sha) /* determenism */

        val outputName = sha1(scalacArguments, (sources:::depfiles).map(_.sha)) + ".jar"

        def report(icon: String) {
          // show only main (ore requested) of artifacts, e.g. hadoop-commoon but not hadoop-auth
          if (verbose) {
            val (shown, hidden) = sources:::depfiles partition {
                                    case mjfs: MyJarFileSource    => false
                                    case mjfs: MavenJarFileSource => mainArtifacts contains mjfs.artifact
                                    case _                        => true
                                  }
            System.err.println(s"$MAGENTA${outputName.take(7)}$RESET$BLACK$BOLD*$RESET.jar $icon " +
                                 compressedSourceFilenames(shown).mkString("\n                ") +
                                 (hidden.length match {
                                   case 0 => ""
                                   case 1 => s"\n                $BLACK$BOLD${compressedSourceFilenames(hidden).mkString}$RESET"
                                   case n => f"\n                $BLACK$BOLD+ $n%3d jars$RESET" /*s" ${compressedSourceFilenames(hidden).mkString}"*/
                                 }))
          }
        }

        // debug
        for (dir <- Cache.readwriteDir :: Cache.mirrorDir.toList) {
          for (fos <- Try(new FileOutputStream(new File(dir, outputName+".legend")))) {
            try     fos.write(s"outputName = $outputName <- SHA1 of\n${scalacArguments:::(sources:::depfiles map (s=>s"${s.sha} $s")) mkString "\n"}" getBytes "UTF-8")
            finally fos.close()
          }
        }

        val outputJar = Cache.getOrElseCreate(
                          outputName,
                          { outputJar =>
                              require(seennames add outputName)
                              report(s"$BOLD<-$RESET")
                              compileCrate(outputJar, sources:::depfiles, scalacArguments, noServer)
                          },
                          { if (seennames add outputName) report("--") } // do not display dups within the tree (todo: optimize them out earlier)
                        )
        require(outputJar.isFile && outputJar.isAbsolute, (outputJar, outputJar.isFile, outputJar.isAbsolute))
        MyJarFileSource(outputJar.getAbsolutePath.replace('\\', '/'), outputJar) :: depfiles ::: files.flatMap{ case _:`.java` | _:`.scala` => None
                                                                                                                case s:OtherFileSource      => None    /* e.g. *.domains */
                                                                                                                case s:FileSource           => Some(s) /* jar+xml */
                                                                                                                case _:OtherInMemSource     => ???     /* inmem .xml? */ }
    }.distinctBy(_.sha)

//    for ((_, similarJars) <- (reals).flatMap{case s:`.jar`=>Some(s) case _=>None}.groupBy(_.file.getName.replaceAll("[0-9_.-]", "")) if similarJars.length>1) {
//      similarJars.zipWithIndex foreach {
//        case (jar:`.jar`, 0) => System.err.println(YELLOW+BOLD+"- "+RESET + jar.relPath +" "+ jar.file.length + YELLOW+BOLD+" similar jar names within crate, possible version conflict"+RESET)
//        case (jar:`.jar`, _) => System.err.println(YELLOW+BOLD+"- "+RESET + jar.relPath +" "+ jar.file.length + YELLOW+BOLD+" ^^^^^"                                                    +RESET)
//      }
//    }
    //for (jar  <- jars)
    //  System.err.println(jar.sha + " " + (jar match { case source@FileSource(f) => f.getAbsoluteFile.toURI.toURL }))
    // todo: find collisions
    //System.err.println(CYAN+jars.map( url=>new File(url.toURI).getName ).sorted.mkString("\n")+RESET)
    reals.flatMap{ case fs:DirSource                         => Some(fs)
                   case fs:`.jar`                            => Some(fs)
                   case fs:OtherFileSource                   => System.err.println(YELLOW+BOLD+"- "+RESET + fs.relPath + YELLOW+BOLD+" ignore non-jar file on classpath" + RESET); None // todo? pack non-jar (.properties and .xml) files and fakes to a new jar and add it to classpath
                   case _:JavaFileSource | _:ScalaFileSource => ???
                 }
  }
}



// for holding global vars (not used by launcher itself), for example
//  - the transitional state between old and new versions of code (onChange would not help when parent object is reloading)
//  - to anchor the global objects which are not supposed to be discarded on the code change (maintenance threads, database connections, etc)
object Registry extends java.util.concurrent.ConcurrentSkipListMap[String, Any]


object Joint {
  private           var roots = List.empty[Joint[_]]

  private[launcher] val mmm = new java.util.IdentityHashMap[ClassLoader, (Joint[_],
                                                                          Launcher.PFTResult,
                                                                          collection.mutable.ArrayBuffer[Joint[_]])]

  def lslr(files: TraversableOnce[File]): TraversableOnce[File] = files flatMap { f =>
    new Traversable[File] { // there could be .class, .properties, ...
      def foreach[U](callback: File=>U) {
        callback(f)
        if (f.isDirectory)
          lslr(f.listFiles.sortBy(_.getName)) foreach callback
      }
    }
  }

  def quickHash(sourceFiles: TraversableOnce[File]): String =
    sha1(lslr(sourceFiles).map{ f => sha1(f.getCanonicalPath, f.lastModified, f.length) }.toList)

  // if it a symlink to /nix/store and is changes:
  //  `f.getCanonicalPath` changes for all the files not reflecting content change
  //  `f.lastModified`     is always 1970-01-01
  //  `f.length`           is not reliable, content might be changed keeping the length
  // what to do?
  //   `makeHash` (calc content sha if f.lastModified==0)
  // + `verifyHash` (check content sha if f.lastModified==0 && f.getCanonicalPath changed)
  class WatchState(files: TraversableOnce[Compiler.FileSource]) {
    private val capturedState: Map[(File, String), Either[(String, String), (Long, Long)]] =
      files.map {
        case fs: Compiler.DirSource =>
          (fs.file, fs.relPath) -> Left(("", fs.sha))
        case fs =>
          val lm = fs.file.lastModified
          if (lm <= 1000) { // /nix/store/...
            if (fs.file.getPath.startsWith("/nix/store/")) println(s"${fs.file} won't be watched")
            (fs.file, fs.relPath) -> Left((fs.file.getCanonicalPath, fs.sha))
          } else {
            (fs.file, fs.relPath) -> Right((lm, fs.file.length))
          }
      }.toMap

    def changed(): Boolean = capturedState exists {
      case ((file, relPath), Left ((canonicalPath, sha   ))) => file.getCanonicalPath!=canonicalPath && Compiler.FileSource.fromFileAndRelpath(file, relPath).sha!=sha
      case ((file, relPath), Right((lastModified,  length))) => file.lastModified!=lastModified || file.length!=length
    }
  }

  /**
    * It watches source files of joints for changes and recompile them
    */
  private[this] def watch(j: Joint[_]) {
    def jlslr(j: Joint[_]): collection.mutable.ArrayBuffer[Joint[_]] = j +: j.instance.get.children.flatMap(jlslr)

    val instance = j.instance.get
    if (j.watched) {
      if (!instance.discarded) {
        if (instance.watchState.get.changed()) {  // sources tried to compile and failed
          // todo?: distinct if there was a compilation failure (no need to check children QuickHash) or constructor failure (prev joint was created and discarded)
          val withChildrenQuickHash = quickHash(jlslr(j) flatMap (_.instance.get.sources.files.map(_.file)))
          if (j.lastTriedQuickHashWithChildren == withChildrenQuickHash) {
            // no need to repeat failed compilation
          } else {
            j.lastTriedQuickHashWithChildren = withChildrenQuickHash
            try {
              j.compileXXX()
            } catch {
              //case e: java.io.FileNotFoundException =>
              //  j.lastTriedQuickHash = ""; // force retry?
              case e: Throwable =>
                System.err.println(s"\n$j: $e")
            }
          }
        } else {                                  // nothing new here, but maybe children have something
          instance.children foreach watch
        }
      }
    } else {                                      // maybe a child is watched
      instance.children foreach watch
    }
  }

  new Thread("FileWatch") {
    override def run() {
      while(true) {
        Thread.sleep(2000)
        //println("===========") // verbose
        for (j <- roots) {
          try {
            watch(j)
          } catch {
            case e: Throwable => System.err.println("---WatchThreadException---"); e.printStackTrace()
          }
        }
      }
    }
    setDaemon(true)
    start()
  }
}


// dependency tree monad
case class DepTree[+A](folders: List[DepTree[A]] = Nil, files: List[A] = Nil) {
  require(folders forall (!_.isEmpty), this)
  def isEmpty = folders.isEmpty && files.isEmpty

  // apply `f` to each file
  def map[B](f: A=>B):              DepTree[B] = new DepTree[B](folders map (_ map f), files map f)

  // remove files not satisfying `pred`
  def filter(pred: A=>Boolean):     DepTree[A] = new DepTree[A](folders map (_ filter pred) filterNot (_.isEmpty), files filter pred)

  // each FILE to be converted to a tree which will take its place (if the tree is empty, the file is filtered out)
  def flatMap[B](f: A=>DepTree[B]): DepTree[B] = flatMapFolders { dt => val dts: List[DepTree[B]] = dt.files map f
                                                                        new DepTree(dts flatMap (_.folders) /*filterNot (_.isEmpty)*/, dts flatMap (_.files))
                                                                }
  // process files at one level at once
  def flatMapFolders[B](f: DepTree[A]=>DepTree[B]): DepTree[B] = {
    val tlist: DepTree[B] = f(this)
    new DepTree[B](folders.map(_ flatMapFolders f) ++ tlist.folders /*filterNot (_.isEmpty)*/, tlist.files)
  }

  def fold[B](f: (List[B],List[A])=>B): B = f(folders map (_ fold f), files)

  def flatten: Traversable[A] = new Traversable[A] {
    def foreach[U](callback: A=>U) {
      traverseFolders foreach (_.files foreach callback)
    }
  }

  def traverseFolders: Traversable[DepTree[A]] = new Traversable[DepTree[A]] {
    def foreach[U](callback: DepTree[A]=>U) {
      folders foreach (_.traverseFolders foreach callback)
      callback(DepTree.this)
    }
  }

  def zipWithDepth: Traversable[(A, Int)] = new Traversable[(A, Int)] {
    def foreach[U](callback: ((A, Int))=>U) {
      folders foreach (_.zipWithDepth foreach { case (a,d) => callback(a,d+1) } )
      files   foreach (callback(_, 0))
    }
  }

  def ++[B >: A](that: DepTree[B]) = new DepTree[B](folders ++ that.folders, files ++ that.files)

  // JSON-like succinct string
  override def toString = files.map('"'+_.toString.replace("\\", "\\\\").replace("\"", "\\\"")+'"')++folders                                       mkString ("[" , ","    ,  "]")
  def toPretty: String  = files.map('"'+_.toString.replace("\\", "\\\\").replace("\"", "\\\"")+'"')++folders.map(_.toPretty.replace("\n", "\n  ")) mkString ("[ ", ",\n  ", " ]")
}

// parse DepTree back from the string form
object DepTree {
  object fromString {
    import fastparse.all._
    private[this] val space                   = P( CharsWhile(Character.isWhitespace, 0) )
    private[this] val string                  = P( space ~ ( "\"" ~/ ( CharsWhile(!"\\\"".contains(_)).! | "\\" ~ CharIn("\"'\\").! ).rep.map(_.mkString) ~ "\""
                                                           | "'"  ~/ ( CharsWhile(!"\\'" .contains(_)).! | "\\" ~ CharIn("\"'\\").! ).rep.map(_.mkString) ~ "'" ))
    private[this] val lst: P[DepTree[String]] = P( space ~ "[" ~/ (lst.map(Left(_)) | string.map(Right(_))).rep(sep=(space ~ ",").~/) ~ space ~ "]" ) map (_.toList) map {
                                                  items: List[Either[DepTree[String], String]] =>
                                                    new DepTree(folders = items.flatMap(_.left.toOption), files = items.flatMap(_.right.toOption))
                                                }
    private[this] val parser                  = lst ~ space ~ End
    def apply(s: String): DepTree[String] = parser.parse(s).get.value
  }

  object empty extends DepTree[Nothing]
}


abstract class Joint[A](val mainClassName:  String,
                        val watched:        Boolean     = false,
              protected val parentCL:       ClassLoader = Thread.currentThread.getContextClassLoader) {

  protected def onInit   (            newObj: A) {}
  protected def onChange (prevObj: A, newObj: A) {}
  protected def onDiscard(prevObj: A           ) {}

  def parentJars:          List[Compiler.FileSource] = LauncherUtils.classpathFiles(parentCL, withSystemJars=false)
  def sourcesOfParentJars: List[Compiler.FileSource] = {
    Joint.mmm.get(parentCL) match {
      case null => Nil
      case (parentjoint, parentsources, _) => parentsources.files ::: parentjoint.sourcesOfParentJars
    }
  }

  def readSourcesPFT(): Launcher.PFTResult // (re)read sources

  case class Instance(generation:  Int,
                      sources:     Launcher.PFTResult,
                      watchState:  Option[Joint.WatchState],
                  var discarded:   Boolean,
                      classLoader: ClassLoader, // with jars compiled from `sources`
                      ref:         A,
                      children:    collection.mutable.ArrayBuffer[Joint[_]])
  private val instance = new AtomicReference[Instance]

  def getObject: A    = instance.get.ref

  private def discard() {
    val oldinstance = instance.get
    if (oldinstance != null) {
      //require (oldinstance eq instance.getAndSet(oldinstance.copy(discarded = true))) // no need to retry, it is inside synchronized
      oldinstance.discarded = true
      try { onDiscard(oldinstance.ref)              } catch { case e: Throwable => e.printStackTrace() }
      for(b <- oldinstance.children) {
        try { b.discard()                           } catch { case e: Throwable => e.printStackTrace() }
      }
    }
  }

  // ActorJoint overrides it
  protected def instantiate(cl: ClassLoader): A = {
    val klass = cl.loadClass(mainClassName)
    if (mainClassName endsWith "$")
      klass.getField("MODULE$").get(null).asInstanceOf[A]
    else
      klass.newInstance.asInstanceOf[A]
  }

  private[this] def internalCompile(sources: Launcher.PFTResult) {
    val watchState = if (watched) Some(new Joint.WatchState(sources.files)) else None // take it before compilation which could last long
    val newjars: List[Compiler.FileSource] = Compiler.compileTreeToJar(sources.deptree,
                                                                       Launcher.cmdline.scalacArguments,
                                                                       Launcher.cmdline.noServer,
                                                                       Launcher.cmdline.verbose,
                                                                       sources.mainArtifacts)
    // ```newjars``` might contain scala-compiler.jar, launcher.jar, etc, added on ```dtx5``` step of ```parseFileTree```.
    // They needed only as compile dependencies, at runtime they can be removed as already provided by parent CLs
    val parentJars = classpathFiles(parentCL, withSystemJars=false).toSet
    val newjars2 = newjars.filter(!parentJars.contains(_))

    val ucl = new java.net.URLClassLoader(newjars2.toArray.map(_.file.getAbsoluteFile.toURI.toURL), parentCL /*parentJS.fold(Thread.currentThread.getContextClassLoader)(_.classLoader)*/)

    val savedThreadCCL = Thread.currentThread.getContextClassLoader
    try {
      val oldinstance = instance.get
      require (oldinstance == null || !oldinstance.discarded, "internalCompile in discarded Joint")
      val newchildren = new collection.mutable.ArrayBuffer[Joint[_]]
      Joint.mmm.put(ucl, (this, sources, newchildren))

      Thread.currentThread setContextClassLoader ucl
      val newobj: A = try {
                        instantiate(ucl) // calling constructor can change current thread's context class loader ?
                      } catch {
                        case e: Throwable =>
                          e match {
                            case e: ExceptionInInitializerError if e.getCause.isInstanceOf[Compiler.Exception] =>
                                                                   //println(s"in ctor1 of `$mainClassName` ${e.getCause}")
                            case e: ExceptionInInitializerError => System.err.println(s"in ctor2 of `$mainClassName`")
                                                                   e.getCause.printStackTrace() // todo: rethow silenter
                            case _                              => System.err.println(s"in ctor3 of `$mainClassName`")
                                                                   //e.printStackTrace()
                          }
                          for(b <- newchildren) {
                            try { b.discard()                     } catch { case e: Throwable => e.printStackTrace() }
                          }
                          throw e
                      }
      require(Thread.currentThread.getContextClassLoader eq ucl)
      val newinstance = Instance(generation   = if (oldinstance==null) 0 else oldinstance.generation+1,
                                 sources      = sources,
                                 watchState   = watchState,
                                 discarded    = false,
                                 classLoader  = ucl,
                                 ref          = newobj,
                                 children     = newchildren)
      require (oldinstance eq instance.getAndSet(newinstance)) // no need to retry, it is inside synchronized
      if (oldinstance != null) {
        require(oldinstance.generation+1 == newinstance.generation) // debug, synchronized around must guarantee it
        try { onChange(oldinstance.ref, newinstance.ref)          } catch { case e: Throwable => e.printStackTrace() }
        for(b <- oldinstance.children) {
          try { b.discard()                                       } catch { case e: Throwable => e.printStackTrace() }
        }
      } else {
        try { onInit(newinstance.ref)                             } catch { case e: Throwable => e.printStackTrace() }
      }
    } finally {
      // restore CL, otherwise Joint("a.ami"); Joint("b.ami") would result in "b.ami" compiled using "a.ami"'s CL as parent
      Thread.currentThread setContextClassLoader savedThreadCCL
    }
  }

  private final var lastTriedQuickHashWithChildren = ""
  private final def compileXXX(): Unit = synchronized {
    require(instance.get==null || !instance.get.discarded, "compile in discarded Joint") // discarded joint does not change its object (and does not recompile sources)
    val sources = readSourcesPFT()

    val b = System.currentTimeMillis
    try {
      internalCompile(sources)
    } finally {
      if (Launcher.cmdline.verbose)
        System.err.println(f"compiled in ${(System.currentTimeMillis - b) / 1000.0}%.3fs")
    }
  }

  locally {
    compileXXX() // initial compilation
    Joint.mmm.get(parentCL) match {
      case null                                         => println(s"root? Joint($mainClassName, $watched, $parentCL)")
                                                           Joint.roots ::= this
      case (parentjoint, parentsources, parentchildren) => parentchildren += this // adopt
    }
  }
}



// bash-style wildcards. string manipulations only, no work with file systems here. (todo: sequences with 0 as prefix {000..002})
// todo: support \\tsclient\c\bin\**
// todo: '\\' is a valid char for Linux filename
object BashWildcard {
  val `**` = '\uF000'
  val `*`  = '\uF001'
  val `?`  = '\uF002'
  val `[^` = '\uF003'
  val `[`  = '\uF004'
  val `]`  = '\uF005'
  private[launcher] def hasWildChar(s: String) = s.exists(c => '\uF000'<=c && c<='\uF005')
  import fastparse.all._
  private[this] val range:            P[Seq[String]] = P( CharIn('0' to '9').rep(1).! ~ ".." ~ CharIn('0' to '9').rep(1).! map {
                                                            case (a, b) => val x=a.toLong; val y=b.toLong
                                                                           assert(x<=y && y<x+10000, s"sanity check: {$x..$y}")
                                                                           (x to y) map ("%0"+a.length+"d" format _)
                                                          })

  private[this] val st:               P[String]      = P( ( "[" ~ CharPred(c=>c!='/'&&c!='^').! ~ "]"   // allow [.] escapes in stableprefix
                                                          | CharIn("\\"  ).map(_=>"/")
                                                          | CharIn("[{*?").map(_=>`**`.toString)
                                                          | AnyChar.!
                                                          ).rep.map{_.mkString split '/' filter (_!=".") takeWhile (!_.contains(`**`)) mkString "/"} )

  private[this] def bl(stop: String): P[Seq[String]] = P( ( "["  ~   CharPred(!"/^".contains(_))                         .!.map(           _       :: Nil) ~ "]" // escape [*] [?] [\] {[} {]} [{] [}]  todo [\x0A]
                                                          | "[^" ~/ (CharPred(_!='/') ~ CharsWhile(!"/]".contains(_), 0)).!.map(    `[^` + _ + `]` :: Nil) ~ "]"
                                                          | "["  ~/ (CharPred(_!='/') ~ CharsWhile(!"/]".contains(_), 0)).!.map(    `[`  + _ + `]` :: Nil) ~ "]"
                                                          | "{"  ~/ (range | bashlistInside).rep(1, sep=",".~/)            .map(_.flatten)                 ~ "}"
                                                          | "**".~/                                                        .map(_ => `**`.toString :: Nil)
                                                          | "*" .~/                                                        .map(_ => `*` .toString :: Nil)
                                                          | "?" .~/                                                        .map(_ => `?` .toString :: Nil)
                                                          | "\\".~/                                                        .map(_ => "/"           :: Nil)
                                                          | CharsWhile(!stop.contains(_)).!                                .map(           _       :: Nil)
                                                          ).rep map {_.foldLeft(Seq(""))((r,n)=>r.flatMap(x=>n.map(x+_)))} )
  private[this] val bashlistInside:   P[Seq[String]] = bl("{,}[]*?\\")
  private[this] val bashlist:         P[Seq[String]] = bl("{}[]*?\\")

  def apply(wc: String): BashWildcard = {
    require(!wc.isEmpty && wc!="." && !(wc.length==2 && wc.last==':') && wc.last!='/' && wc.last!='\\', s"invalid BashWildcard `$wc'")
    val stablePrefix = st.parse(wc).get.value
    val lst          = bashlist.parse(wc).get.value
    new BashWildcard(stablePrefix, for (wc2 <- lst.toList) yield {
                                     val parts = wc2.split('/').filter(_!=".").toList
                                     parts.span(!hasWildChar(_)) match {
                                       case (      Nil, wildparts)                                     => (                ".", wildparts)
                                       case (  ""::Nil, wildparts)                                     => (                "/", wildparts)
                                       case (disk::Nil, wildparts) if disk.length==2 && disk.last==':' => (         disk + "/", wildparts)
                                       case (   prefix, wildparts)                                     => (prefix mkString "/", wildparts)
                                     }
                                   })
  }

  // may be used to construct kind of comma-separated lists
  def fromNonWild(strs: List[String]) = new BashWildcard("", strs/*.toList*/ map (_ -> Nil))
}


class BashWildcard private[launcher](val stablePrefix: String, // globresults.forall(path => path==stablePrefix || path.startsWith(stablePrefix+'/'))
                                                     // foo/{bar} and {foo/bar} differ only in stablePrefix
                                                     // this difference is used by `copy wildcard targetdir', it replaces `stableprefix' of glob results by targetdir to get targetpath of each file
                                     val prefixesAndWildparts: List[(/*prefix*/String, /*wildparts*/List[String])]) {
  import BashWildcard.{`**`, `*`, `?`, `[^`, `[`, `]`}
  require(prefixesAndWildparts.nonEmpty)
  require(stablePrefix=="" || prefixesAndWildparts.forall{ case (p,w) => p.startsWith(stablePrefix+'/') || p==stablePrefix }, (stablePrefix, prefixesAndWildparts))

  def isWild: Boolean = prefixesAndWildparts match {
                          case List((`stablePrefix`, Nil)) => false
                          case _                           => true
                        }

  override def toString = {
    def escape(needescape: String, s: String) = {
      var withinBrackets = false
      s flatMap {
        case `**`                                           => require(!withinBrackets);                       "**"
        case `*`                                            => require(!withinBrackets);                       "*"
        case `?`                                            => require(!withinBrackets);                       "?"
        case `[^`                                           => require(!withinBrackets); withinBrackets=true;  "[^"
        case `[`                                            => require(!withinBrackets); withinBrackets=true;  "["
        case `]`                                            => require(withinBrackets);  withinBrackets=false; "]"
        case c if !withinBrackets && needescape.contains(c) =>                                                 s"[$c]"
        case c                                              =>                                                 c.toString
      }
    }
    if (isWild) {
      val variants = prefixesAndWildparts map { case (p,w) => p.stripSuffix("/")::w filter (_ != ".") mkString "/" }
      require(variants forall (_ startsWith stablePrefix), (stablePrefix, prefixesAndWildparts, variants))
      val beforeBracket = stablePrefix + (if (variants forall (_ startsWith stablePrefix+'/')) "/" else "")
      escape("*?\\[]{}", beforeBracket) + variants.map(_ stripPrefix beforeBracket).map(escape("*?\\[]{},", _)).mkString("{", ",", "}")
    } else {
      escape("*?\\[]{}", stablePrefix)
    }
  }

  // command line to be executed on remote linux
  def linuxFindCmdLine(basePath: String): List[List[String]] = {
    require(basePath=="/" || basePath.startsWith("/") && !basePath.endsWith("/"), basePath)
    def wild2linuxre(s: String) = s flatMap { case `**`                                                                       => ".*"
                                              case `*`                                                                        => "[^/]*"
                                              case `?`                                                                        => "[^/]"
                                              case `[^`                                                                       => "[^"
                                              case `[`                                                                        => "["
                                              case `]`                                                                        => "]"
                                              case c@('.' | '+' | '^' | '$' | '|' | '{' | '}' | '[' | ']' | '?' | '*' | '\\') => "\\" + c // do not escape ()
                                              case c                                                                          => c.toString }
    prefixesAndWildparts map {
      case (prefix, Nil      ) => List("find", basePath.stripSuffix("/") + prefix, "-type", "f")
      case ("/",    wildparts) => List("find", basePath                          , "-type", "f", "-regex", wild2linuxre(         wildparts.map("/" + _).mkString))
      case (prefix, wildparts) => List("find", basePath.stripSuffix("/") + prefix, "-type", "f", "-regex", wild2linuxre(prefix + wildparts.map("/" + _).mkString))
    }
  }
}


// apply BashWildcard against virtual file system
object Glob  {
  trait PathLike[P,S] {
    require(!path.startsWith("./") && !path.contains('\\') && !path.contains("//") /*&& !path.contains(":/")*/, path)
    def getP:                 P
    def stat:                 S
    def path:                 String
    def name:                 String
    def list:                 Iterable[S]
    def concat(name: String): P
  }
  trait StatLike[P,S] {
    def getP:                 P
    def isFile:               Boolean   // if (!isFile && !isDirectory) then not_exists | sysmilk | weird_win7_lock
    def isDir:                Boolean
  }
  // implementation for java.io.File
  implicit class pathLikeFromFile(f: File) extends PathLike[File,File] {
    def getP                 = f
    def stat                 = f
    def path                 = f.getPath.replace(File.separatorChar, '/')
    def name                 = f.getName
    def list                 = f.listFiles match { case null                  => Iterable.empty
                                                   case lst if f.getPath=="." => lst.map(f=>new File(f.getPath stripPrefix "."+File.separatorChar))
                                                   case lst                   => lst }
    def concat(name: String) = new File(name) match { case n if f.getPath=="." => n
                                                      case n if n.isAbsolute   => n
                                                      case n                   => new File(f, name) }
  }
  implicit class statLikeFromFile(f: File) extends StatLike[File,File] {
    def getP                 = f
    def isFile               = f.isFile
    def isDir                = f.isDirectory
  }
}


class Glob[P <% Glob.PathLike[P,S], S <% Glob.StatLike[P,S]](base: P) {
  import BashWildcard.{hasWildChar, `**`, `*`, `?`, `[^`, `[`, `]`}
  private[this] val lsmemo = new java.util.concurrent.ConcurrentHashMap[String, Iterable[S]] // common memoizer for many `glob` invocations
  private[this] def ls  (p: Glob.PathLike[P,S]): Iterable[S] = Option(lsmemo get p.path) getOrElse { val rc=p.list; Option(lsmemo.putIfAbsent(p.path, rc)) getOrElse rc }
//def lslr(p: Glob.PathLike[P,S]): Iterator[S] = ls(p).iterator flatMap (s => if (s.isDir) lslr(s.getP) else Iterator(s))
  private[this] def lslr2(s: S):                 Iterator[S] = if (s.isDir) ls(s.getP).iterator flatMap lslr2 else Iterator(s) // todo: be careful following symlinks, there could be cycles

  def globEx(bw: BashWildcard): Iterator[Either[P,S]] = {
    def wild2javare(s: String) = s flatMap { case `**`                                                                                   => ".*"
                                             case `*`                                                                                    => "[^/]*"
                                             case `?`                                                                                    => "[^/]"
                                             case `[^`                                                                                   => "[^"
                                             case `[`                                                                                    => "["
                                             case `]`                                                                                    => "]"
                                             case '/' | File.separatorChar                                                               => ??? // intended to use only with path segments
                                             case c@('.' | '+' | '^' | '$' | '(' | '|' | ')' | '{' | '}' | '[' | ']' | '?' | '*' | '\\') => "\\" + c // '\\' may be ok on linux
                                             case c                                                                                      => c.toString }

    def glob1(parent: Either[P,S], wildparts: List[String]): Iterator[Either[P,S]] = {
      //println(s"glob1(parent=$parent, wildparts=$wildparts)")
      def flparent: P = parent match { case Left(p)  => p
                                       case Right(s) => require(s.isDir, s); s.getP }
      wildparts match {
        case hd::_  if hd contains `**` => val re1 = ("^" + wild2javare(hd.take(hd indexOf `**`))).r
                                           val re2 = ("^" + wildparts.map(wild2javare).mkString("/") + "$").r
                                            //println(s"parent=`$parent'")
                                            //println(s"flparent=`$flparent'")
                                            //println(s"flparent.path=`${flparent.path}'")
                                            //println(s"re1=`$re1'")
                                            //println(s"re2=`$re2'")
                                           ls(flparent).iterator filter {s=>/*println(s"s=`${s.getP.name}'  `$s'");*/ re1.findFirstIn(s.getP.name).nonEmpty
                                                              } flatMap {s=>/*println(s"x=`$s'");                  */ lslr2(s)
                                                               } filter {s=>/*println(s"y=`${s.getP.path}'  `$s'");*/
                                                                            val pxx = flparent.path match {
                                                                              case "."                                                                                                  => ""
                                                                              case "/"                                                                                                  => "/"
                                                                              case px if px.length==3 && ('a'<=px(0)&&px(0)<='z' || 'A'<=px(0)&&px(0)<='Z') && px(1)==':' && px(2)=='/' => px    // "list c:\**"
                                                                              case px if s.getP.path startsWith px+'/'                                                                  => px+'/' }
                                                                            require(s.getP.path startsWith pxx, (s.getP.path, pxx))
                                                                            re2.findFirstIn(s.getP.path stripPrefix pxx).nonEmpty
                                                                  } map {s=>/*println(s"z=`$s'");                  */ Right(s)}
        case hd::tl if hasWildChar(hd)  => val re1 = ("^" + wild2javare(hd) + "$").r
                                           ls(flparent).iterator filter (s=>re1.findFirstIn(s.getP.name).nonEmpty) flatMap { case s if s.isDir => glob1(Right(s), tl)
                                                                                                                             case s            => Iterator(Right(s)) }
        case hd::tl                     => glob1(Left(flparent concat hd), tl)
        case    Nil                     => parent match { case Left(_)  => Iterator(parent)
                                                          case Right(_) => Iterator.empty }
      }
    }
    for ((prefix, wildparts) <- bw.prefixesAndWildparts.iterator;
                        file <- glob1(Left(base concat prefix), wildparts)) yield file
  }

  // it does not validate existence and attributes of a fully specified file.
  // this is the difference between how `globS' (find -type f) and `globF` work
  def globF(bw: BashWildcard): Iterator[P] = globEx(bw) map (_.right.map(_.getP).merge)

  // it is more like `find -type f'.
  // stat() performed even on fully specified paths to check if the file exist (todo: lookup `lsmemo` before stat)
  def globS(bw: BashWildcard): Iterator[S] = globEx(bw) flatMap {
                                               case Right(s) => Some(s)
                                               case Left(p)  => (try { Some(p.stat) } catch { case _:java.io.FileNotFoundException => None }) filter (_.isFile) // HDFS's .`stat` throws FileNotFoundException, File's return objest which is not .`isFile`
                                             }
}


object Maven {
  var repoUrls = List( "https://repo1.maven.org/maven2",
                       "https://oss.sonatype.org/content/repositories/releases"
                     )
  var verbose = false

  def getstore(url: String, out: File) {
    out.getParentFile.mkdirs()
    Try(out.delete())
    val f=new java.io.FileOutputStream(out)
    try     f.getChannel.transferFrom(java.nio.channels.Channels.newChannel(new java.net.URL(url).openStream), 0, Long.MaxValue)
    finally f.close()
  }

  def getstoreChecked(url: String, out: File) {
    val fsha1 = new File(out.getAbsolutePath + ".sha1")
    val fmd5  = new File(out.getAbsolutePath + ".md5")
    val ftmp  = new File(out.getAbsolutePath + ".tmp")
    if (verbose) System.err.print(s"GET $url ")
    try {
      getstore(url,        ftmp) // throws on 404
      val binary = readFileToByteArray(ftmp)
      (Try {
        getstore(url+".sha1", fsha1)
        require(new String(readFileToByteArray(fsha1)) take 40 equalsIgnoreCase sha1(binary), "sha1 mismatch")
        require(ftmp.renameTo(out) || (binary sameElements readFileToByteArray(out)), s"renameTo($ftmp, $out) failed") // Windows: rename could fail if existing jar is locked
        Try(fmd5.delete())
      } orElse
      Try {
        getstore(url+".md5", fmd5)
        require(new String(readFileToByteArray(fmd5 )) take 32 equalsIgnoreCase  md5(binary), "md5 mismatch" )
        require(ftmp.renameTo(out) || (binary sameElements readFileToByteArray(out)), s"renameTo($ftmp, $out) failed") // Windows: rename could fail if existing jar is locked
        Try(fsha1.delete())
      }).get
      if (verbose) System.err.println(s"${GREEN}ok$RESET")
    } catch {
      case e: java.io.FileNotFoundException => Try(fsha1.delete()); Try(fmd5.delete()); Try(ftmp.delete()); if (verbose) System.err.println(s"${RED}404$RESET"); throw e
      case e: Throwable                     => Try(fsha1.delete()); Try(fmd5.delete()); Try(ftmp.delete()); if (verbose) System.err.println(s"$RED$e$RESET" ); throw e
    }
  }

  case class Exclude(groupId: String, artifactId: String, version: String) {
    require(groupId    matches "[_A-Za-z0-9.-]+\\*?|\\*", s"Exclude($RED$BOLD$groupId$RESET,$artifactId,$version)")
    require(artifactId matches "[_A-Za-z0-9.-]+\\*?|\\*", s"Exclude($groupId,$RED$BOLD$artifactId$RESET,$version)")
    require(version    matches "[_A-Za-z0-9.-]+\\*?|\\*", s"Exclude($groupId,$artifactId,$RED$BOLD$version$RESET)")
    def matches(a: Artifact) =
      (groupId   ==a.groupId    || groupId   .last=='*' && groupId   .regionMatches(0, a.groupId   , 0, groupId   .length-1)) &&
      (artifactId==a.artifactId || artifactId.last=='*' && artifactId.regionMatches(0, a.artifactId, 0, artifactId.length-1)) &&
      (version   ==a.version    || version   .last=='*' && version   .regionMatches(0, a.version   , 0, version   .length-1))
  }
  case class Dependency(artifact: Artifact/*todo: version range*/, optional: Boolean, scope: String, excludes: Set[Exclude]) {
    require("compile"::"runtime"::"test"::"provided"::"import"::"system"::Nil contains scope, scope)
    override def toString = artifact /*s"${artifact.groupId}:${artifact.artifactId}:${artifact.version}"*/ +
                            (if (optional) " optional" else "") +
                            (if (scope!="compile") s" scope=$scope" else "") +
                            (if (excludes.nonEmpty) s" exclude=${excludes map (a=>s"${a.groupId}:${a.artifactId}:${a.version}") mkString ","}" else "")
  }

  private[this] val reProject = """\$\{(project\.parent\.|project\.|pom\.)?([A-Za-z0-9.-]+)\}""".r
  private[this] def o(s: xml.NodeSeq): Option[String] = s.headOption map (_.text.trim) filter (!_.isEmpty)

  class Artifact private(val groupId: String, val artifactId: String, val version: String) extends Ordered[Artifact] {
    require(groupId    matches "[_A-Za-z0-9.-]+", s"Artifact($RED$BOLD$groupId$RESET,$artifactId,$version)")
    require(artifactId matches "[_A-Za-z0-9.-]+", s"Artifact($groupId,$RED$BOLD$artifactId$RESET,$version)")
    require(version    matches "[_A-Za-z0-9.-]+", s"Artifact($groupId,$artifactId,$RED$BOLD$version$RESET)")

    // normal ordering
    private       lazy val cmpvalue = (groupId +: artifactId +: version.split('.').map(x=>Try("%016X" format x.toLong) getOrElse x)) mkString "\u0000"
    override      def      compare(that: Artifact): Int = cmpvalue compare that.cmpvalue

                  def      isInferiorTo(that: Artifact): Boolean = (this ne that) && groupId==that.groupId && artifactId==that.artifactId && {
                                                                     val (mymajor, thatmajor) = (version.split('.').head.toInt, that.version.split('.').head.toInt)
                                                                     /*(mymajor==0 || mymajor==thatmajor) &&*/ this.compare(that) < 0
                                                                   }
    override      def      toString = if (groupId==artifactId) s"$groupId:$version" else s"$groupId:$artifactId:$version"

                  def      jarFile: Option[File] = Artifact.twoFiles(this).jarFile
                  def      pomFile: File         = Artifact.twoFiles(this).pomFile

                  lazy val pom: scala.xml.Node = {
                    val xml = scala.xml.XML loadString new String(readFileToByteArray(pomFile), "UTF-8")
                    require(groupId    == demacro(xml, (o(xml \ "groupId"   ) orElse o(xml \ "parent" \ "groupId"   )).get), s"$pomFile: expected groupId  '$groupId'")
                    require(artifactId == demacro(xml, (o(xml \ "artifactId") orElse o(xml \ "parent" \ "artifactId")).get), s"$pomFile: expected artifactId '$artifactId'")
                    require(version    == demacro(xml, (o(xml \ "version"   ) orElse o(xml \ "parent" \ "version"   )).get), s"$pomFile: expected version '$version'")
                    xml
                  }
    private[this] def      parent:  Option[Artifact] = (pom \ "parent").headOption map (p => Artifact(o(p \ "groupId").get, o(p \ "artifactId").get, o(p \ "version").get))
    private       def      parents: Stream[Artifact] = this #:: parent.fold(Stream.empty[Artifact])(_.parents)

    private[this] def      demacro(xml: scala.xml.Node /*= pom*/, s: String): String =
                    reProject.replaceAllIn(s, { m =>
                      val replacement = ((m group 1, m group 2) match {
                                           case (null,                "groupId"   ) => Some(groupId   )
                                           case (null,                "artifactId") => Some(artifactId)
                                           case (null,                "version"   ) => ???; Some(version   )
                                           case ("pom." | "project.", name        ) => o(xml            \ name) orElse o(xml \ "parent" \ name) orElse parents.flatMap(a => o(a.pom \ "properties" \ s"${m group 1}$name")).headOption
                                           case ("project.parent.",   name        ) => o(xml \ "parent" \ name) orElse o(xml            \ name) // ${project.parent.version} with no <parent> at http://central.maven.org/maven2/org/bytedeco/javacpp-presets/1.4/javacpp-presets-1.4.pom
                                           case (null,                name        ) =>                                                                 parents.flatMap(a => o(a.pom \ "properties" \ name                )).headOption
                                         }) getOrElse (throw new RuntimeException(s"$this: unable to expand macro ${m}; parents=${parents.toList}"))
                      java.util.regex.Matcher.quoteReplacement(demacro(xml, replacement))
                    })

    private       def      getVersionOnParent(groupId: String, artifactId: String): Option[String] = (for (Dependency(Artifact(`groupId`, `artifactId`, v),_,_,_) <- this.dependencyManagement) yield v).headOption
    private       def      getScopeOnParent  (groupId: String, artifactId: String): Option[String] = (for (Dependency(Artifact(`groupId`, `artifactId`, _),_,s,_) <- this.dependencyManagement) yield s).headOption
    private       def      getExcludeOnParent(groupId: String, artifactId: String): List[Exclude]  = (for (Dependency(Artifact(`groupId`, `artifactId`, _),_,_,e) <- this.dependencyManagement) yield e).flatten

    private[this] def      findDependencies(xml: scala.xml.Node, pp: Stream[Artifact]): List[Dependency] = {
                    (for (     d  <- (xml\"dependencies"\"dependency").toList if o(d \ "type") != Some("test-jar");
                          Some(g)  = o(d \ "groupId"   )                                                          map (demacro(pom, _));
                          Some(a)  = o(d \ "artifactId")                                                          map (demacro(pom, _));
                               v   = o(d \ "version"   ) orElse pp.flatMap(_.getVersionOnParent(g, a)).headOption map (demacro(pom, _)) getOrElse "0"; // "org.apache.sis:parent:0.6" depends on junit w/o version
                               v2  = """\[([_A-Za-z0-9.-]+),.*\)""".r.findFirstMatchIn(v).map(_ group 1) getOrElse v) // version range hack tc: "net.arnx:jsonic:1.2.11"
                     yield Dependency(artifact = Artifact(g, a, v2),
                                      optional = o(d \ "optional"   ) == Some("true"),
                                      scope    = o(d \ "scope"      ) orElse pp.flatMap(_.getScopeOnParent(g, a)).headOption getOrElse "compile",
                                      excludes =  (d \ "exclusions" \ "exclusion").toSet.map((e: scala.xml.Node) => Exclude(o(e \ "groupId").get, o(e \ "artifactId" ).get, o(e \ "version" ) getOrElse "*")) ++ pp.flatMap(_.getExcludeOnParent(g, a)))
                    ) ++ parents.tail.flatMap(_.dependencies)
                  }
                  lazy val dependencies:         List[Dependency] = findDependencies(pom, parents) ++
                                                                    (for (profile <- pom\"profiles"\"profile" if o(profile\"activation"\"activeByDefault")==Some("true")
                                                                                                              || o(profile\"activation"\"jdk"            )==Some("1.8");
                                                                          deps <- findDependencies(profile, parents))
                                                                     yield deps)
    private[this] lazy val dependencyManagement: List[Dependency] = (pom \ "dependencyManagement").toList flatMap (findDependencies(_, parents.tail /* avoid inf recursion */))
  }

  sealed trait TwoFiles {
    val a: Artifact
    def jarFile: Option[File]
    def pomFile: File
  }
  // Artifact restored by a .jar found on classpath
  class TwoFilesEntry(val a: Artifact, val pomFile: File, jar: File) extends TwoFiles {
    val jarFile = Some(jar)
  }
  val mavenDirs: List[File] = (for (s    <- Option(System getenv "SCALAUNCHER_RO_M2").toList;
                                    elem <- s split File.pathSeparator if s.nonEmpty;
                                    fdir =  new File(elem) if fdir.isDirectory)
                               yield fdir) :+ {
                                val homeDir = new File(System getenv (if (Properties.isWin) "USERPROFILE" else "HOME")) // $HOME does not exist under "nixbld"
                                new File(if (homeDir.exists) homeDir else new File("/var/tmp"), ".m2")
                              }
  class TwoFilesEntryM2(val a: Artifact) extends TwoFiles {
    private[this] val (pom: File, jar: File) = {
      val pomJars: List[(File, File)] = for (dir <- mavenDirs;
                                             pom =  new File(dir, s"repository/${a.groupId.replace(".", "/")}/${a.artifactId}/${a.version}/${a.artifactId}-${a.version}.pom");
                                             jar =  new File(dir, s"repository/${a.groupId.replace(".", "/")}/${a.artifactId}/${a.version}/${a.artifactId}-${a.version}.jar"))
                                        yield (pom, jar)
      pomJars find {case (pom,jar) => pom.exists && jar.exists} getOrElse pomJars.last
    }
    lazy val pomFile: File =
      if (pom.exists)
        pom
      else {
        var tries: List[(String, Int)] = repoUrls.map(_ -> 3)
        var gotpom = false
        while (tries.nonEmpty && !gotpom) {
          val (repourl, ntries) :: _ = tries
          try {
            getstoreChecked(s"$repourl/${a.groupId.replace(".", "/")}/${a.artifactId}/${a.version}/${pom.getName}", pom)
            gotpom = true
          } catch {
            case _: Throwable => tries = (tries.tail :+ (repourl, ntries-1)) filter (_._2 > 0)
          }
        }
        require(gotpom, s"fail to download $a -> ${pom.getName}")
        // got pom, stick to repourl=tries.head._1

        // https://github.com/coursier/coursier/issues/278#issuecomment-310135710
        // if (allowJarNotFound) jar absence + pom existance signals that there were 404 error downloading jar
        val allowJarNotFound = o(scala.xml.XML.loadString(new String(readFileToByteArray(pom), "UTF-8")) \ "packaging") == Some("pom") // pom, jar, maven-plugin, ejb, war, ear, rar, par
        if (allowJarNotFound) {
          // try download
          var n = 3
          var gotjar = false
          while (n > 0 && !gotjar) {
            try {
              getstoreChecked(s"${tries.head._1}/${a.groupId.replace(".", "/")}/${a.artifactId}/${a.version}/${jar.getName}", jar)
              gotjar = true
            } catch {
              case _: java.io.FileNotFoundException if allowJarNotFound => gotjar = true
              case _: Throwable                                         => n -= 1
            }
          }
          require(gotjar, s"fail to download $a -> ${jar.getName}")
        }
        pom
      }
    lazy val jarFile: Option[File] =
      if (jar.exists)
        Some(jar)
      else if (o(a.pom\"packaging") == Some("pom")) /*allowJarNotFound*/
        None
      else {
        var tries: List[(String, Int)] = repoUrls.map(_ -> 3)
        var gotjar = false
        while (tries.nonEmpty && !gotjar) {
          val (repourl, ntries) :: _ = tries
          try {
            getstoreChecked(s"$repourl/${a.groupId.replace(".", "/")}/${a.artifactId}/${a.version}/${jar.getName}", jar)
            gotjar = true
          } catch {
            case _: Throwable => tries = (tries.tail :+ (repourl, ntries-1)) filter (_._2 > 0)
          }
        }
        require(gotjar, s"fail to download $a -> ${jar.getName}")
        Some(jar)
      }
  }


  object Exclude {
    def fromString(s: String) = s.split(':') match { case Array(g      ) => Exclude(g, "*", "*")
                                                     case Array(g, a   ) => Exclude(g,   a, "*")
                                                     case Array(g, a, v) => Exclude(g,   a,   v) }
  }
  object Artifact {
    private[this] val registry = collection.mutable.Map.empty[(String, String, String), TwoFiles] // todo: impure
    private       def twoFiles(a: Artifact) = {
      val tf = registry((a.groupId, a.artifactId, a.version))
      require(tf.a eq a)
      tf
    }
    def apply(groupId: String, artifactId: String, version: String): Artifact = registry.getOrElseUpdate((groupId, artifactId, version), new TwoFilesEntryM2(new Artifact(groupId, artifactId, version))).a
    def unapply(a: Artifact):                Option[(String, String, String)] = Some((a.groupId, a.artifactId, a.version))
    object Spec {
      private[this] val re2 = "([_A-Za-z0-9.-]+):([_A-Za-z0-9.-]+)".r
      private[this] val re3 = "([_A-Za-z0-9.-]+):([_A-Za-z0-9.-]+):([_A-Za-z0-9.-]+)".r
      private[this] val re4 = "([_A-Za-z0-9.-]+)::([_A-Za-z0-9.-]+):([_A-Za-z0-9.-]+)".r // inspired by ammonite
      def unapply(s: String): Option[Artifact] = s match {
        case re2(g,    v) => Some(Artifact(g, g                                 , v))
        case re3(g, a, v) => Some(Artifact(g, a                                 , v))
        case re4(g, a, v) => Some(Artifact(g, a+"_"+expandVariables.scalaVersion, v))
        case _            => None // throw new RuntimeException(s"error parse '$s'")
      }
    }
    object Path {
      private[this] val reM2Jar         = ".+/(?:.m2/repository|.coursier/cache/v1/https/repo1.maven.org/maven2)/([/_A-Za-z0-9.-]+)/([_A-Za-z0-9.-]+)/([_A-Za-z0-9.-]+)/([_A-Za-z0-9.-]+)\\.jar".r
      private[this] val reNixJar        = "/nix/store/[0-9a-z]{32}-([_A-Za-z0-9.-]+)=([_A-Za-z0-9.-]+)=([_A-Za-z0-9.-]+)/share/java/([_A-Za-z0-9.-]+)\\.jar".r
      private[this] val reNixDerivation = "/nix/store/[0-9a-z]{32}-([_A-Za-z0-9.-]+)=([_A-Za-z0-9.-]+)=([_A-Za-z0-9.-]+)".r
      def unapply(file: File): Option[Artifact] = file.getCanonicalPath.replace('\\','/') match {
        case _ if file.getName endsWith ".pom"      => require(file.isAbsolute, s"path is not absolute: $file")
                                                       val xml = scala.xml.XML.loadString(new String(readFileToByteArray(file), "UTF-8"))
                                                       val Some(g) = o(xml \ "groupId"   ) orElse o(xml \ "parent" \ "groupId"   )
                                                       val Some(a) = o(xml \ "artifactId") orElse o(xml \ "parent" \ "artifactId")
                                                       val Some(v) = o(xml \ "version"   ) orElse o(xml \ "parent" \ "version"   )
                                                       val artifact = Artifact(g                 ,a,v); registry.update((artifact.groupId, artifact.artifactId, artifact.version), new TwoFilesEntry(artifact, file, new File(s"${file.getPath stripSuffix ".pom"}.jar"))); Some(artifact)
        case reM2Jar        (g,a,v,j) if a+"-"+v==j => val artifact = Artifact(g.replace('/','.'),a,v); registry.update((artifact.groupId, artifact.artifactId, artifact.version), new TwoFilesEntry(artifact, new File(s"${file.getPath stripSuffix ".jar"}.pom"), file)); Some(artifact)
        case reNixJar       (g,a,v,j) if a+"-"+v==j => val artifact = Artifact(g                 ,a,v); registry.update((artifact.groupId, artifact.artifactId, artifact.version), new TwoFilesEntry(artifact, new File(s"${file.getPath stripSuffix ".jar"}.pom"), file)); Some(artifact)
        case reNixDerivation(g,a,v  )               => unapply(new File(s"${file.getPath}/share/java/$a-$v.jar"))
        case _                                      => None
      }
    }
    implicit object ordering extends Ordering[Artifact] { // SI-8541
      def compare(x: Artifact, y: Artifact): Int = x compareTo y
    }
  }

  def resolveTogether(artifacts:  Set[Artifact],
                      scopes:     Set[String]  = Set("compile", "runtime" /*, "provided"*/),
                      excludes:   Set[Exclude] = Set.empty,
                      optionals:  Boolean      = false): Map[Artifact, Set[Artifact]] = {
    //System.err.println(s"resolveTogether(${artifacts.toArray.sorted mkString "\n"}, $scopes, $excludes, $optionals)")

    def solveConflicts(upgrades: Map[Artifact, Artifact]): Map[Artifact, Set[Artifact]] = {
      val memo = new collection.mutable.HashMap[(Artifact, Set[Exclude]), DepTree[Artifact]]
      def recur(artifact: Artifact, exs: Set[Exclude]): DepTree[Artifact] = memo.getOrElseUpdate((artifact, exs), {
        @annotation.tailrec def followUpgradeChain(a: Artifact): Artifact = upgrades get a match { case Some(x) => followUpgradeChain(x) case None => a }
        val a = followUpgradeChain(artifact)
        new DepTree[(Artifact     )](folders = for (d <- a.dependencies if !d.optional || optionals
                                                                        if scopes contains d.scope
                                                                        if !exs.exists(_ matches d.artifact)) yield recur(d.artifact, d.excludes ++ exs),
                                     files   = List(a))
      })

      val akl: List[Artifact] = artifacts.toList // keep order for zip below
      val forest = new DepTree[Artifact](folders = akl.map(recur(_, excludes)))

      val ar: Array[(Artifact, /*mindepth*/Int)] = {
        val m = new collection.mutable.HashMap[Artifact, Int] withDefaultValue Int.MaxValue
        for ((a, d) <- forest.zipWithDepth) {
          m(a) = m(a) min d
        }
        m.toArray.sortBy(_._1)
      }

      ar.indices.tail map (i => ar(i-1)->ar(i)) filter { case (a,b) => a._1 isInferiorTo b._1 } match {
        case Seq()                                                     => (akl zip forest.folders.map(_.flatten.toSet)).toMap
        case collisions: IndexedSeq[((Artifact, Int),(Artifact, Int))] =>
          val (olda, newa) = collisions.maxBy{ case (a,b) => (-b._2, b._1) } // first by min depth then by max artifact version
          if (verbose) System.err.println(s"UPGRADE ${newa._2} ${olda._1} -> ${newa._1.version}")
          solveConflicts(upgrades + (olda._1 -> newa._1))
      }
    }
    val solution = solveConflicts(Map.empty)
    //System.err.println(s"solution=\n${solution map { case (a, aa) => a + aa.map("\n  " + _).mkString } mkString "\n"}")
    solution
  }
}



// interface of a "--service" class which will be reload when its source changes
trait MyService {
  def postinit() {}
  def stop() {}
}

private[launcher] object expandVariables {
  val scalaVersion     = if (Properties.versionNumberString matches "2\\.(10|11|12|13)\\.\\d+")  // binary version for release compilers - only 2 digits ("2.10", "2.11", "2.12")
                           Properties.versionNumberString split '.' take 2 mkString "."
                         else                                                                    // beta version - exact string
                           Properties.versionNumberString
  def apply(s: String) = s.replace("%SCALAVERSION%", scalaVersion).
                           replace("%JAVA_HOME%",    System.getProperty("java.home")).
                           replace("%TIME%",         { val sdf = new java.text.SimpleDateFormat("yyyy.MM.dd_HH.mm.ss")
                                                       sdf setTimeZone java.util.TimeZone.getTimeZone("GMT")
                                                       sdf format new java.util.Date })
}

private[launcher] object globImportSpec {
  def apply(globbase: File, importspec: String): List[String] = {
    val bw = BashWildcard(expandVariables(importspec))
    bw.prefixesAndWildparts flatMap {
      case (spec @ Maven.Artifact.Spec(_), Nil)                       => List(spec)
      case (url,                           Nil) if url contains "://" => Cache.getOrElseCreate( s"${sha1(url)}/${url.split('/').last}",
                                                                                                outputFile => Maven.getstoreChecked(url, outputFile)
                                                                                              ).getAbsolutePath.replace('\\', '/') :: Nil

      case pwc                                                        => val bw2 = new BashWildcard(bw.stablePrefix, List(pwc))
                                                                         new Glob(globbase).globF(bw2).map(_.getPath.stripPrefix(globbase.getPath+File.separatorChar).replace('\\', '/')).toList
    }
  }
}


object Launcher extends App {
  System.setProperty("line.separator", "\n") // println must not emit \r

  require(System.getProperty("file.encoding")=="UTF-8" && System.getProperty("sun.jnu.encoding")=="UTF-8") // `System.setProperty(..)` won't work here, it has to be set up in JVM command line (-Dfile.encoding=UTF-8)
  require("\u00A0".getBytes.length==2 && "\uFEDC".getBytes.length==3 && new String(Array(0xC2.toByte, 0xA0.toByte, 0xEF.toByte, 0xBB.toByte, 0x9C.toByte))=="\u00A0\uFEDC") // paranoid ensure that String.getBytes does use UTF-8

  natives.setupSmartConsole()


  private[launcher] case class Params(action:           String                    = "",
                                      base:             File                      = null, // must be set
                                      launcherSrc:      File                      = null, // must be set
                                      tee:              List[File]                = Nil,
                                      verbose:          Boolean                   = false,
                                      appargs:          Array[String]             = Array(),
                                      // classic launcher
                                      deptree:          String                    = "",
                                      klass:            String                    = "",
                                      scalacArguments:  List[String]              = Nil, // as `forces` but then exclude from the results
                                      noServer:         Boolean                   = false,
                                      // mimic scalac
                                      classpath:        List[Compiler.FileSource] = Nil,
                                      outputJar:        File                      = null, // must be set
                                      memoMirrorDir:    Option[File]              = None,
                                      dryRun:           Boolean                   = false,
                                      sources:          List[Compiler.Source]     = Nil,
                                      // mimic coursier
                                      excludes:         Set[Maven.Exclude]        = Set.empty,
                                    //forces:           Set[Maven.Artifact]       = Set.empty,
                                    //loaded:           List[Maven.Artifact]      = Nil, // as `forces` but then exclude from the results
                                    //repositories:     List[String]              = Nil,
                                      printTree:        Boolean                   = false,
                                      optionals:        Boolean                   = false,
                                      artifacts:        Set[Maven.Artifact]       = Set.empty) {
    //private[this] def mkAbsFile(path: String) = { require(base != null, "--base must be set before"); val f = new File(path); if (f.isAbsolute) f else new File(base, path) }
    def parse(args: List[String]): Params = (action, args) match {
      case (_,                                              "--perl-pid"           :: s :: Nil ) if  Properties.isWin  => copy(appargs         =                    natives.getProcessArgsW(s.toInt)                                                                 )
      case (_,                                              "--"                        :: rest) if !Properties.isWin  => copy(appargs         =                    rest.toArray                                                                                     )
      case (_,                                              "--action"             :: s :: rest)                       => copy(action          =                    s, verbose = verbose || s=="resolve" || s=="fetch"                                               ) parse rest
      case (_,                                              "--base"               :: s :: rest)                       => copy(base            =                    {val f=new File(s); require(f.isAbsolute, s"--base is not absolute: $f"); f}                     ) parse rest
      case (_,                                              "--launcher"           :: s :: rest)                       => copy(launcherSrc     =                    new File(s)                                                                                      ) parse rest
      case (_,                                              "--tee"                :: s :: rest)                       => copy(tee             = tee             :+ mkAbsFile(base, expandVariables(s))                                                              ) parse rest
      case (_,                                              "--verbose"                 :: rest)                       => Maven.verbose = true
                                                                                                                          copy(verbose         =                    true                                                                                             ) parse rest
      case (_,                                              "--no-server"               :: rest)                       => copy(noServer        =                    true                                                                                             ) parse rest
      case (_,                                              "--dep-tree"           :: s :: rest)                       => copy(deptree         =                    new String(java.util.Base64.getDecoder decode s, "UTF-8")                                        ) parse rest
      case (_,                                              "--add-repos"          :: s :: rest)                       => Maven.repoUrls ++= s.split("\\s+").toList                                                                                             ; this parse rest
      case ("run"|"service",                                "--class"              :: s :: rest)                       => copy(klass           =                    s                                                                                                ) parse rest
      case ("run"|"service",                                "--scalac-args"        :: s :: rest)                       => copy(scalacArguments =                    s.split("\\s+").map(expandVariables.apply).toList                                                ) parse rest
      case (         "compile-deptree"|"compile-jointtree", "--memo-mirror-dir"    :: s :: rest)                       => copy(memoMirrorDir   =                    Some(mkAbsFile(base, s))                                                                         ) parse rest
      case (         "compile-deptree"|"compile-jointtree", "--dry-run"                 :: rest)                       => copy(dryRun          =                    true                                                                                             ) parse rest
      case ("scalac"|"compile-deptree"|"compile-jointtree", "-classpath"           :: s :: rest)                       => copy(classpath       = classpath       ++ expandVariables(s).split(File.pathSeparator).filter(_.nonEmpty).map(Compiler.FileSource(base, _))) parse rest
      case ("scalac",                                       "-d"                   :: s :: rest)                       => copy(outputJar       =                    mkAbsFile(base, s)                                                                               ) parse rest
      case ("scalac"|"compile-deptree"|"compile-jointtree",                           s :: rest) if  s.startsWith("-") => copy(scalacArguments = scalacArguments :+ s                                                                                                ) parse rest
      case ("scalac",                                                                 s :: rest) if !s.startsWith("-") => copy(sources         = sources         :+ Compiler.FileSource(base, expandVariables(s))                                                    ) parse rest
      case (         "compile-deptree"|"compile-jointtree",                           s :: rest) if !s.startsWith("-") => copy(deptree         = s                                                                                                                   ) parse rest
      case ("resolve"|"fetch"|"launch",                     "--exclude"            :: s :: rest)                       => copy(excludes        = excludes         + Maven.Exclude .fromString(s)                                                                     ) parse rest
    //case ("resolve"|"fetch"|"launch",                     "--force-version"      :: s :: rest)                       => copy(forces          = forces           + Maven.Artifact.fromString(s)                                                                     ) parse rest
    //case ("resolve"|"fetch"|"launch",                     "--for-joint"               :: rest)                       => copy(loaded          =                    Launcher.loadedArtifacts(Launcher.getClass.getClassLoader)                                       ) parse rest
    //case ("resolve"|"fetch"|"launch",                     "--repository"         :: s :: rest)                       => copy(repositories    = repositories    :+ s                                                                                                ) parse rest
      case ("resolve"|"fetch"|"launch",                     "--tree"                    :: rest)                       => copy(printTree       =                    true                                                                                             ) parse rest
      case ("resolve"|"fetch"|"launch",                     "--keep-optional"           :: rest)                       => copy(optionals       =                    true                                                                                             ) parse rest
      case ("resolve"|"fetch"|"launch",                          Maven.Artifact.Spec(a) :: rest)                       => copy(artifacts       = artifacts        + a                                                                                                ) parse rest
      case ("resolve"|"fetch"|"launch",                                               s :: rest) if !s.startsWith("-") => copy(artifacts       = artifacts       ++ (mkAbsFile(base, s) match { case Maven.Artifact.Path(a) => Some(a) case _ => None })             ) parse rest
    }
  }

  private[launcher] val cmdline = Params() parse args.toList
  //System.err.println(s"cmdline=$cmdline")

  for (file <- cmdline.tee) { // there can be many tee files
    tee(file)
  }


  class resolveMagicImports(preprocessCache: String => DepTree[Either[Maven.Artifact, Compiler.Source]]) {
    private[this] val reOptimiseRelPath = """(/|^)[^/]*[^/.]/\.\./""".r
    private[this] def optimiseRelPath(relPath: String): String = reOptimiseRelPath.replaceAllIn(relPath, "$1") match { case `relPath` => relPath case optimized  => optimiseRelPath(optimized) }
    private[this] def oneMagic(scalas: List[Compiler.`.scala`], seen: Set[String] = Set.empty): (DepTree[Either[Maven.Artifact, Compiler.Source]], Set[String]) = {
      //if (scalas.nonEmpty) System.err.println(s"oneMagic(scalas=$scalas, seen=$seen)")
      var cycles = Set.empty[String]

      val rc = new DepTree(files = scalas) flatMap {
        scala: Compiler.`.scala` =>
            // all `Source` are relative to `cmdline.base` in order `__FILE__` to work
            val magicbase = mkAbsFile(cmdline.base, scala.relPath).getParentFile
            new DepTree(files = scala.magicImports) flatMap {
              onemagicstr: String =>
                val dtsources: DepTree[Either[Maven.Artifact, Compiler.Source]] =
                  (if (onemagicstr startsWith "[")
                     DepTree.fromString(onemagicstr)      // import `[ 'ms.webmaster/NormUrl.scala' ]`
                   else
                     new DepTree(files=List(onemagicstr)) // import `ms.webmaster/{NormUrl,HttpUtils}.scala`
                  ) flatMap {
                    s => new DepTree(files=globImportSpec(magicbase, s)) /* magic paths are relative to dirname(.scala-file) */
                  } flatMap {
                    case Maven.Artifact.Spec(artifact) => new DepTree(files = List(Left(artifact)))
                    case path if path contains "://"   => ???
                    case magicpath                     => // check uniqueness using canonical path, there could be "A.scala" and "../a/A.scala"
                                                          val canonicalPath = mkAbsFile(magicbase, magicpath).getCanonicalPath
                                                          if (seen contains canonicalPath) { // cycle!
                                                            //System.err.println(s"cycle! $canonicalPath in seen=${seen}")
                                                            cycles += canonicalPath
                                                            DepTree.empty
                                                          } else {
                                                            val relPath = scala.relPath.take(scala.relPath.lastIndexOf('/') + 1) + magicpath
                                                            // optional optimisation: avoid paths like "../web/../maintenance/../db/Soft404.scala" and reduce number of distinct jars
                                                            preprocessCache(optimiseRelPath(relPath))
                                                          }
                  }

                if (dtsources.flatten forall { case Right(_: Compiler.`.java` | _: Compiler.`.scala` ) => false case _ => true }) { // flatten tree level when it has only jars
                  require(dtsources.folders.isEmpty)
                  dtsources
                } else if (onemagicstr startsWith "[") {
                  val r3 = dtsources flatMapFolders {
                    dt =>
                      val morescalas = for (Right(scala: Compiler.`.scala`) <- dt.files) yield scala
                      val (ttt, cc) = oneMagic(morescalas, seen ++ scalas.map(s=>mkAbsFile(cmdline.base, s.relPath).getCanonicalPath) )
                      require(cc.isEmpty, s"cycles in import `[ ... ]`: $cc")
                                                   ttt ++ new DepTree(files=dt.files)    // flatten here, in case of cycles
                  }
                  new DepTree(folders = List(r3))
                } else {
                  dtsources flatMapFolders {
                    dt =>
                      val morescalas = for (Right(scala: Compiler.`.scala`) <- dt.files) yield scala

                      val (ttt, cc) = oneMagic(morescalas, seen ++ scalas.map(s=>mkAbsFile(cmdline.base, s.relPath).getCanonicalPath) )
                      cycles ++= cc
                      if (cc.nonEmpty) {
                                                   ttt ++ new DepTree(files=dt.files)    // flatten here, in case of cycles
                      } else {
                        new DepTree(folders = List(ttt ++ new DepTree(files=dt.files)))  // dep jar
                      }
                  }
                }
            }
      }
      (rc, cycles -- scalas.map(s=>mkAbsFile(cmdline.base, s.relPath).getCanonicalPath) )
    }

    def apply(dtx: DepTree[Either[Maven.Artifact, Compiler.Source]], seen: Set[String] = Set.empty): DepTree[Either[Maven.Artifact, Compiler.Source]] = {
      dtx flatMapFolders { /*files: List[Either[Maven.Artifact, Compiler.Source]]*/dt =>
        val (qqq, x) = oneMagic(for (Right(scala: Compiler.`.scala`) <- dt.files) yield scala)
        assert(x.isEmpty, x)
        //System.err.println(s"files=$files")
        //System.err.println(s"qqq=${qqq.toPretty}")

        qqq ++ new DepTree[Either[Maven.Artifact, Compiler.Source]](files = dt.files)
      }
    }
  }


  private[this] val allreadfiles = collection.mutable.HashSet.empty[File]

  case class PFTResult(files:         List[Compiler.FileSource],    // files to watch (exclude inmem ?and artifacts?)
                       deptree:       DepTree[Compiler.Source],     // sources to compile (exclude preprocessed out, like .ami)
                       mainArtifacts: Set[Maven.Artifact])          // just to decide what to print with different verbose levels
  def parseFileTree(source:               Either[/*deptreeString*/String, /*singleSource*/Compiler.Source],
                    loadedJars:           List[Compiler.FileSource /* .jar | DirSource                   */], // FIXME: passing Joint.parentJars here re-includes excluded artifacts (the jsch-0.1.54 issue)
                    sourcesOfLoadedJars:  List[Compiler.FileSource /* .scala | .java | .ami | .jar | ... */], // todo: Compiler.FileSource=>List[`.jar`]
                    excludes:             Set[Maven.Exclude]                                                                     = Set.empty,
                    preprocess:           PartialFunction[Compiler.FileSource, DepTree[Either[Maven.Artifact, Compiler.Source]]] = PartialFunction.empty,
                    base:                 File                                                                                   = cmdline.base): PFTResult =
  {
    object preprocessCache extends (String => DepTree[Either[Maven.Artifact, Compiler.Source]]) {
      private def nopreprocess(fileSource: Compiler.FileSource): DepTree[Either[Maven.Artifact, Compiler.Source]] = new DepTree(files=List(Right(fileSource)))
      private val m = new collection.mutable.HashMap[String, DepTree[Either[Maven.Artifact, Compiler.Source]]]
      var files = List.empty[Compiler.FileSource]
      // todo: parent .scala -> loadedjars
      def apply(relPath: String): DepTree[Either[Maven.Artifact, Compiler.Source]] = {
        val canonicalPath = mkAbsFile(base, relPath).getCanonicalPath
        if (sourcesOfLoadedJars exists (_.file.getCanonicalPath == canonicalPath)) {
          //println(s"$relPath/$canonicalPath is among parentjoints")
          new DepTree(files = loadedJars map (Right(_)))
        } else {
          m.getOrElseUpdate(canonicalPath, { val fs = Compiler.FileSource(base, relPath)
                                             files ::= fs
                                             preprocess.applyOrElse(fs, nopreprocess) })
        }
      }
    }

    // canonicalize the tree by sorting and deduplicating. also remove artifacts which childern have
    def optimize(dtx2: DepTree[Either[Maven.Artifact, Compiler.Source]]) = dtx2.fold[DepTree[Either[Maven.Artifact, Compiler.Source]]] {
      (folders, files) =>
        new DepTree(folders = folders.sortBy(_.toString).distinct,
                    files   = (files  .toSet -- folders.flatMap(_.flatten).filter{case Left(_) | Right(_: Compiler.`.jar`) => true case _ => false }).toList.sortBy(_.toString))
    }

    //System.err.println(s"base=${base}")
    //System.err.println(s"s=${s}")
    //System.err.println(s"loadedJars=${loadedJars}")

    // expand %-variables and wildcards
    val dtx1: DepTree[Either[Maven.Artifact, Compiler.Source]] = source match {
      case Left(deptreeString) =>
        DepTree.fromString(deptreeString) flatMap {
          s => new DepTree(files=globImportSpec(base, s))
        } flatMap {
          case Maven.Artifact.Spec(artifact) => new DepTree(files=List(Left(artifact)))
          case path if path contains "://"   => ???
          case relPath                       => preprocessCache(relPath)
          //new DepTree(files=List(Right())
        }
      case Right(singleSource) =>
        new DepTree(files = List(Right(singleSource)))
    }
    //System.err.println("dtx1=\n" + dtx1.toPretty)

    // resolve magic imports
    val dtx2 = new resolveMagicImports(preprocessCache)(dtx1)
    //System.err.println("dtx2=\n" + dtx2.toPretty)


    val dtx3 = optimize(dtx2)
    //System.err.println("dtx3=\n" + dtx3.toPretty)

    // sanity check
    locally {
      val compilationUnits: List[DepTree[Either[Maven.Artifact, Compiler.Source]]] = dtx3.traverseFolders.toSet.toList // that will be myjar
      //println( compilationUnits.map(_.files) map (_.toString) mkString "\n---\n" )
      val sourceFiles = compilationUnits.flatMap(_.files).flatMap{ case Right(fs: Compiler.`.scala`) => Some(fs)
                                                                   case Right(fs: Compiler.`.java` ) => Some(fs)
                                                                   case _                            => None }
      val nonUniqueSources = sourceFiles.groupBy(_.relPath).mapValues(_.length).filter(_._2 > 1).keys
      require(nonUniqueSources.isEmpty, s"nonUniqueSources:\n${nonUniqueSources map (fs => s"$fs\n  in ${compilationUnits.map(_.files.flatMap(_.right.toOption).map(_.relPath)).filter(_ contains fs) mkString "\n AND "}") mkString "\n" }")
    }


    // resolve maven artifacts
    val loadedArtifacts: Set[Maven.Artifact] = (loadedJars flatMap {
      case mjfs: Compiler.MavenJarFileSource => Some(mjfs.artifact)
      case _                                 => None
    }).toSet
    //System.err.println("loadedArtifacts=" + loadedArtifacts)

    val solution: Map[Maven.Artifact, Set[Maven.Artifact]] = {
      // artifacts among sources (e.g. .pom files or nixstore paths)
      val mjfses: Set[Maven.Artifact] = dtx3.flatten.flatMap{ case Right(mjfs: Compiler.MavenJarFileSource) => Some(mjfs.artifact) case _ => None }.toSet -- loadedArtifacts
      //System.err.println("mjfses=" + mjfses)

      // artifacts which do not have files yet
      val artifacts: Set[Maven.Artifact] = dtx3.flatten.flatMap(_.left.toOption).toSet
    //require((artifacts & loadedArtifacts).isEmpty, artifacts & loadedArtifacts)
    //require((artifacts & mjfses         ).isEmpty, artifacts & mjfses         )

      Cache.memo2/*[Map[Maven.Artifact, Set[Maven.Artifact]]]*/(
        key               = s"${(artifacts++loadedArtifacts++mjfses).toList.sorted} ${ classpathFiles(Launcher.getClass.getClassLoader, withSystemJars=false).head.sha /*hash of launcher.jar*/ }",
        toSerialiazable   = (m: Map[Maven.Artifact, Set[Maven.Artifact]]) => m map { case (k                     ,v) => k.toString -> v.map{ _.toString                       } },
        fromSerialiazable = (m: Map[String,         Set[String        ]]) => m map { case (Maven.Artifact.Spec(k),v) => k          -> v.map{ case Maven.Artifact.Spec(a) => a } }) {
      //if (cmdline.verbose) System.err.println(s"""|resolving: $BOLD${artifacts                          mkString " "}$RESET
      //                                            |   loaded: $BOLD${loadedArtifacts                    mkString " "}$RESET
      //                                            |among src: $BOLD${mjfses                             mkString " "}$RESET
      //                                            |    total: $BOLD${artifacts++loadedArtifacts++mjfses mkString " "}$RESET""".stripMargin)
        val solution = Maven.resolveTogether(artifacts = artifacts++loadedArtifacts++mjfses, excludes=excludes)
        for (a <- loadedArtifacts++mjfses) {
          require(solution(a).contains(a), s"loaded artifact $a requires upgrade to ${solution(a)}; todo: auto-downgrade?")
        }
        val rc = solution /*.mapValues(_ -- loaded) -- loaded*/
      //if (cmdline.verbose) System.err.println(s"solution: $BOLD${(rc.keys ++ rc.values.flatten).toArray.sorted mkString " "}$RESET")
      //if (cmdline.verbose) System.err.println(s"solution:\n$BOLD${rc.keys.toArray.sorted map (t => (t :: rc(t).toList.sorted) mkString "\n  ") mkString "\n"}$RESET")
        rc
      }
    }

    // add dependencies
    val dtx3a: DepTree[Either[Maven.Artifact, Compiler.Source]] = dtx3 flatMap {
      case      Left(artifact)                           => new DepTree(files = solution(artifact     ).toList.sorted map (Left(_))) // solution could miss artifact if it is already in `loaded`?
      case rfs@ Right(mjfs: Compiler.MavenJarFileSource) => val sol = solution(mjfs.artifact).toList.sorted
                                                            if (sol contains mjfs.artifact) // keep non-maven paths like "ms.webmaster.amphithere/target/scala-2.11/amphithere_2.11-0.0.20170803.jar" as MavenJarFileSource, not JarFileSource
                                                              new DepTree(files = rfs :: sol.filter(_ != mjfs.artifact).map(Left(_)))
                                                            else                            // artifact was upgraded
                                                              new DepTree(files = sol map (Left(_)))
      case rfs@ Right(fs)                                => new DepTree(files = rfs :: Nil)
    }
    //System.err.println("dtx3a=\n" + dtx3a.toPretty)

    // substitute artifacts with files of them (and their deps)
    val dtx4: DepTree[Compiler.Source] = dtx3a flatMap {
      case Left(artifact) => new DepTree(files = artifact.jarFile.toList map (file => Compiler.FileSource(new File("."), file.getAbsolutePath)))
      case Right(fs)      => new DepTree(files = List(fs))
    }
    //System.err.println("dtx4=\n" + dtx4.toPretty)

    val dtx5: DepTree[Compiler.Source] =
      if (loadedJars.isEmpty) // compile "for export"
        dtx4
      else {
        // it must not include irrelevant jars from "java -cp"
        // number 12 is a HACK: 1 + length(@launcher_artifacts_resolved)
        val `launcher.jar+deps`: List[Compiler.FileSource] = /*classpathFiles(Launcher.getClass.getClassLoader, withSystemJars=false)*/loadedJars take 12
        require(`launcher.jar+deps`.count(_.isInstanceOf[Compiler.`.jar`]            ) == 12 &&
                `launcher.jar+deps`.count(_.isInstanceOf[Compiler.MavenJarFileSource]) == 11 &&
               (`launcher.jar+deps`.count(_.file.getName startsWith "scala-"         )
               +`launcher.jar+deps`.count(_.file.getName startsWith "paradise"       )
               +`launcher.jar+deps`.count(_.file.getName startsWith "quasiquotes"    ))==  5 &&
                `launcher.jar+deps`.count(_.file.getName startsWith "jline"          ) ==  1 &&
                `launcher.jar+deps`.count(_.file.getName startsWith "jna-"           ) ==  2 &&
                `launcher.jar+deps`.count(_.file.getName startsWith "sourcecode"     ) ==  1 &&
                `launcher.jar+deps`.count(_.file.getName startsWith "fastparse"      ) ==  2, s"`launcher.jar+deps`=\n${`launcher.jar+deps` mkString "\n"}")
        val `scala-library.jar` = `launcher.jar+deps`.flatMap{ case fs: Compiler.MavenJarFileSource if fs.artifact.artifactId=="scala-library" => Some(fs) case _ => None }.head

        dtx4 flatMapFolders { dt =>
          val scalas: List[Compiler.`.scala`] = dt.files flatMap { case fs: Compiler.`.scala` => Some(fs) case _ => None }
          new DepTree(files = dt.files ++ (if (scalas exists (_.stringContent contains "import ms.webmaster.launcher"))
                                             `launcher.jar+deps`
                                           else if (scalas.nonEmpty)
                                             List(`scala-library.jar`)
                                           else
                                             Nil))
        }
      }
    //System.err.println("dtx5=\n" + dtx5.toPretty)

    // only to control verbosity of debug output
    val mainArtifacts: Set[Maven.Artifact] = solution.map{ case (a, sol) => sol.find(b=>a.groupId==b.groupId && a.artifactId==b.artifactId).get }.toSet -- loadedArtifacts

    allreadfiles ++= preprocessCache.files.map(_.file)
    PFTResult(files         = preprocessCache.files, // todo: Map[FileSource, /*future jars*/List[`.jar`]]
              deptree       = dtx5,
              mainArtifacts = mainArtifacts)
  }



  cmdline.action match {
    case "resolve" =>  // mimic coursier
      val scopes = Set("compile", "runtime" /*, "provided"*/)
      val solution = Maven.resolveTogether(artifacts = cmdline.artifacts /*++ cmdline.forces*/ /*++ cmdline.loaded*/,
                                           scopes    = scopes,
                                           excludes  = cmdline.excludes,
                                           optionals = cmdline.optionals)/*.mapValues(_ -- cmdline.loaded)*/ /*-- (cmdline.forces.toSet -- cmdline.artifacts)*/ /*-- cmdline.loaded*/
//      for (a <- cmdline.loaded) {
//        require(solution(a).contains(a), s"loaded $a upgrading to ${solution(a)}; todo: auto-downgrade?")
//      }

      if (cmdline.printTree) {
        for (t <- solution.keys.toArray.sorted) {
          val included: Map[(String, String), Maven.Artifact] = solution(t).map(a => (a.groupId, a.artifactId) -> a).toMap

          def printdeps(a: Maven.Artifact, exs: Set[Maven.Exclude], depth: Int=1) {
            for (d <- a.dependencies.sortBy(_.artifact) if cmdline.optionals || !d.optional
                                                        if scopes contains d.scope
                                                        if !exs.exists(_ matches d.artifact)) {
              included get (d.artifact.groupId, d.artifact.artifactId) match {
                case Some(aa) =>
                  println(s"${"  "*depth}+ $d${ if (d.artifact.version != aa.version) " -> " + aa.version else "" }")
                  printdeps(aa, d.excludes ++ exs, depth+1)
                case None =>
                  ???; println(s"${"  "*depth}- $d")
              }
            }
          }
          println(t)
          printdeps(t, cmdline.excludes)
        }
      } else {
        println((/*solution.keys ++*/ solution.values.flatten).toSet.toArray.sorted mkString "\n")
      }

    case "fetch" => // mimic coursier
      val scopes = Set("compile", "runtime" /*, "provided"*/)
      val solution = Maven.resolveTogether(artifacts = cmdline.artifacts /*++ cmdline.forces*/ /*++ cmdline.loaded*/,
                                           scopes    = scopes,
                                           excludes  = cmdline.excludes,
                                           optionals = cmdline.optionals)/*.mapValues(_ -- cmdline.loaded)*/ /*-- (cmdline.forces.toSet -- cmdline.artifacts)*/ /*-- cmdline.loaded*/
      for (a <- solution.values.flatten.toSet[Maven.Artifact].toArray.sorted) {
        a.jarFile match {
          case None    => //println(s"# $a")
          case Some(f) => println(f)
        }
      }

    case "launch" => // mimic coursier
      ???

    case "compile-deptree" =>  // precompile joints
      val cp: List[Compiler.FileSource] = (/* `launcher.jar+deps` + java classpath */classpathFiles(Launcher.getClass.getClassLoader, withSystemJars=false):::cmdline.classpath).distinct
      val pft: PFTResult = parseFileTree(Left(cmdline.deptree),
                                         loadedJars = cp,
                                         sourcesOfLoadedJars = Nil)
      val jars: List[Compiler.FileSource] = if (cmdline.dryRun) {
                                              pft.deptree.flatten.flatMap{ case fs: Compiler.`.jar` => Some(fs) case _ => None }.toList.distinct
                                              //println(artifacts.toArray.sorted /*map (a=>s"${a.groupId}:${a.artifactId}:${a.version}")*/ mkString "\n")
                                            } else {
                                              Compiler.compileTreeToJar(pft.deptree, cmdline.scalacArguments, noServer=cmdline.noServer, verbose=true, pft.mainArtifacts)
                                            }
      println(jars.map{ case mjfs: Compiler.MavenJarFileSource => mjfs.artifact
                        case fs:   Compiler.FileSource         => fs.file.getAbsolutePath }.map(_.toString).sorted mkString "\n")

    case "compile-jointtree" =>  // precompile joints (it knows about "import parentjoint")
      val cp: List[Compiler.FileSource] = (/* `launcher.jar+deps` + java classpath */classpathFiles(Launcher.getClass.getClassLoader, withSystemJars=false):::cmdline.classpath).distinct

      val alljars = new collection.mutable.HashSet[Compiler.FileSource]
      var stack = List.empty[(/*jars*/List[Compiler.FileSource], PFTResult)]
      for(line <- cmdline.deptree.split("joint>").map(_.trim).filter(_.nonEmpty)) {
        val depth = line.takeWhile(_=='>').length
        require(stack.length >= depth)
        stack = stack drop stack.length-depth

        val pft: PFTResult = parseFileTree(Left(line dropWhile (_=='>')),
                                           loadedJars          = cp:::stack./*reverse.*/flatMap(_._1),
                                           sourcesOfLoadedJars = stack./*reverse.*/flatMap(_._2.files))
        if (cmdline.dryRun) {
          stack ::= (pft.deptree.flatten.toList flatMap { case fs: Compiler.`.jar` => Some(fs) case _ => None }, pft) // it affects maven resolving, but compiling does not
        } else {
          val jars: List[Compiler.FileSource] = Compiler.compileTreeToJar(pft.deptree, cmdline.scalacArguments, noServer=cmdline.noServer, verbose=true, pft.mainArtifacts)
          stack ::= (jars, pft)
        }
        alljars ++= stack.head._1
      }

      // collisions might be intentional, but here we assume that we want to have a single classpath for all joints.
      val artifacts: collection.mutable.HashSet[Maven.Artifact] = alljars flatMap { case mjfs: Compiler.MavenJarFileSource => Some(mjfs.artifact) case _ => None }
      val collisions = artifacts.groupBy(a => (a.groupId, a.artifactId)).filter(_._2.size > 1).toArray.flatMap(_._2).sorted
      if (collisions.nonEmpty) {
        System.err.println(s"Collisions:\n${collisions mkString "\n"}")
        sys exit 1
      }

      println(alljars.map { case mjfs: Compiler.MavenJarFileSource => mjfs.artifact
                            case fs                                => fs.file.getAbsolutePath }.map(_.toString).toArray.sorted mkString "\n")


    case "scalac" =>  // mimic scalac
      Compiler.compileCrate(cmdline.outputJar, cmdline.sources:::cmdline.classpath, cmdline.scalacArguments, noServer=cmdline.noServer)

      // also create .pom if destination is ubiquituos artifact location (nix store)
      cmdline.outputJar match {
        case Maven.Artifact.Path(artifact) =>
          require(!artifact.pomFile.exists)
          val fos = new FileOutputStream(artifact.pomFile)
          def toxml(a: Maven.Artifact) = s"<groupId>${a.groupId}</groupId><artifactId>${a.artifactId}</artifactId><version>${a.version}</version>"
          fos write s"""|<?xml version='1.0' encoding='UTF-8'?>
                        |<project>
                        | ${toxml(artifact)}
                        | <dependencies>
                        |${ cmdline.classpath.map{ case mjfs: Compiler.MavenJarFileSource => s"<dependency>${toxml(mjfs.artifact)}</dependency>\n" case _ => "" }.mkString }
                        | </dependencies>
                        |</project>
                        |""".stripMargin.getBytes("UTF-8")
          fos.close()
        case _ =>
      }

    case "service" => // TODO: deprecate; just --run and systemd restarter
      def ifMyService(x: AnyRef)(f: MyService=>Unit): Unit = if (x.isInstanceOf[MyService]) f(x.asInstanceOf[MyService])

      def makeRootJoint: Option[Joint[AnyRef]] = {
        try {
          Some(new Joint[AnyRef](cmdline.klass, watched=true) {
            override def onInit   (newObj:  AnyRef)                 { require(Thread.currentThread.getContextClassLoader eq newObj.getClass.getClassLoader)
                                                                      ifMyService(newObj )(_.postinit()) }
            override def onChange (prevObj: AnyRef, newObj: AnyRef) { require(Thread.currentThread.getContextClassLoader eq newObj.getClass.getClassLoader)
                                                                      ifMyService(prevObj)(_.stop())
                                                                      ifMyService(newObj )(_.postinit()) }
            override def onDiscard(prevObj: AnyRef)                 { ifMyService(prevObj)(_.stop()) }
            def readSourcesPFT() = { //require(parentCL eq Launcher.getClass.getClassLoader) /* debug */;
                                     parseFileTree(Left(cmdline.deptree), // volatile, watcher will glob wildcards each time? todo: on .files change
                                                   loadedJars          = classpathFiles(Launcher.getClass.getClassLoader, withSystemJars=false),
                                                   sourcesOfLoadedJars = Nil) }
          })
        } catch {
          case e: Throwable if e.isInstanceOf[Compiler.Exception] || (e.isInstanceOf[ExceptionInInitializerError] && e.getCause.isInstanceOf[Compiler.Exception]) =>
            print(s"main joint compilation error, waiting for source changes: (${allreadfiles map (_.getName) mkString " "})")
            val qhlauncher = Joint.quickHash(List(cmdline.launcherSrc))
            val qh = Joint.quickHash(allreadfiles)
            do {
              Thread.sleep(1000)
              print("x")
              if (qhlauncher != Joint.quickHash(List(cmdline.launcherSrc)))
                scala.sys.exit(168 /* quick restart */)
            } while(qh == Joint.quickHash(allreadfiles))
            //scala.sys.exit(168 /* TODO: quick restart if perl part have been changed (list of jars, java command line, etc) */)
            None

        //case e: ExceptionInInitializerError =>
        //  scala.sys.exit(2) // stacktrace already printed?
          case e: Throwable =>
            e.printStackTrace()
            scala.sys.exit(3)
        }
      }
      @annotation.tailrec def makeRootJoint2: Option[Joint[AnyRef]] = makeRootJoint match { case None => makeRootJoint2 case x => x }
      val rootJoint = makeRootJoint2

      val pressedCtrlC = new AtomicBoolean(false)
      val sigint = new Signal("INT") // does not work on linux, why?
      val oldSigintHandler = Signal.handle(sigint, new SignalHandler {
        def handle(p1: Signal) {
          println("first Ctrl-C, stoping gracefully")
          pressedCtrlC.synchronized { pressedCtrlC set true; pressedCtrlC.notify() }
        }
      })
      try {
        pressedCtrlC.synchronized { while(!pressedCtrlC.get) pressedCtrlC.wait() }
      } finally {
        Signal.handle(sigint, oldSigintHandler)
      }
      ifMyService(rootJoint)(_.stop())
      println("=bye=")

    case "run" =>
      val klass = try {
        val rootJoint = new Joint[Class[_]](cmdline.klass) {
          def readSourcesPFT() = { //require(parentCL eq Launcher.getClass.getClassLoader) /* debug */;
                                   parseFileTree(Left(cmdline.deptree),
                                                 loadedJars          = classpathFiles(Launcher.getClass.getClassLoader, withSystemJars=false),
                                                 sourcesOfLoadedJars = Nil) }
          override def instantiate(cl: ClassLoader): Class[_] = cl.loadClass(mainClassName)
        }
        //require(rootJoint eq Joint.root)
        rootJoint.getObject
      } catch {
        case e: Compiler.Exception => scala.sys.exit(167 /* initial compilation error */)
      }
      Thread.currentThread setContextClassLoader klass.getClassLoader

      if (cmdline.klass endsWith "$") // create `object` and run its `main`, why?
        klass.getField("MODULE$").get(null).asInstanceOf[{def main(args: Array[String])}].main(cmdline.appargs)
      else { // invoke static main w/o creating `class`
        val method = klass.getMethod("main", classOf[Array[String]])
        require(java.lang.reflect.Modifier.isStatic(method.getModifiers), cmdline.klass+".main is not static")
        try {
          method.invoke(null, cmdline.appargs)
        } catch {
          case e: java.lang.reflect.InvocationTargetException => throw e.getCause
        }
      }

    case action => println(s"invalid action: '$action'")
  }
}

// TODO?: deprecate some of these functions in favor of com.lihaoyi.sourcecode (it is here as fastparse dep anyway)
package object position {
  def `__FILE__`:            File   = { val name = (new Throwable).getStackTrace()(1).getFileName
                                        if (Launcher.cmdline == null) // run precompiled jar, not via Launcher. no sources on disk are expected
                                          new File(if (name startsWith "__") "/"+name.drop(2) else name)
                                        else
                                          mkAbsFile(Launcher.cmdline.base, if (name startsWith "__") "/"+name.drop(2) else name)
                                      }
  def `__DIR__`:             File   = __CALLER_FILE__.getParentFile
  def `__LINE__`:            Int    = (new Throwable).getStackTrace()(1).getLineNumber
  def `__CLASS__`:           String = (new Throwable).getStackTrace()(1).getClassName
  def `__METHOD__`:          String = (new Throwable).getStackTrace()(1).getMethodName
  def `__FUNCTION__`:        String = { val s = (new Throwable).getStackTrace()(1); s.getClassName + "." + s.getMethodName }

  def `__CALLER_FILE__`:     File   = { val name = (new Throwable).getStackTrace()(2).getFileName
                                        if (Launcher.cmdline == null) // run precompiled jar, not via Launcher. no sources on disk are expected
                                          new File(if (name startsWith "__") "/"+name.drop(2) else name)
                                        else
                                          mkAbsFile(Launcher.cmdline.base, if (name startsWith "__") "/"+name.drop(2) else name)
                                      }
  def `__CALLER_LINE__`:     Int    = (new Throwable).getStackTrace()(2).getLineNumber
  def `__CALLER_CLASS__`:    String = (new Throwable).getStackTrace()(2).getClassName
  def `__CALLER_METHOD__`:   String = (new Throwable).getStackTrace()(2).getMethodName
  def `__CALLER_FUNCTION__`: String = { val s = (new Throwable).getStackTrace()(2); s.getClassName + "." + s.getMethodName }
}

} // package launcher
