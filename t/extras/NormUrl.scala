#!
use File::Basename qw(dirname basename);
require(dirname(__FILE__).'/../launcher/Launcher.pl.scala');

run(CLASS     => 'ms.webmaster.url.Test',
    TEE       => "normurl-%TIME%.log");
=cut
!#

package ms.webmaster.url

import scala.util.Try
import scala.Console.{out => _, err => _, _}
import scala.collection.immutable.TreeMap
import java.nio.ByteBuffer
import sun.net.util.IPAddressUtil

import `commons-io:2.4`,    org.apache.commons.io.FileUtils
import `HttpUtils.scala`,   ms.webmaster.httputils
import `BinOrdering.scala`, ms.webmaster.binordering._


class ParseException(msg: String, cause: Throwable=null) extends RuntimeException(msg, cause)

sealed trait Host {
  def matches      (regex: String):               Boolean = toString.matches(regex)           // TODO: can be optimized (Regex works with CharSequence)
  def contains     (cs: CharSequence):            Boolean = toString.contains(cs)             // can be optimized in subclasses
  def contentEquals(cs: CharSequence):            Boolean = toString.contentEquals(cs)        // TODO: compare with all possible forms (xn, uni, up/lo case, ...)
  def startsWith   (s: String/*, begin: Int=0*/): Boolean = toString.startsWith(s/*, begin*/) // TODO: compare with all possible forms
  def endsWith     (s: String):                   Boolean = toString.endsWith(s)              // TODO: compare with all possible forms
  // strLength
  def toUniString:  String
  def isInternet:   Boolean
}

object Host {
  private[this] def parseSlow(s: String): Host = Try(IPv6 parse s) orElse Try(IPv4 parse s) getOrElse (Dom parse s)
  def parse(s: String): Host = {
    if (s==".")
      Root
    else {
      val bin: Array[Byte] = IPAddressUtil.textToNumericFormatV6(if (s.head=='[' && s.last==']') s.substring(1,s.length-1) else s)
      if (bin != null)
        IPv6.fromByteArray(bin)
      else if (s forall ("0123456789." contains _))
        IPv4 parse s
      else if (!(s.head=='0' && "abcdefABCDEF".contains(s.last)))
        Dom parse s
      else
        Try(IPv4 parse s) getOrElse (Dom parse s)
    }
  }

  protected[url] trait AnyOrdering extends Ordering[Host] {  // IPv4 < IPv6 < Dom
    val domainOrdering: Ordering[Dom]

    override def compare(a: Host, b: Host): Int = (a, b) match {
      case (a1: IPv4, b1: IPv4) => a1 compare b1
      case (a1: IPv6, b1: IPv6) => a1 compare b1
      case (a1: Dom,  b1: Dom ) => domainOrdering.compare(a1, b1)
      case ( _: IPv4, _       ) => -1
      case ( _,       _: IPv4 ) => 1
      case ( _: Dom,  _       ) => 1
      case ( _,       _: Dom  ) => -1
    }
  }
  object AsciiOrdering extends AnyOrdering { val domainOrdering = Dom.AsciiOrdering }
  object UniOrdering   extends AnyOrdering { val domainOrdering = Dom.UniOrdering   }

}

sealed trait IP extends Host with IndexedSeq[Int] with java.io.Serializable /* to be used in Akka serialization */ {
           def isInternet:    Boolean
           def isLocal:       Boolean
           def isPrivate:     Boolean
           def isLinkLocal:   Boolean
           def toByteArray:   Array[Byte]
  final    def toInetAddress: java.net.InetAddress = java.net.InetAddress.getByAddress(toByteArray)
  final    def toUniString:   String               = toString
}

private object BitUtils {
  def intBits (bits: Int): Int  = { require(0 <= bits && bits <= 32); (if (bits==32) 0  else 1 <<bits)-1 }
  def longBits(bits: Int): Long = { require(0 <= bits && bits <= 64); (if (bits==64) 0L else 1L<<bits)-1 }
}

final class IPv4 private(val toInt: Int) extends IP with Ordered[IPv4] {
           def length:                                      Int     = 4 // four 8-bit words
           def apply(i: Int):                               Int     = (i match { case 0 => toInt>>>24
                                                                                 case 1 => toInt>>>16
                                                                                 case 2 => toInt>>>8
                                                                                 case 3 => toInt
                                                                                 case _ => throw new IndexOutOfBoundsException }) & 0xFF
  override def equals(that: Any):                           Boolean = that match { case that1: IPv4 => toInt==that1.toInt
                                                                                   case _           => false }
  override def compare(that: IPv4):                         Int     = UIntOrdering.compare(toInt, that.toInt)
  override def hashCode:                                    Int     = toInt.hashCode
  override def toString:                                    String  = s"${(toInt>>>24) & 255}.${(toInt>>>16) & 255}.${(toInt>>>8) & 255}.${toInt & 255}" // mkString(".")
  override def isInternet = !isLocal && !isPrivate && !isLinkLocal && UIntOrdering.lteq(0x01000000, toInt) && UIntOrdering.lt(toInt, 0xE0000000) // exclude 0.0.0.0/8 224.0.0.0/4 240.0.0.0/4
  override def isLocal    =   UIntOrdering.lteq(0x7F000000, toInt) && UIntOrdering.lteq(toInt, 0x7FFFFFFF)  // 127.0.0.0/8
           def isPrivate  = ( UIntOrdering.lteq(0x0A000000, toInt) && UIntOrdering.lteq(toInt, 0x0AFFFFFF)  // 10.0.0.0/8
                           || UIntOrdering.lteq(0xAC100000, toInt) && UIntOrdering.lteq(toInt, 0xAC1FFFFF)  // 172.16.0.0/12
                           || UIntOrdering.lteq(0xC0A80000, toInt) && UIntOrdering.lteq(toInt, 0xC0A8FFFF)  // 192.168.0.0/16
                           || UIntOrdering.lteq(0x64400000, toInt) && UIntOrdering.lteq(toInt, 0x647FFFFF)) // 100.64.0.0/10  RFC 6598
           def isLinkLocal=   UIntOrdering.lteq(0xA9FE0000, toInt) && UIntOrdering.lteq(toInt, 0xA9FEFFFF)  // 169.254.0.0/16
           def toByteArray                                         = Array[Byte]((toInt>>>24).toByte, (toInt>>>16).toByte, (toInt>>>8).toByte, toInt.toByte)
           def toIPv6:                                      IPv6   = new IPv6(0,                   (toInt&0xFFFFFFFFL)) // deprecated?
           def toMappedIPv6:                                IPv6   = new IPv6(0, 0xFFFF00000000L | (toInt&0xFFFFFFFFL))

           def floor(bits: Int):                            IPv4   = IPv4 fromInt (toInt & ~BitUtils.intBits(32-bits))
           def ceil (bits: Int):                            IPv4   = IPv4 fromInt (toInt |  BitUtils.intBits(32-bits))
}
object IPv4 {
           def fromInt(i: Int):                             IPv4    = new IPv4(i)
           def apply(a: Int, b: Int, c: Int, d: Int):       IPv4    = new IPv4(((a&0xFF)<<24)|((b&0xFF)<<16)|((c&0xFF)<<8)|(d&0xFF))
           def fromByteArray(bin: Array[Byte]):                     IPv4 = fromByteArray(bin, 0, bin.length)
           def fromByteArray(bin: Array[Byte], off: Int, len: Int): IPv4 = len match { case 4 => apply(bin(off+0), bin(off+1), bin(off+2), bin(off+3))
                                                                                       case _ => throw new ParseException("expected length=4") }
           def fromInetAddress(addr: java.net.InetAddress): IPv4    = fromByteArray(addr.getAddress)
           def parse(s: String):                            IPv4    = { // todo: optimize
             if (!"0123456789abcdefABCDEF".contains(s.last)) throw new ParseException(s)
             val ints: Array[Long] = s split '.' map {
               case part if part startsWith "0x" => java.lang.Long.parseLong(part stripPrefix "0x", 16)
               case part if part startsWith "0"  => java.lang.Long.parseLong(part, 8)
               case part                         => java.lang.Long.parseLong(part, 10)
             }
             ints.length match {
               case 1 if 0<=ints(0) && ints(0)<=0xFFFFFFFFL                                                        => new IPv4(ints(0).toInt)
               case 2 if 0<=ints(0) && ints(0)<=255 && 0<=ints(1) && ints(1)<=0xFFFFFF                             => new IPv4(((ints(0)<<24)|ints(1)).toInt)
               case 3 if 0<=ints(0) && ints(0)<=255 && 0<=ints(1) && ints(1)<=255 && 0<=ints(2) && ints(2)<=0xFFFF => new IPv4(((ints(0)<<24)|(ints(1)<<24)|ints(2)).toInt)
               case 4 => apply(ints(0).toInt, ints(1).toInt, ints(2).toInt, ints(3).toInt)
               case _ => throw new ParseException(s)
             }
           }
}


final class IPv6 /*private[url]*/(private[url] val hi: Long,
                                  private[url] val lo: Long) extends IP with Ordered[IPv6] {
           def length:                                      Int     = 8 // eight 16-bit words
           def apply(i: Int):                               Int     = (i match { case 0 => hi>>>48
                                                                                 case 1 => hi>>>32
                                                                                 case 2 => hi>>>16
                                                                                 case 3 => hi
                                                                                 case 4 => lo>>>48
                                                                                 case 5 => lo>>>32
                                                                                 case 6 => lo>>>16
                                                                                 case 7 => lo
                                                                                 case _ => throw new IndexOutOfBoundsException }).toInt & 0xFFFF
  override def equals(that: Any):                           Boolean = that match { //case that1:IPv4 => hi==0 && (lo>>>32)==0 && lo.toInt==that1.a
                                                                                    case that1:IPv6 => hi==that1.hi && lo==that1.lo
                                                                                    case _          => false }
  override def hashCode:                                    Int     = hi.hashCode * 31 + lo.hashCode
  override def compare(that: IPv6):                         Int     = { val c=ULongOrdering.compare(hi, that.hi); if (c!=0) c else ULongOrdering.compare(lo, that.lo) }
           def isMappedIPv4:                                Boolean       = hi==0 && (lo>>>32)==0xFFFF
           def mappedIPv4:                                  Option[IPv4]  = if (isMappedIPv4) Some(IPv4.fromInt(lo.toInt)) else None
           def isIPv4:                                      Boolean       = hi==0 && (lo>>>32)==0 && (lo&0xFFFF0000)!=0                // deprecated?
           def toIPv4:                                      Option[IPv4]  = if (isIPv4) Some(IPv4.fromInt(lo.toInt)) else None         // deprecated?
           def toStringUnoptimized:                         String  = map (_.toHexString) mkString ":"
  override def toString:                                    String  = {
             val posA = this.indexOf(0)
             if (posA == -1)                                        this.map(Integer.toHexString _).mkString(":") // not optimizable
             else if (hi==0 && (lo>>>32)==0xFFFF)                   "::ffff:" + IPv4.fromInt(lo.toInt).toString // mapped IPv4
             else if (hi==0 && (lo>>>32)==0 && (lo&0xFFFF0000)!=0)  "::"      + IPv4.fromInt(lo.toInt).toString // non-mapped IPv4
             else this.indexWhere(_ != 0, posA) match {
               case -1   => (0 until posA).map(apply(_).toHexString).mkString(":") + "::"
               case posB => (0 until posA).map(apply(_).toHexString).mkString(":") + "::" + (posB until 8).map(apply(_).toHexString).mkString(":")
             }
           }
  override def isLocal     = hi==0 && lo==1
  override def isPrivate   = false // todo..
  override def isLinkLocal = false // todo..
  override def isInternet  = !isLocal && !isPrivate && !isLinkLocal // todo..
           def toByteArray                                         = ByteBuffer.allocate(16).putLong(hi).putLong(lo).array

           def floor(bits: Int):                            IPv6   =      if (bits >= 64) new IPv6(hi,                               lo & ~BitUtils.longBits(128-bits))
                                                                     else                 new IPv6(hi & ~BitUtils.longBits(64-bits), 0)
           def ceil (bits: Int):                            IPv6   =      if (bits >= 64) new IPv6(hi,                               lo |  BitUtils.longBits(128-bits))
                                                                     else                 new IPv6(hi |  BitUtils.longBits(64-bits), -1L)
}
object IPv6 {
           def apply(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) = new IPv6( ((a&0xFFFFL)<<48)|((b&0xFFFFL)<<32)|((c&0xFFFFL)<<16)|(d&0xFFFFL),
                                                                                                 ((e&0xFFFFL)<<48)|((f&0xFFFFL)<<32)|((g&0xFFFFL)<<16)|(h&0xFFFFL) )
           def fromByteArray(bin: Array[Byte]):                     IPv6 = fromByteArray(bin, 0, bin.length)
           def fromByteArray(bin: Array[Byte], off: Int, len: Int): IPv6 = {
             val bb = ByteBuffer.wrap(bin, off, len)
             bb.remaining match { case 16 => new IPv6(bb.getLong, bb.getLong)
                                  case  4 => new IPv6(0,          0x0000FFFF00000000L|(bb.getInt&0xFFFFFFFFL))
                                  case  _ => throw new ParseException("expected length of 4 or 16") }
           }
           def fromInetAddress(addr: java.net.InetAddress): IPv6    = fromByteArray(addr.getAddress)
           def parse(s: String):                            IPv6    = {
             val bin: Array[Byte] = IPAddressUtil.textToNumericFormatV6(if (s.head=='[' && s.last==']') s.substring(1,s.length-1) else s)
             if (bin == null)
               throw new ParseException(s"error parse IPv6 `$s'")
             fromByteArray(bin)
           }
}


object IP extends Ordering[IP] {
  def fromInetAddress(addr: java.net.InetAddress): IP    = {
    val raw = addr.getAddress
    raw.length match { case  4 => IPv4.fromByteArray(raw)
                       case 16 => IPv6.fromByteArray(raw)
                       case _  => ??? }
  }

  def parse(s: String): IP = if (s contains ':') {
                               val ip6 = IPv6.parse(s)
                               ip6.mappedIPv4 match {
                                 case Some(ip4) => ip4
                                 case None      => ip6
                               }
                             } else {
                               IPv4.parse(s)
                             }
  // first IPv4 then IPv6
  override def compare(a: IP, b: IP): Int = (a, b) match {
    case (a: IPv4, b: IPv4) => a compare b
    case (a: IPv6, b: IPv6) => a compare b
    case (_: IPv4, _: IPv6) => -1
    case (_: IPv6, _: IPv4) => 1
  }
}


// todo:=============================
trait IPNet {
  val head: IP
  val bits: Int
  def contains(ip: IP): Boolean
}

case class IPv4Net(head: IPv4, bits: Int) extends IPNet {
  require(head == head.floor(bits), (head, bits))
  override def toString:          String  = s"$head/$bits"
           def last:                       IPv4    = head.ceil(bits)
           def contains(ip: IP):           Boolean = ip match {
                                                       case ip4: IPv4 => ip4.floor(bits) == head
                                                       case ip6: IPv6 => (ip6.mappedIPv4 orElse ip6.toIPv4).exists((_:IPv4).floor(bits) == head)
                                                     }
}

case class IPv6Net(head: IPv6, bits: Int) extends IPNet {
  require(head == head.floor(bits), (head, bits))
  override def toString:          String  = s"$head/$bits"
           def last:                       IPv6    = head.ceil(bits)
           def contains(ip: IP):           Boolean = ip match {
                                                       case ip4: IPv4 => ip4.toMappedIPv6.floor(bits) == head || ip4.toIPv6.floor(bits) == head
                                                       case ip6: IPv6 => ip6.floor(bits) == head
                                                     }
}

object IPNet {
  def parse(s: String): IPNet = {
    val Array(headstr, bitsstr) = s.split('/')
    IP.parse(headstr) match {
      case ip: IPv4 => IPv4Net(ip, bitsstr.toInt)
      case ip: IPv6 => IPv6Net(ip, bitsstr.toInt)
    }
  }
}

// string with
// * limited length
// * limited set of allowed characters
// * IDNA conversions
// * intern cache (todo)
// Vec[1..63, 'a'..'z' | '0'..'9' | '-' | '_']
final class Name private(override val toString: String) /*extends AnyVal*/ {
  //override def toString:            String  = xn
     final val isIDNA:              Boolean = toString startsWith "xn--"
     final val toUniString:         String  = if (isIDNA) java.net.IDN.toUnicode(toString, java.net.IDN.ALLOW_UNASSIGNED) else toString
           def asciiLength:         Int     = toString.length
           def asciiCharAt(i: Int): Char    = toString charAt i
           def uniLength:           Int     = if (isIDNA) toUniString.length                   else asciiLength
           def uniByteLength:       Int     = if (isIDNA) toUniString.getBytes("UTF-8").length else asciiLength // todo: optimize

  override def equals(that: Any):                           Boolean = that match { case that1: Name => toString==that1.toString
                                                                                   case _           => false }
  override def hashCode:                                    Int     = toString.hashCode

//         def uniCharAt(i: Int):   Char   = ???
           def contentEquals(cs: CharSequence): Boolean = toString.contentEquals(cs) /* fast path */ || toString == java.net.IDN.toASCII(cs.toString, java.net.IDN.ALLOW_UNASSIGNED).toLowerCase /* slow path */
//         def startsWith(s: String):           Boolean = ???
//         def endsWith(s: String):             Boolean = ???
}

object Name {
  // todo: intern LRU cache
  final val star = new Name("*")

  def fromUnvalidatedString(xn: String): Name = new Name(xn)

  implicit def apply(s: String): Name = {
             if (s == "*") {
               star
             } else {
               if (s.length==0 || s.length > 63)
                 throw new ParseException(s"Invalid length ${s.length}: '$s'")
               if (s.forall(c => 'a'<=c && c<='z' || '0'<=c && c<='9' || c=='-' || c=='_')) {
                 fromUnvalidatedString(s)
               } else {
                 val xn = java.net.IDN.toASCII(s, java.net.IDN.ALLOW_UNASSIGNED).toLowerCase
                 if (xn.length > 63)
                   throw new ParseException(s"Invalid length ${xn.length}: '$xn'")
                 if (!xn.forall(c => 'a'<=c && c<='z' || '0'<=c && c<='9' || c=='-' || c=='_'))
                   throw new ParseException(s"Invalid character ${xn.length}: '$xn'")

                 fromUnvalidatedString(xn)
               }
             }
           }

  // like `String#compareTo` but order of '_' after the letters
  private[this] def compareStrings(a: String, b: String): Int = {
    val len1 = a.length
    val len2 = b.length
    val lim = Math.min(len1, len2)
    var k = 0
    while (k < lim) {
      val c1 = a(k) match { case '_' => '~' case c => c }
      val c2 = b(k) match { case '_' => '~' case c => c }
      if (c1 != c2) {
        return c1 - c2
      }
      k += 1
    }
    len1 - len2
  }
  object AsciiOrdering extends Ordering[Name] { def compare(a: Name, b: Name): Int = compareStrings(a.toString,    b.toString   ) }
  object UniOrdering   extends Ordering[Name] { def compare(a: Name, b: Name): Int = compareStrings(a.toUniString, b.toUniString) }
}

sealed trait Dom extends Host /*with IndexedSeq[Name]*/ { outer =>
           def tld:                TLD    // takeRight(1)
         //def take2:              Dom2   // takeRight(2)
         //def take3:              Dom3   // takeRight(3)
     final def takeLeft  (i: Int): List[Name] = List.tabulate(i)(applyLeft _)
     final def dropRight (i: Int): List[Name] = takeLeft(length-i)

           def takeRight (i: Int): Dom
     final def dropLeft  (i: Int): Dom        =  takeRight(length-i)
           def +: (n: Name):       Dom
     final def ++:(xs: Seq[Name]): Dom        = xs.foldRight(this)(_ +: _)
           def length:             Int          // IndexedSeq[Name], apply(0) is TLD's name
           def applyLeft (i: Int): Name
     final def applyRight(i: Int): Name       = applyLeft(length-i-1)

     final def childOf(that: Dom   ): Boolean = length>=that.length && (0 until that.length).forall(i=>this.applyRight(i)==that.applyRight(i))
   //final def childOf(that: String): Boolean = this.contentEquals(that) || this.endsWith("."+that) // todo: optimize

           def toUniString:        String     = (0 until length) map (applyLeft(_).toUniString) mkString "."
           def asciiLength:        Int  //    = toString.length = (0 until length).map(apply(_).length).sum + length - 1
           def asciiCharAt(i:Int): Char       = { var j=i; var k=0;
                                                  while(k<length) {
                                                    val n=applyLeft(k); val l=n.asciiLength;
                                                    if (j<=l) return if (j==l) '.' else n.asciiCharAt(j)
                                                    k+=1
                                                    j-=l+1
                                                  }
                                                  ???
                                                  tld.name.asciiCharAt(j)
                                                }
           def uniLength:          Int  //    = toUniString.length
           def uniByteLength:      Int  //    = toUniString.getBytes("UTF-8").length

  override def contentEquals(cs: CharSequence): Boolean = {
           val len = cs.length
           var pos = 0
           for (i <- 0 until length) {
             if (i != 0) {
               if (pos==len || cs.charAt(pos) != '.')
                 return false
               pos += 1
             }
             val name = applyLeft(i)
             val ascii = name.toString
             if (pos+ascii.length <= len && (0 until ascii.length).forall(j=>cs.charAt(pos+j).toLower==ascii.charAt(j))) {
               pos += name.asciiLength
             } else if (name.isIDNA) {
               val uni = name.toUniString
               if (pos+uni.length <= len && (0 until uni.length).forall(j=>cs.charAt(pos+j).toLower==uni.charAt(j))) {
                 pos += uni.length
               } else {
                 return false
               }
             } else {
               return false
             }
           }
           return pos == len
         }

//override def startsWith(s: String, begin: Int): Boolean = { val len=asciiLength
//                                                            0<=begin && begin<len && s.length<=len-begin && (0 until  s.length).forall(i=>charAt(begin+i)==s.charAt(i)) }
//override def endsWith(s: String):               Boolean = { val len=asciiLength
//                                                            val begin=len-s.length
//                                                            (s.length<=len) && (0 until s.length).forall(i=>charAt(begin+i)==s.charAt(i)) }
 @deprecated
     final def toSeq = new IndexedSeq[Name] {
             def length        = outer.length
             def apply(i: Int) = outer applyLeft i
           }
     final def toAsciiCharSequence = new CharSequence {
             def length         = outer.asciiLength
             def charAt(i: Int) = outer asciiCharAt i
             def subSequence(off: Int, len: Int) = toString.subSequence(off, len)
           }
           def isInternet = tld.isValid
}

object Dom {
  def parse(s: String): Dom = fromIndexedSeq(s split '.')
  private[url] def fromIndexedSeq(a: IndexedSeq[String]): Dom = a.length match {
    case 0          => Root
    case 1          => TLD.interned(a(0))
    case 2          => new Dom2(TLD.interned(a(1)), a( 0))
    case 3          => new Dom3(TLD.interned(a(2)), a( 1),a(0))
    case 4          => new Dom4(TLD.interned(a(3)), a( 2),a(1),a(0))
    case 5          => new Dom5(TLD.interned(a(4)), a( 3),a(2),a(1),a(0))
    case n if n<130 => new DomN(TLD.interned(a(n-1)), a.map(Name(_)))
    case n          => throw new ParseException(s"domain has too many sections: $n")
  }

  protected[this] trait AnyOrdering extends Ordering[Dom] {
    val nameOrdering: Ordering[Name]
    def compare(a: Dom, b: Dom): Int = { val aty=a.length; val bty=b.length; val minty=Math.min(aty, bty)
                                         var i=0
                                         while (i<minty) {
                                           val c = nameOrdering.compare(a.applyRight(i), b.applyRight(i))
                                           if (c!=0) return c
                                           i+=1
                                         }
                                         return aty - bty
                                       }
  }
  object AsciiOrdering extends AnyOrdering { val nameOrdering = Name.AsciiOrdering }
  object UniOrdering   extends AnyOrdering { val nameOrdering = Name.UniOrdering   }
  //todo: NaturalOrdering ("s9" then "s10")
}

/*private[url]*/ case object Root extends Dom {
  override def tld                = throw new UnsupportedOperationException
  override def takeRight(i: Int)  = throw new UnsupportedOperationException
  override def +:(n: Name)        = TLD.interned(n)
  override def toUniString        = "."
  override def toString           = "."
  override def asciiLength        = 1
  override def uniLength          = 1
  override def uniByteLength      = 1
  override def asciiCharAt(i:Int) = ".".charAt(i)
  override def length             = 0
  override def applyLeft(i: Int)  = throw new IndexOutOfBoundsException
  override def isInternet         = false
}
case class TLD private(/*interned*/name: Name) extends Dom {
  require(TLD.allTLDs==null || TLD.allTLDs.get(name).forall(_ eq this), this)

  override def tld                = this
  override def takeRight(i: Int)  = i match {
                                      case 0 => Root
                                      case 1 => this
                                      case _ => throw new UnsupportedOperationException
                                    }
  override def +:(n: Name)        = new Dom2(this, n)
  override def toUniString        = name.toUniString
  override def toString           = name.toString
  override def asciiLength        = name.asciiLength
  override def uniLength          = name.uniLength
  override def uniByteLength      = name.uniByteLength
  override def asciiCharAt(i:Int) = name.asciiCharAt(i)
  override def length             = 1
  override def applyLeft(i: Int)  = i match { case 0 => name
                                              case _ => throw new IndexOutOfBoundsException }
           def isValid            = TLD.allTLDs contains name
           def isOld              = name.toString match { case _ if name.asciiLength==2          => true
                                                          case "com" | "net" | "org"
                                                             | "mil" | "edu" | "gov"
                                                             | "arpa"
                                                             | "info" | "biz" | "name" | "pro"
                                                             | "aero" | "asia" | "cat" | "coop" | "int" | "jobs" | "mobi" | "museum" | "post" | "tel" | "travel" | "xxx" => true
                                                          case _                                 => false
                                                        }
  override def isInternet         = name.toString match { case "ai" | "dk" | "tk" | "pn" | "to" | "uz" | "xn--l1acc" => true /* only TLDs with A-record; many TLDs (e.g google) point to 127.0.53.53 */
                                                          case "cm" | "gg" | "je" | "ac" | "io" | "sh" | "tm"        => true // resolved to IP with no http-server
                                                          case _                                                     => false }
}

object TLD {
  /*private[url]*/ def interned(n: Name) = allTLDs get n getOrElse new TLD(n)

  private[this] var readTldsAlphaByDomain: Array[String] = """
    aaa aarp abarth abb abbott abbvie abc able abogado abudhabi ac academy accenture accountant accountants aco active actor ad adac ads adult ae aeg aero aetna af afamilycompany afl africa ag agakhan agency ai aig aigo airbus airforce airtel akdn al alfaromeo alibaba alipay allfinanz allstate ally alsace alstom am americanexpress americanfamily amex amfam amica amsterdam analytics android anquan anz ao aol apartments app apple aq aquarelle ar arab aramco archi army arpa art arte as asda asia associates at athleta attorney au auction audi audible audio auspost author auto autos avianca aw aws ax axa az azure
    ba baby baidu banamex bananarepublic band bank bar barcelona barclaycard barclays barefoot bargains baseball basketball bauhaus bayern bb bbc bbt bbva bcg bcn bd be beats beauty beer bentley berlin best bestbuy bet bf bg bh bharti bi bible bid bike bing bingo bio biz bj black blackfriday blanco blockbuster blog bloomberg blue bm bms bmw bn bnl bnpparibas bo boats boehringer bofa bom bond boo book booking boots bosch bostik boston bot boutique box br bradesco bridgestone broadway broker brother brussels bs bt budapest bugatti build builders business buy buzz bv bw by bz bzh
    ca cab cafe cal call calvinklein cam camera camp cancerresearch canon capetown capital capitalone car caravan cards care career careers cars cartier casa case caseih cash casino cat catering catholic cba cbn cbre cbs cc cd ceb center ceo cern cf cfa cfd cg ch chanel channel chase chat cheap chintai chloe christmas chrome chrysler church ci cipriani circle cisco citadel citi citic city cityeats ck cl claims cleaning click clinic clinique clothing cloud club clubmed cm cn co coach codes coffee college cologne com comcast commbank community company compare computer comsec condos construction consulting contact contractors cooking cookingchannel cool coop corsica country coupon coupons courses cr credit creditcard creditunion cricket crown crs cruise cruises csc cu cuisinella cv cw cx cy cymru cyou cz
    dabur dad dance data date dating datsun day dclk dds de deal dealer deals degree delivery dell deloitte delta democrat dental dentist desi design dev dhl diamonds diet digital direct directory discount discover dish diy dj dk dm dnp do docs doctor dodge dog doha domains dot download drive dtv dubai duck dunlop duns dupont durban dvag dvr dz
    earth eat ec eco edeka edu education ee eg email emerck energy engineer engineering enterprises epost epson equipment er ericsson erni es esq estate esurance et etisalat eu eurovision eus events everbank exchange expert exposed express extraspace
    fage fail fairwinds faith family fan fans farm farmers fashion fast fedex feedback ferrari ferrero fi fiat fidelity fido film final finance financial fire firestone firmdale fish fishing fit fitness fj fk flickr flights flir florist flowers fly fm fo foo food foodnetwork football ford forex forsale forum foundation fox fr free fresenius frl frogans frontdoor frontier ftr fujitsu fujixerox fun fund furniture futbol fyi
    ga gal gallery gallo gallup game games gap garden gb gbiz gd gdn ge gea gent genting george gf gg ggee gh gi gift gifts gives giving gl glade glass gle global globo gm gmail gmbh gmo gmx gn godaddy gold goldpoint golf goo goodhands goodyear goog google gop got gov gp gq gr grainger graphics gratis green gripe grocery group gs gt gu guardian gucci guge guide guitars guru gw gy
    hair hamburg hangout haus hbo hdfc hdfcbank health healthcare help helsinki here hermes hgtv hiphop hisamitsu hitachi hiv hk hkt hm hn hockey holdings holiday homedepot homegoods homes homesense honda honeywell horse hospital host hosting hot hoteles hotels hotmail house how hr hsbc ht htc hu hughes hyatt hyundai
    ibm icbc ice icu id ie ieee ifm iinet ikano il im imamat imdb immo immobilien in industries infiniti info ing ink institute insurance insure int intel international intuit investments io ipiranga iq ir irish is iselect ismaili ist istanbul it itau itv iveco iwc
    jaguar java jcb jcp je jeep jetzt jewelry jio jlc jll jm jmp jnj jo jobs joburg jot joy jp jpmorgan jprs juegos juniper
    kaufen kddi ke kerryhotels kerrylogistics kerryproperties kfh kg kh ki kia kim kinder kindle kitchen kiwi km kn koeln komatsu kosher kp kpmg kpn kr krd kred kuokgroup kw ky kyoto kz
    la lacaixa ladbrokes lamborghini lamer lancaster lancia lancome land landrover lanxess lasalle lat latino latrobe law lawyer lb lc lds lease leclerc lefrak legal lego lexus lgbt li liaison lidl life lifeinsurance lifestyle lighting like lilly limited limo lincoln linde link lipsy live living lixil lk llc loan loans locker locus loft lol london lotte lotto love lpl lplfinancial lr ls lt ltd ltda lu lundbeck lupin luxe luxury lv ly
    ma macys madrid maif maison makeup man management mango map market marketing markets marriott marshalls maserati mattel mba mc mcd mcdonalds mckinsey md me med media meet melbourne meme memorial men menu meo merckmsd metlife mg mh miami microsoft mil mini mint mit mitsubishi mk ml mlb mls mm mma mn mo mobi mobile mobily moda moe moi mom monash money monster montblanc mopar mormon mortgage moscow moto motorcycles mov movie movistar mp mq mr ms msd mt mtn mtpc mtr mu museum mutual mutuelle mv mw mx my mz
    na nab nadex nagoya name nationwide natura navy nba nc ne nec net netbank netflix network neustar new newholland news next nextdirect nexus nf nfl ng ngo nhk ni nico nike nikon ninja nissan nissay nl no nokia northwesternmutual norton now nowruz nowtv np nr nra nrw ntt nu nyc nz
    obi observer off office okinawa olayan olayangroup oldnavy ollo om omega one ong onl online onyourside ooo open oracle orange org organic orientexpress origins osaka otsuka ott ovh
    pa page pamperedchef panasonic panerai paris pars partners parts party passagens pay pccw pe pet pf pfizer pg ph pharmacy phd philips phone photo photography photos physio piaget pics pictet pictures pid pin ping pink pioneer pizza pk pl place play playstation plumbing plus pm pn pnc pohl poker politie porn post pr pramerica praxi press prime pro prod productions prof progressive promo properties property protection pru prudential ps pt pub pw pwc py
    qa qpon quebec quest qvc
    racing radio raid re read realestate realtor realty recipes red redstone redumbrella rehab reise reisen reit reliance ren rent rentals repair report republican rest restaurant review reviews rexroth rich richardli ricoh rightathome ril rio rip rmit ro rocher rocks rodeo rogers room rs rsvp ru rugby ruhr run rw rwe ryukyu
    sa saarland safe safety sakura sale salon samsclub samsung sandvik sandvikcoromant sanofi sap sapo sarl sas save saxo sb sbi sbs sc sca scb schaeffler schmidt scholarships school schule schwarz science scjohnson scor scot sd se search seat secure security seek select sener services ses seven sew sex sexy sfr sg sh shangrila sharp shaw shell shia shiksha shoes shop shopping shouji show showtime shriram si silk sina singles site sj sk ski skin sky skype sl sling sm smart smile sn sncf so soccer social softbank software sohu solar solutions song sony soy space spiegel sport spot spreadbetting sr srl srt st stada staples star starhub statebank statefarm statoil stc stcgroup stockholm storage store stream studio study style su sucks supplies supply support surf surgery suzuki sv swatch swiftcover swiss sx sy sydney symantec systems sz
    tab taipei talk taobao target tatamotors tatar tattoo tax taxi tc tci td tdk team tech technology tel telecity telefonica temasek tennis teva tf tg th thd theater theatre tiaa tickets tienda tiffany tips tires tirol tj tjmaxx tjx tk tkmaxx tl tm tmall tn to today tokyo tools top toray toshiba total tours town toyota toys tr trade trading training travel travelchannel travelers travelersinsurance trust trv tt tube tui tunes tushu tv tvs tw tz
    ua ubank ubs uconnect ug uk unicom university uno uol ups us uy uz
    va vacations vana vanguard vc ve vegas ventures verisign versicherung vet vg vi viajes video vig viking villas vin vip virgin visa vision vista vistaprint viva vivo vlaanderen vn vodka volkswagen volvo vote voting voto voyage vu vuelos
    wales walmart walter wang wanggou warman watch watches weather weatherchannel webcam weber website wed wedding weibo weir wf whoswho wien wiki williamhill win windows wine winners wme wolterskluwer woodside work works world wow ws wtc wtf
    xbox xerox xfinity xihuan xin xperia xxx xyz
    xn--11b4c3d xn--1ck2e1b xn--1qqw23a
    xn--2scrj9c
    xn--30rr7y xn--3bst00m xn--3ds443g xn--3e0b707e xn--3hcrj9c xn--3oq18vl8pn36a xn--3pxu8k
    xn--42c2d9a xn--45br5cyl xn--45brj9c xn--45q11c xn--4gbrim
    xn--54b7fta0cc xn--55qw42g xn--55qx5d xn--5su34j936bgsg xn--5tzm5g
    xn--6frz82g xn--6qq986b3xl
    xn--80adxhks xn--80ao21a xn--80aqecdr1a xn--80asehdb xn--80aswg xn--8y0a063a
    xn--90a3ac xn--90ae xn--90ais xn--9dbq2a xn--9et52u xn--9krt00a
    xn--b4w605ferd xn--bck1b9a5dre4c
    xn--c1avg xn--c2br7g xn--cck2b3b xn--cg4bki xn--clchc0ea0b2g2a9gcd xn--czr694b xn--czrs0t xn--czru2d
    xn--d1acj3b xn--d1alf
    xn--e1a4c xn--eckvdtc9d xn--efvy88h xn--estv75g
    xn--fct429k xn--fhbei xn--fiq228c5hs xn--fiq64b xn--fiqs8s xn--fiqz9s xn--fjq720a xn--flw351e xn--fpcrj9c3d xn--fzc2c9e2c xn--fzys8d69uvgm
    xn--g2xx48c xn--gckr3f0f xn--gecrj9c xn--gk3at1e
    xn--h2breg3eve xn--h2brj9c xn--h2brj9c8c xn--hxt814e
    xn--i1b6b1a6a2e xn--imr513n xn--io0a7i
    xn--j1aef xn--j1amh xn--j6w193g xn--jlq61u9w7b xn--jvr189m
    xn--kcrx77d1x4a xn--kprw13d xn--kpry57d xn--kpu716f xn--kput3i
    xn--l1acc xn--lgbbat1ad8j
    xn--mgb9awbf xn--mgba3a3ejt xn--mgba3a4f16a xn--mgba7c0bbn0a xn--mgbaakc7dvf xn--mgbaam7a8h xn--mgbab2bd xn--mgbai9azgqp6j xn--mgbayh7gpa xn--mgbb9fbpob xn--mgbbh1a xn--mgbbh1a71e xn--mgbc0a9azcg xn--mgbca7dzdo xn--mgberp4a5d4ar xn--mgbgu82a xn--mgbi4ecexp xn--mgbpl2fh xn--mgbt3dhd xn--mgbtx2b xn--mgbx4cd0ab xn--mix891f xn--mk1bu44c xn--mxtq1m
    xn--ngbc5azd xn--ngbe9e0a xn--ngbrx xn--node xn--nqv7f xn--nqv7fs00ema xn--nyqy26a
    xn--o3cw4h xn--ogbpf8fl xn--otu796d
    xn--p1acf xn--p1ai xn--pbt977c xn--pgbs0dh xn--pssy2u
    xn--q9jyb4c xn--qcka1pmc xn--qxam
    xn--rhqv96g xn--rovu88b xn--rvc1e0am3e
    xn--s9brj9c xn--ses554g
    xn--t60b56a xn--tckwe xn--tiq49xqyj
    xn--unup4y
    xn--vermgensberater-ctb xn--vermgensberatung-pwb xn--vhquv xn--vuq861b
    xn--w4r85el8fhu5dnra xn--w4rs40l xn--wgbh1c xn--wgbl6a
    xn--xhq521b xn--xkc2al3hye2a xn--xkc2dl3a5ee0h
    xn--y9a3aq xn--yfro4i67o xn--ygbi2ammx
    xn--zfr164b
    yachts yahoo yamaxun yandex ye yodobashi yoga yokohama you youtube yt yun
    za zappos zara zero zip zippo zm zone zuerich zw
  """.split("\\s+").filterNot(_.isEmpty)

  // todo: auto update (once a day?)
  private[url] def format(a: Iterable[String]): String = a.groupBy(s => s.substring(0, if (s startsWith "xn--") 5 else 1)).toList.sortBy(_._1).map(_._2.toList.sorted.mkString("    ", " ", "")) mkString "\n"
  private[url] def checkEmergingTLDs() {

    val newTLDs: Array[String] = httputils.GETAsString("http://data.iana.org/TLD/tlds-alpha-by-domain.txt").split('\n').map(_.toLowerCase).filterNot(_ contains ' ')
    val disappear: Set[String] = readTldsAlphaByDomain.toSet -- newTLDs.toSet
    val appear:    Set[String] = newTLDs.toSet -- readTldsAlphaByDomain.toSet
    if (!disappear.isEmpty) println("disappear:\n"+ format(disappear)) // will NOT be removed from NormUrl.scala
    if (!appear   .isEmpty) println("appear:\n"   + format(appear))    // will     be added   to   NormUrl.scala
    if (/*!disappear.isEmpty ||*/ !appear.isEmpty) {
      //println("update `readTldsAlphaByDomain` to:\n" + format(newTLDs))
      val file = new java.io.File("NormUrl.scala")
      val src1 = FileUtils.readFileToString(file, "UTF-8")
      val src2 = src1.replaceAll("(?s)^(.+readTldsAlphaByDomain: Array\\[String\\] = \"\"\"\n)[^\"]+(\n\\s+\"\"\".+)$", "$1" + format(readTldsAlphaByDomain.toSet ++ newTLDs) + "$2")
      FileUtils.writeStringToFile(file, src2, "UTF-8")
    }
  }
  /*
  */

  //{
  // // todo??: "tlds-alpha-by-domain.txt" can be placed either next to 'NormUrl.scala' or in the same .jar (in scout)
  // // wget -O tlds-alpha-by-domain.txt http://data.iana.org/TLD/tlds-alpha-by-domain.txt
  //  val lst = try {
  //              scala.io.Source.fromFile(new java.io.File(__FILE__.getParentFile, "tlds-alpha-by-domain.txt")).getLines.toList
  //            } catch {
  //              case e: FileNotFoundException =>
  //                httputils.GETAsString("http://data.iana.org/TLD/tlds-alpha-by-domain.txt").split('\n').toList
  //            }
  //  require(lst.length > 730)
  //  lst
  //}

  // '.yu', which does not work, but archive.org has snapshots. also former: .an .bu .cs .dd .um .yu .zr .tp (.tp works but will disappear)
  // reserved .bl .bq .eh .mf .ss
  val allTLDs: TreeMap[Name,TLD] = TreeMap.empty[Name,TLD](Name.AsciiOrdering) ++ (for (x <- readTldsAlphaByDomain ++
                                                                                             "an bu cs dd um yu zr tp bl bq eh mf ss".split(' ') ++
                                                                                             // https://en.wikipedia.org/wiki/List_of_Internet_top-level_domains#test_TLDs
                                                                                             "xn--kgbechtv xn--hgbk6aj7f53bba xn--0zwm56d xn--g6w251d xn--80akhbyknj4f xn--11b5bs3a9aj6g xn--jxalpdlp xn--9t4b11yi5a xn--deba0ad xn--zckzah xn--hlcj6aya9esc7a".split(' ');
                                                                                        name = Name(x))
                                                                                   yield name->new TLD(name))
  val oldTLDs: TreeMap[Name,TLD] = allTLDs.filter   (_._2.isOld)
  val newTLDs: TreeMap[Name,TLD] = allTLDs.filterNot(_._2.isOld)
}

/*private[url]*/ case class Dom2(tld: TLD, _1: Name) extends Dom {
  override def takeRight(i: Int)  = i match {
                                     case 0 => Root
                                     case 1 => tld
                                     case 2 => this
                                     case _ => throw new UnsupportedOperationException
                                   }
  override def +:(n: Name)        = new Dom3(tld, _1, n)
  override def toString           = s"${_1}.$tld" // (new StringBuilder(asciiLength) append _1 append '.' append tld).toString
  override def asciiLength        = _1.asciiLength   + 1 + tld.asciiLength
  override def uniLength          = _1.uniLength     + 1 + tld.uniLength
  override def uniByteLength      = _1.uniByteLength + 1 + tld.uniByteLength
  override def asciiCharAt(i:Int) = { val l=_1.asciiLength; if (i<l) _1.asciiCharAt(i) else if (i==l) '.' else tld.asciiCharAt(i-l-1) }
  override def length             = 2
  override def applyLeft(i: Int)  = i match { case 0 => _1
                                              case 1 => tld.name
                                              case _ => throw new IndexOutOfBoundsException }
}
/*private[url]*/ case class Dom3(tld: TLD, _1: Name, _2: Name) extends Dom {
  override def takeRight(i: Int) = i match {
                                     case 0 => Root
                                     case 1 => tld
                                     case 2 => new Dom2(tld, _1)
                                     case 3 => this
                                     case _ => throw new UnsupportedOperationException
                                   }
  override def +:(n: Name)  = new Dom4(tld, _1, _2, n)
  override def toString     = s"${_2}.${_1}.$tld"
  override def asciiLength  = _2.asciiLength   + 1 + _1.asciiLength   + 1 + tld.asciiLength
  override def uniLength    = _2.uniLength     + 1 + _1.uniLength     + 1 + tld.uniLength
  override def uniByteLength= _2.uniByteLength + 1 + _1.uniByteLength + 1 + tld.uniByteLength
  override def length       = 3
  override def applyLeft(i: Int)= i match { case 0 => _2
                                            case 1 => _1
                                            case 2 => tld.name
                                            case _ => throw new IndexOutOfBoundsException }
}
/*private[url]*/ case class Dom4(tld: TLD, _1: Name, _2: Name, _3: Name) extends Dom {
  override def takeRight(i: Int) = i match {
                                     case 0 => Root
                                     case 1 => tld
                                     case 2 => new Dom2(tld, _1)
                                     case 3 => new Dom3(tld, _1, _2)
                                     case 4 => this
                                     case _ => throw new UnsupportedOperationException
                                   }
  override def +:(n: Name)  = new Dom5(tld, _1, _2, _3, n)
  override def toString     = s"${_3}.${_2}.${_1}.$tld"
  override def asciiLength  = _3.asciiLength   + 1 + _2.asciiLength   + 1 + _1.asciiLength   + 1 + tld.asciiLength
  override def uniLength    = _3.uniLength     + 1 + _2.uniLength     + 1 + _1.uniLength     + 1 + tld.uniLength
  override def uniByteLength= _3.uniByteLength + 1 + _2.uniByteLength + 1 + _1.uniByteLength + 1 + tld.uniByteLength
  override def length       = 4
  override def applyLeft(i: Int)= i match { case 0 => _3
                                            case 1 => _2
                                            case 2 => _1
                                            case 3 => tld.name
                                            case _ => throw new IndexOutOfBoundsException }
}
/*private[url]*/ case class Dom5(tld: TLD, _1: Name, _2: Name, _3: Name, _4: Name) extends Dom {
  override def takeRight(i: Int) = i match {
                                     case 0 => Root
                                     case 1 => tld
                                     case 2 => new Dom2(tld, _1)
                                     case 3 => new Dom3(tld, _1, _2)
                                     case 4 => new Dom4(tld, _1, _2, _3)
                                     case 5 => this
                                     case _ => throw new UnsupportedOperationException
                                   }
  override def +:(n: Name)  = new DomN(tld, IndexedSeq(n, _4, _3, _2, _1, tld.name))
  override def toString     = s"${_4}.${_3}.${_2}.${_1}.$tld"
  override def asciiLength  = _4.asciiLength   + 1 + _3.asciiLength   + 1 + _2.asciiLength   + 1 + _1.asciiLength   + 1 + tld.asciiLength
  override def uniLength    = _4.uniLength     + 1 + _3.uniLength     + 1 + _2.uniLength     + 1 + _1.uniLength     + 1 + tld.uniLength
  override def uniByteLength= _4.uniByteLength + 1 + _3.uniByteLength + 1 + _2.uniByteLength + 1 + _1.uniByteLength + 1 + tld.uniByteLength
  override def length       = 5
  override def applyLeft(i: Int)= i match { case 0 => _4
                                            case 1 => _3
                                            case 2 => _2
                                            case 3 => _1
                                            case 4 => tld.name
                                            case _ => throw new IndexOutOfBoundsException }
}
/*private[url]*/ case class DomN(tld: TLD, private[url] val elements: IndexedSeq[Name]) extends Dom {
  require(elements.length>=6, s"elements.length=${elements.length}")
  override def takeRight(i: Int) = i match {
                                     case 0             => Root
                                     case 1              => tld
                                     case 2              => new Dom2(tld, elements(length-2))
                                     case 3              => new Dom3(tld, elements(length-2), elements(length-3))
                                     case 4              => new Dom4(tld, elements(length-2), elements(length-3), elements(length-4))
                                     case 5              => new Dom5(tld, elements(length-2), elements(length-3), elements(length-4), elements(length-5))
                                     case n if n<=length => new DomN(tld, elements.takeRight(n))
                                     case n if n==length => this
                                     case _              => throw new UnsupportedOperationException
                                   }
  override def +:(n: Name)  = new DomN(tld, n +: elements)
  override def toString     = elements.mkString(".")
  override def asciiLength  = elements.foldLeft(elements.length-1)(_ + _.asciiLength)
  override def uniLength    = elements.foldLeft(elements.length-1)(_ + _.uniLength)
  override def uniByteLength= elements.foldLeft(elements.length-1)(_ + _.uniByteLength)
  override def length       = elements.length
  override def applyLeft(i: Int)= elements.apply(i)
}



//trait HostPort { def host: Host; def port: Int }
//trait IPPort extends HostPost { def host: IP }
//class IPv4Port(val host: IPv4, val port: Int) extends IPPort
//class IPv6Port(val host: IPv6, val port: Int) extends IPPort
//class DomPort(val host: Dom, val port: Int) extends HostPort

//AuthorityChain = List[Dom]
// todo:=============================

// todo:=============================
//sealed abstract class Scheme(override val toString: String)
//object Http  extends Scheme("http")
//object Https extends Scheme("https")
// todo:=============================

class NormUrl /*private*/(val scheme: String /* "http" | "https" */,
                          val host:   Host,
                          val port:   Int            /*= NormUrl.portFromScheme(scheme)*/,
                          val path:   String,   // minimal ESCAPED form
                          val query:  Option[String], // minimal ESCAPED form, but it can have '+' and %20 (in queryMap both are ' ') and '=' and %3D (in queryMap both are '=')
                          val hash:   Option[String]) {
  import NormUrl._

  if (!(scheme=="http" || scheme=="https")) throw new ParseException(s"Invalid scheme: `$scheme'")
  if (!(0<=port && port<=65535)) throw new ParseException(s"Invalid port: `$port'")

  def portsuffix:     String  = if (port == portFromScheme(scheme)) "" else ":"+port
  def xnhostport:     String  = host match { case ip: IPv6 => '[' + ip.toString + ']' + portsuffix
                                             case ip4ordom => ip4ordom.toString       + portsuffix }
  def unihostport:    String  = host match { case  ip: IP  => xnhostport
                                             case dom: Dom => dom.toUniString + portsuffix }

  def requestUri:     String  = {
    // returns us-ascii, it is safe fror further .getBytes, it is like php's REQUEST_URI but without starting /
    val s = path + query.map("?" + _).getOrElse("") + hash.map("#" + _).getOrElse("")
    // debug only
    assert(s forall { case c if c <= ' ' => println(c); false
                      case c if c > '~'  => println(c); false
                      case c @ ('"' | '<' | '>' | '[' | '\\' | ']' | '^' | '`' | '{' | '|' | '}') => println(c); false
                      case _ => true }, s)
    s
  }
  override lazy val toString = scheme + "://" + xnhostport + "/" + requestUri

  // unescaped data:
  def rawpath:        String                  = unhexFull(path) // for opening files
  def querySeq:       Array[(String,String)]  = query map (parseQuery _) getOrElse Array.empty
  lazy val queryMap:  Map[String,String]      = querySeq.toMap


  //todo: def uniquery = // from first '?' till first '#' (todo: what to do with & and =)
  //todo: def unihash  = // from first '#'

  // todo: avoid space at the end, change it to %20
  def toUniString:    String                  = scheme + "://" + unihostport + "/" + unhexuri(requestUri) // still have some %-encoded: %00..1F, %2B, space->+, %3F im path part, %25, %23

  // case class'-like copy()
  def copy(scheme:  String        =this.scheme,
           host:    Host          =this.host,
           port:    Int           =this.port,
           path:    String        =this.path,
           query:   Option[String]=this.query,
           hash:    Option[String]=this.hash) = NormUrl.apply(scheme, host, port, path, query, hash)

  override def hashCode:          Int     = scheme.hashCode * 31 + (host.hashCode * 31 + (port.hashCode * 31 + (path.hashCode * 31 + (query.hashCode * 31 + hash.hashCode))))
  override def equals(that: Any): Boolean = that match { case that1: NormUrl => NormUrl.equiv(this, that1)
                                                         case _ => false }

  def join(relative: String): NormUrl = {
         if (relative.isEmpty)                this
    else if (relative matches "[a-z]+://.*")  parse(relative)
    else if (relative startsWith "//")        parse(this.scheme + ":" + relative)
    else if (relative startsWith "/")         parse(this.scheme + "://" + this.xnhostport + relative)
    else if (relative startsWith "?")         parse(this.scheme + "://" + this.xnhostport + "/" + this.path + relative)
    else if (relative startsWith "#")         parse(this.scheme + "://" + this.xnhostport + "/" + this.path + this.query.map("?" + _).getOrElse("") + relative)
    else {
      val dir = this.path.lastIndexOf('/') match {
                  case -1 => ""
                  case  p => this.path.substring(0, p+1) // including '/' at the end
                }
      parse(this.scheme + "://" + this.xnhostport + "/" + dir + relative)
    }
  }

  def withNormalizedPath: NormUrl = new NormUrl(scheme, host, port, NormUrl.normalizePath(path), query, hash)

  def doppelganger: NormUrl = 0 match {
    case _ if scheme=="http"  && port== 80 => copy(scheme="https", port=443) // todo: inkl to `aggressiveCanonicalizeUrl` as *://
    case _ if scheme=="https" && port==443 => copy(scheme="http",  port= 80)
    case _ if scheme=="http"               => copy(scheme="https"          )
    case _ if scheme=="https"              => copy(scheme="http"           )
  }
}



object NormUrl extends Equiv[NormUrl] {
  def equiv(x: NormUrl, y: NormUrl): Boolean = (x eq y) || x.scheme==y.scheme && x.host==y.host && x.port==y.port && x.path==y.path && x.query==y.query && x.hash==y.hash

  @inline private[this] def unhexchar(c: Char): Int = (c : @annotation.switch) match {
                                                        case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => c - '0'
                                                        case 'A'|'B'|'C'|'D'|'E'|'F'                 => c - 'A' + 10
                                                        case 'a'|'b'|'c'|'d'|'e'|'f'                 => c - 'a' + 10
                                                        case _                                       => -1
                                                      }
  @inline private[this] def unhex2chars(a: Char, b: Char): Int = unhexchar(a) match { case -1 => -1
                                                                                      case x  => unhexchar(b) match { case -1 => -1
                                                                                                                      case y  => x*16+y } }

  private[this] def re1 = """/\./""".r
  private[this] def re2 = """[^/]+/\.\./""".r
  def normalizePath(path: String) = re2.replaceAllIn(re1.replaceAllIn(path, ""), "")

  // it works for whole url, for path, and for path+query+hash
  private[this] def unhexAsciiOnly(s0: String): String = { // in case of failed utf-8 unhex, assume there is another encoding (japaneese, koi-8, etc)
    var rc: java.lang.StringBuilder = null
    var i = 0
    val len = s0.length
    var seenq, seenh = false
    while (i<len) {
      val c = s0(i)
      if (c=='?') seenq = true
      if (c=='#') seenh = true
      if (c=='%' && i < len-2) {
        val h1 = unhex2chars(s0(i+1), s0(i+2))
        if (0x00<=h1 && h1<=0x7F) {
          if (h1<=0x20 || h1==0x7F || "%+&=\\".contains(h1) || (h1=='?' && !seenq) || (h1=='#' && !seenh)) { // keep escaped
            if (rc != null) rc append c
            i += 1
          } else {
            if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
            rc append h1.toChar
            i += 3
          }
        } else {
          if (rc != null) rc append c
          i += 1
        }
      } else {
        if (rc != null) rc append c
        i += 1
      }
    }
    if (rc == null) s0 else rc.toString
  }

  def unhexuri(s0: String): String = {
    var rc: java.lang.StringBuilder = null
    var i = 0
    val len = s0.length
    var seenq, seenh = false
    while (i<len) {
      val c = s0(i)
      if (c=='?') seenq = true
      if (c=='#') seenh = true
      if (c=='%' && i < len-2) {
        val h1 = unhex2chars(s0(i+1), s0(i+2))
        if (0x00<=h1 && h1<=0x7F) {
          if (h1<=0x20 || h1==0x7F || "%+&=\\".contains(h1) || (h1=='?' && !seenq) || (h1=='#' && !seenh)) { // keep escaped
            if (rc != null) rc append c
            i += 1
          } else { // keep escaped
            if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
            rc append h1.toChar
            i += 3
          }
        } else if (0xC0 <= h1 && h1 <= 0xDF && i < len-5  && s0(i+3)=='%') {
          val h2 = unhex2chars(s0(i+4), s0(i+5))
          if (0x80<=h2 && h2<=0xBF) {
            if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
            rc append (((h1&0x1F)<<6) | (h2&0x3F)).toChar  // 5+6 = 11 bits
            i += 6
          } else {
            return unhexAsciiOnly(s0)
          }
        } else if (0xE0 <= h1 && h1 <= 0xEF && i < len-8  && s0(i+3)=='%' && s0(i+6)=='%') {
          val h2 = unhex2chars(s0(i+4), s0(i+5))
          val h3 = unhex2chars(s0(i+7), s0(i+8))
          if (0x80<=h2 && h2<=0xBF && 0x80<=h3 && h3<=0xBF) {
            if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
            rc append (((h1&0x0F)<<12) | ((h2&0x3F)<<6) | (h3&0x3F)).toChar // 4+6+6 = 16 bits
            i += 9
          } else {
            return unhexAsciiOnly(s0)
          }
        } else if (0xF0 <= h1 && h1 <= 0xF7 && i < len-11 && s0(i+3)=='%' && s0(i+6)=='%' && s0(i+9)=='%') {
          val h2 = unhex2chars(s0(i+4), s0(i+5))
          val h3 = unhex2chars(s0(i+7), s0(i+8))
          val h4 = unhex2chars(s0(i+10), s0(i+11))
          if (0x80<=h2 && h2<=0xBF && 0x80<=h3 && h3<=0xBF && 0x80<=h4 && h4<=0xBF) {
            val cp = ((h1&0x07)<<18) | ((h2&0x3F)<<12) | ((h3&0x3F)<<6) | (h4&0x3F) // 3+6+6+6 = 21 bits
            if (Character.isValidCodePoint(cp)) {
              if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
              rc appendCodePoint cp
              i += 12
            } else {
              return unhexAsciiOnly(s0)
            }
          } else {
            return unhexAsciiOnly(s0)
          }
        } else {
          return unhexAsciiOnly(s0)
        }
      } else {
        if (rc != null) rc append c
        i += 1
      }
    }
    if (rc == null) s0 else rc.toString
  }

  //private[this]
  def unhexQueryParam(s: String) = unhexFull(s.replace('+', ' '))

  private[url] def unhexFull(s0: String): String = {
    var rc: java.lang.StringBuilder = null
    var i = 0
    val len = s0.length
    while (i<len) {
      val c = s0(i)
      if (c=='%' && i < len-2) {
        val h1 = unhex2chars(s0(i+1), s0(i+2))
        if (0x00<=h1 && h1<=0x7F) {
          if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
          rc append h1.toChar
          i += 3
        } else if (0xC0 <= h1 && h1 <= 0xDF && i < len-5  && s0(i+3)=='%') {
          val h2 = unhex2chars(s0(i+4), s0(i+5))
          if (0x80<=h2 && h2<=0xBF) {
            if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
            rc append (((h1&0x1F)<<6) | (h2&0x3F)).toChar  // 5+6 = 11 bits
            i += 6
          } else {
            if (rc != null) rc append c
            i += 1
          }
        } else if (0xE0 <= h1 && h1 <= 0xEF && i < len-8  && s0(i+3)=='%' && s0(i+6)=='%') {
          val h2 = unhex2chars(s0(i+4), s0(i+5))
          val h3 = unhex2chars(s0(i+7), s0(i+8))
          if (0x80<=h2 && h2<=0xBF && 0x80<=h3 && h3<=0xBF) {
            if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
            rc append (((h1&0x0F)<<12) | ((h2&0x3F)<<6) | (h3&0x3F)).toChar // 4+6+6 = 16 bits
            i += 9
          } else {
            if (rc != null) rc append c
            i += 1
          }
        } else if (0xF0 <= h1 && h1 <= 0xF7 && i < len-11 && s0(i+3)=='%' && s0(i+6)=='%' && s0(i+9)=='%') {
          val h2 = unhex2chars(s0(i+4), s0(i+5))
          val h3 = unhex2chars(s0(i+7), s0(i+8))
          val h4 = unhex2chars(s0(i+10), s0(i+11))
          if (0x80<=h2 && h2<=0xBF && 0x80<=h3 && h3<=0xBF && 0x80<=h4 && h4<=0xBF) {
            val cp = ((h1&0x07)<<18) | ((h2&0x3F)<<12) | ((h3&0x3F)<<6) | (h4&0x3F) // 3+6+6+6 = 21 bits
            if (Character.isValidCodePoint(cp)) {
              if (rc == null) rc = (new java.lang.StringBuilder).append(s0, 0, i)
              rc appendCodePoint cp
              i += 12
            } else {
              if (rc != null) rc append c
              i += 1
            }
          } else {
            if (rc != null) rc append c
            i += 1
          }
        } else {
          if (rc != null) rc append c
          i += 1
        }
      } else {
        if (rc != null) rc append c
        i += 1
      }
    }
    if (rc == null) s0 else rc.toString
  }

  // todo: ascii only: escape ':' and '/' in query values (not param names). sometimes params must be escaped (yandex caches), sometimes values does not have to: http://nestroy.at/eingang.html?nestroy-materialien/basisbibliographie/index.shtml
  // todo: not escape ' ' in path (unicode only)
  // todo: escape '|' (%7C) in path (unicode also)
  // escape uri same way as browser does (from pasting into location till document.URL and mongodb key)
  // note: TPL.hash depends on NormUrl.escape, so all hashes has to be recalculated after any escaping rule change
  private[this] def escapeInternal(isYandexCache: Boolean, // "...&url=http://lj" does not work for yandex cache, ':' and '/' are to be escaped
                                   where: Char, out: ByteBuffer, in: CharSequence, begin: Int, end: Int): Boolean = {
    require("/?#" contains where)
    var iskey = true
    var changed = false
    var i = begin
    @inline def hex1(c: Int) = (if (c<10) 0x30+c else 0x41+(c-10)).toByte
    @inline def hex2(c: Int) = out put '%'.toByte put hex1((c>>4)&15) put hex1(c&15)
    @inline def unhexchar(c: Char): Int = {
           if (0x30 <= c && c <= 0x39) c-0x30
      else if (0x41 <= c && c <= 0x46) c-0x41+10
      else if (0x61 <= c && c <= 0x66) c-0x61+10
      else -1
    }
    while (i < end) {
      val c = in.charAt(i)
      if (i+1<end && 0xD800<=c && c<=0xDBFF) {
        val d=in.charAt(i+1)
        if (0xDC00<=d && d<=0xDFFF) {
          val cp = 0x10000 + (c-0xD800)*1024 + d-0xDC00 // 0x10000-0x10FFFF
          hex2(0xF0 | ((cp >> 18) & 0x07))
          hex2(0x80 | ((cp >> 12) & 0x3F))
          hex2(0x80 | ((cp >>  6) & 0x3F))
          hex2(0x80 |  (cp        & 0x3F))
          changed = true
          i += 2
        } else {
          hex2(0xE0 | ((c >> 12) & 0x0F))
          hex2(0x80 | ((c >>  6) & 0x3F))
          hex2(0x80 |  (c        & 0x3F))
          changed = true
          i += 1
        }
      } else if (c >= 0x0800) {
        hex2(0xE0 | ((c >> 12) & 0x0F))
        hex2(0x80 | ((c >>  6) & 0x3F))
        hex2(0x80 |  (c        & 0x3F))
        changed = true
        i += 1
      } else if (c >= 0x0080) {
        hex2(0xC0 | ((c >>  6) & 0x1F))
        hex2(0x80 |  (c        & 0x3F))
        changed = true
        i += 1
      } else if (c=='%') {
        if (i+2>=end) { // % not followed by 2 hex chars to be replaced with %25
          out put '%'.toByte put '2'.toByte put '5'.toByte
          changed = true
          i += 1
        } else {
          val d = in.charAt(i+1)
          val e = in.charAt(i+2)
          val x1 = unhexchar(d)
          val x2 = unhexchar(e)
          val x  = x1*16 + x2
          if (x1 < 0 || x2 < 0) { // % not followed by 2 hex chars to be replaced with %25
            out put '%'.toByte put '2'.toByte put '5'.toByte
            changed = true
            i += 1
          //} else if (x==0x20 && where=='?') {
          //  out put '+'.toByte            // change %20 -> +
          //  i += 3
          } else if (x<=0x20||0x7F<=x||
                     x==0x22||x==0x3C||x==0x3E||x==0x5B||x==0x5C||x==0x5D||x==0x5E||x==0x60||x==0x7B||x==0x7C||x==0x7D|| // "<>[\]^`{|}
                     x==0x25||x==0x26||x==0x2B||x==0x3D||                 // keep %25 %26 %2B %3D ('%' | '+' | '&' | '=')
   isYandexCache && (x==0x2F || x==0x3A) && where=='?' && !iskey ||       // HACK: keep %2F %3A ('/' | ':') in query params, not keys // todo: separate, as %20 and +
                                                                                                                                      // http://archive.is/ayaQj https://twitter.com/search?f=realtime&q=http:%2F%2Farchive.is/&src=typd
                                                                                                                                      // !=
                                                                                                                                      // http://archive.is/plrJQ https://twitter.com/search?f=realtime&q=http://archive.is/&src=typd
                    (x==0x23 || x==0x3F) && (where=='/' || where=='?')) { // keep %23 and %3F in path and query, in hash %23 -> '#'
            hex2(x)
            changed |= 0x61<=d || 0x61<=e           // upcased a(i:i+2)
            i += 3
          } else {
            out put x.toByte                        // change %xx -> char
            changed = true
            i += 3
          }
        }
      } else if (c==0x20 && where=='?') {
        out put '+'.toByte                          // change ' ' -> + in query
        changed = true
        i += 1
      } else if (    c<=0x20||0x7F<=c||
                     c==0x22||c==0x3C||c==0x3E||c==0x5B||c==0x5C||c==0x5D||c==0x5E||c==0x60||c==0x7B||c==0x7C||c==0x7D||
   isYandexCache && (c==0x2F || c==0x3A) && where=='?' && !iskey ||           // HACK: escape '/' and ':' in query params, not keys
                    (c==0x23 || c==0x3F) && (where=='/' || where=='?')) {     // escape '?' and '#' in path and query
        hex2(c.toInt)                               // change char -> %xx
        changed = true
        i += 1
      } else {
        if (where=='?' && c=='&') iskey = true
        if (where=='?' && c=='=') iskey = false
        out put c.toByte
        i += 1
      }
    }
    changed
  }

  // resulting string has chars in [21..7E]
  private[url] def escape(isYandexCache: Boolean,
                          where: Char /* '/' | '?' | '#' */, in: String, begin: Int, end: Int): String = {
    val out = ByteBuffer allocate in.length*9 // worst case (0u0800..0uFFFF escape to %xx%xx%xx)
    if (escapeInternal(isYandexCache, where, out, in, begin, end))
      new String(out.array, 0, out.position)
    else
      in.substring(begin, end)
  }


  // parse a shitty url
  def parse(s: String): NormUrl = {
    try {
      // todo: trim more:
      // Unicode Character 'ZERO WIDTH NO-BREAK SPACE' (U+FEFF)
      // Unicode Character 'LEFT-TO-RIGHT MARK' (U+200E)
      // Unicode Character 'ZERO WIDTH SPACE' (U+200B)
      // Unicode Character 'LINE SEPARATOR' (U+2028)
      // \s\u00A0\u2002-\u200F\u3000\u0085
      // def isSpace(c: Char) = ....; val u1 = s.dropWhile(isSpace).reverse.dropWhile(isSpace).reverse
      val u1 = s.trim
      // bare IPv6 without [] is a "valid" url, as long as only IPv4 or only domain is
      val binIPv6: Array[Byte] = IPAddressUtil.textToNumericFormatV6(u1)
      if (binIPv6 != null) {
        apply("http", IPv6.fromByteArray(binIPv6))
      } else {
        val u: String = u1 match {
        //case y if y.startsWith("http://http://")        =>              y.stripPrefix("http://")
        //case y if y.startsWith("http://http//http://")  =>              y.stripPrefix("http://http//")
        //case y if y.startsWith("http://http//")         => "http://"  + y.stripPrefix("http://http//")
          case y if y.startsWith("http:///")              => "http://"  + y.stripPrefix("http:///")
          case y if y.startsWith("https:///")             => "https://" + y.stripPrefix("https:///")
          case y if y.startsWith("http://")               =>              y
          case y if y.startsWith("https://")              =>              y
          case y if y.startsWith("http:/")                => "http://"  + y.stripPrefix("http:/")
          case y if y.startsWith("https:/")               => "https://" + y.stripPrefix("https:/")
          case y if y.startsWith("http:")                 => "http://"  + y.stripPrefix("http:")
          case y if y.startsWith("https:")                => "https://" + y.stripPrefix("https:")
          case y if !y.matches("[a-z]+://.*")             => "http://"  + y // "://" inside url is ok, tc: "web.archive.org/web/2000/http://sexblbn.com/videos?o=bw"
          case _                                          => throw new ParseException(s"bad scheme '$s'")
        }
        def toint(s: String) = try s.toInt
                               catch {
                                case e: NumberFormatException => throw new ParseException(s"port is not integer: `$s'", e)
                              }

        val x1 = u.indexOf("://")
        val x5 = {val x=u.indexOf('#', x1+3); if (x<0) u.length else x}
        val x4 = {val x=u.indexOf('?', x1+3); if (x<0) u.length else x} min x5
        val x2 = {val x=u.indexOf('/', x1+3); if (x<0) u.length else x} min x4
        val x6 = {val x=u.lastIndexOf('@', x2); if (x>x1) { // has creds
                                                  x
                                                } else {
                                                  x1+2
                                                }}
        // it throws on s=="http:///"
        val x3 = if (u(x6+1)=='[' && u(x2-1)==']') x2 else { val x = u.lastIndexOf(':', x2); if (x>x6) x else x2 }
        val scheme = u.substring(0, x1).toLowerCase
        val host   = Host parse unhexFull(u.substring(x6+1, x3))
        val port   = if (x3<x2-1) {val portstr = u.substring(x3+1, x2)
                                   try portstr.toInt
                                   catch {
                                     case _: NumberFormatException => throw new ParseException(s"url=`$s`, port is not integer:`$portstr'")
                                   }}
                    else portFromScheme(scheme)

        val isYandexCache = host.contentEquals("hghltd.yandex.net")

        val path   = if (x2!=x4)                 escape(isYandexCache, '/', u, x2+1, x4)         else ""
        val query  = if (x4!=x5)            Some(escape(isYandexCache, '?', u, x4+1, x5)       ) else None
        val hash   = if (x5!=u.length)      Some(escape(isYandexCache, '#', u, x5+1, u.length) ) else None

        // todo: normalize path (/../, /./, etc)
        new NormUrl(internScheme(scheme), host, port, path match { case "."                  => ""
                                                                   case p if p endsWith "/." => p stripSuffix "."
                                                                   case p                    => p }, query, hash)
      }
    } catch {
      case e: ParseException => throw e
      case e: Throwable      => throw new ParseException(s"error parse url `$s'", e)
    }
  }

  def apply(scheme: String, host: Host): NormUrl = {
    new NormUrl(internScheme(scheme), host, portFromScheme(scheme), "", None, None)
  }

  def apply(scheme: String, host: Host, port: Int, path: String="", query: Option[String]=None, hash: Option[String]=None): NormUrl = {
    val isYandexCache = host.contentEquals("hghltd.yandex.net")
    new NormUrl(internScheme(scheme), host, port, escape(isYandexCache, '/', path, 0, path.length),
                                      query map (q=>escape(isYandexCache, '?', q,    0, q.length)),
                                      hash  map (h=>escape(isYandexCache, '#', h,    0, h.length)))
  }

  // String#intern is slow
  private[this] final val HTTP = "http"
  private[this] final val HTTPS = "https"
  private[this] def internScheme(s: String) = s match {
    case "http"  => HTTP
    case "https" => HTTPS
    case s       => s.intern
  }

  def portFromScheme(s: String) = s match {
    case "http"  => 80
    case "https" => 443
    case scheme  => throw new ParseException(s"bad scheme `$scheme'")
  }

  // todo: check if it differs from java.net.URLEncoder.encode
  // result is is ok for NormUrl#query but not for construct url by string concatenation
  def mkQuery(params: (String, Any)*): Option[String] = {
    if (params.isEmpty)
      None
    else {
      def escapeCgiParam(s: String, iskey: Boolean) = s flatMap {
        case '=' if iskey => "%3D"
        case '%' => "%25"
        case '#' => "%23"
        case '&' => "%26"
        case '+' => "%2B"
        case ' ' => "+" // todo: no need, escape() does it anyway
          case c   => c.toString
      }
      val str = params map { case(k, v) => escapeCgiParam(k, true) + "=" + escapeCgiParam(v.toString, false) } mkString "&"
      Some(escape(isYandexCache=false/* todo: why?*/, '?', str, 0, str.length))
    }
  }

  private[url] def parseQuery(query: String): Array[(String, String)] = {
    for (part <- query split '&') yield {
      part indexOf '=' match {
        case -1  => unhexQueryParam(part)                   -> ""
        case pos => unhexQueryParam(part.substring(0, pos)) -> unhexQueryParam(part.substring(pos+1))
      }
    }
  }

  object AsciiOrdering extends Ordering[NormUrl] {
    override def compare(a: NormUrl, b: NormUrl): Int = { val x1=Host.AsciiOrdering.compare(a.host, b.host); if (x1!=0) return x1
                                                          val x2=Integer.compare(a.port, b.port);            if (x2!=0) return x2
                                                          val x3=a.scheme     compareTo b.scheme;            if (x3!=0) return x3
                                                          if (false) {
                                                            a.requestUri compareTo b.requestUri
                                                          } else {
                                                            val x4=a.path  compareTo b.path;                                        if (x4!=0) return x4
                                                            val x5=implicitly[Ordering[Option[String]]].compare(a.query, b.query);  if (x5!=0) return x5
                                                            val x6=implicitly[Ordering[Option[String]]].compare(a.hash,  b.hash );  return x6
                                                          }
                                                        }
  }
  object UniOrdering   extends Ordering[NormUrl] {
    override def compare(a: NormUrl, b: NormUrl): Int = { val x1=Host.UniOrdering.compare(a.host, b.host);   if (x1!=0) return x1
                                                          val x2=Integer.compare(a.port, b.port);            if (x2!=0) return x2
                                                          val x3=a.scheme     compareTo b.scheme;            if (x3!=0) return x3
                                                          if (false) {
                                                            // it is not correct to compare UTF-16, must be compared as UTF-8 or UTF-32
                                                            // <s>NormUrl.unhexuri(a.requestUri) compareTo NormUrl.unhexuri(b.requestUri)</s>
                                                            UByteArrayOrdering.compare( NormUrl.unhexuri(a.requestUri).getBytes("UTF-8"),
                                                                                        NormUrl.unhexuri(b.requestUri).getBytes("UTF-8") ) // todo: optimize
                                                          } else {
                                                            val x4 = UByteArrayOrdering.compare( NormUrl.unhexuri(a.path).getBytes("UTF-8"),
                                                                                                 NormUrl.unhexuri(b.path).getBytes("UTF-8") )
                                                            if (x4!=0) return x4
                                                            val x5 = (a.query, b.query) match {
                                                                       case (None,     None    ) => 0
                                                                       case (None,     Some(_) ) => -1
                                                                       case (Some(_),  None    ) => 1
                                                                       case (Some(qa), Some(qb)) => UByteArrayOrdering.compare( NormUrl.unhexuri("?"+qa).getBytes("UTF-8"),
                                                                                                                                NormUrl.unhexuri("?"+qb).getBytes("UTF-8") )
                                                                     }
                                                            if (x5!=0) return x5

                                                            val x6 = (a.hash, b.hash) match {
                                                                       case (None,     None    ) => 0
                                                                       case (None,     Some(_) ) => -1
                                                                       case (Some(_),  None    ) => 1
                                                                       case (Some(ha), Some(hb)) => UByteArrayOrdering.compare( NormUrl.unhexuri("#"+ha).getBytes("UTF-8"),
                                                                                                                                NormUrl.unhexuri("#"+hb).getBytes("UTF-8") )
                                                                     }
                                                            return x6
                                                          }
                                                        }
  }
}






object IPv4BinOrdering extends BinOrdering[IPv4] {
  override def compare(a: IPv4, b: IPv4):                   Int         = a compare b
           def fromBuffer(bb: ByteBuffer):                  IPv4        = IPv4 fromInt bb.getInt
           def toBuffer(ip: IPv4, bb: ByteBuffer):          ByteBuffer  = bb putInt ip.toInt
           def toByteArray(ip: IPv4):                       Array[Byte] = toBuffer(ip, ByteBuffer allocate 4).array
}

object IPv6BinOrdering extends BinOrdering[IPv6] {
  override def compare(a: IPv6, b: IPv6):                   Int         = a compare b
           def fromBuffer(bb: ByteBuffer):                  IPv6        = new IPv6(bb.getLong, bb.getLong)
           def toBuffer(ip: IPv6, bb: ByteBuffer):          ByteBuffer  = bb putLong ip.hi putLong ip.lo
           def toByteArray(ip: IPv6):                       Array[Byte] = toBuffer(ip, ByteBuffer allocate 16).array
}

object DomAsciiBinOrdering extends BinOrdering[Dom] {  // "xn--" are between "xm" and "xo"
  override def compare(a: Dom, b: Dom):            Int         = Dom.AsciiOrdering.compare(a, b)
           def fromBuffer(bb: ByteBuffer):         Dom         = { val s = new String(bb.array, bb.position, bb.remaining)
                                                                   assert(!s.contains('/') && !s.contains('.'), s)
                                                                   Dom.fromIndexedSeq(s.split('!').reverse) }
           def toByteArray(dom: Dom):              Array[Byte] = { val bb = toBuffer(dom, ByteBuffer allocate dom.asciiLength)
                                                                   assert(bb.remaining==0, bb.remaining)
                                                                   bb.array }
           def toBuffer(dom: Dom, bb: ByteBuffer): ByteBuffer  = {
             @inline def q(n: Name) = n.toString.foreach(c => bb put c.toUpper.toByte)
             @inline def t() = bb put '!'.toByte // do not serialize root as '.', '.' has greater code than '*'
             dom match {
               case Root   => t()
               case d:TLD  => q(d.name)
               case d:Dom2 => q(d.tld.name); t(); q(d._1)
               case d:Dom3 => q(d.tld.name); t(); q(d._1); t(); q(d._2)
               case d:Dom4 => q(d.tld.name); t(); q(d._1); t(); q(d._2); t(); q(d._3)
               case d:Dom5 => q(d.tld.name); t(); q(d._1); t(); q(d._2); t(); q(d._3); t(); q(d._4)
               case d:DomN => q(d.tld.name); (1 until dom.length).foreach{ i => t(); q(dom.applyRight(i)) }
             }
             bb
           }
}
object DomUniBinOrdering extends BinOrdering[Dom] { // "xn--" are after "zz"
  override def compare(a: Dom, b: Dom):            Int         = Dom.UniOrdering.compare(a, b)
           def fromBuffer(bb: ByteBuffer):         Dom         = DomAsciiBinOrdering.fromBuffer(bb)
           def toByteArray(dom: Dom):              Array[Byte] = { val bb = toBuffer(dom, ByteBuffer allocate dom.uniByteLength)
                                                                   assert(bb.remaining==0, bb.remaining)
                                                                   bb.array }
           def toBuffer(dom: Dom, bb: ByteBuffer): ByteBuffer  = {
             @inline def q(n: Name) = bb.put(n.toUniString.getBytes("UTF-8") map { case b if 'a'<=b && b<='z' => (b & ~0x20).toByte /* .toChar.toUpper.toByte */
                                                                                   case b                     => b })
             @inline def t() = bb put '!'.toByte // do not serialize root as '.', '.' has greater code than '*'
             dom match {
               case Root   => t()
               case d:TLD  => q(d.name)
               case d:Dom2 => q(d.tld.name); t(); q(d._1)
               case d:Dom3 => q(d.tld.name); t(); q(d._1); t(); q(d._2)
               case d:Dom4 => q(d.tld.name); t(); q(d._1); t(); q(d._2); t(); q(d._3)
               case d:Dom5 => q(d.tld.name); t(); q(d._1); t(); q(d._2); t(); q(d._3); t(); q(d._4)
               case d:DomN => q(d.tld.name); (1 until dom.length).foreach{ i => t(); q(dom.applyRight(i)) }
             }
             bb
           }
}

// ===== archive.is specific starts here ===== todo: move to is.archive.db?
protected[url] trait HostBinOrdering extends BinOrdering[Host] {  // IPv4 < IPv6 < Dom
  val hostOrder:         Host.AnyOrdering
  val domainBinOrdering: BinOrdering[Dom]

  override def compare(a: Host, b: Host):            Int         = hostOrder.compare(a, b)

  @inline private[this] def unhex(c: Byte): Int = {
         if (0x30 <= c && c <= 0x39) (c-0x30)
    else if (0x41 <= c && c <= 0x46) (c-0x41+10)
    else ???
  }

  def fromBuffer(bb: ByteBuffer): Host = {
    bb.get(bb.position) match {
      case /*legacy*/'#'|4        => val i  = ( 1 to  8).foldLeft( 0){case (sum, idx) => (sum<<4) + unhex(bb get bb.position+idx)}
                                     bb.position(bb.position+9)
                                     IPv4.fromInt(i)
      case /*legacy*/'$'|6        => val hi = ( 1 to 16).foldLeft(0L){case (sum, idx) => (sum<<4) + unhex(bb get bb.position+idx)}
                                     val lo = (17 to 32).foldLeft(0L){case (sum, idx) => (sum<<4) + unhex(bb get bb.position+idx)}
                                     bb.position(bb.position+33)
                                     new IPv6(hi, lo)
      case b if '!'<=b && b<='_'  => domainBinOrdering.fromBuffer(bb)
      case b if (b & 0x80)==0x80  => domainBinOrdering.fromBuffer(bb) // unicode domain
    }
  }

  def toByteArray(host: Host): Array[Byte] = host match {
    case  ip: IPv4 => toBuffer(ip, ByteBuffer allocate  9).array
    case  ip: IPv6 => toBuffer(ip, ByteBuffer allocate 33).array
    case dom: Dom  => domainBinOrdering.toByteArray(dom)
  }
  def toBuffer(host: Host, bb: ByteBuffer): ByteBuffer = host match {
    case  ip: IPv4 => bb put 4.toByte put ip.map("%02X" format _).mkString.getBytes
    case  ip: IPv6 => bb put 6.toByte put ip.map("%04X" format _).mkString.getBytes
    case dom: Dom  => domainBinOrdering.toBuffer(dom, bb)
  }
}



object HostAsciiBinOrdering extends HostBinOrdering { val hostOrder = Host.AsciiOrdering; val domainBinOrdering = DomAsciiBinOrdering }
object HostUniBinOrdering   extends HostBinOrdering { val hostOrder = Host.UniOrdering;   val domainBinOrdering = DomUniBinOrdering   }

//private[url]
object NormUrlAsciiBinOrdering extends BinOrdering[NormUrl] {
  override def compare(a: NormUrl, b: NormUrl):       Int         = NormUrl.AsciiOrdering.compare(a, b)
           def fromBuffer(bb: ByteBuffer):            NormUrl     = {
             val x1 = bb.position    // host
             while (bb.get!='\u0000') {}
             val x2 = bb.position    // port
             while (bb.get!='\u0000') {}
             val x3 = bb.position    // scheme
             while (bb.get!='\u0000') {}
             val x4 = bb.position    // path
             while (bb.get!='\u0000') {}
             val x5 = bb.position    // query
             while (bb.get!='\u0000') {}
             val x6 = bb.position    // hash
             val host   = HostAsciiBinOrdering.fromBuffer(ByteBuffer.wrap(bb.array, x1, x2-x1-1))
             val port   = Integer.parseInt(new String(bb.array, x2, x3-x2-1), 16)
             val scheme = new String(bb.array, x3, x4-x3-1)
             val path   = new String(bb.array, x4, x5-x4-1)
             val query  = x6-x5-1 match { case 0   => None
                                          case len => Some(new String(bb.array, x5+1, len-1)) }
             val hash   = bb.remaining match { case 0   => None
                                               case len => Some(new String(bb.array, x6+1, bb.remaining-1)) }
             NormUrl.apply(scheme, host, port, path, query, hash)
           }
           def toByteArray(nu: NormUrl):              Array[Byte] = HostAsciiBinOrdering.toByteArray(nu.host)  ++  f"\u0000${nu.port}%04X\u0000${nu.scheme}\u0000${nu.path}\u0000${nu.query.fold("")("?"+_)}\u0000${nu.hash.fold("")("#"+_)}".getBytes("UTF-8")
           def toBuffer(nu: NormUrl, bb: ByteBuffer): ByteBuffer  = HostAsciiBinOrdering.toBuffer(nu.host, bb) put f"\u0000${nu.port}%04X\u0000${nu.scheme}\u0000${nu.path}\u0000${nu.query.fold("")("?"+_)}\u0000${nu.hash.fold("")("#"+_)}".getBytes("UTF-8")
}

object NormUrlUniBinOrdering extends BinOrdering[NormUrl] {
  override def compare(a: NormUrl, b: NormUrl):       Int         = NormUrl.UniOrdering.compare(a, b)
           def fromBuffer(bb: ByteBuffer):            NormUrl     = NormUrlAsciiBinOrdering.fromBuffer(bb)
           def toByteArray(nu: NormUrl):              Array[Byte] = HostUniBinOrdering.toByteArray(nu.host)  ++  f"\u0000${nu.port}%04X\u0000${nu.scheme}\u0000${NormUrl.unhexuri(nu.path)}\u0000${nu.query.fold("")(q=>NormUrl.unhexuri("?"+q))}\u0000${nu.hash.fold("")(h=>NormUrl.unhexuri("#"+h))}".getBytes("UTF-8")
           def toBuffer(nu: NormUrl, bb: ByteBuffer): ByteBuffer  = HostUniBinOrdering.toBuffer(nu.host, bb) put f"\u0000${nu.port}%04X\u0000${nu.scheme}\u0000${NormUrl.unhexuri(nu.path)}\u0000${nu.query.fold("")(q=>NormUrl.unhexuri("?"+q))}\u0000${nu.hash.fold("")(h=>NormUrl.unhexuri("#"+h))}".getBytes("UTF-8")
}


@deprecated
object NormUrlAsciiBinOrderingOld extends BinOrdering[NormUrl] {
  override def compare(a: NormUrl, b: NormUrl):       Int         = NormUrl.AsciiOrdering.compare(a, b)
           def fromBuffer(bb: ByteBuffer):            NormUrl     = {
             val x1 = bb.position    // host
             while (bb.get!=' ') {}
             val x2 = bb.position    // port
             while (bb.get!=' ') {}
             val x3 = bb.position    // scheme
             while (bb.get!=' ') {}
             val x4 = bb.position    // uri
             val host   = HostAsciiBinOrdering.fromBuffer(ByteBuffer.wrap(bb.array, x1, x2-x1-1))
             val port   = Integer.parseInt(new String(bb.array, x2, x3-x2-1), 16)
             val scheme = new String(bb.array, x3, x4-x3-1)
             while (bb.remaining>0 && !"?#".contains(bb.get)) {}
             if (bb.get(bb.position-1)=='?') {
               val x5 = bb.position  // query
               while (bb.remaining>0 && bb.get!='#') {}
               if (bb.get(bb.position-1)!='#') {      // yes query no hash
                  NormUrl.apply(scheme, host, port, new String(bb.array, x4, x5-x4-1), Some(new String(bb.array, x5, bb.position-x5)),   None)
               } else {                               // yes query yes hash
                  NormUrl.apply(scheme, host, port, new String(bb.array, x4, x5-x4-1), Some(new String(bb.array, x5, bb.position-x5-1)), Some(new String(bb.array, bb.position, bb.remaining)))
               }
             } else if (bb.get(bb.position-1)=='#') { // no query, yes hash
               NormUrl.apply(scheme, host, port, new String(bb.array, x4, bb.position-x4-1), None, Some(new String(bb.array, bb.position, bb.remaining)))
             } else {                                 // no query no hash
               NormUrl.apply(scheme, host, port, new String(bb.array, x4, bb.position-x4), None, None)
             }
           }
           def toByteArray(nu: NormUrl):              Array[Byte] = HostAsciiBinOrdering.toByteArray(nu.host)  ++  f" ${nu.port}%04X ${nu.scheme} ${nu.requestUri}".getBytes("UTF-8")
           def toBuffer(nu: NormUrl, bb: ByteBuffer): ByteBuffer  = HostAsciiBinOrdering.toBuffer(nu.host, bb) put f" ${nu.port}%04X ${nu.scheme} ${nu.requestUri}".getBytes("UTF-8")
}

@deprecated
object NormUrlUniBinOrderingOld extends BinOrdering[NormUrl] {
  override def compare(a: NormUrl, b: NormUrl):       Int         = NormUrl.UniOrdering.compare(a, b)
           def fromBuffer(bb: ByteBuffer):            NormUrl     = NormUrlAsciiBinOrderingOld.fromBuffer(bb)
           def toByteArray(nu: NormUrl):              Array[Byte] = HostUniBinOrdering.toByteArray(nu.host)  ++  f" ${nu.port}%04X ${nu.scheme} ${NormUrl.unhexuri(nu.requestUri)}".getBytes("UTF-8")
           def toBuffer(nu: NormUrl, bb: ByteBuffer): ByteBuffer  = HostUniBinOrdering.toBuffer(nu.host, bb) put f" ${nu.port}%04X ${nu.scheme} ${NormUrl.unhexuri(nu.requestUri)}".getBytes("UTF-8")
}

// ===== archive.is specific ends here =====



object Test {
  import NormUrl._
  import ms.webmaster.launcher.position._

  def hex(a: Array[Byte]) = a map ("%02x" format _) mkString " "

  implicit class Test[T](x: T) {
    def =~~=(y: T) {
      if (x!=y) {
        println(s"${__CALLER_FILE__}:${__CALLER_LINE__}: Test failed @ `$x'==`$y'")
        //sys.exit(1)
      }
    }
  }

  def main(args: Array[String]) {
    locally {
      def t(cs: CharSequence): String = (0 until cs.length).map(cs charAt _).mkString // .toString ?
      require(t(Dom.parse("7.z.a.abc.l.google.com").toAsciiCharSequence) == "7.z.a.abc.l.google.com")
      require(t(Dom.parse(  "z.a.abc.l.google.com").toAsciiCharSequence) ==   "z.a.abc.l.google.com")
      require(t(Dom.parse(    "a.abc.l.google.com").toAsciiCharSequence) ==     "a.abc.l.google.com")
      require(t(Dom.parse(      "abc.l.google.com").toAsciiCharSequence) ==       "abc.l.google.com")
      require(t(Dom.parse(        "abc.google.com").toAsciiCharSequence) ==         "abc.google.com")
      require(t(Dom.parse(                   "com").toAsciiCharSequence) ==                    "com")
      require(t(Dom.parse(                     ".").toAsciiCharSequence) ==                      ".")
    }
    require(Dom.parse("143.228.193.in-addr.arpa").toString == "143.228.193.in-addr.arpa")
    //require(Dom.parse("abc.google.com") !="abc.google.com")
    //require(Dom.parse("com")            !="com")
    //require(Dom.parse(".")              !=".")
    require(Dom.parse("f.com").endsWith("f.com"))
    require(Dom.parse("f.com").endsWith(".com"))
    require(Dom.parse("f.com").endsWith("com"))
    require(Dom.parse("f.com").endsWith("om"))
    require(Dom.parse("f.com").endsWith("m"))
    require(Dom.parse("f.com").endsWith(""))


    //println(NormUrl parse "http://photo.net/gallery/tag-search/search?query_string=Mary's+Cathedral")    // firefox preferred form
    //println(NormUrl parse "http://photo.net/gallery/tag-search/search?query_string=Mary%27s+Cathedral")  // chrome preferred form

    require(!Dom.parse("f.com").endsWith("f.cOm")) // todo: why? fixme

    require(Name("www")     ==Name("www")     , Name("www"))
    require(Name("xn--p1ai")==Name("рф")      )
    require(Name("рф")      ==Name("xn--p1ai"))

    require(Name("xn--p1ai")==Name("Рф")      )
    require(Name("рф")      ==Name("XN--p1ai"))

    require(Name("www")      contentEquals "www"     )
    require(Name("www")      contentEquals "wWw"     )
    require(Name("www")      contentEquals "WWW"     )
    require(Name("xn--p1ai") contentEquals "рф"      )
    require(Name("xn--p1ai") contentEquals "рФ"      )
    require(Name("xn--p1ai") contentEquals "РФ"      )
    require(Name("рф")       contentEquals "xn--p1ai")
    require(Name("рФ")       contentEquals "xn--p1ai")
    require(Name("РФ")       contentEquals "xn--p1ai")

    require(Host.parse("www")          == Host.parse("www")         )
    require(Host.parse("xn--p1ai")     == Host.parse("рф")          )
    require(Host.parse("рф")           == Host.parse("xn--p1ai")    )
    require(Host.parse("www.xn--p1ai") == Host.parse("www.рф")      )
    require(Host.parse("www.рф")       == Host.parse("www.xn--p1ai"))
    require(Host.parse("рф.xn--p1ai")  == Host.parse("xn--p1ai.рф") )
    require(Host.parse("xn--p1ai.рф")  == Host.parse("рф.xn--p1ai") )
    require(Host.parse("РФ.XN--P1AI")  == Host.parse("xn--p1ai.рф") )
    require(Host.parse("xn--p1ai.рф")  == Host.parse("РФ.XN--P1AI") )

    require(Host.parse("www")          contentEquals "www"         )
    require(Host.parse("xn--p1ai")     contentEquals "рф"          )
    require(Host.parse("рф")           contentEquals "xn--p1ai"    )
    require(Host.parse("www.xn--p1ai") contentEquals "www.рф"      )
    require(Host.parse("www.рф")       contentEquals "www.xn--p1ai")
    require(Host.parse("рф.xn--p1ai")  contentEquals "xn--p1ai.рф" )
    require(Host.parse("xn--p1ai.рф")  contentEquals "рф.xn--p1ai" )
    require(Host.parse("рф.xn--p1ai")  contentEquals "xn--p1ai.Рф" )
    require(Host.parse("xn--p1ai.рф")  contentEquals "рф.xn--P1AI" )

    require(Dom.parse("www")          == Dom.parse("www")         )
    require(Dom.parse("xn--p1ai")     == Dom.parse("рф")          )
    require(Dom.parse("рф")           == Dom.parse("xn--p1ai")    )
    require(Dom.parse("www.xn--p1ai") == Dom.parse("www.рф")      )
    require(Dom.parse("www.рф")       == Dom.parse("www.xn--p1ai"))
    require(Dom.parse("рф.xn--p1ai")  == Dom.parse("xn--p1ai.рф") )
    require(Dom.parse("xn--p1ai.рф")  == Dom.parse("рф.xn--p1ai") )

    require(Dom.parse("www")          contentEquals "www"         )
    require(Dom.parse("xn--p1ai")     contentEquals "рф"          )
    require(Dom.parse("рф")           contentEquals "xn--p1ai"    )
    require(Dom.parse("www.xn--p1ai") contentEquals "www.рф"      )
    require(Dom.parse("www.рф")       contentEquals "www.xn--p1ai")
    require(Dom.parse("рф.xn--p1ai")  contentEquals "xn--p1ai.рф" )
    require(Dom.parse("xn--p1ai.рф")  contentEquals "рф.xn--p1ai" )

    Dom.parse(                     ".") .toString =~~= "."
    Dom.parse(                   "com") .toString =~~= "com"
    Dom.parse(        "abc.google.com") .toString =~~= "abc.google.com"
    Dom.parse(        "www.google.com.") =~~= Dom.parse(        "www.google.com")
    Dom.parse(      "www.l.google.com.") =~~= Dom.parse(      "www.l.google.com")
    Dom.parse(    "a.www.l.google.com.") =~~= Dom.parse(    "a.www.l.google.com")
    Dom.parse(  "z.a.www.l.google.com.") =~~= Dom.parse(  "z.a.www.l.google.com")
    Dom.parse("7.z.a.www.l.google.com.") =~~= Dom.parse("7.z.a.www.l.google.com")
    Dom.parse(                      ".").length =~~= 0
    Dom.parse(                   "com.").length =~~= 1
    Dom.parse(            "google.com.").length =~~= 2
    Dom.parse(        "www.google.com.").length =~~= 3
    Dom.parse(      "www.l.google.com.").length =~~= 4
    Dom.parse(    "a.www.l.google.com.").length =~~= 5
    Dom.parse(  "z.a.www.l.google.com.").length =~~= 6
    Dom.parse("7.z.a.www.l.google.com.").length =~~= 7
    Dom.parse(                    "com").length =~~= 1
    Dom.parse(             "google.com").length =~~= 2
    Dom.parse(         "www.google.com").length =~~= 3
    Dom.parse(       "www.l.google.com").length =~~= 4
    Dom.parse(     "a.www.l.google.com").length =~~= 5
    Dom.parse(   "z.a.www.l.google.com").length =~~= 6
    Dom.parse( "7.z.a.www.l.google.com").length =~~= 7

    require(Dom.parse(".")==Root)
    require(Dom.parse("www.google.com") childOf Dom.parse("www.google.com"))
    require(Dom.parse("www.google.com") childOf Dom.parse("google.com"))
    require(Dom.parse("www.google.com") childOf Dom.parse("com"))
    require(Dom.parse("www.google.com") childOf Dom.parse("."))
    require(Dom.parse(".") childOf Dom.parse("."))
    require(!Dom.parse(".").childOf(Dom.parse("net")))
    require(!Dom.parse("www.google.com").childOf(Dom.parse("net")))
    require(Dom.parse("www.google.com.").toString=="www.google.com")
    require(Dom.parse("www.google.com" ).toString=="www.google.com")

    require(Dom.parse("www.google.com" ).applyLeft(0) contentEquals "www")
    //require("www" contentEquals Dom.parse("www.google.com" ).applyLeft(0))


    (              "www" +:        Dom.parse("google.com")).toString =~~=       "www.google.com"
    (              "www" +: "l" +: Dom.parse("google.com")).toString =~~=     "www.l.google.com"
    (       "a" +: "www" +: "l" +: Dom.parse("google.com")).toString =~~=   "a.www.l.google.com"
    ("z" +: "a" +: "www" +: "l" +: Dom.parse("google.com")).toString =~~= "z.a.www.l.google.com"
    Dom.parse("www.google.com").dropLeft(1).toString                 =~~=           "google.com"


    Dom.parse(                   "com").dropLeft(1).toString =~~=                    "."
    Dom.parse(            "google.com").dropLeft(1).toString =~~=                  "com"
    Dom.parse(          "l.google.com").dropLeft(1).toString =~~=           "google.com"
    Dom.parse(      "www.l.google.com").dropLeft(1).toString =~~=         "l.google.com"
    Dom.parse(    "a.www.l.google.com").dropLeft(1).toString =~~=     "www.l.google.com"
    Dom.parse(  "z.a.www.l.google.com").dropLeft(1).toString =~~=   "a.www.l.google.com"
    Dom.parse("7.z.a.www.l.google.com").dropLeft(1).toString =~~= "z.a.www.l.google.com"

    //println(Dom.parse(".com"))
    //println(Dom.parse("net..").productArity)
    //println(DomAsciiOrder.compare(Dom.parse("com"),     Dom.parse("net")))


    require(Host.AsciiOrdering.lt(IPv4.parse("8.8.8.8"), IPv6.parse("::1")))
    require(Host.AsciiOrdering.lt(IPv6.parse("::1")    , Dom.parse(".")))
    require(Host.AsciiOrdering.lt(IPv4.parse("8.8.8.8"), Dom.parse(".")))
    require(Host.AsciiOrdering.lt(IPv4.parse("0.0.0.0"), Dom.parse(".")))

    require(Host.AsciiOrdering.lt(Dom.parse("."),       Dom.parse("ac")))
    require(Host.AsciiOrdering.lt(Dom.parse("com"),     Dom.parse("net")))
    require(Host.AsciiOrdering.lt(Dom.parse("xxx.com"), Dom.parse("net")))
    require(Host.AsciiOrdering.lt(Dom.parse("com"),     Dom.parse("xxx.com")))
    require(Host.AsciiOrdering.lt(Dom.parse("com"),     Dom.parse("xxx.net")))
    require(Host.AsciiOrdering.lt(Dom.parse("xxx.com"), Dom.parse("xxx.net")))
    require(Host.AsciiOrdering.lt(Dom.parse("xxx.com"), Dom.parse("xxy.com")))
    require(Host.AsciiOrdering.lt(Dom.parse("xxx.com"), Dom.parse("www.xxx.com")))

    require(hex(NormUrlAsciiBinOrdering.toByteArray(NormUrl parse "https://0.0.0.0/")) < hex(NormUrlAsciiBinOrdering.toByteArray(NormUrl parse "http://./")),
           (hex(NormUrlAsciiBinOrdering.toByteArray(NormUrl parse "https://0.0.0.0/")),
            hex(NormUrlAsciiBinOrdering.toByteArray(NormUrl parse "http://./"))))



    IPv4(127,  0,  0, 66)  =~~= IPv4.parse("127.0.0.66")
    IPv4(127,  0,  0,102)  =~~= IPv4.parse("127.0.0.0x66")
    IPv4(127,  0,  0, 54)  =~~= IPv4.parse("127.0.0.066")
    IPv4(127,  0,  0,  1)  =~~= IPv4.parse("0x7F000001")
    IPv4(127,  0,  0,  1)  =~~= IPv4.parse("2130706433")
    IPv4(127,  0,  0,  1)  =~~= IPv4.parse("017700000001")
    IPv4(255,255,255,255)  =~~= IPv4.parse("0xFFFFFFFF")
    IPv4(255,255,255,255)  =~~= IPv4.parse("037777777777")
    IPv4(255,255,255,255)  =~~= IPv4.parse("4294967295")

    IPv6(0,0,0,0,0,0,0,0)  =~~= IPv6.parse("::")
    IPv6(0,0,0,0,0,0,0,0)  =~~= IPv6.parse("::0")
    IPv6(0,0,0,0,0,0,0,0)  =~~= IPv6.parse("0::")
    IPv6(0,0,0,0,0,0,0,0)  =~~= IPv6.parse("0::0")
    IPv6(0,0,0,0,0,0,0,1)  =~~= IPv6.parse("::1")
    IPv6(1,0,0,0,0,0,0,0)  =~~= IPv6.parse("1::")
    IPv6(1,0,0,0,0,0,0,1)  =~~= IPv6.parse("1::1")
    IPv6(0,0,0,0,0,0xFFFF,129*256+144,52*256+38) =~~= IPv6.parse("::FFFF:129.144.52.38")
    IPv6(0,0,0,0,0,0xFFFF,129*256+144,52*256+38) =~~= IPv6.parse("::FFFF:8190:3426")

    "::"                     =~~= IPv6.parse("::")                                       .toString
    "::"                     =~~= IPv6.parse("::0")                                      .toString
    "::"                     =~~= IPv6.parse("0::")                                      .toString
    "::"                     =~~= IPv6.parse("0::0")                                     .toString
    "::1"                    =~~= IPv6.parse("::1")                                      .toString
    "1::"                    =~~= IPv6.parse("1::")                                      .toString
    "1::1"                   =~~= IPv6.parse("1::1")                                     .toString
    "::ffff:129.144.52.38"   =~~= IPv6.parse("::FFFF:129.144.52.38")                     .toString
    "::ffff:129.144.52.38"   =~~= IPv6.parse("::FFFF:8190:3426")                         .toString
    "::8.8.8.8"              =~~= IPv6.parse("::8.8.8.8")                                .toString
    "::255.255.255.255"      =~~= IPv6.parse("::255.255.255.255")                        .toString
    "2a00:1450:4001:c02::67" =~~= IPv6.parse("2a00:1450:4001:c02::67")                   .toString
    "2a00:1450:4001:c02::67" =~~= IPv6.parse("[2a00:1450:4001:c02::67]")                 .toString
    "2a00:1450:4001:c02::67" =~~= IPv6.parse("2a00:1450:4001:c02:0:0:0:67")              .toString
    "2a00:1450:4001:c02::67" =~~= IPv6.parse("2A00:1450:4001:0C02:0000:0000:0000:0067")  .toString
    "2a01:4f8:b10:5001::2"   =~~= IPv6.parse("2a01:4f8:b10:5001:0:0:0:2")                .toString
    "2001:db8::1:a00:6789"   =~~= IPv6.parse("2001:DB8::1:10.0.103.137")                 .toString

    IPv4.parse("0.0.0.0").ceil (32) =~~= IPv4.parse("0.0.0.0")
    IPv4.parse("0.0.0.0").ceil (31) =~~= IPv4.parse("0.0.0.1")
    IPv4.parse("0.0.0.0").ceil (24) =~~= IPv4.parse("0.0.0.255")
    IPv4.parse("0.0.0.0").ceil ( 1) =~~= IPv4.parse("127.255.255.255")
    IPv4.parse("0.0.0.0").ceil ( 0) =~~= IPv4.parse("255.255.255.255")

    IPv4.parse("255.255.255.255").floor(32) =~~= IPv4.parse("255.255.255.255")
    IPv4.parse("255.255.255.255").floor(31) =~~= IPv4.parse("255.255.255.254")
    IPv4.parse("255.255.255.255").floor(24) =~~= IPv4.parse("255.255.255.0")
    IPv4.parse("255.255.255.255").floor( 1) =~~= IPv4.parse("128.0.0.0")
    IPv4.parse("255.255.255.255").floor( 0) =~~= IPv4.parse("0.0.0.0")

    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor(128) =~~= IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor(127) =~~= IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe")
    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor( 65) =~~= IPv6.parse("ffff:ffff:ffff:ffff:8000:0000:0000:0000")
    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor( 64) =~~= IPv6.parse("ffff:ffff:ffff:ffff:0000:0000:0000:0000")
    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor( 63) =~~= IPv6.parse("ffff:ffff:ffff:fffe:0000:0000:0000:0000")
    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor(  1) =~~= IPv6.parse("8000:0000:0000:0000:0000:0000:0000:0000")
    IPv6.parse("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff").floor(  0) =~~= IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000")

    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil (128) =~~= IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000")
    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil (127) =~~= IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0001")
    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil ( 65) =~~= IPv6.parse("0000:0000:0000:0000:7FFF:FFFF:FFFF:FFFF")
    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil ( 64) =~~= IPv6.parse("0000:0000:0000:0000:FFFF:FFFF:FFFF:FFFF")
    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil ( 63) =~~= IPv6.parse("0000:0000:0000:0001:FFFF:FFFF:FFFF:FFFF")
    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil (  1) =~~= IPv6.parse("7FFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")
    IPv6.parse("0000:0000:0000:0000:0000:0000:0000:0000").ceil (  0) =~~= IPv6.parse("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")

    IPv4.parse("192.168.1.1").toIPv6.toString             =~~= "::192.168.1.1"
    IPv4.parse("192.168.1.1").toMappedIPv6.toString       =~~= "::ffff:192.168.1.1"
    IPv6.parse("::ffff:192.168.1.1").toString             =~~= "::ffff:192.168.1.1"
    IPv6.parse("::ffff:192.168.1.1").mappedIPv4.toString  =~~= "Some(192.168.1.1)"

    require(IP.parse("69.73.131.113").startsWith("69.73.131."))

    "http://255.255.255.255/"                           =~~= NormUrl.parse("4294967295").toString
    "http://8.8.8.8/"                                   =~~= NormUrl.parse("8.8.8.8").toString
    "http://8.8.8.8/"                                   =~~= NormUrl.parse("8.8.8.8:").toString
    "http://8.8.8.8/"                                   =~~= NormUrl.parse("8.8.8.8:80").toString
    "http://8.8.8.8:9/"                                 =~~= NormUrl.parse("8.8.8.8:9").toString
    "http://[::8.8.8.8]/"                               =~~= NormUrl.parse("[::8.8.8.8]").toString
    "http://[::8.8.8.8]/"                               =~~= NormUrl.parse("[::8.8.8.8]:").toString
    "http://[::8.8.8.8]/"                               =~~= NormUrl.parse("[::8.8.8.8]:80").toString
    "http://[::8.8.8.8]:9/"                             =~~= NormUrl.parse("[::8.8.8.8]:9").toString
    "http://[::1]/"                                     =~~= NormUrl.parse("[::1]").toString
    "http://[::1]/"                                     =~~= NormUrl.parse("[::1]:").toString
    "http://[::1]/"                                     =~~= NormUrl.parse("[::1]:80").toString
    "http://[::1]:9/"                                   =~~= NormUrl.parse("[::1]:9").toString
    "http://twitter.com/"                               =~~= NormUrl.parse("twitter.com").toString
    "http://twitter.com/"                               =~~= NormUrl.parse("twitter.com:").toString
    "http://twitter.com/"                               =~~= NormUrl.parse("twitter.com:80").toString
    "http://twitter.com:9/"                             =~~= NormUrl.parse("twitter.com:9").toString
    "http://[2a00:1450:4001:c02::67]/"                  =~~= NormUrl.parse("[2a00:1450:4001:c02::67]").toString
    // bare IPv6 must be parsed
    "http://[::1]/"                                     =~~= NormUrl.parse("::1").toString
    "http://[::8.8.8.8]/"                               =~~= NormUrl.parse("::8.8.8.8").toString
    "http://[2a00:1450:4001:c02::67]/"                  =~~= NormUrl.parse("2a00:1450:4001:c02::67").toString
    "http://f.com/"                                     =~~= NormUrl.parse("%46.com").toString

    def ser(a: String, b: String, c: String) {
      val bx = new String(NormUrlAsciiBinOrdering.toByteArray(NormUrl.parse(a)))
      require(b == bx, s"\na=$BLUE_B$a$RESET\nb=$RED_B$bx$RESET\nb=$GREEN_B$b$RESET")
      val cx = NormUrlAsciiBinOrdering.fromBuffer(ByteBuffer wrap b.getBytes).toString
      require(c == cx, s"\na=$BLUE_B$a$RESET\nb=$BLUE_B$b$RESET\nc=$RED_B$cx$RESET\nc=$GREEN_B$c$RESET")
    }

    ser("4294967295",                    "\u0004FFFFFFFF\u00000050\u0000http\u0000\u0000\u0000",                                        "http://255.255.255.255/"             )
    ser("[::1]",                         "\u000600000000000000000000000000000001\u00000050\u0000http\u0000\u0000\u0000",                "http://[::1]/"                       )
    ser("[::8.8.8.8]",                   "\u000600000000000000000000000008080808\u00000050\u0000http\u0000\u0000\u0000",                "http://[::8.8.8.8]/"                 )
    ser("[2a00:1450:4001:c02::67]",      "\u00062A00145040010C020000000000000067\u00000050\u0000http\u0000\u0000\u0000",                "http://[2a00:1450:4001:c02::67]/"    )
    ser("[::8.8.8.8]:9/path?query#hash", "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000?query\u0000#hash", "http://[::8.8.8.8]:9/path?query#hash")
    ser("[::8.8.8.8]:9/path?query#",     "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000?query\u0000#",     "http://[::8.8.8.8]:9/path?query#"    )
    ser("[::8.8.8.8]:9/path?query",      "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000?query\u0000",      "http://[::8.8.8.8]:9/path?query"     )
    ser("[::8.8.8.8]:9/path?#hash",      "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000?\u0000#hash",      "http://[::8.8.8.8]:9/path?#hash"     )
    ser("[::8.8.8.8]:9/path#hash",       "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000\u0000#hash",       "http://[::8.8.8.8]:9/path#hash"      )
    ser("[::8.8.8.8]:9/?query#hash",     "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000?query\u0000#hash",     "http://[::8.8.8.8]:9/?query#hash"    )
    ser("[::8.8.8.8]:9/?query#",         "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000?query\u0000#",         "http://[::8.8.8.8]:9/?query#"        )
    ser("[::8.8.8.8]:9/?query",          "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000?query\u0000",          "http://[::8.8.8.8]:9/?query"         )
    ser("[::8.8.8.8]:9/?#hash",          "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000?\u0000#hash",          "http://[::8.8.8.8]:9/?#hash"         )
    ser("[::8.8.8.8]:9/path?#",          "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000?\u0000#",          "http://[::8.8.8.8]:9/path?#"         )
    ser("[::8.8.8.8]:9/path#",           "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000\u0000#",           "http://[::8.8.8.8]:9/path#"          )
    ser("[::8.8.8.8]:9/path?",           "\u000600000000000000000000000008080808\u00000009\u0000http\u0000path\u0000?\u0000",           "http://[::8.8.8.8]:9/path?"          )
    ser("[::8.8.8.8]:9/?#",              "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000?\u0000#",              "http://[::8.8.8.8]:9/?#"             )
    ser("[::8.8.8.8]:9/#",               "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000\u0000#",               "http://[::8.8.8.8]:9/#"              )
    ser("[::8.8.8.8]:9/?",               "\u000600000000000000000000000008080808\u00000009\u0000http\u0000\u0000?\u0000",               "http://[::8.8.8.8]:9/?"              )


    val lst = List( NormUrl parse "http://untitled.urbansheep.com/archive"
                  , NormUrl parse "http://untitled.urbansheep.com/archive#"
                  , NormUrl parse "http://untitled.urbansheep.com/archive/"
                  , NormUrl parse "http://untitled.urbansheep.com/archive/#"
                  , NormUrl parse "http://untitled.urbansheep.com/archive/2015/9"
                  , NormUrl parse "http://untitled.urbansheep.com/archive/?before_time=1376952202"
                  , NormUrl parse "http://untitled.urbansheep.com/archive?before_time=1376952202")
    lst.sorted(NormUrl.UniOrdering) =~~= lst.sortBy(u=>NormUrlAsciiBinOrdering.toByteArray(u))(UByteArrayOrdering)



    parse("8.8.8.8")                                    =~~= NormUrl.parse("http://8.8.8.8/")
    parse("[::8.8.8.8]")                                =~~= NormUrl.parse("http://[::8.8.8.8]/")
    parse("twitter.com")                                =~~= NormUrl.parse("http://twitter.com/")
    parse("twitter.com:")                               =~~= NormUrl.parse("http://twitter.com/")

    val u = parse("https://twitter.com/intent/tweet?original_referer=http%3A%2F%2Fwww.theguardian.com%2Fcommentisfree%2F2013%2Faug%2F25%2Ftoughen-up-scottish-nationalists-mckenna%3FINTCMP%3DSRCH&related=commentisfree&text=Toughen%20up%2C%20Scottish%20nationalists%2C%20and%20stop%20playing%20nice%20%7C%20Kevin%20McKenna&tw_p=tweetbutton&url=http%3A%2F%2Fgu.com%2Fp%2F3t9g5%2Ftw&via=guardian")
    //println(u)
    val u2 = u.copy(query = mkQuery((u.queryMap + ("url" -> "replacement")).toSeq:_*))
    //println(u2)

    require(parse("https://twitter.com/search?f=realtime&q=http://archive.is/&src=typd") ==
            parse("https://twitter.com/search?f=realtime&q=http:%2F%2Farchive.is/&src=typd"))
    require(parse("http://webcache.googleusercontent.com/search?q=cache:IUzzdyXslLEJ:https://twitter.com/rinda0818+&cd=1&hl=ja&ct=clnk&gl=jp&lr=lang_ja&inlang=ja") !=
            parse("http://webcache.googleusercontent.com/search?q=cache:IUzzdyXslLEJ:https://twitter.com/rinda0818%20&cd=1&hl=ja&ct=clnk&gl=jp&lr=lang_ja&inlang=ja"))

    val a = List("status" -> "http://archive.is/evNtK História - Tudósnaptár - Sarb%F3 Art%FAr", "url" -> "http://archive.is/evNtK")
    val Some(q) = mkQuery(a:_*)
    require(parseQuery(q) sameElements a)

    for(cp <- 0 to 0x10FFFF by 101) {
      val s = if (cp <= 0xFFFF) cp.toChar.toString else new String(Array(cp), 0, 1)
    //val hexuri = s.getBytes.map("%%%02x" format _).mkString
      val hexuri2 = escape(isYandexCache=false, '/', s, 0, s.length) // it handles alone surrogates 0xD800, s.getBytes does not
      require(unhexFull(hexuri2) == s, (cp.toHexString, s.map(_.toInt), hexuri2, unhexFull(hexuri2).toList))
    }

    require(escape(isYandexCache=false, '?', "\uD800\uDC00=\uDBFF\uDFFF", 0, 5)  == "%F0%90%80%80=%F4%8F%BF%BF")
    require(mkQuery("\uD800\uDC00" -> "\uDBFF\uDFFF") == Some("%F0%90%80%80=%F4%8F%BF%BF"))
    for(cp <- 0 to 0x10FFFF by 103) {
      val s1 = if (cp <= 0xFFFF) cp.toChar.toString else new String(Array(cp), 0, 1)
      val s2 = s1 * 5
      val Some(q) = mkQuery(s1 -> s2)
      parseQuery(q) match { case Array((`s1`, `s2`)) =>
                            case x => require(false, (cp.toHexString, s1, s2, q, parseQuery(q).toList)) }
    }

    escape(isYandexCache=false, '/', "?#", 0, 2) =~~= "%3F%23"
    escape(isYandexCache=false, '?', "?#", 0, 2) =~~= "%3F%23"
    escape(isYandexCache=false, '#', "?#", 0, 2) =~~= "?#"
//    require(isEscaped('/', "%20") == true)
//    require(isEscaped('?', "%20") == true)
//    require(isEscaped('#', "%20") == true)
//    require(isEscaped('/', " ") == false)
//    require(isEscaped('?', " ") == false)
//    require(isEscaped('#', " ") == false)
    //println(NormUrl("http", "t.com", 80, "hash?in#path", None, None))
    //println(NormUrl("http", "t.com", 80, "hash%3Fin%23path", None, None))
    parse("http://t.com/hash%3Fin%23path%20%25%00%46"             ).toString    =~~= "http://t.com/hash%3Fin%23path%20%25%00F"
    parse("http://t.com/hash%3Fin%23path%20%25%00%46"             ).toUniString =~~= "http://t.com/hash%3Fin%23path%20%25%00F"
    parse("http://t.com/hash%3Fin%23path%20%25%00%46"             ).path        =~~= "hash%3Fin%23path%20%25%00F"
    parse("http://t.com/hash%3Fin%23path%20%25%00%46"             ).rawpath     =~~= "hash?in#path %\u0000F"
    parse("http://t.com/?%00=hash%3Fin%23%26path%20+ %2B%25%00%46").toString    =~~= "http://t.com/?%00=hash%3Fin%23%26path%20++%2B%25%00F"
    parse("http://t.com/?%00=hash%3Fin%23%26path%20+ %2B%25%00%46").toUniString =~~= "http://t.com/?%00=hash?in%23%26path%20++%2B%25%00F"
    parse("http://t.com/?%00=hash%3Fin%23%26path%20+ %2B%25%00%46").query       =~~= Some("%00=hash%3Fin%23%26path%20++%2B%25%00F")
    parse("http://t.com/?%00=hash%3Fin%23%26path%20+ %2B%25%00%46").queryMap    =~~= Map("\u0000" -> "hash?in#&path   +%\u0000F")


    parse("http://t.com/|").toString      =~~= "http://t.com/%7C"  // println(parse("http://t.com/|"  ).toString   )
  //parse("http://t.com/|").toUniString   =~~= "http://t.com/%7C"  // println(parse("http://t.com/|"  ).toUniString)
    parse("http://t.com/%7C").toString    =~~= "http://t.com/%7C"  // println(parse("http://t.com/%7C").toString   )
  //parse("http://t.com/%7C").toUniString =~~= "http://t.com/%7C"  // println(parse("http://t.com/%7C").toUniString)

    parse("http://t.com/%C0%A6") .toString =~~= "http://t.com/%C0%A6"
    parse("http://t.com/%C")     .toString =~~= "http://t.com/%25C"
    parse("http://t.com/%xx")    .toString =~~= "http://t.com/%25xx"
    parse("http://t.com/%45")    .toString =~~= "http://t.com/E"
    parse("http://t.com/%xx%45") .toString =~~= "http://t.com/%25xxE"
    parse("http://t.com./%xx%45").toString =~~= "http://t.com/%25xxE"

    parse("https://portal.dnb.de/opac.htm?query=Woe%3D%3118679694&method=simpleSearch").toString =~~= "https://portal.dnb.de/opac.htm?query=Woe%3D118679694&method=simpleSearch"
    parse("https://portal.dnb.de/opac.htm?query=Woe%3D118679694&method=simpleSearch")  .toString =~~= "https://portal.dnb.de/opac.htm?query=Woe%3D118679694&method=simpleSearch"
    parse("https://portal.dnb.de/opac.htm?query=Woe=118679694&method=simpleSearch")    .toString =~~= "https://portal.dnb.de/opac.htm?query=Woe=118679694&method=simpleSearch"

    NormUrl.parse("http://rab1n0v1ch:xxxxxx@wolf-kitses.livejournal.com/data/rss").toString =~~= "http://wolf-kitses.livejournal.com/data/rss"
    NormUrl.parse("http://rab1n0v1ch@wolf-kitses.livejournal.com/data/rss"       ).toString =~~= "http://wolf-kitses.livejournal.com/data/rss"
    NormUrl.parse("http://h@wolf-kitses.livejournal.com/data/rss"                ).toString =~~= "http://wolf-kitses.livejournal.com/data/rss"
    NormUrl.parse("http://:@wolf-kitses.livejournal.com/data/rss"                ).toString =~~= "http://wolf-kitses.livejournal.com/data/rss"
    NormUrl.parse("http://@wolf-kitses.livejournal.com/data/rss"                 ).toString =~~= "http://wolf-kitses.livejournal.com/data/rss"
    NormUrl.parse("http://rab1n0v1ch:xxxx@-adept-.livejournal.com/data/rss"      ).toString =~~= "http://-adept-.livejournal.com/data/rss"
    NormUrl.parse("http://engineering-matlab.blogspot.in/:Matlab"                ).toString =~~= "http://engineering-matlab.blogspot.in/:Matlab"

    IP.parse("172.15.255.255").isInternet =~~= true
    IP.parse("172.16.0.0"    ).isInternet =~~= false
    IP.parse("172.31.255.255").isInternet =~~= false
    IP.parse("172.32.0.0"    ).isInternet =~~= true

    require(Try(NormUrl parse "web.archive.org/web/2000/http://sexblbn.com/videos?o=bw"      ).isSuccess) // ok
    require(Try(NormUrl parse "ftp://web.archive.org/web/2000/http://sexblbn.com/videos?o=bw").isFailure) // bad scheme
    require((NormUrl parse "http://www.stebenkova.com/.")    == (NormUrl parse "http://www.stebenkova.com/"))
    require((NormUrl parse "http://www.stebenkova.com/xxx/") == (NormUrl parse "http://www.stebenkova.com/xxx/."))
    require((NormUrl parse "http://www.stebenkova.com/xxx/") != (NormUrl parse "http://www.stebenkova.com/xxx"))

    require(Try(NormUrl parse "").isFailure)
    require(Try(NormUrl parse "http//").isSuccess)
    require(Try(NormUrl parse "http://").isFailure)
    require(Try(NormUrl parse "http://http//").isSuccess)

    require(!NormUrl.parse("http://com/").host.isInternet)
    require( NormUrl.parse("http://tk/" ).host.isInternet) // the only TLD with A-record

    NormUrl.parse("http://www.google.com/search?q=Pay%20per%20install").toString                                       =~~= "http://www.google.com/search?q=Pay%20per%20install"
    NormUrl.parse("http://www.google.com/search?q=Pay+per+install"    ).toString                                       =~~= "http://www.google.com/search?q=Pay+per+install"
    NormUrl.parse("http://www.google.com/search?q=Pay%20per%20install").toUniString                                    =~~= "http://www.google.com/search?q=Pay%20per%20install"
    NormUrl.parse("http://www.google.com/search?q=Pay+per+install"    ).toUniString                                    =~~= "http://www.google.com/search?q=Pay+per+install"
    NormUrl.parse("http://www.google.com/search?q=http://?"           ).toString                                       =~~= "http://www.google.com/search?q=http://%3F" /* "http://www.google.com/search?q=http%3A%2F%2F%3F" */
    NormUrl.parse("http://www.google.com/search?q=http%3A%2F%2F%3F"   ).toString                                       =~~= "http://www.google.com/search?q=http://%3F" /* "http://www.google.com/search?q=http%3A%2F%2F%3F" */
    NormUrl.parse("http://www.google.com/search?q=http://?"           ).toUniString                                    =~~= "http://www.google.com/search?q=http://?"
    NormUrl.parse("http://www.google.com/search?q=http%3A%2F%2F%3F"   ).toUniString                                    =~~= "http://www.google.com/search?q=http://?"
    NormUrl.parse("http://nestroy.at/eingang.html?nestroy-materialien/basisbibliographie/index.shtml"    ).toString    =~~= "http://nestroy.at/eingang.html?nestroy-materialien/basisbibliographie/index.shtml"
    NormUrl.parse("http://nestroy.at/eingang.html?nestroy-materialien/basisbibliographie/index.shtml"    ).toUniString =~~= "http://nestroy.at/eingang.html?nestroy-materialien/basisbibliographie/index.shtml"
    // todo: they are the save in key-form, is it good?
    // tc: https://archive.today/http://nestroy.at/eingang.html?nestroy-materialien/basisbibliographie/index.shtml
    require(NormUrl.parse("http://nestroy.at/eingang.html?url=nestroy-materialien/basisbibliographie/index.shtml").toString    == "http://nestroy.at/eingang.html?url=nestroy-materialien/basisbibliographie/index.shtml"
                                                                                                                               /* "http://nestroy.at/eingang.html?url=nestroy-materialien%2Fbasisbibliographie%2Findex.shtml" */)
    require(NormUrl.parse("http://nestroy.at/eingang.html?url=nestroy-materialien/basisbibliographie/index.shtml").toUniString == "http://nestroy.at/eingang.html?url=nestroy-materialien/basisbibliographie/index.shtml"    )

    /*
    val ux = NormUrl.parse("http://www.xn--n8jvce0l6c.com/entry/2014/03/29/%E3%80%90%E6%96%87%E5%AD%97%E8%B5%B7%E3%81%93%E3%81%97%E3%80%91%E3%80%8E%E5%B1%B1%E6%9C%AC%E4%BC%B8%E3%80%8F%EF%BC%88%E8%B6%85%E4%BA%BA%E6%B0%97%E3%80%81%E6%A0%AA%E5%BC%8F%E8%A9%95%E8%AB%96%E5%AE%B6")
    println(esc(HostAsciiOrder.toByteArray(ux.host)))
    println(esc(HostUniOrder.toByteArray(ux.host)))
    println(ux.host.toUniString)
    println(ux.toUniString)
    */
    locally {
      val u1 = NormUrl parse "http://scholar.google.com/scholar?&q=%22%E9%98%AE%E7%A6%8F%E7%B6%BF%EF%BC%88%E5%AE%80%E7%96%8C%EF%BC%89%22"
      val u2 = NormUrl parse "http://scholar.google.com/scholar?&q=%22%E9%98%AE%E7%A6%8F%E7%B6%BF%F0%A1%A9%80%22"
      require(-1 == NormUrl.AsciiOrdering.compare(u1, u2))
      require(-1 == u1.toString.compare(u2.toString))
      require(-1 == UByteArrayOrdering.compare( NormUrlAsciiBinOrdering.toByteArray(u1), NormUrlAsciiBinOrdering.toByteArray(u2) ))

      require(-1 == NormUrl.UniOrdering.compare(u1, u2))
      require(-1 != u1.toUniString.compare(u2.toUniString)) // UTF-16 comparision
      require(-1 == UByteArrayOrdering.compare( u1.toUniString.getBytes("UTF-8"), u2.toUniString.getBytes("UTF-8") ))
      require(-1 == UByteArrayOrdering.compare( NormUrlUniBinOrdering.toByteArray(u1),  NormUrlUniBinOrdering.toByteArray(u2)  ))
    }

    TLD.checkEmergingTLDs()

    java.net.IDN.toUnicode("xn--57h.ws", java.net.IDN.ALLOW_UNASSIGNED) =~~= "⚡.ws"
    java.net.IDN.toASCII("⚡.ws",         java.net.IDN.ALLOW_UNASSIGNED) =~~= "xn--57h.ws"

    java.net.IDN.toASCII("☠.ws"    ) =~~= "xn--h4h.ws"
    java.net.IDN.toASCII("❤❤❤.ws"  ) =~~= "xn--qeiaa.ws"
    java.net.IDN.toASCII("мой_сайт") =~~= "xn--_-8sbzclmxk"
    java.net.IDN.toASCII("МОЙ_САЙТ") =~~= "xn--_-8sbzclmxk"

    java.net.IDN.toUnicode("xn--_-8sbzclmxk"               ) =~~= "мой_сайт"
    java.net.IDN.toUnicode("XN--_-8SBZCLMXK"               ) =~~= "мой_сайт"
    java.net.IDN.toUnicode("xn--_cgbgp-2tbbbbbbb6650i"     ) =~~= "ƒ_ƒcƒgƒbƒgƒpƒ“"
    java.net.IDN.toUnicode("xn--bad_mnder-u9a"             ) =~~= "bad_münder"
    java.net.IDN.toUnicode("xn--dwignia_finansowa-vxd"     ) =~~= "dźwignia_finansowa"
    java.net.IDN.toUnicode("xn--br_blank-eq3d"             ) =~~= "br”_blank"
    java.net.IDN.toUnicode("xn--_www-941s40i7ux"           ) =~~= "페이지_www"
    java.net.IDN.toUnicode("xn--_-ctbjjdgf3agwdeh9o"       ) =~~= "советский_союз"
    java.net.IDN.toUnicode("xn--xkcd___-8fgai9b6a0arpyqcql") =~~= "xkcd_про_пик_балмера"
    java.net.IDN.toUnicode("xn--pl_oficil_mjaslapa-j9bbc"  ) =~~= "pl_oficiālā_mājaslapa"
    java.net.IDN.toUnicode("xn----ctbjaxcuetidc"           ) =~~= "воскресенск-"
    java.net.IDN.toUnicode("xn--_-8sbabrqfafkd8aie4an"     ) =~~= "сборники_заданий"
    java.net.IDN.toUnicode("xn--huformalogkexp_m1-pyb"     ) =~~= "huformalogókexp_m1"
    java.net.IDN.toUnicode("xn--_-7sbbf2b7bj7b"            ) =~~= "ваш_сайт"


    // but they are different, aren't they?
    NormUrl.parse("https://webcache.googleusercontent.com/search?q=cache:https://www.youtube.com/playlist?list=PLx5jDempMhFNHKYaWwNUBVsbop0op-fZK") =~~=
    NormUrl.parse("https://webcache.googleusercontent.com/search?q=cache:https://www.youtube.com/playlist%3Flist=PLx5jDempMhFNHKYaWwNUBVsbop0op-fZK")



    /*
    println("=== old ===")
    println(TLD.format(TLD.oldTLDs.keys.map(_.toString)))
    println("=== new ===")
    println(TLD.format(TLD.newTLDs.keys.map(_.toString)))
    */

    if (false) {
      import scala.collection.JavaConversions._
      // https://raw.githubusercontent.com/andrewsmhay/higher-education/a4a87e5382ac477cd56277d1faab9521d66ea0e1/master.doms
      // https://raw.githubusercontent.com/hichamjabbour/floodlight-firewall/99e8b34347a99a2c2a753d9080d9f1c726940388/BL/military/domains
      val pw = new java.io.PrintWriter("out.txt")
      val backlinks: Array[NormUrl] = for (line <- org.apache.commons.io.FileUtils.lineIterator(new java.io.File("C:\\archive.is\\yandex\\archive.is.txt"), "UTF-8").toArray)
                                      yield NormUrl.parse(line.split('\t') match {
                                                            case Array(to, from, text) => from
                                                            case Array(to, from)       => from
                                                          })
      pw println(backlinks.sorted(NormUrl.AsciiOrdering).distinct mkString "\n")
      pw.close()

      /*
      var eduzones = Set.empty[Dom]

      edudoms = for (dom <- edudoms;
                     dom2 <- if (dom.applyRight(0).contentEquals("edu")) {
                               eduzones += dom.takeRight(1)
                               None
                             } else if (dom.applyRight(1).contentEquals("edu") || dom.applyRight(1).contentEquals("ac")
                                     || dom.applyRight(1).contentEquals("mil")
                                  // || dom.applyRight(1).contentEquals("gouv") || dom.applyRight(1).contentEquals("gov") || dom.applyRight(1).contentEquals("gv")
                                        ) {
                               eduzones += dom.takeRight(2)
                               None
                             } else {
                               Some(dom)
                             }) yield dom2

      println((edudoms ++ eduzones).sorted(Dom.AsciiOrdering) map ("*." + _) mkString " ")
      */
    }

    if (false) {
      for (c <- 0x00A0 to /*'\uFFEE'*/ 0x10FFFF) {
        try {
          val a = java.net.IDN.toASCII(new String(Array(c), 0, 1), java.net.IDN.ALLOW_UNASSIGNED)
          val s = java.net.IDN.toUnicode(a, java.net.IDN.ALLOW_UNASSIGNED)
          val d = s.codePoints.toArray.toList
          if (List(c)==d) {
            //println("+" + c.toHexString)
          } else {
            println("+" + c.toHexString + " " + d.map(_.toHexString))
          }
        } catch {
          case e: Throwable => println("-" + c.toHexString)
        }
      }
    }

  }
}
