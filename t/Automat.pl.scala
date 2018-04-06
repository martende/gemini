#!perl

use File::Basename qw(dirname basename);
require(dirname(__FILE__).'/../launcher/Launcher.pl.scala');

run(CLASS    => 'ms.webmaster.chromium.automat.Automat$',
    TEE      => "automat.txt");
=cut
!#

package ms.webmaster.chromium.automat

import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise, Await}
import `ChromeDebugProtocol.scala`,                           ms.webmaster.chromium.chromedebugprotocol.{ChromeDebugProtocol, ChromeDebugProtocolException},
                                                              ChromeDebugProtocol._
import `ChromeExe.scala`,                                     ms.webmaster.chromium.chromeexe.ChromeExe


object Automat {
  import monix.execution.Scheduler.Implicits.global

  def main(args: Array[String]) {

    val chromeExe = new ChromeExe(extraFlags=Array())
    try {
      println(s"cdp=${chromeExe.cdp}")

      chromeExe.cdp.cdpEvents foreach { e =>
        println(s"CDPEVENT $e")
        e match {
          case chromeExe.cdp.Inspector.targetCrashed             => sys.exit(1)
          case chromeExe.cdp.Inspector.detached("target_closed") => sys.exit(1)
          case _                                                 =>
        }
      }

      val fsession: Future[chromeExe.cdp.Session] = for (//target1  <- chromeExe.cdp.createTarget("about:blank");
                                                         //session1 <- target1.attachToTarget();
                                                         target2  <- chromeExe.cdp.createTarget("about:blank");
                                                         session2 <- target2.attachToTarget();
                                                         _        <- session2.Emulation.setDeviceMetricsOverride(1024, 768, deviceScaleFactor=1, mobile=false))
                                                    yield session2

      fsession andThen {
        case Success(session) =>
          session.sessionEvents.foreach { e =>
            println(s"EVENT $e")
            e match {
              case e: session.Debugger.scriptParsed =>
              case e: session.Page.loadEventFired =>
                for (x <- session.Runtime.evaluate("""
                            document.querySelector('#text').value = 'xx';
                            document.querySelector('form').submit();
                          """)) {
                  println(s"XXX $x")
                }
              case e =>
            }
          }

          for( _ <- session.Log        .enable();
               _ <- session.Page       .enable();
               _ <- session.Runtime    .enable();
//             _ <- session.Security   .enable();
//             _ <- session.Network    .enable();
               _ <- session.DOM        .enable();
//             _ <- session.Performance.enable();
               _ <- session.CSS        .enable();
//             _ <- session.Inspector  .enable();
               _ <- session.Console    .enable();
               _ <- session.Debugger   .enable();
               _ <- session.Network    .clearBrowserCache   ();
               _ <- session.Network    .clearBrowserCookies ();
               x <- session.Page.navigate("http://ya.ru")) {
            println(s"NAVIGATED $x")
          }

        case Failure(e) => e.printStackTrace
      }
      while(true)
        Thread sleep 20000

    } finally {
      chromeExe.quit()
    }

  }
}