package akka.gemini


import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.{FunSuiteLike, WordSpecLike, BeforeAndAfterAll}

import org.scalatest.time.Span
import org.scalatest.time.{Millis,Seconds}



import org.scalatest.concurrent.ScalaFutures

import akka.testkit.ImplicitSender

import com.typesafe.config.ConfigFactory

import scala.concurrent.Future
import scala.concurrent.duration._

import scala.util.{Success, Failure}
/*
class geminiSpec // extends TestKit(ActorSystem())
  extends  FunSuiteLike with BeforeAndAfterAll /*
with ImplicitSender with  */
*/

class PhantomSpec extends TestKit(ActorSystem()) with FunSuiteLike with ScalaFutures with ImplicitSender with BeforeAndAfterAll {



  //TODO: bad phantom bin should produce IOException
  //TODO: bad core.js path should produce some Exception ( not start timeout )

  override def beforeAll {
    PhantomExecutor.initEnvironment
  }

  test("Actor successfully started") {
    val fetcher = system.actorOf(PhantomExecutionActor.props(isDebug=false),"test1")
    within (2 seconds) {
      fetcher ! PhantomExecutionActor.Events.Start()

      expectMsgPF() {
        case PhantomExecutionActor.Events.Started() =>
      }

      fetcher ! PhantomExecutionActor.Events.OpenUrl(this.getClass.getResource("/testdata/t2.html").toString)

      expectMsgPF() {
        case PhantomExecutionActor.Events.OpenUrlResult(Success(s)) =>
      }

      fetcher ! akka.actor.PoisonPill
    }
  }


  test("t1") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    assert(p.title == "t2")

    assert(p.title == "t2")

    assert(p.$("#d1").innerHTML == "D1")
    assert(p.$("#d1no").innerHTML == "")

    p.$("#d2").click()

    assert(p.$("#d1").innerHTML == "D1C")

    intercept[AutomationException] {
      p.$("#d1no").click()
    }

    assert(p.$("#d2").attr("class") == "t3 d2class")

    p.close()
  }

  test("length") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val t = p.$("div.t3")
    assert( t.length == 2 )

  }


  test("t3") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    p.$("div.t3").foreach { x =>
      val t = x.attr("id")
      assert( t == "d1" || t == "d2")
    }

    val divs = p.$("div.t3")
    assert(divs.length==2)
    var cnt = 0
    for ( el <- divs if el.attr("id")=="d1" ) {
      cnt+=1
    }
    assert(cnt==1)
  }

  test("children $") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val c0 = p.$("div.c0")
    assert(c0.$("div").length == 2)
    val c1= p.$("div.c1")
    assert(c1.$("div").length == 4)

    assert(p.$("#c3").$("span").length == 3)
    assert(p.$("#c3").children.length == 2)
  }

  test("children $.$") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val c1= p.$("div.c1")
    val spans = c1.$("div").$("span")
    assert(spans.length == 3)

    val spanF =  spans.find{
      x => x.innerHTML == "span2"
    }.get

    val s2 = spans.head

    assert(s2.innerHTML == "span1")
  }

  test("getBoundingClientRect") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val cr= p.$("#d2").getBoundingClientRect

    assert(cr == ClientRect(8.0,108.0,27.0,129.0,100.0,100.0) )

  }

  test("offsetParent")  {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val op= p.$("#d2").offsetParent
    assert(op.tagName == "BODY")
    // many offset parents
    val ops= p.$("div.t3").offsetParent
    assert(ops.length == 1)

    val opspn = p.$("span").offsetParent

    assert(opspn.length == 3 )
  }

  test("re selector")  {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val t = p.$("span").re("span[13]")
    assert(t.length == 2 )
  }



  test("double open")  {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    val t = p.$("span").re("span[13]")
    assert(t.length == 2 )

    assert ( p.open(this.getClass.getResource("/testdata/t3.html").toString).futureValue )

    val t2 = p.$("span").re("span[13]")
    assert(t2.length == 2 )
  }



  test("uploadfile") {
    val p = PhantomExecutor(isDebug=false)

    assert ( p.open(this.getClass.getResource("/testdata/t3.html").toString).futureValue )

    p.uploadFile( p.$("input[type=file]") , this.getClass.getResource("/testdata/t3.html").toString)

  }


  test("uploadfileIframe - oldfantom(phantom 1.10)") {
    val p = PhantomExecutor(isDebug=false,PhantomConfig(mediumBin = "./bin/phantomjs"))

    assert ( p.open(this.getClass.getResource("/testdata/t4.html").toString).futureValue )

    p.uploadFile( p.$("input[type=file]") , this.getClass.getResource("/testdata/t3.html").toString)

  }


}


class PyQtSpec extends TestKit(ActorSystem()) with FunSuiteLike with ScalaFutures with ImplicitSender with BeforeAndAfterAll {



  override def beforeAll {
    PhantomExecutor.initEnvironment
  }

  val conf = PhantomConfig(startTimeout = 2 seconds,mediumType = MediumType.PyQt)
  implicit override val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(100, Millis))

  /*
    test("Actor successfully started") {
      val fetcher = system.actorOf(PhantomExecutionActor.props(isDebug=true,conf = conf),"test1")
      within (10 seconds) {
        fetcher ! PhantomExecutionActor.Events.Start()

        expectMsgPF() {
          case PhantomExecutionActor.Events.Started() =>
        }

        fetcher ! PhantomExecutionActor.Events.OpenUrl(this.getClass.getResource("/testdata/t2.html").toString)

        expectMsgPF() {
          case PhantomExecutionActor.Events.OpenUrlResult(Success(s)) =>
        }

        fetcher ! akka.actor.PoisonPill
      }
    }


  test("viewportSize") {
    val p = PhantomExecutor(isDebug=true,conf = conf)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    assert( p.evaljs[Double]("page.viewportSize.height") == 768.0 )
  }


  test("t1") {
    val p = PhantomExecutor(isDebug=true,conf = conf)

    assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

    assert(p.title == "t2")

    assert(p.title == "t2")

    assert(p.$("#d1").innerHTML == "D1")
    assert(p.$("#d1no").innerHTML == "")

    p.$("#d2").click()

    assert(p.$("#d1").innerHTML == "D1C")

    intercept[AutomationException] {
      p.$("#d1no").click()
    }

    assert(p.$("#d2").attr("class") == "t3 d2class")

    p.close()
  }

    test("length") {
      val p = PhantomExecutor(isDebug=false,conf = conf)

      assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

      val t = p.$("div.t3")
      assert( t.length == 2 )

    }

*/
  /*
          test("t3") {
            val p = PhantomExecutor(isDebug=false,conf = conf)

            assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

            p.$("div.t3").foreach { x =>
              val t = x.attr("id")
              assert( t == "d1" || t == "d2")
            }

            val divs = p.$("div.t3")
            assert(divs.length==2)
            var cnt = 0
            for ( el <- divs if el.attr("id")=="d1" ) {
              cnt+=1
            }
            assert(cnt==1)
          }
  */
            test("children $") {
              val p = PhantomExecutor(isDebug=false,conf = conf)

              assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

              val c0 = p.$("div.c0")
              assert(c0.$("div").length == 2)
              val c1= p.$("div.c1")
              assert(c1.$("div").length == 4)

              assert(p.$("#c3").$("span").length == 3)
              assert(p.$("#c3").children.length == 2)
            }

            test("children $.$") {
              val p = PhantomExecutor(isDebug=false,conf = conf)

              assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

              val c1= p.$("div.c1")
              val spans = c1.$("div").$("span")
              assert(spans.length == 3)

              val spanF =  spans.find{
                x => x.innerHTML == "span2"
              }.get

              val s2 = spans.head

              assert(s2.innerHTML == "span1")
            }

            test("getBoundingClientRect") {
              val p = PhantomExecutor(isDebug=false,conf = conf)

              assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

              val cr= p.$("#d2").getBoundingClientRect

              assert(cr == ClientRect(8.0,108.0,27.0,129.0,100.0,100.0) )

            }

              test("offsetParent")  {
                val p = PhantomExecutor(isDebug=false,conf = conf)

                assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

                val op= p.$("#d2").offsetParent
                assert(op.tagName == "BODY")
                // many offset parents
                val ops= p.$("div.t3").offsetParent
                assert(ops.length == 1)

                val opspn = p.$("span").offsetParent

                assert(opspn.length == 3 )
              }

              test("re selector")  {
                val p = PhantomExecutor(isDebug=false,conf = conf)

                assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

                val t = p.$("span").re("span[13]")
                assert(t.length == 2 )
              }

              test("11111") {
                val p: Page = PhantomExecutor(isDebug=false) // Create page
                val r: Future[Boolean] = p.open("/testdata/t2.html")

              }


              test("double open")  {
                val p = PhantomExecutor(isDebug=false,conf = conf)

                assert ( p.open(this.getClass.getResource("/testdata/t2.html").toString).futureValue )

                val t = p.$("span").re("span[13]")
                assert(t.length == 2 )

                assert ( p.open(this.getClass.getResource("/testdata/t3.html").toString).futureValue )

                val t2 = p.$("span").re("span[13]")
                assert(t2.length == 2 )
              }



              test("uploadfile") {
                val p = PhantomExecutor(isDebug=false,conf = conf)

                assert ( p.open(this.getClass.getResource("/testdata/t3.html").toString).futureValue )

                p.uploadFile( p.$("input[type=file]") , this.getClass.getResource("/testdata/t3.html").toString)

              }




}


class OnlineTests extends TestKit(ActorSystem()) with FunSuiteLike with ScalaFutures with ImplicitSender with BeforeAndAfterAll {
  override def beforeAll {
    PhantomExecutor.initEnvironment
  }
  implicit override val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(100, Millis))


  test("upload html5 style") {
    val p = PhantomExecutor(isDebug=true,PhantomConfig(mediumBin = "./bin/phantomjs"))

    p.open("http://www.dropzonejs.com/bootstrap.html").futureValue


    p.uploadFile(p.$(".btn.btn-success.fileinput-button.dz-clickable"),this.getClass.getResource("/testdata/t3.html").toString)


  }

}