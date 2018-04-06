package ms.webmaster.chromium.chromeexe

import `io.monix::monix:2.1.2`
import `io.netty:netty-all:4.1.21.Final`
import `../ms.webmaster/HttpUtils.scala`,                     ms.webmaster.httputils.{GETAsString, retry}
import `../ms.webmaster.macroserialization/json.macro.scala`, ms.webmaster.macroserialization.Json
import `ChromeDebugProtocol.scala`,                           ms.webmaster.chromium.chromedebugprotocol.ChromeDebugProtocol


// It controls Chrome process
class ChromeExe(extraFlags: Array[String])(implicit s: monix.execution.Scheduler) {
  private val group = new io.netty.channel.nio.NioEventLoopGroup
  private val port = 9999
  private val tmpdir = "/tmp/xxx"

  Runtime.getRuntime.exec(Array("rm", "-rf", tmpdir))

  private val p: java.lang.Process = Runtime.getRuntime.exec( Array(  "/run/current-system/sw/bin/chromium"
                                                                   , s"--remote-debugging-port=$port"
                                                                   , s"--user-data-dir=$tmpdir"
                                                                   ,  "--no-first-run"
                                                                   ,  "--no-default-browser-check"
                                                                   )
                                                           ++ extraFlags
                                                           ++ Array("about:blank")
                                                             )
  //println(s"p=${p.waitFor}")
  //Thread sleep 3000
  private case class InstanceInfo(description: String, devtoolsFrontendUrl: String, id: String, title: String, `type`: String, url: String, webSocketDebuggerUrl: String)
  private def instances: List[InstanceInfo] = {
    val json = retry(5, { case _:java.net.ConnectException        => Thread.sleep(1000); true
                          case _                                  =>                     false }) {
                 GETAsString(s"http://127.0.0.1:$port/json")
               }
    Json.unpackFromString[List[InstanceInfo]](json)
  }

  println(instances mkString "\n")
  private val Some(instance) = instances.find(_.url == "about:blank")
  println(s"instance=$instance")

  val cdp = new ChromeDebugProtocol(instance.webSocketDebuggerUrl, group.next, trace=false)

  def quit() {
    group.shutdownGracefully(/*10, 10, java.util.concurrent.TimeUnit.MILLISECONDS*/)

    //println(p.getPid)
    val pid = { val f = p.getClass.getDeclaredField("pid"); f.setAccessible(true); f.getLong(p) }
    println(s"kill pid=$pid")
    Runtime.getRuntime.exec(Array("pkill", "-P", s"$pid"))
    //Runtime.getRuntime.exec(Array("kill", s"$pid"))
    p.waitFor
    //p.destroyForcibly
  }
}
