package akka.gemini

import java.io._
import java.nio.channels.FileChannel
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.event.DiagnosticLoggingAdapter
import akka.event.Logging.MDC
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Awaitable, Await, Future, Promise}
import scala.reflect.ClassTag
import scala.sys.process._
import scala.util.{Failure, Success, Try}

import play.api.libs.json._
import play.api.libs.functional.syntax._

abstract class PhantomException extends Throwable
class NoSuchElementException extends PhantomException
class ManyElementsException extends PhantomException
class TimeoutException extends PhantomException
class AutomationException(msg:String) extends PhantomException {
  override def toString = s"AutomationException($msg)"
}


class PhantomIdCounter extends Actor {
  var idx = 0
  override def preStart() {

  }
  def receive = {
    case _ =>
      idx+=1
      sender ! idx
  }
}

class PhantomExecutionActor(_isDebug:Boolean,conf:PhantomConfig) extends Actor with akka.actor.DiagnosticActorLogging {
  import PhantomExecutionActor.Events._

  val mdc = Map("phantomId" -> {
    val parts =  self.path.elements.last.split("-")
    if ( parts.length > 1 ) parts(1) else 1
  }) ++ conf.extraMdcArgs

  override def mdc(currentMessage: Any): MDC = mdc

  var coreJsFile:Option[File] = None
  var cookiesFile:Option[File] = None

  private def inputToFile(is: java.io.InputStream, f: java.io.File) {
    val in = scala.io.Source.fromInputStream(is)
    val out = new java.io.PrintWriter(f)
    try { in.getLines().foreach(out.println(_)) }
    finally { out.close }
  }

  private def mkProxySeq(proxy:String) = {
    if (proxy == "") Seq()
    else if (proxy.startsWith("http://")) Seq("--proxy=" + proxy.substring(7),"--proxy-type=http")
    else if (proxy.startsWith("socks://")) Seq("--proxy=" + proxy.substring(8),"--proxy-type=socks5")
    else Seq("--proxy=" + proxy,"--proxy-type=http")
  }

  def writeFile(fname:String,content:String) {
    Some(new java.io.PrintWriter(fname)).foreach{p => p.write(content); p.close}
  }

  def getCookiesArgs(s: String) = {
    val tmpFile = File.createTempFile("cookies", ".js")
    val fName = tmpFile.getPath;
    tmpFile.deleteOnExit()
    cookiesFile = Some(tmpFile)

    writeFile(fName,s)

    Seq("--cookies-file="+fName)
  }

  val phantomCmd:Seq[String] = {
    val c = context.system.settings.config
    import scala.collection.JavaConverters._

    val tmpFile = File.createTempFile("core", ".js")
    tmpFile.deleteOnExit()
    coreJsFile = Some(tmpFile)

    inputToFile(this.getClass.getResourceAsStream("/core.js"),tmpFile)

    val args:Seq[String] = c.getStringList("phantom.args").asScala

    val phantomArgs: Seq[String] = args ++
      ( if ( conf.proxy != "") mkProxySeq(conf.proxy)  else Seq() ) ++
      getCookiesArgs(conf.startCookies)


    val appArgs:Seq[String] = Seq(
      tmpFile.getPath,
      "--viewportHeight="+conf.viewportHeight.toString,
      "--viewportWidth="+conf.viewportWidth.toString,
      "--userAgent="+conf.userAgent
    )

    Seq(if ( conf.phantomBin == null) c.getString("phantom.bin") else conf.phantomBin ) ++ phantomArgs ++ appArgs

  }

  import context.{become, dispatcher, system}
  var isDebug = _isDebug

  private val fatalPromise = Promise[Int]()
  private var outputStream:OutputStream = null
  private var evalId:Int = 0
  private var currentInEvaluation:Map[Int,ActorRef] = Map()
  private var gonaDie = false
  private var currentUrl = ""

  // var inOpen = false 

  override def preStart() {
    debug(s"preStart DebugMode=$isDebug")
  }

  override def postStop() {
    gonaDie = true
    //info(s"stopped url=$currentUrl")
    fatalPromise.trySuccess(1)

    coreJsFile.map(fname => fname.delete() )
    cookiesFile.map(fname => fname.delete() )
  }

  def instart(_sender:ActorRef):PartialFunction[Any, Unit] = {
    {
      // Inner
      case ev @ Failed(_) =>
        become(failed())
        _sender ! ev
      case ev @ Started() =>
        become(started())
        _sender ! ev
      case ev @ Finished() =>
        become(failed())
        _sender ! Failed(new Exception("phantom binary can't be started"))
    }
  }

  def failed():PartialFunction[Any, Unit] = {
    case _ => sender ! Failed(new Exception("stopped"))
  }


  def debug(message: => String) = if (isDebug) { log.mdc(mdc); log.debug(message);log.clearMDC() }

  def warning(message: => String) = if (isDebug) log.warning(message)
  def info(message: => String) = log.info(message)
  def error(message: => String) = log.error(message)

  def getCookies() = cookiesFile.fold("") {
    fName => scala.io.Source.fromFile(fName).mkString
  }

  def started():PartialFunction[Any, Unit] = {
    {
      // case Start() => error("Already started")
      case OpenUrl(url)  =>
        info(s"OpenUrl '$url'")
        become(waitUrl(sender()))
        sendPhantom("OPEN "+url)
        currentUrl = url
      case Eval(js) =>
        debug("RECV Eval()")
        evalId+=1
        currentInEvaluation += ( evalId -> sender )
        sendPhantom("EVAL " + evalId + " " + js)
      case AsyncEval(js) =>
        debug("RECV AsyncEval()")
        evalId+=1
        currentInEvaluation += ( evalId -> sender )
        sendPhantom("AVAL " + evalId + " " + js)
      case Stats() =>
        sender ! StatsResult(evalId)
      case SetDebug(newDebug) =>
        if ( newDebug != isDebug) {
          if (newDebug) {
            isDebug = newDebug
            debug("Debug mode on")
          } else {
            debug("Debug mode off")
            isDebug = newDebug
          }
        }
      // Inner
      case EvalComplete(_evlid,result) =>
        currentInEvaluation.get(_evlid) match {
          case Some(ret) => ret ! EvalResult(result)

            currentInEvaluation -= _evlid
          case None => error(s"EvalComplete Duplication: ${_evlid} $result")
        }
      case ev @ Failed(_) =>
        become(failed())
        for ( (id,_sender) <- currentInEvaluation) _sender ! ev

      //case ev : OpenUrlResult => 
      //    _sender ! ev
      case ev @ GetCookies() => sender ! CookiesResult(getCookies())

      case ev @ Finished() =>
        error(s"Finished from phantom received - seems that phantom unstable")
        become(failed())
        val fev = Failed(new Exception("phantom binary closed"))
        for ( (id,_sender) <- currentInEvaluation) _sender ! fev

      case x => error(s"Unknown action: $x")

    }
  }

  def waitUrl(_sender:ActorRef):PartialFunction[Any, Unit] = {
    case OpenUrl(url) => sender ! OpenUrlResult(Failure(new Exception("duplicate")))
    case ev : OpenUrlResult =>
      _sender ! ev
      become(started())
  }

  def receive = {
    case Start() =>
      debug("RECV Start()")
      become(instart(sender()))
      execAsync()

    case x => error(s"Unknown Signal $x")
  }

  def sendPhantom(cmd:String) {
    debug("sendPhantom " + cmd)
    //try {
    outputStream.write((cmd + "\n").getBytes)
    outputStream.flush()
    //} catch {
    //  case ex:java.io.IOException => fatalPromise.failure(ex)
    //}
  }

  def execAsync() = {

    val outputOpened = Promise[OutputStream]()
    val magicReceived = Promise[Boolean]()

    def processLine(s:String) {
      val cmd = if ( s.length >=4 ) s.substring(0,3) else "???"
      val arg = if ( s.length >=4 ) s.substring(4) else s
      cmd match {
        case "INF" =>
          arg match {
            case "STARTED" => magicReceived.success(true)
          }
        case "RET" =>
          val retParser = """(\d+):(.*)$""".r
          val retParser(retId,result) = arg
          debug(s"ret cmd: '$retId,$result'")
          self ! EvalComplete(retId.toInt,Success(result))
        case "ERT" =>
          val retParser = """(\d+):(.*)$""".r
          val retParser(retId,result) = arg
          val except = result match {
            case "NoSuchElementException" => new NoSuchElementException()
            case "ManyElementsException" => new ManyElementsException()
            case "TimeoutException" => new TimeoutException()
            case _ => new Exception(result)
          }
          self ! EvalComplete(retId.toInt,Failure(except))
        case "DBG" =>
          debug(s"debug cmd: '$s'")
        case "CLT" =>
          debug(s"debug client: '$s'")
        case "OPN" =>
          val openUrlRet = """(\d+):(\w+)""".r
          val openUrlRet(pageId,status) = arg
          debug(s"opn cmd: '$pageId,$status'")
          if ( status == "success") self ! OpenUrlResult(Success(true))
          else self ! OpenUrlResult(Failure(new Exception(s"openStatus=$status")))
        case "ERR" => error(s"error '$arg'")
        case _ => debug(s"Unknown phantom answer '$s'")
      }
    }

    val processor = new ProcessIO(
    {
      out =>
        outputOpened.success(out)
    },
    {
      in =>
        val reader = new BufferedReader(new InputStreamReader(in))
        def readFully() {
          val line = reader.readLine()
          if ( line != null ) {
            processLine(line)
            readFully()
          }
        }
        try {
          readFully()
        } catch {
          case ex:Throwable =>
            error(s"Read error. $ex. Close Phantom")
            self ! Finished()
        }

    },
    _ => ()
    )

    val processOp = try {
      debug("Start: " + phantomCmd.mkString(" "))
      Some(Process(phantomCmd).run(processor))
    } catch {
      case ex:Throwable =>
        error(s"Start phantom exception $ex")
        self ! Failed(ex)
        None
    }

    processOp match {
      case Some(process) =>
        val startedTimeout = system.scheduler.scheduleOnce(conf.startTimeout) {
          fatalPromise.failure(new Exception("start-timeout"))
        }
        val exec = Future {
          val ev = process.exitValue()
          debug(s"phantom cmd finished and returns $ev")
          ev
        }

        val timeout = system.scheduler.scheduleOnce(conf.execTimeout) {
          //process.destroy()
          val msg = s"Exec timeout=${conf.execTimeout}, url=$currentUrl"
          error(msg)
          fatalPromise.failure(new Exception(msg))
        }

        val startFuture = Future.sequence(List(outputOpened.future, magicReceived.future))

        startFuture.onSuccess {
          case List(out:OutputStream,true) =>
            outputStream=out
            startedTimeout.cancel()
            self ! Started()
        }

        val completeFuture = Future.firstCompletedOf(Seq(fatalPromise.future,exec))

        completeFuture.onComplete {
          case Success(v) =>
            process.destroy()
            timeout.cancel()
            if (! gonaDie) self ! Finished()
          case Failure(except:Throwable) =>
            process.destroy()
            timeout.cancel()
            if (! gonaDie) self ! Failed(except:Throwable)
        }

      case None =>
    }


  }

}

/**
 *
 * @param execTimeout
 * @param startTimeout
 * @param viewportHeight
 * @param viewportWidth
 * @param proxy
 * @param userAgent
 * @param phantomBin
 * @param startCookies
 * @param extraMdcArgs    - Map with extra parameters for logback logging. All params would be added to each logging record and can be used in logback.xml.
 *                        This can be used for some log splitting and packing each communication session to different log.
 */

case class PhantomConfig(execTimeout:FiniteDuration=120.seconds,startTimeout:FiniteDuration=1 seconds,viewportHeight:Int=768,viewportWidth:Int=1024,
  proxy:String="",userAgent:String="Mozilla/5.0 (Windows NT 6.0;) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36",
  phantomBin:String=null,
  startCookies:String = "",extraMdcArgs:Map[String,String]=Map())

object PhantomExecutionActor {
  def props(isDebug: Boolean = true, conf:PhantomConfig = PhantomConfig()): Props = Props(new PhantomExecutionActor(isDebug,conf))
  object Events {
    case class Start()
    case class Started()
    case class Finished()
    case class Stats()
    case class Failed(excp:Throwable)
    case class OpenUrl(url:String)
    case class Eval(js:String)
    case class AsyncEval(js:String)
    case class EvalComplete(evlid:Int,ret:Try[String])
    case class EvalResult(js:Try[String])
    case class OpenUrlResult(ret:Try[Boolean])
    case class StatsResult(evaluatedItems:Int)
    case class SetDebug(isDebug:Boolean)
    case class GetCookies()
    case class CookiesResult(cookies:String)
  }
}

case class ClientRect(left:Double,right:Double,top:Double,bottom:Double,height:Double,width:Double) {
  override def toString = s"ClientRect( ($left,$top) / $width x $height)"
}

class Page(val fetcher:ActorRef,val system:ActorSystem,val phantomId:Int) {
  // Implicit Execution context
  import system.dispatcher
  import PhantomExecutionActor.Events._
  import PhantomExecutor.{fastTimeout,quote,htmlquote}

  //implicit val tm = PhantomExecutor.askTimeout

  var opened  = true
  var started = false

  private def wrapException(x: => Unit) = try {
    x
  } catch {
    case e:Throwable => throw new AutomationException(s"failed: " + e.toString )
  }

  private def evaljs[T](js:String,timeout:Duration = fastTimeout)(implicit r:Reads[T]):T = {
    assert(opened)

    implicit val askTimeout = Timeout(timeout.toMillis*2,TimeUnit.MILLISECONDS)

    Await.result( (
      fetcher ? Eval(js)).mapTo[EvalResult].map {
      case EvalResult(hui) => Json.parse(hui.get).as[T](r)
    } , timeout)
  }

  lazy val title:String = evaljs[String]("""page.evaluate(function() {return document.title;});""")

  def evalJsClient(js:String) = wrapException {
    evaljs[Boolean](s"""page.evaluate(function(){"""+js.replaceAll("\n"," ")+""";return true;} );""")
  }

  def switchToChildFrame(frameNum:Int) = wrapException {
    evaljs[Boolean](s"""page.switchToChildFrame($frameNum);true;""")
  }

  def switchToParentFrame() = wrapException {
    evaljs[Boolean](s"""page.switchToParentFrame();true;""")
  }

  def stop() = wrapException {
    evaljs[Boolean](s"""page.stop();true;""")
  }


  def stats = {
    assert(opened)
    implicit val askTimeout:akka.util.Timeout = fastTimeout * 2
    Await.result( (fetcher ? Stats() ).mapTo[StatsResult] , fastTimeout)
  }

  def cookies = {
    assert(opened)
    implicit val askTimeout:akka.util.Timeout = fastTimeout * 2
    Await.result( (fetcher ? GetCookies() ).mapTo[CookiesResult].map(_.cookies) , fastTimeout)
  }

  def render(_fname:String) = {
    val fname = if ( _fname.endsWith(".png") ) _fname.substring(0,_fname.length-4) else _fname
    try {
      evaljs[Boolean](s"page.render('$fname.png');fs.write('$fname.html',page.content,'w');true;",5 seconds)
      //Logger("PhantomExecutor").info(s"Render page to $fname - OK")
    } catch {
      case ex:Throwable => //Logger("PhantomExecutor").warn(s"Render page to $fname - failed Exception($ex")
    }
  }

  def open(url:String,openTimeout:FiniteDuration = 10.seconds): Future[Boolean] = {
    assert(opened)

    def _open = {
      fetcher.ask(OpenUrl(url))(openTimeout).recover{
        case ex:AskTimeoutException => OpenUrlResult(Failure(ex))
      }.map {
        case OpenUrlResult(Success(_)) => true
        case OpenUrlResult(Failure(ex)) => println(s"Open - failed $ex");throw new AutomationException(s"Open url:'$url' - failed $ex")
      }
    }

    if ( ! started )
    fetcher.ask(Start())(openTimeout).flatMap {
      case Started() =>
        started = true
        _open
      case Failed(ex) =>
        throw new AutomationException(s"Open - failed $ex")
    } else
      _open

  }

  def uploadFile(inputType:Selector,filename:String) = {
    val res = try {

      evaljs[Boolean](s"""page.uploadOk=false;page.onFilePicker2 = page.onFilePicker;page.onFilePicker=function(){page.uploadOk=true;return '$filename';};true;""")

      inputType.click()

      evaljs[Boolean](s"""page.onFilePicker = page.onFilePicker2;page.uploadOk;""")

    } catch {
      case e:Throwable => throw new AutomationException(s"failed: " + e.toString )
    }

    if ( ! res ) throw new AutomationException(s"uploadFile - failed. res=$res")

  }

  def typeValue(input:Selector,v:String) {
    input.click()
    input.value = v
  }

  def selectDatepickerDate(date:org.joda.time.DateTime,
                           dateFormat: String,
                           button: Option[Selector],
                           datepicker: Selector,
                           monthHeader:Selector,
                           nextEl: Selector,prevEl:Selector,dateEl:Selector
                            ) {

    button.map { el =>
      el.click()
      Thread.sleep(500)
    }


    if (! datepicker.isVisible ) throw new AutomationException("Datepicker not visible")

    val datepickerFormatter = new java.text.SimpleDateFormat(dateFormat)


    def slideToDate(year:Int,month:Int,day:Int,maxIterations:Int=30) {
      if ( maxIterations < 0 ) throw new AutomationException("cant slide to date")

      val curDateMonTxt = monthHeader.innerText
      val curJoda = new org.joda.time.DateTime(datepickerFormatter.parse(curDateMonTxt))

      val curMonth = curJoda.getMonthOfYear
      val curYear = curJoda.getYearOfEra

      if ( year * 12 + month > curYear * 12 + curMonth ) {
        nextEl.click()
        slideToDate(year,month,day,maxIterations-1)
      } else if ( year * 12 + month < curYear * 12 + curMonth ) {
        prevEl.click()
        slideToDate(year,month,day,maxIterations-1)
      } else {
        dateEl.re("^\\\\W*"+day.toString+"\\\\W*$").click()
      }

    }

    slideToDate(date.getYearOfEra,date.getMonthOfYear,date.getDayOfMonth)
  }

  def selectJSSelect(button:Selector,_targetEl: => Selector  , waitOpening:Boolean = false) {
    button.click()
    def invalidate(el : => Selector,cnt:Int,sleep:Duration):Selector = {
      try {
        val ret = el
        ret
      } catch {
        case x:Throwable => if (cnt == 0) throw x else {
          Thread.sleep(sleep.toMillis)
          invalidate(el,cnt-1,sleep)
        }
      }
    }
    val targetEl = if( waitOpening ) invalidate(_targetEl,4,500 milliseconds) else _targetEl

    val ob = targetEl.parentNode.getBoundingClientRect

    val tb = targetEl.getBoundingClientRect

    if ( tb.height == 0 || tb.width == 0 ) throw new AutomationException("Scrolling failed - targetEl is invisible")

    if ( tb.top >= ob.top &&  tb.bottom <= ob.bottom ) {
      targetEl.click()
    } else {
      val sof = tb.bottom - ob.top - ob.height

      targetEl.parentNode.scrollTop += sof

      val newtb = targetEl.getBoundingClientRect
      if ( newtb.top >= ob.top &&  newtb.bottom <= ob.bottom ) {

        targetEl.click()
      } else {
        throw new AutomationException("Scrolling failed")
      }
    }
  }

  def close() = {
    assert(opened)
    opened = false
    fetcher ! akka.actor.PoisonPill
  }

  def selectRadio(radioEls:Selector,value:String) = {
    var found = false
    for (r <- radioEls) {
      if (r.value == value) {
        r.checked = "checked"
        found = true
      } else {
        r.checked = ""
      }
    }
    if (! found ) throw new AutomationException("Radio selection failed")

  }

  def selectOption(el:Selector,value:String,force:Boolean=false) {
    el.value = value
    if ( el.value != value ) {
      if ( ! force ) throw new AutomationException("selectOption failed")
      val qv = quote(value)
      val hqv = htmlquote(value)
      el.appendChild(s"<option value='$qv'>$hqv</option>")
      el.value = value
    }
  }

  def setDebug(isDebug:Boolean) = fetcher ! SetDebug(isDebug)

  private def _waitForSelector(el:Selector,selectorFunction:String,timeout:Duration) = {
    assert(opened)
    val tms = Math.max(timeout.toMillis,100)

    implicit val askTimeout = Timeout(timeout.toMillis*2,TimeUnit.MILLISECONDS)

    val js = """
        var timeout = """+tms+""";
        var ic = timeout / 100;
        var selector = function() {
          return page.evaluate(function(){
              if (!window._query)console.log("$$INJECT");
                              """+selectorFunction+"""
          });
        };
        var depressed = false;
        var itvl = setInterval(function() {
          if (depressed) return;
          if (selector()) {
            clearInterval(itvl);
            depressed = true;
            ret(eid,true);
          }
          ic--;
          if ( ic <= 0) {
            clearInterval(itvl);
            depressed = true;
            errret(eid,"TimeoutException");
          }
        },100);"""
    (fetcher ? AsyncEval(js.replaceAll("[\r\n]",""))).map {
      case EvalResult(Failure(ex)) => throw new AutomationException(s"waitForSelection $ex")
      case EvalResult(hui) => Json.parse(hui.get).as[Boolean]
      case Failed(ex:Throwable) => throw new AutomationException(s"waitForSelection $ex")
    }
  }

  def waitForSelector(el:Selector,timeout:Duration=1000.milliseconds): Future[Boolean] = {
    val selectorFunction = "return " + ( el match {
      case els:ListedSelector =>
        els.items.map {
          el => s"_query(${el.selector}).length >= 1"
        }.mkString(if ( els.conjType == ConjunctionType.AND) " && " else " || ");
      case _ =>
        s"_query(${el.selector}).length >= 1"
    })
    _waitForSelector(el,selectorFunction,timeout)
  }

  def waitForSelectorAttr(el:Selector,attr:String,v:String,timeout:Duration=1000.milliseconds) = {
    val selectorFunction = el match {
      case els:ListedSelector =>
        "return " + els.items.map {
          el => s"_query(${el.selector}).length > 0 && (_query(${el.selector})[0].getAttribute('$attr') || '' ) == '${quote(v)}' "
        }.mkString(if ( els.conjType == ConjunctionType.AND) " && " else " || ");
      case _ =>
        s"return  _query(${el.selector}).length > 0 && (_query(${el.selector})[0].getAttribute('$attr') || '' ) == '${quote(v)}' "
    }
    _waitForSelector(el,selectorFunction,timeout)
  }

  def waitUntilVisible(el:Selector,timeout:Duration=1000.milliseconds) = {
    val selectorFunction = el match {
      case els:ListedSelector =>
        "return " + els.items.map {
          el => s"_query(${el.selector}).length > 0 && (! _query(${el.selector})[0].offsetParent ) "
        }.mkString(if ( els.conjType == ConjunctionType.AND) " && " else " || ");
      case _ =>
        s"return  _query(${el.selector}).length > 0 && (! _query(${el.selector})[0].offsetParent ) "
    }
    _waitForSelector(el,selectorFunction,timeout)
  }


  def $(cssSelectors:String*) = if (cssSelectors.length == 1 ) Selector(this,cssSelectors.head)
  else Selector(this,cssSelectors.map {css => Selector(this,css) },ConjunctionType.AND)

}

abstract class Selector(page:Page) extends Traversable[Selector] {
  private val fetcher = page.fetcher
  import page.system.dispatcher
  import PhantomExecutionActor.Events._
  import PhantomExecutor.{fastTimeout,quote/*,htmlquote*/}
  implicit val tm = PhantomExecutor.askTimeout

  override def toString() = "$("+selector+")"
  //extends scala.collection.TraversableLike[Selector, Selector]  {
  def selector:String

  def setDebug(isDebug:Boolean) = fetcher ! SetDebug(isDebug)

  private def evfun(js:String): String = "page.evaluate(" +
    "function(selector) {" +
    """if (!window._query)console.log("$$INJECT");var R;var d = _query(selector);""" +
    js.replaceAll("[\r\n]","") + "return R; } " + ","+selector+");"

  def innerHTML:String = try Await.result( (
    fetcher ? Eval(
      evfun("""R="";
            for (var i=0;i<d.length;i++) {R+=d[i].innerHTML;}
            """)
    )).mapTo[EvalResult].map {
    case EvalResult(hui) => Json.parse(hui.get).as[String]
  } , fastTimeout)
  catch {
    case e:Throwable => throw new AutomationException(s"innerHTML $selector failed: $e" )
  }

  def outerHTML:String = try Await.result( (
    fetcher ? Eval(
      evfun("""R="";
            for (var i=0;i<d.length;i++) {R+=d[i].outerHTML;}
            """)
    )).mapTo[EvalResult].map {
    case EvalResult(hui) => Json.parse(hui.get).as[String]
  } , fastTimeout)
  catch {
    case e:Throwable => throw new AutomationException(s"outerHTML $selector failed: $e" )
  }

  def _cleanup(s:String) = s.replace("\u00a0"," ")

  def innerText:String = try Await.result( (
    fetcher ? Eval(
      evfun("""R="";
            for (var i=0;i<d.length;i++) {R+=d[i].innerText;}
            """)
    )).mapTo[EvalResult].map {
    case EvalResult(hui) => _cleanup(Json.parse(hui.get).as[String])
  } , fastTimeout)
  catch {
    case e:Throwable => throw new AutomationException(s"innerText $selector failed: $e" )
  }


  def length:Int = try Await.result( (
    fetcher ? Eval(
      evfun("""R=d.length;""")
    )).mapTo[EvalResult].map {
    case EvalResult(hui) => Json.parse(hui.get).as[Int]
  } , fastTimeout )
  catch {
    case e:Throwable => throw new AutomationException(s"singleValJs $selector failed: $e" )
  }

  def exists() = length > 0
  def at(idx:Int) = Selector(page,this,idx)

  def apply(idx:Int) = at(idx)

  private def singleValJs[T](js:String)(implicit r:Reads[T]) = {
    try Await.result( ( fetcher ? Eval(
      """var retval = """ + evfun("""R=null;
            if ( d.length == 0 ) {
              console.log("ERROR:singleValJs:"+JSON.stringify(selector)+" element not found");
              R = -1;
            } else if ( d.length > 1 ) {
              console.log("ERROR:singleValJs:"+JSON.stringify(selector)+" many elements");
              R = -2;
            } else {
              R = { val:"""+js+""" };
            }
                               """) +
        """
          if ( retval == -1 ) {
            throw("NoSuchElementException");
          } else if ( retval == -2 ) {
            throw("ManyElementsException");
          } else if ( retval ) {
            retval.val;
          } else {
            throw("attr failed");
          }

        """.replaceAll("[\r\n]","")

    )).mapTo[EvalResult].map {
      case EvalResult(hui) => Json.parse(hui.get).as[T](r)
    } , fastTimeout)
    catch {
      case e:Throwable => throw new AutomationException(s"singleValJs $selector failed: $e" )
    }
  }

  private def singleValJsSet(js:String) = {
    try Await.result( ( fetcher ? Eval(
      """var retval = """ + evfun("""R=null;
          if ( d.length == 0 ) {
            console.log("ERROR:click:"+JSON.stringify(selector)+" element not found");
            R = -1;
          } else if ( d.length > 1 ) {
            console.log("ERROR:click:"+JSON.stringify(selector)+" many elements");
            R = -2;
          } else {
                                  """+js+""";
            R = 1;
          }
                                         """) +
        """
        if ( retval == -1 ) {
          throw("NoSuchElementException");
        } else if ( retval == -2 ) {
          throw("ManyElementsException");
        } else if ( retval == 1 ) {
          true;
        } else {
          throw("attr failed");
        }

        """.replaceAll("[\r\n]","")

    )).mapTo[EvalResult].map {
      case EvalResult(hui) => Json.parse(hui.get).as[Boolean]
    } , fastTimeout)
    catch {
      case e:Throwable => throw new AutomationException(s"singleValJsSet $selector failed: $e" )
    }
  }

  def attr(_attr:String):String = singleValJs[String]("d[0].getAttribute('"+_attr+"')")
  def attr(_attr:String,v:String) = singleValJsSet("d[0].setAttribute('"+_attr+"','"+quote(v)+"')")

  def removeAttribute(_attr:String) = singleValJsSet("d[0].removeAttribute('"+_attr+"')")

  def appendChild(html:String) = singleValJsSet(
    """
      var div = document.createElement('div');
      div.innerHTML = '"""+quote(html)+"""';
      var elements = div.childNodes;
      for ( var i = 0 ; i < elements.length;i++) {
        d[0].appendChild(elements[i]);
      }
                                       """
  )

  def submit() = singleValJsSet("""d[0].submit()""")

  implicit val clReads = (
    (__ \ "left").read[Double] and
      (__ \ "right").read[Double] and
      (__ \ "top").read[Double] and
      (__ \ "bottom").read[Double] and
      (__ \ "height").read[Double] and
      (__ \ "width").read[Double]
    )(ClientRect)

  def getBoundingClientRect:ClientRect = singleValJs[ClientRect]("d[0].getBoundingClientRect()")

  def isVisible:Boolean = {
    val br = getBoundingClientRect

    ! ( br.width == 0 || br.height == 0)
  }

  def isChecked:Boolean = singleValJs[Boolean]("d[0].checked")
  def isDisabled:Boolean = singleValJs[Boolean]("d[0].disabled")


  def tagName:String = singleValJs[String]("d[0].tagName")
  def id:String = singleValJs[String]("d[0].id")

  def value:String = singleValJs[String]("d[0].value")
  def value_=(v:String) = wrapException {
    singleValJsSet("d[0].value='"+quote(v)+"'")
  }

  private def wrapException[T](x: => T):T = try {
    x
  } catch {
    case e:Throwable => throw new AutomationException(s"$selector failed: " + e.toString )
  }


  def scrollTop:Double = singleValJs[Double]("d[0].scrollTop")
  def scrollTop_=(v:Double) = singleValJsSet("d[0].scrollTop="+v)

  def checked:String = singleValJs[String]("d[0].checked")
  def checked_=(v:String) = singleValJsSet("d[0].checked='"+quote(v)+"'")

  def className:String = singleValJs[String]("d[0].className")
  def hasClass(selector:String):Boolean = (" " + className + " " ).replaceAll("[\n\t]"," ").indexOf(" " + selector + " ") != -1

  def click(timeout:Duration = fastTimeout) = {
    def clickJsFunction(js:String) = "page.evaluate(" +
      "function(selector,h,w) {" +
      """if (!window._query)console.log("$$INJECT");var R;var d = _query(selector);""" +
      js.replaceAll("[\r\n]","") + "return R; } " + ","+selector+",page.viewportSize.height,page.viewportSize.width);"

    wrapException {
      Await.result((
        fetcher ? Eval(
          """var rect = """ + clickJsFunction( """R=null;
            if ( d.length == 0 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" element not found");
              R = -1;
            } else if ( d.length > 1 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" many elements");
              R = -2;
            } else {""" + __calcBoundingRect +
            """
              var p = R.top + R.height / 2;
              if ( p > h ) {
                window.document.body.scrollTop = R.top;
                R = {
                  top: R.top - window.document.body.scrollTop,
                  left: R.left,
                  width: R.width,
                  height: R.height,
                  rollback: true
                };
              }
            }
                                                """) + """
          if ( rect == -1 ) {
            throw("NoSuchElementException");
          } else if ( rect == -2 ) {
            throw("ManyElementsException");
          } else if ( rect == -3 ) {
            throw("OutOfScreen");
          } else if ( rect ) {
            debug("click " + (rect.left + rect.width / 2) + "   "  + (rect.top + rect.height / 2) );
            page.sendEvent('click', rect.left + rect.width / 2, rect.top + rect.height / 2  );
            if ( rect.rollback ) {page.evaluate(function() {window.document.body.scrollTop = 0;});}
            true;
          } else {
            throw("click failed");
          }

            """.replaceAll("[\r\n]","")

        )).mapTo[EvalResult].map {
        case EvalResult(hui) => Json.parse(hui.get).as[Boolean]
      } , timeout)
    }
  }

  def move() = {
    def movefun(js:String) = "page.evaluate(" +
      "function(selector,h,w) {" +
      """if (!window._query)console.log("$$INJECT");var R;var d = _query(selector);""" +
      js.replaceAll("[\r\n]","") + "return R; } " + ","+selector+",page.viewportSize.height,page.viewportSize.width);"

    wrapException {
      Await.result( (
        fetcher ? Eval(
          """var rect = """ + movefun("""R=null;
            if ( d.length == 0 ) {
              console.log("ERROR:move:"+JSON.stringify(selector)+" element not found");
              R = -1;
            } else if ( d.length > 1 ) {
              console.log("ERROR:move:"+JSON.stringify(selector)+" many elements");
              R = -2;
            } else { """ + __calcBoundingRect + """
              var p = R.top + R.height / 2;
              if ( p > h ) {
                window.document.body.scrollTop = R.top;
                R = {
                  top: R.top - window.document.body.scrollTop,
                  left: R.left,
                  width: R.width,
                  height: R.height,
                  rollback: true
                };
              }
            }
                                      """) +
            """
          if ( rect == -1 ) {
            throw("NoSuchElementException");
          } else if ( rect == -2 ) {
            throw("ManyElementsException");
          } else if ( rect == -3 ) {
            throw("OutOfScreen");
          } else if ( rect ) {
            page.sendEvent('mousemove', rect.left + rect.width / 2, rect.top + rect.height / 2);
            if ( rect.rollback ) {page.evaluate(function() {window.document.body.scrollTop = 0;});}
            true;
          } else {
            throw("move failed");
          }

            """.replaceAll("[\r\n]","")

        )).mapTo[EvalResult].map {
        case EvalResult(hui) => Json.parse(hui.get).as[Boolean]
      } , fastTimeout)
    }
  }

  val __calcBoundingRect ="""R = d[0].getBoundingClientRect();
              R = {'top':R.top,'left':R.left,'height':R.height,'width':R.width};
              console.log(1,R.top,R.left);
              var fe = d[0].ownerDocument.defaultView.frameElement;
              while ( fe ) {
                var R0 = fe.getBoundingClientRect();
                console.log(2,R0.top,R0.left);
                R.top += R0.top;
                R.left += R0.left + 5;
                fe = fe.ownerDocument.defaultView.frameElement;
              }"""

  def highlight() = wrapException {
    Await.result( (
      fetcher ? Eval(
        """var rect = """ + evfun("""R=null;
            if ( d.length == 0 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" element not found");
              R = -1;
            } else if ( d.length > 1 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" many elements");
              R = -2;
            } else {""" + __calcBoundingRect + """
            }
                                               """) +
          """
          if ( rect == -1 ) {
            throw("NoSuchElementException");
          } else if ( rect == -2 ) {
            throw("ManyElementsException");
          } else if ( rect ) {
            page.evaluate(function(rect) {
              var v = document.createElement("div");
              v.style.position = "absolute";


              v.style.top = rect.top + "px";
              v.style.height = rect.height +"px";
              v.style.left = rect.left + "px";
              v.style.width = rect.width +"px";
              v.style['z-index'] = 10000;

              v.style['background-color'] = "rgba(0,255,0,0.5)";
              document.body.appendChild(v);
            },rect);
            true;
          } else {
            throw("click failed");
          }

          """.replaceAll("[\r\n]","")

      )).mapTo[EvalResult].map {
      case EvalResult(hui) => Json.parse(hui.get).as[Boolean]
    } , fastTimeout)
  }

  def render(_fname:String) = {
    val fname = if ( _fname.endsWith(".png") ) _fname.substring(0,_fname.length-4) else _fname
    val _renderCode = s"var pcr = this.page.clipRect;this.page.clipRect=rect;page.render('$fname.png');this.page.clipRect=pcr;"
    wrapException {
      Await.result( (
        fetcher ? Eval(
          ("""var rect = """ + evfun("""R=null;
            if ( d.length == 0 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" element not found");
              R = -1;
            } else if ( d.length > 1 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" many elements");
              R = -2;
            } else {""" + __calcBoundingRect + """
            }
                                               """) +
            """
          if ( rect == -1 ) {
            throw("NoSuchElementException");
          } else if ( rect == -2 ) {
            throw("ManyElementsException");
          } else if ( rect ) {
            """ + _renderCode + """
            true;
          } else {
            throw("click failed");
          }
            """).replaceAll("[\r\n]","")

        )).mapTo[EvalResult].map {
        case EvalResult(hui) => Json.parse(hui.get).as[Boolean]
      } , fastTimeout)
    }
  }

  def renderBase64() = {
    val _renderCode = s"var pcr = this.page.clipRect;this.page.clipRect=rect;var b64 = page.renderBase64('PNG');this.page.clipRect=pcr;b64;"
    wrapException[String] {
      Await.result( (
        fetcher ? Eval(
          ("""var rect = """ + evfun("""R=null;
            if ( d.length == 0 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" element not found");
              R = -1;
            } else if ( d.length > 1 ) {
              console.log("ERROR:click:"+JSON.stringify(selector)+" many elements");
              R = -2;
            } else {""" + __calcBoundingRect + """
            }
                                               """) +
            """
          if ( rect == -1 ) {
            throw("NoSuchElementException");
          } else if ( rect == -2 ) {
            throw("ManyElementsException");
          } else if ( rect ) {
            """ + _renderCode + """
          } else {
            throw("click failed");
          }
                                """).replaceAll("[\r\n]","")

        )).mapTo[EvalResult].map {
        case EvalResult(hui) => Json.parse(hui.get).as[String]
      } , fastTimeout)
    }
  }



  //def seq:Selector = this

  /*def filter(p: Selector => Boolean) = {

  }*/
  def foreach[U](f: Selector => U): Unit = {
    val l = length
    var i = 0
    while (i < l) {
      f(at(i))
      i+=1
    }
  }

  def indexWhere(p: Selector => Boolean, from: Int): Int = {
    var i = from
    var these = this drop from
    while (these.nonEmpty) {
      if (p(these.head))
        return i

      i += 1
      these = these.tail
    }
    -1
  }

  def $(selector:String) = new NestedSelector(page,this,selector)
  def root(selector:String) = Selector(page,selector)
  def children = new ChildSelector(page,this)
  def offsetParent = new OffsetParentSelector(page,this)
  def parentNode = new ParentSelector(page,this)
  def nextSibling = new NextSiblingSelector(page,this)
  def previousSibling = new PreviousSiblingSelector(page,this)
  def re(_re:String) = new RegexpSelector(page,this,_re)

  def getOrElse(default: => Selector): Selector = if ( exists() ) this else default

}

class CssSelector(page:Page,css:String) extends Selector(page) {
  def selector = "'" + css.replace("\\","\\\\").replace("'","\\'") + "'"
}

class NestedSelector(page:Page,parent:Selector,css:String) extends Selector(page) {
  def selector = "[" + parent.selector + ",'>>','"+css  +"']"
}

class RegexpSelector(page:Page,parent:Selector,re:String) extends Selector(page) {
  def selector = "[" + parent.selector + ",'re','"+re.replace("\\","\\\\").replace("'","\\'")  +"']"
}

class ChildSelector(page:Page,parent:Selector) extends Selector(page) {
  def selector = "[" + parent.selector + ",'>']"
}

class OffsetParentSelector(page:Page,parent:Selector) extends Selector(page) {
  def selector = "[" + parent.selector + ",'op']"
}

class ParentSelector(page:Page,parent:Selector) extends Selector(page) {
  def selector = "[" + parent.selector + ",'up']"
}

class NextSiblingSelector(page:Page,parent:Selector) extends Selector(page) {
  def selector = "[" + parent.selector + ",'ns']"
}

class PreviousSiblingSelector(page:Page,parent:Selector) extends Selector(page) {
  def selector = "[" + parent.selector + ",'ps']"
}

class IdxSelector(page:Page,parent:Selector,idx:Int) extends Selector(page) {
  def selector = "[" + parent.selector + ",'=',"+idx  +"]"
  override def foreach[U](f: Selector => U): Unit = {
    f(this)
  }
}

object ConjunctionType extends Enumeration{
    val AND,OR = Value
}

class ListedSelector(page:Page,val items:Seq[Selector],val conjType:ConjunctionType.Value) extends Selector(page) {
  /* Non implemented selector Item - that this selector can't be sent to JS */
  def selector = throw new Exception("Not implemented - that this selector can't be sent to JS")

  override def foreach[U](f: Selector => U): Unit = {
    for (x <- items) f(x)
  }
}

object Selector {
  def apply(page:Page,css:String) = new CssSelector(page,css)
  def apply(page:Page,parent:Selector,idx:Int) = new IdxSelector(page,parent,idx)
  def apply(page:Page,items:Seq[Selector],conjType:ConjunctionType.Value) = new ListedSelector(page,items,conjType)
}


object PhantomExecutor {
  private var environmentReady = false

  val fastTimeout = 500.milliseconds
  implicit val askTimeout = Timeout(60.seconds)
  def quote(q:String) = q.replace("'","\\'")
  def htmlquote(q:String) = q.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")

  def apply(isDebug:Boolean=true,conf:PhantomConfig=PhantomConfig())(implicit system:ActorSystem): Page = {
    assert(environmentReady)
    val idd = nextPhantomId
    val fetcher = system.actorOf(PhantomExecutionActor.props(isDebug=isDebug,conf),name=s"PhantomExecutor-$idd")
    new Page(fetcher,system,idd)
  }


  private def fastAwait[T](awaitable: Awaitable[T]): T = try {
    Await.result(awaitable,fastTimeout)
  } catch {
    case x:java.util.concurrent.TimeoutException => throw new java.util.concurrent.TimeoutException(s"timeout $fastTimeout")
    case x:Throwable => throw x
  }

  private def nextPhantomId(implicit system:ActorSystem) = {
    val phantomIdCounter = selectActor[PhantomIdCounter](system,"PhantomIdCounter")
    fastAwait( (phantomIdCounter  ? 1 ).mapTo[Int] )
  }

  def selectActor[T <: Actor  : ClassTag](system:ActorSystem,name:String) = {
    val timeout = Timeout(0.1 seconds)
    val myFutureStuff = system.actorSelection("akka://"+system.name+"/user/"+name)
    val aid:ActorIdentity = Await.result(myFutureStuff.ask(Identify(1))(timeout).mapTo[ActorIdentity],
      0.1 seconds)

    aid.ref match {
      case Some(cacher) =>
        cacher
      case None =>
        system.actorOf(Props[T],name)
    }
  }

  def initEnvironment(implicit system:ActorSystem) {

    val phantomIdCounter = selectActor[PhantomIdCounter](system,"PhantomIdCounter")

    environmentReady = true
  }
}