#!
use File::Basename qw(dirname basename);
require(dirname(__FILE__).'/../launcher/Launcher.pl.scala');

run(CLASS    => 'ms.webmaster.httputils',
    TEE      => "httputils.txt",
  # JAVAARGS => [ '-Djavax.net.debug=all' ],
   );
=cut
!#

package ms.webmaster


import java.io.{Console => _, _}
import java.net.{InetSocketAddress, URL, Proxy, CookieManager}
import java.util.zip.GZIPInputStream
//import scala.Console.{out => _, err => _, _}
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import `commons-io:2.4`,                            org.apache.commons.io.IOUtils
import `commons-codec:1.6`,                         org.apache.commons.codec.binary.Base64
import `org.apache.commons:commons-compress:1.4.1`, org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import `NormUrl.scala`,                             ms.webmaster.url.NormUrl // only for NormUrl.mkQuery


object httputils {

  // escape unprintable and unicode characters with %xx, printables get untouched
  private[this] def escapeUnprintableAndUnicode(str: String) = {
    val sb = new StringBuilder
    for (b <- str.getBytes("UTF-8")) {
      if (0x20<b && b<0x7F)
        sb append b.toChar
      else
        sb append "%%%02X".format(b)
    }
    sb.toString
  }


  // ssl failed even before `HostnameVerifier` is called, tc: https://thehackerblog.com/feed/index.html and https://opentrackers.org/feed/
//javax.net.ssl.HttpsURLConnection.setDefaultHostnameVerifier(new javax.net.ssl.HostnameVerifier {
//                                                              def verify(hostname: String, session: javax.net.ssl.SSLSession) = true
//                                                            })

  /**
   * Trust every server - dont check for any certificate
   */
  private[this] def trustAllHosts() {
    import java.security.cert.X509Certificate
    // Create a trust manager that does not validate certificate chains
    val trustAllCerts = Array[javax.net.ssl.TrustManager](new javax.net.ssl.X509TrustManager {
        def getAcceptedIssuers = Array[X509Certificate]()
        def checkClientTrusted(chain: Array[X509Certificate], authType: String) {}
        def checkServerTrusted(chain: Array[X509Certificate], authType: String) {}
    })

    // Install the all-trusting trust manager
    try {
      val sc = javax.net.ssl.SSLContext.getInstance("TLS");
      sc.init(null, trustAllCerts, new java.security.SecureRandom())
      javax.net.ssl.HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }
  trustAllHosts()

  // sometimes it does not work, even if set before each GET/POST
  // http://stackoverflow.com/questions/7648872/can-i-override-the-host-header-where-using-javas-httpurlconnection-class
  System.setProperty("sun.net.http.allowRestrictedHeaders", "true")

  // IOUtils.toByteArray with socks option and support for unicode urls
  def GETAsByteArray(url:             String,
                     socks:         =>Option[String]                = None, // each retry can use own sock
                     headers:         Traversable[(String, String)] = Map.empty,
                     cookie:          Option[CookieManager]         = None,
                     followRedirects: Boolean                       = true,
                     timeout:       =>Duration                      = 5.seconds,
                     method:          String                        = "GET" /* "GET" | "HEAD" | "DELETE" */ ,
                     retries:         Int                           = 1) =
    retry(retries, { case _:java.net.SocketException         => true
                     case _:java.net.SocketTimeoutException  => true
                     case _:java.net.ConnectException        => true
                     case _                                  => false }) {
      IOUtils.toByteArray(GET(url, socks, headers, cookie, followRedirects, timeout, method).getInputStream)
    }

  def GETAsString   (url:             String,
                     socks:         =>Option[String]                = None, // each retry can use own sock. todo: ADT from PhantomClient
                     headers:         Traversable[(String, String)] = Map.empty,
                     cookie:          Option[CookieManager]         = None,
                     followRedirects: Boolean                       = true,
                     timeout:       =>Duration                      = 5.seconds,
                     method:          String                        = "GET" /* "GET" | "HEAD" | "DELETE" */ ,
                     retries:         Int                           = 1) =
    retry(retries, { case _:java.net.SocketException         => true
                     case _:java.net.SocketTimeoutException  => true
                     case _:java.net.ConnectException        => true
                     case _                                  => false }) {
      IOUtils.toString(guessUnpackStream(GET(url, socks, headers, cookie, followRedirects, timeout, method).getInputStream), "UTF-8")
    }


  private[this] val reHostPort = "([^:]+):([0-9]+)".r
  // FileUtils.copyURLToFile with socks option and support for unicode urls
  // todo: followRedirects to https://
  // todo: use cookie for all followRedirects requests
  def GET(url:              String,
          //todo: bind:             Option[InetAddress]       = None,
          socks:            Option[String]                = None,
          headers:          Traversable[(String, String)] = Map.empty, // todo: Map[String, List[String]]
          cookie:           Option[CookieManager]         = None,
          followRedirects:  Boolean                       = true,
          timeout:          Duration                      = 5.seconds,
          method:           String                        = "GET" /* "GET" | "HEAD" | "DELETE" */ ): java.net.URLConnection = {
    require(System.getProperty("sun.net.http.allowRestrictedHeaders") == "true")

    val u = new URL(escapeUnprintableAndUnicode(url))
    val conn = socks match {
      case None                   => u.openConnection()
      case Some(reHostPort(h, p)) => u.openConnection(new Proxy(Proxy.Type.SOCKS, new InetSocketAddress(h, p.toInt)))
    }

    conn setConnectTimeout  timeout.toMillis.toInt
    conn setReadTimeout     timeout.toMillis.toInt

    if (u.getProtocol startsWith "http") {
      conn.asInstanceOf[java.net.HttpURLConnection] setInstanceFollowRedirects followRedirects

      if (headers exists (_._1 equalsIgnoreCase "Host")) // `System.setProperty("sun.net.http.allowRestrictedHeaders", "true")` in httputils.$init is not enough sometimes, why?
        System.setProperty("sun.net.http.allowRestrictedHeaders", "true")
      conn setRequestProperty ("User-Agent", headers.find(_._1=="User-Agent").fold("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:31.0) Gecko/20100101 Firefox/31.0")(_._2))
      for ((k, v) <- headers if k!="User-Agent") {
        conn setRequestProperty (k, v)
      }
      if(u.getUserInfo != null)
        conn setRequestProperty ("Authorization", "Basic " + Base64.encodeBase64String(u.getUserInfo.getBytes).trim)
    }
    for(coo <- cookie; (k, vv) <- coo.get(u.toURI, new java.util.HashMap))
      conn setRequestProperty (k, vv mkString "; ")
    method match {
      case "GET" =>
      case "HEAD" | "DELETE" => conn.asInstanceOf[java.net.HttpURLConnection] setRequestMethod method
    }
    conn.connect()
    for(coo <- cookie)
      coo.put(u.toURI, conn.getHeaderFields)
    conn
  }

  // fix unicode in headers, i.e. Location
  def getHeaderFieldUrl(conn: java.net.URLConnection, key: String): String = {
    conn getHeaderField key match {
      case null  => null
      case value => value flatMap { case c if '\u0020'<=c && c<='\u007E' => c.toString
                                    case c if                c<='\u00FF' => "%%%02X" format c&0xFF
                                    case _                               => ??? }
    }
  }

  // todo: followRedirects to https://
  // todo: use cookie for all followRedirects requests
  def POST(url:             String,
           socks:           Option[String]                = None,
           headers:         Traversable[(String, String)] = Map.empty,
           cookie:          Option[CookieManager]         = None,
           followRedirects: Boolean                       = true,
           timeout:         Duration                      = 5.seconds,
           method:          String                        = "POST" /* "POST" | "PUT" */ )
          (writer:          OutputStream => Unit): java.net.HttpURLConnection = {
    require(System.getProperty("sun.net.http.allowRestrictedHeaders") == "true")

    val u = new URL(escapeUnprintableAndUnicode(url))
    require (u.getProtocol startsWith "http")

    val conn = (socks match {
      case None                   => u.openConnection()
      case Some(reHostPort(h, p)) => u.openConnection(new Proxy(Proxy.Type.SOCKS, new InetSocketAddress(h, p.toInt)))
    }).asInstanceOf[java.net.HttpURLConnection]

    conn setConnectTimeout  timeout.toMillis.toInt
    conn setReadTimeout     timeout.toMillis.toInt

    conn setInstanceFollowRedirects followRedirects

    if (headers exists (_._1 equalsIgnoreCase "Host")) // `System.setProperty("sun.net.http.allowRestrictedHeaders", "true")` in httputils.$init is not enough sometimes, why?
      System.setProperty("sun.net.http.allowRestrictedHeaders", "true")
    conn setRequestProperty ("User-Agent", headers.find(_._1=="User-Agent").fold("Mozilla/5.0 (Windows NT 6.1; WOW64; rv:31.0) Gecko/20100101 Firefox/31.0")(_._2))
    for ((k, v) <- headers if k!="User-Agent") {
      conn setRequestProperty (k, v)
    }
    if(u.getUserInfo != null)
      conn setRequestProperty ("Authorization", "Basic " + Base64.encodeBase64String(u.getUserInfo.getBytes).trim)
    for(coo <- cookie; (k, vv) <- coo.get(u.toURI, new java.util.HashMap))
      conn setRequestProperty (k, vv mkString "; ")

    conn setRequestMethod method
    conn setDoOutput true
    conn.connect()
    writer(conn.getOutputStream) // <-- write here, before read cookies
    for(coo <- cookie)
      coo.put(u.toURI, conn.getHeaderFields)
    conn
  }


  def POSTUrlencoded(url:             String,
                     params:          Seq[(String, Any)],
                     socks:           Option[String]                = None,
                     headers:         Traversable[(String, String)] = Map.empty,
                     cookie:          Option[CookieManager]         = None,
                     followRedirects: Boolean                       = true,
                     timeout:         Duration                      = 5.seconds): String = {
    val conn = POST(url             = url,
                    socks           = socks,
                    headers         = headers ++ Traversable("Content-Type" -> "application/x-www-form-urlencoded"),
                    cookie          = cookie,
                    followRedirects = followRedirects,
                    timeout         = timeout){ outputStream =>
                 outputStream.write(NormUrl.mkQuery(params:_*).get getBytes "UTF-8")
                 outputStream.close()
            }
    //conn.getOutputStream write (NormUrl.mkQuery(params:_*).get getBytes "UTF-8")
    IOUtils.toString(guessUnpackStream(conn.getInputStream), "UTF-8")
  }


  def POSTMultipart(url:             String,
                    params:          Seq[(String, Any)],
                    socks:           Option[String]                = None,
                    headers:         Traversable[(String, String)] = Map.empty,
                    cookie:          Option[CookieManager]         = None,
                    followRedirects: Boolean                       = true,
                    timeout:         Duration                      = 5.seconds): String = {
    val boundary  = "----------NEXT PART----------"
    val conn = POST(url             = url,
                    socks           = socks,
                    headers         = headers ++ Traversable("Content-Type" -> s"multipart/form-data; boundary=$boundary"),
                    cookie          = cookie,
                    followRedirects = followRedirects,
                    timeout         = timeout){ outputStream =>
                 val out = new DataOutputStream(new BufferedOutputStream(outputStream))
                 out writeBytes s"--$boundary\r\n"
                 params foreach {
                   case (name, value: Array[Byte]) =>
                     out writeBytes  s"Content-Disposition: form-data; name=\042$name\042\r\n"
                     out writeBytes  "Content-Type: application/octet-stream\r\n\r\n"
                     out write       value
                     out writeBytes  s"\r\n--$boundary\r\n"
                   case (name, value) =>
                     out writeBytes  s"Content-Disposition: form-data; name=\042$name\042\r\n"
                     out writeBytes  "Content-Type: text/plain; charset=UTF-8\r\n\r\n"
                     out write       (value.toString getBytes "UTF-8")
                     out writeBytes  s"\r\n--$boundary\r\n"
                 }
                 out writeBytes "--\r\n"
                 out.close()
               }
    IOUtils.toString(guessUnpackStream(conn.getInputStream), "UTF-8")
  }


  def guessUnpackStream(is: InputStream): InputStream = {
    if (!is.markSupported)
      guessUnpackStream(new BufferedInputStream(is))
    else {
      is.mark(2)
      val a=is.read()
      val b=is.read()
      is.reset()
      (a,b) match {
        case (0x42, 0x5A) => new BZip2CompressorInputStream(is)
        case (0x1F, 0x8B) => new GZIPInputStream(is)
        case _            => is
      }
    }
  }

  def retry[A](num: Int, epred: Throwable=>Boolean)(body: =>A): A = {
    if (num<=1)
      body
    else
      try body
      catch {
        case e:Throwable if epred(e) => retry(num-1, epred)(body)
      }
  }

  def main(args: Array[String]) {
    println(util.Try(GETAsByteArray("https://thehackerblog.com/feed/index.html").length))
    println(util.Try(GETAsByteArray("https://opentrackers.org/feed/").length))
  }
}
