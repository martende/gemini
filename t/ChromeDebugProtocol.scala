#!perl
=cut
!#

package ms.webmaster.chromium.chromedebugprotocol

import scala.util.{Try, Success, Failure}
import scala.Console._
import scala.concurrent.{Future, Promise}

import `io.monix::monix:2.1.2`, monix.reactive.{Observable, OverflowStrategy},
                                monix.reactive.observers.Subscriber,
                                monix.execution.Cancelable
import `org.json:json:20160810`  // pretty printer
import `io.netty:netty-all:4.1.16.Final`
import `commons-codec:1.6`,                                   org.apache.commons.codec.binary.Base64
import `../ms.webmaster.macroserialization/json.macro.scala`, ms.webmaster.macroserialization.Json


private[chromedebugprotocol] class WebSocketClient(uri: java.net.URI, eventLoop: io.netty.channel.EventLoop) {
  private[this] val contextPromise = Promise[io.netty.channel.ChannelHandlerContext]()

  def connect(): Observable[String] =
    Observable.create[String](OverflowStrategy.Unbounded) { subscriber =>
      new io.netty.bootstrap.Bootstrap()
        .group(eventLoop)
        .channel(classOf[io.netty.channel.socket.nio.NioSocketChannel])
        .handler(new io.netty.channel.ChannelInitializer[io.netty.channel.socket.SocketChannel] {
                   override protected def initChannel(ch: io.netty.channel.socket.SocketChannel) {
                     ch.pipeline  addLast new io.netty.handler.codec.http.HttpClientCodec
                     ch.pipeline  addLast new io.netty.handler.codec.http.HttpObjectAggregator(8192)
                   //ch.pipeline  addLast io.netty.handler.codec.http.websocketx.extensions.compression.WebSocketClientCompressionHandler.INSTANCE
                     ch.pipeline  addLast new io.netty.channel.SimpleChannelInboundHandler[Object] {
                                            val handshaker = io.netty.handler.codec.http.websocketx.WebSocketClientHandshakerFactory.newHandshaker(
                                                               uri,
                                                               io.netty.handler.codec.http.websocketx.WebSocketVersion.V13,
                                                               /*"protocol-post-demo"*/null,
                                                               true,
                                                               new io.netty.handler.codec.http.DefaultHttpHeaders,
                                                               100*1024*1024 /* default 64k is too small */ )
                                            override def channelActive(ctx: io.netty.channel.ChannelHandlerContext) {
                                              //println(s"channelActive")
                                              handshaker.handshake(ctx.channel)
                                            }
                                            override def channelInactive(ctx: io.netty.channel.ChannelHandlerContext) {
                                              println("WebSocket Client disconnected!")
                                            }
                                            override def channelRead0(ctx: io.netty.channel.ChannelHandlerContext, msg: Object) {
                                              try {
                                                //println(s"msg=$msg")
                                                msg match {
                                                  case response: io.netty.handler.codec.http.FullHttpResponse if handshaker.isHandshakeComplete => throw new IllegalStateException(s"Unexpected FullHttpResponse (getStatus=${response.status}, content=${response.content.toString(io.netty.util.CharsetUtil.UTF_8)})")
                                                  case response: io.netty.handler.codec.http.FullHttpResponse                                   => try {
                                                                                                                                                     handshaker.finishHandshake(ctx.channel, response)
                                                                                                                                                     println("WebSocket Client connected!")
                                                                                                                                                     contextPromise success ctx
                                                                                                                                                   } catch {
                                                                                                                                                     case e: io.netty.handler.codec.http.websocketx.WebSocketHandshakeException =>
                                                                                                                                                       println("WebSocket Client failed to connect")
                                                                                                                                                       contextPromise failure e
                                                                                                                                                       subscriber.onError(e)
                                                                                                                                                   }
                                                  case textFrame:io.netty.handler.codec.http.websocketx.TextWebSocketFrame                      => /*println(textFrame.text);*/ subscriber.onNext(textFrame.text)
                                                //case frame:    io.netty.handler.codec.http.websocketx.PongWebSocketFrame                      => println("WebSocket Client received pong")
                                                  case frame:    io.netty.handler.codec.http.websocketx.CloseWebSocketFrame                     => println("WebSocket Client received closing")
                                                                                                                                                   subscriber.onComplete()
                                                                                                                                                   ctx.channel.close()
                                                }
                                              } catch {
                                                case e: Throwable =>
                                                  e.printStackTrace
                                              }
                                            }
                                            override def exceptionCaught(ctx: io.netty.channel.ChannelHandlerContext, cause: Throwable) {
                                              cause.printStackTrace
                                              contextPromise tryFailure cause
                                              subscriber.onError(cause)
                                              ctx.close()
                                            }
                                          }
                   }
                 })
        .connect(uri.getHost, uri.getPort)

      Cancelable.empty // Cancelable(() => ...)
    }//.share // make the Observable multicast with refCount

  def send(text: String)(implicit s: scala.concurrent.ExecutionContext) {
    contextPromise.future foreach { _.writeAndFlush(new io.netty.handler.codec.http.websocketx.TextWebSocketFrame(text)) }
  }
}





object ChromeDebugProtocol {
  // json case classes are here because:
  // 1. value classes cannot be nested to other classes
  // 2. problem with access to companion object for default values (like Node.isSVG) because of https://github.com/scala/bug/issues/10585

  // Runtime
  sealed trait RemoteObjectType
  object RemoteObjectType {
    case object `object`  extends RemoteObjectType
    case object function  extends RemoteObjectType
    case object undefined extends RemoteObjectType
    case object string    extends RemoteObjectType
    case object number    extends RemoteObjectType
    case object boolean   extends RemoteObjectType
    case object symbol    extends RemoteObjectType
    case object accessor  extends RemoteObjectType
  }
  sealed trait RemoteObjectSubtype
  object RemoteObjectSubtype {
    case object array      extends RemoteObjectSubtype
    case object `null`     extends RemoteObjectSubtype
    case object node       extends RemoteObjectSubtype
    case object regexp     extends RemoteObjectSubtype
    case object date       extends RemoteObjectSubtype
    case object map        extends RemoteObjectSubtype
    case object set        extends RemoteObjectSubtype
    case object weakmap    extends RemoteObjectSubtype
    case object weakset    extends RemoteObjectSubtype
    case object iterator   extends RemoteObjectSubtype
    case object generator  extends RemoteObjectSubtype
    case object error      extends RemoteObjectSubtype
    case object proxy      extends RemoteObjectSubtype
    case object promise    extends RemoteObjectSubtype
    case object typedarray extends RemoteObjectSubtype
  }
  sealed trait UnserializableValue
  object UnserializableValue {
    case object Infinity    extends UnserializableValue
    case object NaN         extends UnserializableValue
    case object `-Infinity` extends UnserializableValue
    case object `-0`        extends UnserializableValue
  }

  class RemoteObjectId    (val value: String) extends AnyVal { override def toString = s"ROBJ$value" }
  class ExecutionContextId(val value: Int   ) extends AnyVal { override def toString = s"ECTX$value" }
  class AsyncTaskId       (val value: String) extends AnyVal { override def toString = s"ATASK$value" }
  class ScriptId          (val value: String) extends AnyVal { override def toString = s"SCRIPT$value" }
  class Timestamp         (val value: Double) extends AnyVal { override def toString = s"T$value" } // Number of milliseconds since epoch.
  case class ExecutionContextDescription(id: ExecutionContextId, origin: String, name: String, auxData: Option[Any])
  case class EntryPreview(key: Option[ObjectPreview], value: ObjectPreview)
  case class PropertyPreview(name: String, `type`: RemoteObjectType, value: Option[String], valuePreview: Option[ObjectPreview], subtype: Option[RemoteObjectSubtype])
  case class ObjectPreview  (`type`: RemoteObjectType, subtype: Option[RemoteObjectSubtype], description: Option[String], overflow: Boolean, properties: Vector[PropertyPreview], entries: Vector[EntryPreview]=Vector.empty)
  case class CustomPreview  (header: String, hasBody: Boolean, formatterObjectId: RemoteObjectId, bindRemoteObjectFunctionId: RemoteObjectId, configObjectId: Option[RemoteObjectId])
  case class RemoteObject   (`type`:              RemoteObjectType,
                             subtype:             Option[RemoteObjectSubtype],
                             className:           Option[String],
                             value:               Option[Any],
                             unserializableValue: Option[UnserializableValue],
                             description:         Option[String],
                             objectId:            Option[RemoteObjectId],
                             preview:             Option[ObjectPreview],
                             customPreview:       Option[CustomPreview])
  case class ExceptionDetails(exceptionId:        Int,
                              text:               String,
                              lineNumber:         Int,
                              columnNumber:       Int,
                              scriptId:           Option[ScriptId],
                              url:                Option[String],
                              stackTrace:         Option[StackTrace],
                              exception:          Option[RemoteObject],
                              executionContextId: Option[ExecutionContextId])
  case class CallFrame(functionName: String, scriptId: ScriptId, url: String, lineNumber: Int, columnNumber: Int)
  case class StackTrace(description: Option[String], callFrames: Vector[CallFrame], parent: Option[StackTrace], promiseCreationFrame: Option[CallFrame])
  case class PropertyDescriptor(name: String, value: Option[RemoteObject], writable: Boolean=false, get: Option[RemoteObject], set: Option[RemoteObject], configurable: Boolean, enumerable: Boolean, wasThrown: Boolean=false, isOwn: Boolean=false, symbol: Option[RemoteObject])
  case class InternalPropertyDescriptor(name: String, value: Option[RemoteObject])
  case class CallArgument(value: Option[Any], unserializableValue: Option[UnserializableValue], objectId: Option[RemoteObjectId])

  // Debugger
  class BreakpointId    (val value: String) extends AnyVal { override def toString = s"BRK$value" }
  case class Location(scriptId: ScriptId, lineNumber: Int, columnNumber: Option[Int])
//case class Scope(`type`: String, `object`: RemoteObject, name: Option[String], startLocation: Option[Location], endLocation: Option[Location])
  case class SearchMatch(lineNumber: Int, lineContent: String)

  // Page
  sealed abstract class ResourceType//(val value: String)
  object ResourceType {
    case object Document    extends ResourceType//("Document")
    case object Stylesheet  extends ResourceType
    case object Image       extends ResourceType
    case object Media       extends ResourceType
    case object Font        extends ResourceType
    case object Script      extends ResourceType
    case object TextTrack   extends ResourceType
    case object XHR         extends ResourceType
    case object Fetch       extends ResourceType
    case object EventSource extends ResourceType
    case object WebSocket   extends ResourceType
    case object Manifest    extends ResourceType
    case object Other       extends ResourceType
  }
  sealed trait LivecycleEventType
  object LivecycleEventType {
    case object load                          extends LivecycleEventType
    case object commit                        extends LivecycleEventType
    case object firstPaint                    extends LivecycleEventType
    case object firstContentfulPaint          extends LivecycleEventType
    case object firstTextPaint                extends LivecycleEventType
    case object firstImagePaint               extends LivecycleEventType
    case object firstMeaningfulPaintCandidate extends LivecycleEventType
    case object DOMContentLoaded              extends LivecycleEventType
    case object networkAlmostIdle             extends LivecycleEventType
    case object networkIdle                   extends LivecycleEventType
  }
  sealed trait TransitionType
  object TransitionType {
    case object link              extends TransitionType
    case object typed             extends TransitionType
    case object auto_bookmark     extends TransitionType
    case object auto_subframe     extends TransitionType
    case object manual_subframe   extends TransitionType
    case object generated         extends TransitionType
    case object auto_toplevel     extends TransitionType
    case object form_submit       extends TransitionType
    case object reload            extends TransitionType
    case object keyword           extends TransitionType
    case object keyword_generated extends TransitionType
    case object other             extends TransitionType
  }
  class FrameId         (val value: String) extends AnyVal { override def toString = s"F$value" }
  class ScriptIdentifier(val value: String) extends AnyVal { override def toString = s"SCRIPT$value" }
  case class Frame(id:             FrameId,
                   parentId:       Option[FrameId],
                   loaderId:       LoaderId,
                   name:           Option[String],
                   url:            String,
                   securityOrigin: String,
                   mimeType:       String,
                   unreachableUrl: Option[String])
  case class NavigationEntry(id: Int, url: String, userTypedURL: String, title: String, transitionType: TransitionType)
  case class FrameResource(url: String, `type`: ResourceType, mimeType: String, lastModified: Option[TimeSinceEpoch], contentSize: Option[Int], failed: Boolean=false, canceled: Boolean=false)
  case class FrameResourceTree(frame: Frame, childFrames: Vector[FrameResourceTree]=Vector.empty, resources: Vector[FrameResource])
  case class FrameTree(frame: Frame, childFrames: Vector[FrameTree]=Vector.empty)
  case class AppManifestError(message: String, critical: Int, line: Int, column: Int)
  case class LayoutViewport(pageX: Int, pageY: Int, clientWidth: Int, clientHeight: Int)
  case class VisualViewport(offsetX: Double, offsetY: Double, pageX: Double, pageY: Double, clientWidth: Double, clientHeight: Double, scale: Double)
  case class Viewport(x: Double, y: Double, width: Double, height: Double, scale: Double)

  // DOM
  sealed trait ShadowRootType
  object ShadowRootType {
    case object `user-agent` extends ShadowRootType
    case object open         extends ShadowRootType
    case object closed       extends ShadowRootType
  }
  sealed trait PseudoType
  object PseudoType {
    case object `first-line`            extends PseudoType
    case object `first-letter`          extends PseudoType
    case object before                  extends PseudoType
    case object after                   extends PseudoType
    case object backdrop                extends PseudoType
    case object selection               extends PseudoType
    case object `first-line-inherited`  extends PseudoType
    case object scrollbar               extends PseudoType
    case object `scrollbar-thumb`       extends PseudoType
    case object `scrollbar-button`      extends PseudoType
    case object `scrollbar-track`       extends PseudoType
    case object `scrollbar-track-piece` extends PseudoType
    case object `scrollbar-corner`      extends PseudoType
    case object resizer                 extends PseudoType
    case object `input-list-button`     extends PseudoType
  }
  class BackendNodeId (val value: Int) extends AnyVal { override def toString = s"@$value" }
  class NodeId        (val value: Int) extends AnyVal { override def toString = s"#$value" }
  class NodeType      (val value: Int) extends AnyVal { override def toString = this match { case NodeType.ELEMENT                => "ELEMENT"
                                                                                             case NodeType.TEXT                   => "TEXT"
                                                                                             case NodeType.PROCESSING_INSTRUCTION => "PROCESSING_INSTRUCTION"
                                                                                             case NodeType.COMMENT                => "COMMENT"
                                                                                             case NodeType.DOCUMENT               => "DOCUMENT"
                                                                                             case NodeType.DOCUMENT_TYPE          => "DOCUMENT_TYPE"
                                                                                             case NodeType.DOCUMENT_FRAGMENT      => "DOCUMENT_FRAGMENT" } }
  object NodeType {
    val ELEMENT                = new NodeType( 1)
    val TEXT                   = new NodeType( 3)
    val PROCESSING_INSTRUCTION = new NodeType( 7)
    val COMMENT                = new NodeType( 8)
    val DOCUMENT               = new NodeType( 9)
    val DOCUMENT_TYPE          = new NodeType(10)
    val DOCUMENT_FRAGMENT      = new NodeType(11)
  }

  case class RGBA(r: Double, g: Double, b: Double, a: Double=1.0)
  case class Rect(x: Double, y: Double, width: Double, height: Double)
  case class Quad(top: Double, right: Double, bottom: Double, left: Double) // encoded as array
  case class ShapeOutsideInfo(bounds: Quad, shape: Vector[Any], marginShape: Vector[Any])
  case class BoxModel(content: Quad, padding: Quad, border: Quad, margin: Quad, width: Int, height: Int, shapeOutside: Option[ShapeOutsideInfo])
  case class BackendNode(nodeType: Int, nodeName: String, backendNodeId: BackendNodeId)
  case class Node(nodeId:           NodeId,                             // Node identifier that is passed into the rest of the DOM messages as the nodeId. Backend will only push node with given id once. It is aware of all requested nodes and will only fire DOM events for nodes known to the client.
                  parentId:         Option[NodeId],                     // The id of the parent node if any.
                  backendNodeId:    BackendNodeId,
                  nodeType:         NodeType,
                  nodeName:         String,
                  localName:        String,
                  nodeValue:        String,
                  childNodeCount:   Option[Int],                        // Child count for Container nodes.
                  children:         Vector[Node]        = Vector.empty, // Child nodes of this node when requested with children.
                  attributes:       Vector[String]      = Vector.empty, // Attributes of the Element node in the form of flat array [name1, value1, name2, value2].
                  documentURL:      Option[String],                     // Document URL that Document or FrameOwner node points to.
                  baseURL:          Option[String],                     // Base URL that Document or FrameOwner node uses for URL completion.
                  publicId:         Option[String],                     //
                  systemId:         Option[String],                     //
                  internalSubset:   Option[String],                     //
                  xmlVersion:       Option[String],                     // Document's XML version in case of XML documents.
                  name:             Option[String],                     //
                  value:            Option[String],                     //
                  pseudoType:       Option[PseudoType],                 //
                  shadowRootType:   Option[ShadowRootType],
                  frameId:          Option[FrameId],                    // Frame ID for frame owner elements.
                  contentDocument:  Option[Node],                       // Content document for frame owner elements.
                  shadowRoots:      Vector[Node]        = Vector.empty, // Shadow root list for given element host.
                  templateContent:  Option[Node],                       // Content document fragment for template elements.
                  pseudoElements:   Vector[Node]        = Vector.empty, // Pseudo elements associated with this node.
                  importedDocument: Option[Node],                       // Import document for the HTMLImport links.
                  distributedNodes: Vector[BackendNode] = Vector.empty, // Distributed nodes for given insertion point.
                  isSVG:            Boolean = false) {                   // Whether the node is SVG.
    override def toString = {
      val sb = new java.lang.StringBuilder
      sb append "Node("
      sb append nodeId
      if (parentId.nonEmpty) sb append s",${parentId.get}"
      sb append ","
      sb append backendNodeId
      sb append ","
      sb append nodeType
      sb append ","
      sb append nodeName
      sb append ","
      sb append localName
      sb append ","
      sb append nodeValue
      if (childNodeCount  .nonEmpty) sb append s",childNodeCount=${childNodeCount.get}"
      if (children        .nonEmpty) sb append s",children=${children}"
      if (attributes      .nonEmpty) sb append s",attributes=${attributes}"
      if (documentURL     .nonEmpty) sb append s",documentURL=${documentURL.get}"
      if (baseURL         .nonEmpty) sb append s",baseURL=${baseURL.get}"
      if (publicId        .nonEmpty) sb append s",publicId=${publicId.get}"
      if (systemId        .nonEmpty) sb append s",systemId=${systemId.get}"
      if (internalSubset  .nonEmpty) sb append s",internalSubset =${internalSubset.get}"
      if (xmlVersion      .nonEmpty) sb append s",xmlVersion=${xmlVersion.get}"
      if (name            .nonEmpty) sb append s",name=${name.get}"
      if (value           .nonEmpty) sb append s",value=${value.get}"
      if (pseudoType      .nonEmpty) sb append s",pseudoType=${pseudoType.get}"
      if (shadowRootType  .nonEmpty) sb append s",shadowRootType=${shadowRootType.get}"
      if (frameId         .nonEmpty) sb append s",frameId=${frameId.get}"
      if (contentDocument .nonEmpty) sb append s",contentDocument=${contentDocument.get}"
      if (shadowRoots     .nonEmpty) sb append s",shadowRoots=${shadowRoots}"
      if (templateContent .nonEmpty) sb append s",templateContent=${templateContent.get}"
      if (pseudoElements  .nonEmpty) sb append s",pseudoElements=${pseudoElements}"
      if (importedDocument.nonEmpty) sb append s",importedDocument=${importedDocument.get}"
      if (distributedNodes.nonEmpty) sb append s",distributedNodes=${distributedNodes}"
      if (isSVG                    ) sb append s",isSVG"
      sb append ")"
      sb.toString
    }
  }

  // CSS
  sealed trait StyleSheetOrigin
  object StyleSheetOrigin {
    case object injected     extends StyleSheetOrigin
    case object `user-agent` extends StyleSheetOrigin
    case object inspector    extends StyleSheetOrigin
    case object regular      extends StyleSheetOrigin
  }
  class StyleSheetId  (val value: String) extends AnyVal { override def toString = s"SS$value" }
  case class CSSStyleSheetHeader(styleSheetId:    StyleSheetId,
                                 frameId:         FrameId,
                                 sourceURL:       String,
                                 sourceMapURL:    Option[String],
                                 origin:          StyleSheetOrigin,
                                 title:           String,
                                 ownerNode:       Option[BackendNodeId],
                                 disabled:        Boolean,
                                 hasSourceURL:    Boolean = false,
                                 isInline:        Boolean,
                                 startLine:       Int,
                                 startColumn:     Int,
                                 length:          Int)
  case class SourceRange              (startLine: Int, startColumn: Int, endLine: Int, endColumn: Int) // Text range within a resource. All numbers are zero-based
  case class Value                    (text: String, range: Option[SourceRange]) // Data for a simple selector (these are delimited by commas in a selector list)
  case class SelectorList             (selectors: Vector[Value], text: String)
  case class ShorthandEntry           (name: String/*adt?*/, value: String, important: Boolean=false)
  case class CSSComputedStyleProperty (name: String/*adt?*/, value: String)
  case class StyleDeclarationEdit     (styleSheetId: StyleSheetId, range: SourceRange, text: String)
  case class CSSKeyframeRule          (styleSheetId: Option[StyleSheetId], origin: StyleSheetOrigin, keyText: Value, style: CSSStyle)
  case class CSSKeyframesRule         (animationName: Value, keyframes: Vector[CSSKeyframeRule])
  case class PlatformFontUsage        (familyName: String, isCustomFont: Boolean, glyphCount: Int)
  case class MediaQueryExpression     (value: Double, unit: String, feature: String, valueRange: Option[SourceRange], computedLength: Option[Double])
  case class MediaQuery               (expressions: Vector[MediaQueryExpression], active: Boolean)
  case class CSSMedia                 (text: String, source: String, sourceURL: Option[String], range: Option[SourceRange], styleSheetId: Option[StyleSheetId], mediaList: Vector[MediaQuery] = Vector.empty)
  case class CSSProperty              (name: String/*adt?*/, value: String, important: Boolean=false, `implicit`: Boolean=false, text: Option[String], parsedOk: Boolean=true, disabled: Boolean=false, range: Option[SourceRange])
  case class CSSStyle                 (styleSheetId: Option[StyleSheetId], cssProperties: Vector[CSSProperty], shorthandEntries: Vector[ShorthandEntry], cssText: Option[String], range: Option[SourceRange])
  case class CSSRule                  (styleSheetId: Option[StyleSheetId], selectorList: SelectorList, origin: StyleSheetOrigin, style: CSSStyle, media: Vector[CSSMedia]=Vector.empty)
  case class RuleUsage                (styleSheetId: StyleSheetId, startOffset: Int, endOffset: Int, used: Boolean)
  case class RuleMatch                (rule: CSSRule, matchingSelectors: Vector[Int])
  case class InheritedStyleEntry      (inlineStyle: Option[CSSStyle], matchedCSSRules: Vector[RuleMatch])
  case class PseudoElementMatches     (pseudoType: PseudoType, matches: Vector[RuleMatch])
  // return types
  case class MatchedStylesForNode(inlineStyle: Option[CSSStyle], attributesStyle: Option[CSSStyle], matchedCSSRules: Vector[RuleMatch]=Vector.empty, pseudoElements: Vector[PseudoElementMatches]=Vector.empty, inherited: Vector[InheritedStyleEntry]=Vector.empty, cssKeyframesRules: Vector[CSSKeyframesRule]=Vector.empty)
  case class InlineStylesForNode (inlineStyle: Option[CSSStyle], attributesStyle: Option[CSSStyle])
  case class BackgroundColors    (backgroundColors: Vector[String]=Vector.empty, computedFontSize: Option[String], computedFontWeight: Option[String], computedBodyFontSize: Option[String])


  // Network
  class LoaderId      (val value: String) extends AnyVal { override def toString = s"L$value" }
  class RequestId     (val value: String) extends AnyVal { override def toString = s"R$value" }
  class InterceptionId(val value: String) extends AnyVal { override def toString = s"I$value" }
  class MonotonicTime (val value: Double) extends AnyVal { override def toString = s"MT$value" } // Monotonically increasing time in seconds since an arbitrary point in the past.
  class TimeSinceEpoch(val value: Double) extends AnyVal { override def toString = s"UT$value" } // UTC time in seconds, counted from January 1, 1970.
  case class Initiator(`type`: String/*parser|script|preload|other*/, stack: Option[Json.Keep], url: Option[String], lineNumber: Option[Int])
  case class Request(url: String, method: String, headers: Map[String, String], postData: Option[String], mixedContentType: Option[String], initialPriority: String, referrerPolicy: String, isLinkPreload: Boolean=false)
  case class Response(url:                String,                     // Response URL. This URL can be different from CachedResource.url in case of redirect.
                      status:             Int,                        // HTTP response status code.
                      statusText:         String,                     // HTTP response status text.
                      headers:            Map[String,String],         // HTTP response headers.
                      headersText:        Option[String],             // HTTP response headers text.
                      mimeType:           String,                     // Resource mimeType as determined by the browser.
                      requestHeaders:     Option[Map[String,String]], // Refined HTTP request headers that were actually transmitted over the network.
                      requestHeadersText: Option[String],             // HTTP request headers text.
                      connectionReused:   Boolean,                    // Specifies whether physical connection was actually reused for this request.
                      connectionId:       Double,                     // Physical connection id that was actually used for this request.
                      remoteIPAddress:    Option[String],             // Remote IP address.
                      remotePort:         Option[Int],                // Remote port.
                      fromDiskCache:      Boolean = false,            // Specifies that the request was served from the disk cache.
                      fromServiceWorker:  Boolean = false,            // Specifies that the request was served from the ServiceWorker.
                      encodedDataLength:  Int,                        // Total number of bytes received for this request so far.
                      timing:             Option[Json.Keep],          // Timing information for the given request.
                      protocol:           Option[String],             // Protocol used to fetch this request.
                      securityState:      String,                     // Security state of the request resource.
                      securityDetails:    Option[Json.Keep])          // Security details for the request.
  case class Cookie(name: String, value: String, domain: String, path: String, expires: TimeSinceEpoch, size: Int, httpOnly: Boolean, secure: Boolean, session: Boolean, sameSite: Option[String])
  case class RequestPattern(urlPattern: Option[String], resourceType: Option[ResourceType])

  // DOMSnapshot
  case class NameValue(name: String, value: String)
  case class DOMNode(nodeType:              NodeType,
                     nodeName:              String,
                     nodeValue:             String,
                     textValue:             Option[String],                     // Only set for textarea elements, contains the text value.
                     inputValue:            Option[String],                     // Only set for input elements, contains the input's associated text value.
                     inputChecked:          Boolean = false,                    // Only set for radio and checkbox input elements, indicates if the element has been checked
                     optionSelected:        Boolean = false,                    // Only set for option elements, indicates if the element has been selected
                     backendNodeId:         BackendNodeId,
                     childNodeIndexes:      Vector[Int]       = Vector.empty,   // `domNodes` index
                     attributes:            Vector[NameValue] = Vector.empty,
                     pseudoElementIndexes:  Vector[Int]       = Vector.empty,   // `domNodes` index
                     layoutNodeIndex:       Option[Int],                        // `layoutTreeNodes` index
                     documentURL:           Option[String],                     // Document URL that Document or FrameOwner node points to.
                     baseURL:               Option[String],                     // Base URL that Document or FrameOwner node uses for URL completion.
                     contentLanguage:       Option[String],                     // Only set for documents, contains the document's content language.
                     documentEncoding:      Option[String],                     // Only set for documents, contains the document's character set encoding.
                     publicId:              Option[String],                     // DocumentType node's publicId.
                     systemId:              Option[String],                     // DocumentType node's systemId.
                     frameId:               Option[FrameId],                    // Frame ID for frame owner elements and also for the document node.
                     contentDocumentIndex:  Option[Int],                        // `domNodes` index
                     importedDocumentIndex: Option[Int],                        // `domNodes` index
                     templateContentIndex:  Option[Int],                        // `domNodes` index
                     pseudoType:            Option[PseudoType],
                     isClickable:           Boolean = false)                    // Whether this DOM node responds to mouse clicks. This includes nodes that have had click event listeners attached via JavaScript as well as anchor tags that naturally navigate when clicked.
  case class InlineTextBox(boundingBox:         Rect,     // The absolute position bounding box.
                           startCharacterIndex: Int,      // The starting index in characters, for this post layout textbox substring.
                           numCharacters:       Int)      // The number of characters in this post layout textbox substring.
  case class LayoutTreeNode(domNodeIndex:   Int,                                  // `domNodes` index
                            boundingBox:    Rect,                                 // The absolute position bounding box.
                            layoutText:     Option[String],                       // Contents of the LayoutText, if any.
                            inlineTextNodes:Vector[InlineTextBox] = Vector.empty, // The post-layout inline text nodes, if any.
                            styleIndex:     Option[Int])                          // `computedStyles` index
  case class ComputedStyle(properties: Vector[NameValue])

  // Input
  case class TouchPoint(x: Double, y: Double, radiusX: Double=1.0, radiusY: Double=1.0, rotationAngle: Double=0.0, force: Double=1.0, id: Option[Double])

  // Emulation
  sealed trait ScreenOrientationType
  object ScreenOrientationType {
    case object portraitPrimary    extends ScreenOrientationType
    case object portraitSecondary  extends ScreenOrientationType
    case object landscapePrimary   extends ScreenOrientationType
    case object landscapeSecondary extends ScreenOrientationType
  }
  case class ScreenOrientation(`type`: ScreenOrientationType, angle: Int)

  // Console
  sealed trait ConsoleMessageSource
  object ConsoleMessageSource {
    case object xml           extends ConsoleMessageSource
    case object javascript    extends ConsoleMessageSource
    case object network       extends ConsoleMessageSource
    case object `console-api` extends ConsoleMessageSource
    case object storage       extends ConsoleMessageSource
    case object appcache      extends ConsoleMessageSource
    case object rendering     extends ConsoleMessageSource
    case object security      extends ConsoleMessageSource
    case object other         extends ConsoleMessageSource
    case object deprecation   extends ConsoleMessageSource
    case object worker        extends ConsoleMessageSource
  }
  sealed trait ConsoleMessageLevel
  object ConsoleMessageLevel {
    case object log     extends ConsoleMessageLevel
    case object warning extends ConsoleMessageLevel
    case object error   extends ConsoleMessageLevel
    case object debug   extends ConsoleMessageLevel
    case object info    extends ConsoleMessageLevel
  }
  case class ConsoleMessage(source: ConsoleMessageSource, level: ConsoleMessageLevel, text: String, url: Option[String], line: Option[Int], column: Option[Int])

  // Log
  sealed trait LogEntrySource
  object LogEntrySource {
    case object xml            extends LogEntrySource
    case object javascript     extends LogEntrySource
    case object network        extends LogEntrySource
    case object storage        extends LogEntrySource
    case object appcache       extends LogEntrySource
    case object rendering      extends LogEntrySource
    case object security       extends LogEntrySource
    case object deprecation    extends LogEntrySource
    case object worker         extends LogEntrySource
    case object violation      extends LogEntrySource
    case object intervention   extends LogEntrySource
    case object recommendation extends LogEntrySource
    case object other          extends LogEntrySource
  }
  sealed trait LogEntryLevel
  object LogEntryLevel {
    case object verbose extends LogEntryLevel
    case object info    extends LogEntryLevel
    case object warning extends LogEntryLevel
    case object error   extends LogEntryLevel
  }
  case class ViolationSetting(name: String, threshold: Double)
  case class LogEntry(source: LogEntrySource, level: LogEntryLevel, text: String, timestamp: Timestamp, url: Option[String], lineNumber: Option[Int], stackTrace: Option[StackTrace], networkRequestId: Option[RequestId], workerId: Option[String], args: Vector[RemoteObject]=Vector.empty)

  // SystemInfo
  case class GPUDevice(vendorId: Double, deviceId: Double, vendorString: String, deviceString: String)
  case class GPUInfo(devices: Vector[GPUDevice], auxAttributes: Option[Any], featureStatus: Option[Any], driverBugWorkarounds: Vector[String])

  // Security
  case class SecurityStateExplanation(securityState: String, summary: String, description: String, mixedContentType: String, certificate: Vector[String])
  case class InsecureContentStatus(ranMixedContent: Boolean, displayedMixedContent: Boolean, containedMixedForm: Boolean, ranContentWithCertErrors: Boolean, displayedContentWithCertErrors: Boolean, ranInsecureContentStyle: String, displayedInsecureContentStyle: String)

  // Performance
  case class Metric(name: String, value: Double)
}



class ChromeDebugProtocolException(val code: Int, val message: String) extends RuntimeException(s"Error $code: $message")

object ChromeDebugProtocolException {
  def fromJSON(json: String) = { // `{"code":-32602,"message":"Invalid parameters","data":"url: string value expected"}`
    val cc = Json.unpackFromString[{def code: Int; def message: String; def data: Option[String]}](json)
    new ChromeDebugProtocolException(cc.code, cc.message + cc.data.fold("")(": "+_))
  }
}



class ChromeDebugProtocol(url: String, eventLoop: io.netty.channel.EventLoop, trace: Boolean=false)(implicit s: monix.execution.Scheduler) {
  import ChromeDebugProtocol._

  private[this] val ws = new WebSocketClient(new java.net.URI(url), eventLoop)
  private[this] val sequencer = new java.util.concurrent.atomic.AtomicInteger(0)
  private[this] val infly     = new scala.collection.concurrent.TrieMap[Int, Promise[String]]
  private[this] val sessions  = new scala.collection.concurrent.TrieMap[String, Session]


  private[this] var cdpSubscriber: Subscriber[CdpEvent] = null // one subscriber because order matters
  sealed trait CdpEvent
  val cdpEvents: Observable[CdpEvent] = // monix.Pipe?
    Observable.create[CdpEvent](OverflowStrategy.Unbounded) {
      s => require(cdpSubscriber == null, "cannot be, there is .share")
           cdpSubscriber = s
           Cancelable.empty
    }.share // make the Observable multicast with refCount


  def send(method: String, params: Map[String, String]): Future[String /*json result*/] = {
    val id = sequencer.incrementAndGet
    val p = Promise[String]()
    infly.put(id, p)
    val json = Json.packToString(Map("id" -> id, "method" -> method, "params" -> params))
    if (trace) println(s"< $json")
    ws.send(json)
    p.future
  }

  private[this] case class JsonResponse(id: Option[Int], result: Option[Json.Keep], error: Option[Json.Keep], method: Option[String], params: Option[Json.Keep])

  ws.connect() foreach { json =>
    if (trace) println(s"> $json")
    Json.unpackFromString[JsonResponse](json) match {
      case JsonResponse(Some(id), Some(result), None,        None,                                          None        ) => (infly remove id).get success result.jsonstring
      case JsonResponse(Some(id), None,         Some(error), None,                                          None        ) => (infly remove id).get failure ChromeDebugProtocolException.fromJSON(error.jsonstring)
      case JsonResponse(None,     None,         None,        Some(     "Target.receivedMessageFromTarget"), Some(params)) => val cc = params.unpack[{def sessionId: String; def message: String; def targetId: String}]
                                                                                                                             sessions(cc.sessionId) incoming cc.message
      case JsonResponse(None,     None,         None,        Some(  "Inspector.detached"                 ), Some(params)) => cdpSubscriber onNext params.unpack[Inspector.detached     ]
      case JsonResponse(None,     None,         None,        Some(  "Inspector.targetCrashed"            ), Some(params)) => cdpSubscriber onNext               Inspector.targetCrashed
      case JsonResponse(None,     None,         None,        Some(method),                                  Some(params)) => println(f"NN> ${method}%-32s ${params.jsonstring}") // todo
      case _ => ???
    }
  }


  case class Session(sessionId: String) /*extends Observable[...] */{ // todo: within Target?
    private[this] val sequencer = new java.util.concurrent.atomic.AtomicInteger(0)

    private[this] sealed trait Answer { /*def p: Promise[_]*/ }
    private[this] case class AnswerRawJson                   (p: Promise[String])                              extends Answer
    private[this] case class AnswerToEvent[T <: SessionEvent](p: Promise[T], unpacker: Try[String]=>Option[T]) extends Answer
    private[this] val infly = new scala.collection.concurrent.TrieMap[Int, Answer]
    sessions.put(sessionId, this)

    private[this] var sessionSubscriber: Subscriber[SessionEvent] = null // one subscriber because order matters

    sealed trait SessionEvent
    val sessionEvents: Observable[SessionEvent] = // monix.Pipe?
      Observable.create[SessionEvent](OverflowStrategy.Unbounded) {
        s => require(sessionSubscriber == null, "cannot be, there is .share")
             sessionSubscriber = s
             Cancelable.empty //Cancelable(() => sendMessageToTarget(s"$groupName.disable") andThen { case _ => subscriber = null })
      }.share // make the Observable multicast with refCount


    private[ChromeDebugProtocol] def incoming(message: String) {
      try {
        Json.unpackFromString[JsonResponse](message) match {
          case JsonResponse(Some(id), Some(result), None,        None,                                                  None        ) => (infly remove id) match { case Some(AnswerRawJson(p          )) => p success result.jsonstring
                                                                                                                                                                   case Some(AnswerToEvent(p, unpacker)) => val Some(okevent) = unpacker(Success(result.jsonstring))
                                                                                                                                                                                                            sessionSubscriber onNext okevent // to keep this answer's order among other events
                                                                                                                                                                                                            p success okevent }
          case JsonResponse(Some(id), None,         Some(error), None,                                                  None        ) => (infly remove id) match { case Some(AnswerRawJson(p          )) => p failure ChromeDebugProtocolException.fromJSON(error.jsonstring)
                                                                                                                                                                   case Some(AnswerToEvent(p, unpacker)) => val exc = ChromeDebugProtocolException.fromJSON(error.jsonstring)
                                                                                                                                                                                                            unpacker(Failure(exc)) foreach { failevent => sessionSubscriber onNext failevent }
                                                                                                                                                                                                            p failure exc }
          case JsonResponse(None,     None,         None,        Some(       "Page.domContentEventFired"              ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.domContentEventFired              ] // `subscriber` shouldn't be null during subscribtion
          case JsonResponse(None,     None,         None,        Some(       "Page.loadEventFired"                    ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.loadEventFired                    ]
          case JsonResponse(None,     None,         None,        Some(       "Page.lifecycleEvent"                    ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.lifecycleEvent                    ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameAttached"                     ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameAttached                     ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameNavigated"                    ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameNavigated                    ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameDetached"                     ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameDetached                     ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameStartedLoading"               ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameStartedLoading               ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameStoppedLoading"               ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameStoppedLoading               ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameScheduledNavigation"          ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameScheduledNavigation          ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameClearedScheduledNavigation"   ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.frameClearedScheduledNavigation   ]
          case JsonResponse(None,     None,         None,        Some(       "Page.frameResized"                      ), Some(_     )) => sessionSubscriber onNext                      Page.frameResized
          case JsonResponse(None,     None,         None,        Some(       "Page.javascriptDialogClosed"            ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.javascriptDialogClosed            ]
          case JsonResponse(None,     None,         None,        Some(       "Page.screencastFrame"                   ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.screencastFrame                   ]
          case JsonResponse(None,     None,         None,        Some(       "Page.screencastVisibilityChanged"       ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.screencastVisibilityChanged       ]
          case JsonResponse(None,     None,         None,        Some(       "Page.interstitialShown"                 ), Some(_     )) => sessionSubscriber onNext                      Page.interstitialShown
          case JsonResponse(None,     None,         None,        Some(       "Page.interstitialHidden"                ), Some(_     )) => sessionSubscriber onNext                      Page.interstitialHidden
          case JsonResponse(None,     None,         None,        Some(       "Page.windowOpen"                        ), Some(params)) => sessionSubscriber onNext params.unpack[       Page.windowOpen                        ]
          case JsonResponse(None,     None,         None,        Some(    "Runtime.executionContextCreated"           ), Some(params)) => sessionSubscriber onNext params.unpack[    Runtime.executionContextCreated           ]
          case JsonResponse(None,     None,         None,        Some(    "Runtime.executionContextDestroyed"         ), Some(params)) => sessionSubscriber onNext params.unpack[    Runtime.executionContextDestroyed         ]
          case JsonResponse(None,     None,         None,        Some(    "Runtime.executionContextsCleared"          ), Some(_     )) => sessionSubscriber onNext                   Runtime.executionContextsCleared
          case JsonResponse(None,     None,         None,        Some(    "Runtime.exceptionThrown"                   ), Some(params)) => sessionSubscriber onNext params.unpack[    Runtime.exceptionThrown                   ]
          case JsonResponse(None,     None,         None,        Some(    "Runtime.exceptionRevoked"                  ), Some(params)) => sessionSubscriber onNext params.unpack[    Runtime.exceptionRevoked                  ]
          case JsonResponse(None,     None,         None,        Some(    "Runtime.consoleAPICalled"                  ), Some(params)) => sessionSubscriber onNext params.unpack[    Runtime.consoleAPICalled                  ]
          case JsonResponse(None,     None,         None,        Some(    "Runtime.inspectRequested"                  ), Some(params)) => sessionSubscriber onNext params.unpack[    Runtime.inspectRequested                  ]
          case JsonResponse(None,     None,         None,        Some(    "Network.resourceChangedPriority"           ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.resourceChangedPriority           ]
          case JsonResponse(None,     None,         None,        Some(    "Network.requestWillBeSent"                 ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.requestWillBeSent                 ]
          case JsonResponse(None,     None,         None,        Some(    "Network.requestServedFromCache"            ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.requestServedFromCache            ]
          case JsonResponse(None,     None,         None,        Some(    "Network.responseReceived"                  ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.responseReceived                  ]
          case JsonResponse(None,     None,         None,        Some(    "Network.dataReceived"                      ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.dataReceived                      ]
          case JsonResponse(None,     None,         None,        Some(    "Network.loadingFinished"                   ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.loadingFinished                   ]
          case JsonResponse(None,     None,         None,        Some(    "Network.loadingFailed"                     ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.loadingFailed                     ]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketWillSendHandshakeRequest" ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketWillSendHandshakeRequest ]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketHandshakeResponseReceived"), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketHandshakeResponseReceived]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketCreated"                  ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketCreated                  ]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketClosed"                   ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketClosed                   ]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketFrameReceived"            ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketFrameReceived            ]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketFrameError"               ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketFrameError               ]
          case JsonResponse(None,     None,         None,        Some(    "Network.webSocketFrameSent"                ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.webSocketFrameSent                ]
          case JsonResponse(None,     None,         None,        Some(    "Network.eventSourceMessageReceived"        ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.eventSourceMessageReceived        ]
          case JsonResponse(None,     None,         None,        Some(    "Network.requestIntercepted"                ), Some(params)) => sessionSubscriber onNext params.unpack[    Network.requestIntercepted                ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.documentUpdated"                   ), Some(_     )) => sessionSubscriber onNext                       DOM.documentUpdated
          case JsonResponse(None,     None,         None,        Some(        "DOM.setChildNodes"                     ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.setChildNodes                     ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.attributeModified"                 ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.attributeModified                 ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.attributeRemoved"                  ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.attributeRemoved                  ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.inlineStyleInvalidated"            ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.inlineStyleInvalidated            ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.characterDataModified"             ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.characterDataModified             ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.childNodeCountUpdated"             ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.childNodeCountUpdated             ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.childNodeInserted"                 ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.childNodeInserted                 ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.childNodeRemoved"                  ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.childNodeRemoved                  ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.shadowRootPushed"                  ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.shadowRootPushed                  ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.shadowRootPopped"                  ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.shadowRootPopped                  ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.pseudoElementAdded"                ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.pseudoElementAdded                ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.pseudoElementRemoved"              ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.pseudoElementRemoved              ]
          case JsonResponse(None,     None,         None,        Some(        "DOM.distributedNodesUpdated"           ), Some(params)) => sessionSubscriber onNext params.unpack[        DOM.distributedNodesUpdated           ]
          case JsonResponse(None,     None,         None,        Some(        "CSS.mediaQueryResultChanged"           ), Some(params)) => sessionSubscriber onNext                       CSS.mediaQueryResultChanged
          case JsonResponse(None,     None,         None,        Some(        "CSS.fontsUpdated"                      ), Some(params)) => sessionSubscriber onNext                       CSS.fontsUpdated
          case JsonResponse(None,     None,         None,        Some(        "CSS.styleSheetChanged"                 ), Some(params)) => sessionSubscriber onNext params.unpack[        CSS.styleSheetChanged                 ]
          case JsonResponse(None,     None,         None,        Some(        "CSS.styleSheetAdded"                   ), Some(params)) => sessionSubscriber onNext params.unpack[        CSS.styleSheetAdded                   ]
          case JsonResponse(None,     None,         None,        Some(        "CSS.styleSheetRemoved"                 ), Some(params)) => sessionSubscriber onNext params.unpack[        CSS.styleSheetRemoved                 ]
          case JsonResponse(None,     None,         None,        Some(   "Debugger.scriptParsed"                      ), Some(params)) => sessionSubscriber onNext params.unpack[   Debugger.scriptParsed                      ]
          case JsonResponse(None,     None,         None,        Some(   "Debugger.scriptFailedToParse"               ), Some(params)) => sessionSubscriber onNext params.unpack[   Debugger.scriptFailedToParse               ]
          case JsonResponse(None,     None,         None,        Some(   "Debugger.breakpointResolved"                ), Some(params)) => sessionSubscriber onNext params.unpack[   Debugger.breakpointResolved                ]
          case JsonResponse(None,     None,         None,        Some(   "Debugger.paused"                            ), Some(params)) => sessionSubscriber onNext params.unpack[   Debugger.paused                            ]
          case JsonResponse(None,     None,         None,        Some(   "Debugger.resumed"                           ), Some(params)) => sessionSubscriber onNext                  Debugger.resumed
          case JsonResponse(None,     None,         None,        Some(  "Emulation.virtualTimeBudgetExpired"          ), Some(params)) => sessionSubscriber onNext                 Emulation.virtualTimeBudgetExpired
          case JsonResponse(None,     None,         None,        Some(  "Emulation.virtualTimeAdvanced"               ), Some(params)) => sessionSubscriber onNext params.unpack[  Emulation.virtualTimeAdvanced               ]
          case JsonResponse(None,     None,         None,        Some(  "Emulation.virtualTimePaused"                 ), Some(params)) => sessionSubscriber onNext params.unpack[  Emulation.virtualTimePaused                 ]
          case JsonResponse(None,     None,         None,        Some(    "Console.messageAdded"                      ), Some(params)) => sessionSubscriber onNext params.unpack[    Console.messageAdded                      ]
          case JsonResponse(None,     None,         None,        Some(        "Log.entryAdded"                        ), Some(params)) => sessionSubscriber onNext params.unpack[        Log.entryAdded                        ]
          case JsonResponse(None,     None,         None,        Some(   "Security.securityStateChanged"              ), Some(params)) => sessionSubscriber onNext params.unpack[   Security.securityStateChanged              ]
          case JsonResponse(None,     None,         None,        Some(   "Security.certificateError"                  ), Some(params)) => sessionSubscriber onNext params.unpack[   Security.certificateError                  ]
          case JsonResponse(None,     None,         None,        Some("Performance.metrics"                           ), Some(params)) => sessionSubscriber onNext params.unpack[Performance.metrics                           ]
          case JsonResponse(None,     None,         None,        Some(method                                          ), Some(params)) => println(f"UNKNOWN EVENT> ${method}%-32s ${params.jsonstring}")
          case _ => ???
        }
      } catch {
        case e: NullPointerException => println("please subscribe to `events` before `.enable()`")
                                        e.printStackTrace
        case e: Throwable            => println(s"$RED${BOLD}$e\n${new org.json.JSONObject(message).toString(4)}$RESET")
      }
    }

    def sendMessageToTarget(method: String, params: Map[String, Any]): Future[String /*json result*/] = {
      val id = sequencer.incrementAndGet
      val p = Promise[String]()
      infly.put(id, AnswerRawJson(p))
      val json = Json.packToString(Map("id" -> id, "method" -> method, "params" -> params))
      send("Target.sendMessageToTarget", Map("sessionId"->sessionId, "message"->json)) // ignore ack answer, wait for Target.receivedMessageFromTarget
      p.future
    }

    def sendMessageGetEvent[SessionEventT <: SessionEvent](method: String, params: Map[String, Any], unpacker: Try[String]=>Option[SessionEventT]): Future[SessionEventT] = {
      val id = sequencer.incrementAndGet
      val p = Promise[SessionEventT]()
      infly.put(id, AnswerToEvent(p, unpacker))
      val json = Json.packToString(Map("id" -> id, "method" -> method, "params" -> params))
      send("Target.sendMessageToTarget", Map("sessionId"->sessionId, "message"->json)) // ignore ack answer, wait for Target.receivedMessageFromTarget
      p.future
    }

    object Emulation {
      case object virtualTimeBudgetExpired                     extends SessionEvent
      case class  virtualTimeAdvanced(virtualTimeElapsed: Int) extends SessionEvent
      case class  virtualTimePaused  (virtualTimeElapsed: Int) extends SessionEvent

      def setDeviceMetricsOverride         (width: Int, height: Int, deviceScaleFactor: Double, mobile: Boolean, scale: Option[Double]=None, screenWidth: Option[Int]=None, screenHeight: Option[Int]=None, positionX: Option[Int]=None, positionY: Option[Int]=None, dontSetVisibleSize: Boolean=false, screenOrientation: Option[ScreenOrientation]=None, viewport: Option[Viewport]=None):
                                                                                                                                  Future[_]       = sendMessageToTarget("Emulation.setDeviceMetricsOverride", Map("width"->width, "height"->height, "deviceScaleFactor"->deviceScaleFactor, "mobile"->mobile) ++ scale.map("scale"->_) ++ screenWidth.map("screenWidth"->_) ++ screenHeight.map("screenHeight"->_) ++ positionX.map("positionX"->_) ++ positionY.map("positionY"->_) ++ (if (dontSetVisibleSize) List("dontSetVisibleSize"->true) else Nil) ++ screenOrientation.map("screenOrientation"->_) ++ viewport.map("viewport"->_))
      def clearDeviceMetricsOverride       ():                                                                                    Future[_]       = sendMessageToTarget("Emulation.clearDeviceMetricsOverride",        Map()                                                                                                                            )
      def resetPageScaleFactor             ():                                                                                    Future[_]       = sendMessageToTarget("Emulation.resetPageScaleFactor",              Map()                                                                                                                            )
      def setPageScaleFactor               (pageScaleFactor: Double):                                                             Future[_]       = sendMessageToTarget("Emulation.setPageScaleFactor",                Map("pageScaleFactor"->pageScaleFactor)                                                                                          )
      def setVisibleSize                   (width: Int, height: Int):                                                             Future[_]       = sendMessageToTarget("Emulation.setVisibleSize",                    Map("width"->width, "height"->height)                                                                                            )
      def setScriptExecutionDisabled       (value: Boolean):                                                                      Future[_]       = sendMessageToTarget("Emulation.setScriptExecutionDisabled",        Map("value"->value)                                                                                                              )
      def setGeolocationOverride           (latitude: Option[Double], longitude: Option[Double], accuracy: Option[Double]):       Future[_]       = sendMessageToTarget("Emulation.setGeolocationOverride",            Map.empty ++ latitude.map("latitude"->_) ++ longitude.map("longitude"->_) ++ accuracy.map("accuracy"->_)                         )
      def clearGeolocationOverride         ():                                                                                    Future[_]       = sendMessageToTarget("Emulation.clearGeolocationOverride",          Map()                                                                                                                            )
      def setTouchEmulationEnabled         (enabled: Boolean, maxTouchPoints: Int=1):                                             Future[_]       = sendMessageToTarget("Emulation.setTouchEmulationEnabled",          Map("enabled"->enabled, "maxTouchPoints"->maxTouchPoints)                                                                        )
      def setEmitTouchEventsForMouse       (enabled: Boolean, configuration: Option[String]):                                     Future[_]       = sendMessageToTarget("Emulation.setEmitTouchEventsForMouse",        Map("enabled"->enabled) ++ configuration.map("configuration"->_)                                                                 )
      def setEmulatedMedia                 (media: String):                                                                       Future[_]       = sendMessageToTarget("Emulation.setEmulatedMedia",                  Map("media"->media)                                                                                                              )
      def setCPUThrottlingRate             (rate: Double):                                                                        Future[_]       = sendMessageToTarget("Emulation.setCPUThrottlingRate",              Map("rate"->rate)                                                                                                                )
      def canEmulate                       ():                                                                                    Future[Boolean] = sendMessageToTarget("Emulation.canEmulate",                        Map()                                                                                                                            ) map (Json.unpackFromString[{def result: Boolean}](_).result)
      def setVirtualTimePolicy             (policy: String, budget: Option[Int], maxVirtualTimeTaskStarvationCount: Option[Int]): Future[_]       = sendMessageToTarget("Emulation.setVirtualTimePolicy",              Map("policy"->policy) ++ budget.map("budget"->_) ++ maxVirtualTimeTaskStarvationCount.map("maxVirtualTimeTaskStarvationCount"->_))
      def setNavigatorOverrides            (platform: String):                                                                    Future[_]       = sendMessageToTarget("Emulation.setNavigatorOverrides",             Map("platform"->platform)                                                                                                        )
      def setDefaultBackgroundColorOverride(color: Option[RGBA]):                                                                 Future[_]       = sendMessageToTarget("Emulation.setDefaultBackgroundColorOverride", Map.empty ++ color.map("color"->_)                                                                                               )
    }

    object Page {
      case class  domContentEventFired           (timestamp: Double)                                                                                        extends SessionEvent
      case class  loadEventFired                 (timestamp: Double)                                                                                        extends SessionEvent
      case class  lifecycleEvent                 (frameId: Option[FrameId], loaderId: Option[LoaderId], name: LivecycleEventType, timestamp: MonotonicTime) extends SessionEvent
      case class  frameAttached                  (frameId: FrameId, parentFrameId: FrameId, stack: Option[StackTrace])                                      extends SessionEvent
      case class  frameNavigated                 (frame: Frame)                                                                                             extends SessionEvent
      case class  frameDetached                  (frameId: FrameId)                                                                                         extends SessionEvent
      case class  frameStartedLoading            (frameId: FrameId)                                                                                         extends SessionEvent
      case class  frameStoppedLoading            (frameId: FrameId)                                                                                         extends SessionEvent
      case class  frameScheduledNavigation       (frameId: FrameId, delay: Double, reason: String, url: String)                                             extends SessionEvent
      case class  frameClearedScheduledNavigation(frameId: FrameId)                                                                                         extends SessionEvent
      case object frameResized                                                                                                                              extends SessionEvent
      case class  javascriptDialogOpening        (url: String, message: String, `type`: String, defaultPrompt: Option[String])                              extends SessionEvent // alert|confirm|prompt|beforeunload
      case class  javascriptDialogClosed         (result: Boolean, userInput: String)                                                                       extends SessionEvent
      case class  screencastFrame                (data: String, metadata: Json.Keep, sessionId: Int /*frame number*/)                                       extends SessionEvent
      case class  screencastVisibilityChanged    (visible: Boolean)                                                                                         extends SessionEvent
      case object interstitialShown                                                                                                                         extends SessionEvent
      case object interstitialHidden                                                                                                                        extends SessionEvent
      case class  windowOpen                     (url: String, windowName: String, windowFeatures: List[String], userGesture: Boolean)                      extends SessionEvent

      case class NavigateResult(frameId: FrameId, loaderId: Option[String])
      case class NavigationHistoryResult(currentIndex: Int, entries: Vector[NavigationEntry])
      case class AppManifestResult(url: String, errors: Vector[AppManifestError], data: Option[String])
      case class LayoutMetricsResult(layoutViewport: LayoutViewport, visualViewport: VisualViewport, contentSize: Rect)

      def enable                             ():                                                                                                        Future[_]                            = sendMessageToTarget("Page.enable" ,                             Map()                                                                                                                                                            )
      def disable                            ():                                                                                                        Future[_]                            = sendMessageToTarget("Page.disable",                             Map()                                                                                                                                                            )
      def addScriptToEvaluateOnLoad          (scriptSource: String):                                                                                    Future[ScriptIdentifier]             = sendMessageToTarget("Page.addScriptToEvaluateOnLoad",           Map("scriptSource"->scriptSource)                                                                                                                                ) map (Json.unpackFromString[{def identifier: ScriptIdentifier}](_).identifier)
      def removeScriptToEvaluateOnLoad       (identifier: ScriptIdentifier):                                                                            Future[_]                            = sendMessageToTarget("Page.removeScriptToEvaluateOnLoad",        Map("identifier"->identifier)                                                                                                                                    )
      def addScriptToEvaluateOnNewDocument   (source: String):                                                                                          Future[ScriptIdentifier]             = sendMessageToTarget("Page.addScriptToEvaluateOnNewDocument",    Map("source"->source)                                                                                                                                            ) map (Json.unpackFromString[{def identifier: ScriptIdentifier}](_).identifier)
      def removeScriptToEvaluateOnNewDocument(identifier: ScriptIdentifier):                                                                            Future[_]                            = sendMessageToTarget("Page.removeScriptToEvaluateOnNewDocument", Map("identifier"->identifier)                                                                                                                                    )
      def setAutoAttachToCreatedPages        (autoAttach: Boolean):                                                                                     Future[_]                            = sendMessageToTarget("Page.setAutoAttachToCreatedPages",         Map("autoAttach"->autoAttach)                                                                                                                                    )
      def setLifecycleEventsEnabled          (enabled: Boolean):                                                                                        Future[_]                            = sendMessageToTarget("Page.setLifecycleEventsEnabled",           Map("enabled"->enabled)                                                                                                                                          )
      def reload                             (ignoreCache: Boolean=false, scriptToEvaluateOnLoad: Option[String]=None):                                 Future[_]                            = sendMessageToTarget("Page.reload",                              Map.empty ++ (if (ignoreCache) List("ignoreCache"->true) else Nil) ++ scriptToEvaluateOnLoad.map("scriptToEvaluateOnLoad"->_)                                    )
      def setAdBlockingEnabled               (enabled: Boolean):                                                                                        Future[_]                            = sendMessageToTarget("Page.setAdBlockingEnabled",                Map("enabled"->enabled)                                                                                                                                          )
      def navigate                           (url: String, referrer: Option[String]=None, transitionType: Option[TransitionType]=None):                 Future[Page.NavigateResult]          = sendMessageToTarget("Page.navigate",                            Map("url"->url) ++ referrer.map("referrer"->_) ++ transitionType.map("transitionType"->_)                                                                        ) map (Json.unpackFromString[Page.NavigateResult](_))
      def stopLoading                        ():                                                                                                        Future[_]                            = sendMessageToTarget("Page.stopLoading",                         Map()                                                                                                                                                            )
      def getNavigationHistory               ():                                                                                                        Future[Page.NavigationHistoryResult] = sendMessageToTarget("Page.getNavigationHistory",                Map()                                                                                                                                                            ) map (Json.unpackFromString[Page.NavigationHistoryResult](_))
      def navigateToHistoryEntry             (entryId: Int):                                                                                            Future[_]                            = sendMessageToTarget("Page.navigateToHistoryEntry",              Map("entryId"->entryId)                                                                                                                                          )
      def getResourceTree                    ():                                                                                                        Future[FrameResourceTree]            = sendMessageToTarget("Page.getResourceTree",                     Map()                                                                                                                                                            ) map (Json.unpackFromString[{def frameTree: FrameResourceTree}](_).frameTree)
      def getFrameTree                       ():                                                                                                        Future[FrameTree]                    = sendMessageToTarget("Page.getFrameTree",                        Map()                                                                                                                                                            ) map (Json.unpackFromString[{def frameTree: FrameTree}](_).frameTree)
      def getResourceContent                 (frameId: FrameId, url: String):                                                                           Future[Array[Byte]]                  = sendMessageToTarget("Page.getResourceContent",                  Map("frameId"->frameId, "url"->url)                                                                                                                              ) map { json => val r= Json.unpackFromString[{def body: String; def base64Encoded: Boolean}](json); if (r.base64Encoded) Base64.decodeBase64(r.body) else r.body.getBytes("UTF-8") }
      def searchInResource                   (frameId: FrameId, url: String, query: String, caseSensitive: Boolean=false, isRegex: Boolean=false):      Future[Vector[SearchMatch]]          = sendMessageToTarget("Page.searchInResource",                    Map("frameId"->frameId, "url"->url, "query"->query) ++ (if (caseSensitive) List("caseSensitive"->true) else Nil) ++ (if (isRegex) List("isRegex"->true) else Nil)) map (Json.unpackFromString[{def result: Vector[SearchMatch]}](_).result)
      def setDocumentContent                 (frameId: FrameId, html: String):                                                                          Future[_]                            = sendMessageToTarget("Page.setDocumentContent",                  Map("frameId"->frameId, "html"->html)                                                                                                                            )
      def captureScreenshot                  (format: String="png", quality: Option[Int]=None, clip: Option[Viewport]=None, fromSurface: Boolean=true): Future[Array[Byte]]                  = sendMessageToTarget("Page.captureScreenshot",                   Map("format"->format) ++ quality.map("quality"->_) ++ clip.map("clip"->_) ++ (if (fromSurface) Nil else List("fromSurface"->false))                              ) map (json => Base64.decodeBase64(Json.unpackFromString[{def data: String}](json).data))
      def printToPDF                         (landscape: Boolean=false, displayHeaderFooter: Boolean=false, printBackground: Boolean=false, scale: Option[Double]=None, paperWidth: Option[Double]=None, paperHeight: Option[Double]=None, marginTop: Option[Double]=None, marginBottom: Option[Double]=None, marginLeft: Option[Double]=None, marginRight: Option[Double]=None, pageRanges: Option[String]=None, ignoreInvalidPageRanges: Boolean=false):
                                                                                                                                                        Future[Array[Byte]]                  = sendMessageToTarget("Page.printToPDF",                          Map.empty ++ (if (landscape) List("landscape"->true) else Nil) ++ (if (displayHeaderFooter) List("displayHeaderFooter"->true) else Nil) ++ (if (printBackground) List("printBackground"->true) else Nil) ++ scale.map("scale"->_) ++ paperWidth.map("paperWidth"->_) ++ paperHeight.map("paperHeight"->_) ++ marginTop.map("marginTop"->_) ++ marginBottom.map("marginBottom"->_) ++ marginLeft.map("marginLeft"->_) ++ marginRight.map("marginRight"->_) ++ pageRanges.map("pageRanges"->_) ++ (if (ignoreInvalidPageRanges) List("ignoreInvalidPageRanges"->true) else Nil)
                                                                                                                                                                                                                                                                                                                                                                                                                                ) map (json => Base64.decodeBase64(Json.unpackFromString[{def data: String}](json).data))
      def startScreencast                    (format: String="png", quality: Option[Int]=None, maxWidth: Option[Int]=None, maxHeight: Option[Int]=None, everyNthFrame: Option[Int]=None):
                                                                                                                                                        Future[_]                            = sendMessageToTarget("Page.startScreencast",                     Map("format"->format) ++ quality.map("quality"->_) ++ maxWidth.map("maxWidth"->_) ++ maxHeight.map("maxHeight"->_) ++ everyNthFrame.map("everyNthFrame"->_)      )
      def stopScreencast                     ():                                                                                                        Future[_]                            = sendMessageToTarget("Page.stopScreencast",                      Map()                                                                                                                                                            )
      def screencastFrameAck                 (sessionId: Int):                                                                                          Future[_]                            = sendMessageToTarget("Page.screencastFrameAck",                  Map("sessionId"->sessionId)                                                                                                                                      )
      def handleJavaScriptDialog             (accept: Boolean, promptText: Option[String]):                                                             Future[_]                            = sendMessageToTarget("Page.handleJavaScriptDialog",              Map("accept"->accept) ++ promptText.map("promptText"->_)                                                                                                         )
      def getAppManifest                     ():                                                                                                        Future[Page.AppManifestResult]       = sendMessageToTarget("Page.getAppManifest",                      Map()                                                                                                                                                            ) map (Json.unpackFromString[Page.AppManifestResult](_))
      def requestAppBanner                   ():                                                                                                        Future[_]                            = sendMessageToTarget("Page.requestAppBanner",                    Map()                                                                                                                                                            )
      def getLayoutMetrics                   ():                                                                                                        Future[Page.LayoutMetricsResult]     = sendMessageToTarget("Page.getLayoutMetrics",                    Map()                                                                                                                                                            ) map (Json.unpackFromString[Page.LayoutMetricsResult](_))
      def createIsolatedWorld                (frameId: FrameId, worldName: Option[String], grantUniveralAccess: Boolean=false):                         Future[ExecutionContextId]           = sendMessageToTarget("Page.createIsolatedWorld",                 Map("frameId"->frameId) ++ worldName.map("worldName"->_) ++ (if (grantUniveralAccess) List("grantUniveralAccess"->true) else Nil)                                ) map (Json.unpackFromString[{def executionContextId: ExecutionContextId}](_).executionContextId)
      def bringToFront                       ():                                                                                                        Future[_]                            = sendMessageToTarget("Page.bringToFront",                        Map()                                                                                                                                                            )
      def setDownloadBehavior                (behavior: String, downloadPath: Option[String]):                                                          Future[_]                            = sendMessageToTarget("Page.setDownloadBehavior",                 Map("behavior"->behavior) ++ downloadPath.map("downloadPath"->_)                                                                                                 )
    }

    object Runtime {
      case class  executionContextCreated  (context: ExecutionContextDescription)                                                                                                                              extends SessionEvent
      case class  executionContextDestroyed(executionContextId: ExecutionContextId)                                                                                                                            extends SessionEvent
      case object executionContextsCleared                                                                                                                                                                     extends SessionEvent
      case class  exceptionThrown          (timestamp: Timestamp, exceptionDetails: ExceptionDetails)                                                                                                          extends SessionEvent
      case class  exceptionRevoked         (reason: String, exceptionId: Int)                                                                                                                                  extends SessionEvent
      case class  consoleAPICalled         (`type`: String, args: Vector[RemoteObject], executionContextId: ExecutionContextId, timestamp: Timestamp, stackTrace: Option[StackTrace], context: Option[String]) extends SessionEvent
      case class  inspectRequested         (`object`: RemoteObject, hints: Json.Keep)                                                                                                                          extends SessionEvent

      case class GetPropertiesResult(result: Vector[PropertyDescriptor], internalProperties: Vector[InternalPropertyDescriptor]=Vector.empty, exceptionDetails: Option[ExceptionDetails])
      case class CompileScriptResult(scriptId: Option[ScriptId], exceptionDetails: Option[ExceptionDetails])
      case class RunScriptResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails])

      def enable                         ():                                                    Future[_]                           = sendMessageToTarget("Runtime.enable" , Map())
      def disable                        ():                                                    Future[_]                           = sendMessageToTarget("Runtime.disable", Map())
      def evaluate                       (expression:             String,
                                          objectGroup:            Option[String]             = None,
                                          includeCommandLineAPI:  Option[Boolean]            = None,
                                          silent:                 Option[Boolean]            = None,
                                          contextId:              Option[ExecutionContextId] = None,
                                          returnByValue:          Option[Boolean]            = None,
                                          generatePreview:        Boolean                    = false,
                                          userGesture:            Boolean                    = false,
                                          awaitPromise:           Boolean                    = false
                                         ): Future[Runtime.RunScriptResult] =
        sendMessageToTarget("Runtime.evaluate", Map("expression"->expression, "generatePreview"->generatePreview, "userGesture"->userGesture, "awaitPromise"->awaitPromise) ++
                                                    objectGroup          .map("objectGroup"->_) ++
                                                    includeCommandLineAPI.map("includeCommandLineAPI"->_) ++
                                                    silent               .map("silent"->_) ++
                                                    contextId            .map("contextId"->_) ++
                                                    returnByValue        .map("returnByValue"->_)) map (Json.unpackFromString[Runtime.RunScriptResult](_))

      def awaitPromise                   (promiseObjectId:  RemoteObjectId,
                                          returnByValue:    Option[Boolean]=None,
                                          generatePreview:  Boolean=false):                     Future[Runtime.RunScriptResult]     = ???
      def callFunctionOn                 (functionDeclaration:  String,
                                          objectId:             Option[RemoteObjectId]=None,
                                          arguments:            Traversable[CallArgument]=Nil,
                                          silent:               Option[Boolean]=None,
                                          returnByValue:        Option[Boolean]=None,
                                          generatePreview:      Boolean=false,
                                          userGesture:          Boolean=false,
                                          awaitPromise:         Boolean=false,
                                          executionContextId:   Option[ExecutionContextId]=None,
                                          objectGroup:          Option[String]=None):           Future[Runtime.RunScriptResult]     = ???
      def getProperties                  (objectId:               RemoteObjectId,
                                          ownProperties:          Boolean=false,
                                          accessorPropertiesOnly: Boolean=false,
                                          generatePreview: Boolean=false):                      Future[Runtime.GetPropertiesResult] = ???
      def releaseObject                  (objectId: RemoteObjectId):                            Future[_]                           = ???
      def releaseObjectGroup             (objectGroup: String):                                 Future[_]                           = ???
      def runIfWaitingForDebugger        ():                                                    Future[_]                           = ???
      def discardConsoleEntries          ():                                                    Future[_]                           = ???
      def setCustomObjectFormatterEnabled(enabled: Boolean):                                    Future[_]                           = ???
      def compileScript                  (expression: String,
                                          sourceURL: String,
                                          persistScript: Boolean,
                                          executionContextId: Option[ExecutionContextId]=None): Future[Runtime.CompileScriptResult] = ???
      def runScript                      (scriptId: ScriptId,
                                          executionContextId: Option[ExecutionContextId]=None,
                                          objectGroup: Option[String]=None,
                                          silent: Option[Boolean]=None,
                                          includeCommandLineAPI: Option[Boolean]=None,
                                          returnByValue: Option[Boolean]=None,
                                          generatePreview: Boolean=false,
                                          awaitPromise: Boolean=false):                         Future[Runtime.RunScriptResult]     = ???
      def queryObjects                   (prototypeObjectId: RemoteObjectId):                   Future[RemoteObject]                = ???
      def globalLexicalScopeNames        (executionContextId: Option[ExecutionContextId]=None): Future[Vector[String]]              = ???
    }

    object SystemInfo {
      case class InfoResult(gpu: GPUInfo, modelName: String, modelVersion: String, commandLine: String)
      def getInfo(): Future[InfoResult] = sendMessageToTarget("SystemInfo.getInfo", Map()) map (Json.unpackFromString[InfoResult](_))
    }

    object Log {
      case class  entryAdded(entry: LogEntry) extends SessionEvent

      def enable():                                                     Future[_] = sendMessageToTarget("Log.enable" ,                Map())
      def disable():                                                    Future[_] = sendMessageToTarget("Log.disable",                Map())
      def clear():                                                      Future[_] = sendMessageToTarget("Log.clear",                  Map())
      def startViolationsReport(config: Traversable[ViolationSetting]): Future[_] = sendMessageToTarget("Log.startViolationsReport",  Map("config"->config))
      def stopViolationsReport():                                       Future[_] = sendMessageToTarget("Log.stopViolationsReport",   Map())
    }

    object Security {
      case class  securityStateChanged(securityState: String, schemeIsCryptographic: Boolean, explanations: Vector[SecurityStateExplanation], insecureContentStatus: InsecureContentStatus, summary: Option[String]) extends SessionEvent
      case class  certificateError    (eventId: Int, errorType: String, requestURL: String                                                                                                                         ) extends SessionEvent

      def enable                      ():                             Future[_] = sendMessageToTarget("Security.enable" ,                      Map())
      def disable                     ():                             Future[_] = sendMessageToTarget("Security.disable",                      Map())
      def handleCertificateError      (eventId: Int, action: String): Future[_] = sendMessageToTarget("Security.handleCertificateError",       Map("eventId"->eventId, "action"->action))
      def setOverrideCertificateErrors(`override`: Boolean):          Future[_] = sendMessageToTarget("Security.setOverrideCertificateErrors", Map("override"->`override`))
    }

    object Performance {
      case class  metrics(metrics: Vector[Metric], title: String) extends SessionEvent

      def enable    (): Future[_]              = sendMessageToTarget("Performance.enable" ,    Map())
      def disable   (): Future[_]              = sendMessageToTarget("Performance.disable",    Map())
      def getMetrics(): Future[Vector[Metric]] = sendMessageToTarget("Performance.getMetrics", Map()) map (Json.unpackFromString[{def metrics: Vector[Metric]}](_).metrics)
    }

    object DOM {
      case object documentUpdated                                                                          extends SessionEvent
      case class  setChildNodes          (parentId: NodeId, nodes: Vector[Node])                           extends SessionEvent
      case class  attributeModified      (nodeId: NodeId, name: String, value: String)                     extends SessionEvent
      case class  attributeRemoved       (nodeId: NodeId, name: String)                                    extends SessionEvent
      case class  inlineStyleInvalidated (nodeIds: Vector[NodeId])                                         extends SessionEvent
      case class  characterDataModified  (nodeId: NodeId, characterData: String)                           extends SessionEvent
      case class  childNodeCountUpdated  (nodeId: NodeId, childNodeCount: Int)                             extends SessionEvent
      case class  childNodeInserted      (parentNodeId: NodeId, previousNodeId: NodeId, node: Node)        extends SessionEvent
      case class  childNodeRemoved       (parentNodeId: NodeId, nodeId: NodeId)                            extends SessionEvent
      case class  shadowRootPushed       (hostId: NodeId, root: Node)                                      extends SessionEvent
      case class  shadowRootPopped       (hostId: NodeId, rootId: NodeId)                                  extends SessionEvent
      case class  pseudoElementAdded     (parentId: NodeId, pseudoElement: Node)                           extends SessionEvent
      case class  pseudoElementRemoved   (parentId: NodeId, pseudoElementId: NodeId)                       extends SessionEvent
      case class  distributedNodesUpdated(insertionPointId: NodeId, distributedNodes: Vector[BackendNode]) extends SessionEvent

      case class  getDocumentAnswer(root: Node) extends SessionEvent

      def enable                         ():                                                                                                              Future[_]              = sendMessageToTarget("DOM.enable" ,                         Map())
      def disable                        ():                                                                                                              Future[_]              = sendMessageToTarget("DOM.disable",                         Map())
      def getDocument                    (depth: Int=1, pierce: Boolean=false):                                                                           Future[Node]           = sendMessageGetEvent("DOM.getDocument",                     Map("depth"->depth, "pierce"->pierce), t => t.toOption.map(s => Json.unpackFromString[getDocumentAnswer](s))                            ) map (_.root)
      def getFlattenedDocument           (depth: Int=1, pierce: Boolean=false):                                                                           Future[Vector[Node]]   = sendMessageToTarget("DOM.getFlattenedDocument",            Map("depth"->depth, "pierce"->pierce)                                                                                                   ) map (Json.unpackFromString[{def nodes: Vector[Node]}](_).nodes)
      def collectClassNamesFromSubtree   (nodeId: NodeId):                                                                                                Future[Vector[String]] = sendMessageToTarget("DOM.collectClassNamesFromSubtree",    Map("nodeId"->nodeId)                                                                                                                   ) map (Json.unpackFromString[{def classNames: Vector[String]}](_).classNames)
      def requestChildNodes              (nodeId: NodeId, depth: Int=1, pierce: Boolean=false):                                                           Future[_]              = sendMessageToTarget("DOM.requestChildNodes",               Map("nodeId"->nodeId, "depth"->depth, "pierce"->pierce)                                                                                 )
      def querySelector                  (nodeId: NodeId, selector: String):                                                                              Future[NodeId]         = sendMessageToTarget("DOM.querySelector",                   Map("nodeId"->nodeId, "selector"->selector)                                                                                             ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def querySelectorAll               (nodeId: NodeId, selector: String):                                                                              Future[Vector[NodeId]] = sendMessageToTarget("DOM.querySelectorAll",                Map("nodeId"->nodeId, "selector"->selector)                                                                                             ) map (Json.unpackFromString[{def nodeIds: Vector[NodeId]}](_).nodeIds)
      def setNodeName                    (nodeId: NodeId, name: String):                                                                                  Future[NodeId]         = sendMessageToTarget("DOM.setNodeName",                     Map("nodeId"->nodeId, "name"->name)                                                                                                     ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def setNodeValue                   (nodeId: NodeId, value: String):                                                                                 Future[_]              = sendMessageToTarget("DOM.setNodeValue",                    Map("nodeId"->nodeId, "value"->value)                                                                                                   )
      def removeNode                     (nodeId: NodeId):                                                                                                Future[_]              = sendMessageToTarget("DOM.removeNode",                      Map("nodeId"->nodeId)                                                                                                                   )
      def setAttributeValue              (nodeId: NodeId, name: String, value: String):                                                                   Future[String]         = sendMessageToTarget("DOM.setAttributeValue",               Map("nodeId"->nodeId, "name"->name, "value"->value)                                                                                     )
      def setAttributesAsText            (nodeId: NodeId, text: String, name: Option[String]=None):                                                       Future[String]         = sendMessageToTarget("DOM.setAttributesAsText",             Map("nodeId"->nodeId, "text"->text) ++ name.map("name"->_)                                                                              )
      def removeAttribute                (nodeId: NodeId, name: String):                                                                                  Future[_]              = sendMessageToTarget("DOM.removeAttribute",                 Map("nodeId"->nodeId, "name"->name)                                                                                                     )
      def getOuterHTML                   (nodeId: Option[NodeId]=None, backendNodeId: Option[BackendNodeId]=None, objectId: Option[RemoteObjectId]=None): Future[String]         = sendMessageToTarget("DOM.getOuterHTML",                    Map.empty ++ nodeId.map("nodeId"->_) ++ backendNodeId.map("backendNodeId"->_) ++ objectId.map("objectId"->_)                            ) map (Json.unpackFromString[{def outerHTML: String}](_).outerHTML)
      def setOuterHTML                   (nodeId: NodeId, outerHTML: String):                                                                             Future[_]              = sendMessageToTarget("DOM.setOuterHTML",                    Map("nodeId"->nodeId, "outerHTML"->outerHTML)                                                                                           )
      def performSearch                  (query: String, includeUserAgentShadowDOM: Boolean=false):                                                       Future[(String, Int)]  = sendMessageToTarget("DOM.performSearch",                   Map("query"->query, "includeUserAgentShadowDOM"->includeUserAgentShadowDOM)                                                             ) map (Json.unpackFromString[{def searchId: String; def resultCount: Int}](_).tupled)
      def getSearchResults               (searchId: String, fromIndex: Int, toIndex: Int):                                                                Future[Vector[NodeId]] = sendMessageToTarget("DOM.getSearchResults",                Map("searchId"->searchId, "fromIndex"->fromIndex, "toIndex"->toIndex)                                                                   ) map (Json.unpackFromString[{def nodeIds: Vector[NodeId]}](_).nodeIds)
      def discardSearchResults           (searchId: String):                                                                                              Future[_]              = sendMessageToTarget("DOM.discardSearchResults",            Map("searchId"->searchId)                                                                                                               )
      def requestNode                    (objectId: RemoteObjectId):                                                                                      Future[NodeId]         = sendMessageToTarget("DOM.requestNode",                     Map("objectId"->objectId)                                                                                                               ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def pushNodeByPathToFrontend       (path: String):                                                                                                  Future[NodeId]         = sendMessageToTarget("DOM.pushNodeByPathToFrontend",        Map("path"->path)                                                                                                                       ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def pushNodesByBackendIdsToFrontend(backendNodeIds: Traversable[BackendNodeId]):                                                                    Future[Vector[NodeId]] = sendMessageToTarget("DOM.pushNodesByBackendIdsToFrontend", Map("backendNodeIds"->backendNodeIds)                                                                                                   ) map (Json.unpackFromString[{def nodeIds: Vector[NodeId]}](_).nodeIds)
      def setInspectedNode               (nodeId: NodeId):                                                                                                Future[_]              = sendMessageToTarget("DOM.setInspectedNode",                Map("nodeId"->nodeId)                                                                                                                   )
      def resolveNode                    (nodeId: Option[NodeId]=None, backendNodeId: Option[BackendNodeId]=None, objectGroup: Option[String]=None):      Future[RemoteObject]   = sendMessageToTarget("DOM.resolveNode",                     Map.empty ++ nodeId.map("nodeId"->_) ++ backendNodeId.map("backendNodeId"->_) ++ objectGroup.map("objectGroup"->_)                      ) map (Json.unpackFromString[{def `object`: RemoteObject}](_).`object`)
      def getAttributes                  (nodeId: NodeId):                                                                                                Future[Vector[String]] = sendMessageToTarget("DOM.collectClassNamesFromSubtree",    Map("nodeId"->nodeId)                                                                                                                   ) map (Json.unpackFromString[{def attributes: Vector[String]}](_).attributes) // .sliding(2,2).map{case Vector(k,v)=>k->v}.toMap
      def copyTo                         (nodeId: NodeId, targetNodeId: NodeId, insertBeforeNodeId: Option[NodeId]=None):                                 Future[NodeId]         = sendMessageToTarget("DOM.copyTo",                          Map("nodeId"->nodeId, "targetNodeId"->targetNodeId) ++ insertBeforeNodeId.map("insertBeforeNodeId"->_)                                  ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def moveTo                         (nodeId: NodeId, targetNodeId: NodeId, insertBeforeNodeId: Option[NodeId]=None):                                 Future[NodeId]         = sendMessageToTarget("DOM.moveTo",                          Map("nodeId"->nodeId, "targetNodeId"->targetNodeId) ++ insertBeforeNodeId.map("insertBeforeNodeId"->_)                                  ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def undo                           ():                                                                                                              Future[_]              = sendMessageToTarget("DOM.undo",                            Map()                                                                                                                                   )
      def redo                           ():                                                                                                              Future[_]              = sendMessageToTarget("DOM.redo",                            Map()                                                                                                                                   )
      def markUndoableState              ():                                                                                                              Future[_]              = sendMessageToTarget("DOM.markUndoableState",               Map()                                                                                                                                   )
      def focus                          (nodeId: Option[NodeId]=None, backendNodeId: Option[BackendNodeId]=None, objectId: Option[RemoteObjectId]=None): Future[_]              = sendMessageToTarget("DOM.focus",                           Map.empty ++ nodeId.map("nodeId"->_) ++ backendNodeId.map("backendNodeId"->_) ++ objectId.map("objectId"->_)      )
      def setFileInputFiles              (files: Traversable[String],
                                          nodeId: Option[NodeId]=None, backendNodeId: Option[BackendNodeId]=None, objectId: Option[RemoteObjectId]=None): Future[_]              = sendMessageToTarget("DOM.setFileInputFiles",               Map("files"->files) ++ nodeId.map("nodeId"->_) ++ backendNodeId.map("backendNodeId"->_) ++ objectId.map("objectId"->_))
      def getBoxModel                    (nodeId: Option[NodeId]=None, backendNodeId: Option[BackendNodeId]=None, objectId: Option[RemoteObjectId]=None): Future[BoxModel]       = sendMessageToTarget("DOM.getBoxModel",                     Map.empty ++ nodeId.map("nodeId"->_) ++ backendNodeId.map("backendNodeId"->_) ++ objectId.map("objectId"->_)                            ) map (Json.unpackFromString[{def model: BoxModel}](_).model)
      def getNodeForLocation             (x: Int, y: Int, includeUserAgentShadowDOM: Boolean=false):                                                      Future[NodeId]         = sendMessageToTarget("DOM.getNodeForLocation",              Map("x"->x, "y"->y, "includeUserAgentShadowDOM"->includeUserAgentShadowDOM)                                                             ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def getRelayoutBoundary            (nodeId: NodeId):                                                                                                Future[NodeId]         = sendMessageToTarget("DOM.getRelayoutBoundary",             Map("nodeId"->nodeId)                                                                                                                   ) map (Json.unpackFromString[{def nodeId: NodeId}](_).nodeId)
      def describeNode                   (nodeId: Option[NodeId]=None, backendNodeId: Option[BackendNodeId]=None, objectId: Option[RemoteObjectId]=None,
                                          depth: Int=1, pierce: Boolean=false):                                                                           Future[Node]           = sendMessageToTarget("DOM.getDocument",                     Map("depth"->depth, "pierce"->pierce) ++ nodeId.map("nodeId"->_) ++ backendNodeId.map("backendNodeId"->_) ++ objectId.map("objectId"->_)) map (Json.unpackFromString[{def node: Node}](_).node)
    }

    object DOMSnapshot {
      case class getSnapshotAnswer(domNodes: Vector[DOMNode], layoutTreeNodes: Vector[LayoutTreeNode], computedStyles: Vector[ComputedStyle]) extends SessionEvent

      def getSnapshot(computedStyleWhitelist: Traversable[String]): Future[getSnapshotAnswer] = sendMessageGetEvent("DOMSnapshot.getSnapshot", Map("computedStyleWhitelist"->computedStyleWhitelist), _.toOption.map(Json.unpackFromString[getSnapshotAnswer](_)))
    }

    object DOMDebugger {
      def setDOMBreakpoint               () = ???
      def removeDOMBreakpoint            () = ???
      def setEventListenerBreakpoint     () = ???
      def removeEventListenerBreakpoint  () = ???
      def setInstrumentationBreakpoint   () = ???
      def removeInstrumentationBreakpoint() = ???
      def setXHRBreakpoint               () = ???
      def removeXHRBreakpoint            () = ???
      def getEventListeners              () = ???
    }

    object CSS {
      case object mediaQueryResultChanged                         extends SessionEvent
      case object fontsUpdated                                    extends SessionEvent
      case class  styleSheetChanged(styleSheetId: StyleSheetId)   extends SessionEvent
      case class  styleSheetAdded  (header: CSSStyleSheetHeader)  extends SessionEvent
      case class  styleSheetRemoved(styleSheetId: StyleSheetId)   extends SessionEvent

      case class matchedStylesForNodeAnswer (nodeId: NodeId, msfn: Try[MatchedStylesForNode])             extends SessionEvent
      case class inlineStylesForNodeAnswer  (nodeId: NodeId, isfn: Try[InlineStylesForNode])              extends SessionEvent
      case class computedStylesForNodeAnswer(nodeId: NodeId, csfn: Try[Vector[CSSComputedStyleProperty]]) extends SessionEvent
      case class platformFontsForNodeAnswer (nodeId: NodeId, pffn: Try[Vector[PlatformFontUsage]])        extends SessionEvent

      def enable                          ():                                                                    Future[_]                                = sendMessageToTarget("CSS.enable" ,                          Map()                                                                        )
      def disable                         ():                                                                    Future[_]                                = sendMessageToTarget("CSS.disable",                          Map()                                                                        )
      def getMatchedStylesForNode         (nodeId: NodeId):                                                      Future[MatchedStylesForNode]             = sendMessageGetEvent("CSS.getMatchedStylesForNode",          Map("nodeId"->nodeId), t => Some(matchedStylesForNodeAnswer (nodeId, t map { s => Json.unpackFromString[MatchedStylesForNode                                 ](s)              }))) map (_.msfn.get)
      def getInlineStylesForNode          (nodeId: NodeId):                                                      Future[InlineStylesForNode]              = sendMessageGetEvent("CSS.getInlineStylesForNode",           Map("nodeId"->nodeId), t => Some(inlineStylesForNodeAnswer  (nodeId, t map { s => Json.unpackFromString[InlineStylesForNode                                  ](s)              }))) map (_.isfn.get)
      def getComputedStyleForNode         (nodeId: NodeId): /* returns huge list (337..378 styles) */            Future[Vector[CSSComputedStyleProperty]] = sendMessageGetEvent("CSS.getComputedStyleForNode",          Map("nodeId"->nodeId), t => Some(computedStylesForNodeAnswer(nodeId, t map { s => Json.unpackFromString[{def computedStyle: Vector[CSSComputedStyleProperty]}](s).computedStyle}))) map (_.csfn.get)
      def getPlatformFontsForNode         (nodeId: NodeId):                                                      Future[Vector[PlatformFontUsage]]        = sendMessageGetEvent("CSS.getPlatformFontsForNode",          Map("nodeId"->nodeId), t => Some(platformFontsForNodeAnswer (nodeId, t map { s => Json.unpackFromString[{def fonts:         Vector[PlatformFontUsage       ]}](s).fonts        }))) map (_.pffn.get)
      def getStyleSheetText               (styleSheetId: StyleSheetId):                                          Future[String]                           = sendMessageToTarget("CSS.getStyleSheetText",                Map("styleSheetId"->styleSheetId)                                            ) map (Json.unpackFromString[{def text: String}](_).text)
      def collectClassNames               (styleSheetId: StyleSheetId):                                          Future[Vector[String]]                   = sendMessageToTarget("CSS.collectClassNames",                Map("styleSheetId"->styleSheetId)                                            ) map (Json.unpackFromString[{def classNames: Vector[String]}](_).classNames)
      def setStyleSheetText               (styleSheetId: StyleSheetId, text: String):                            Future[Option[String]]                   = sendMessageToTarget("CSS.setStyleSheetText",                Map("styleSheetId"->styleSheetId, "text"->text)                              ) map (Json.unpackFromString[{def sourceMapURL: Option[String]}](_).sourceMapURL)
      def setRuleSelector                 (styleSheetId: StyleSheetId, range: SourceRange, selector: String):    Future[SelectorList]                     = sendMessageToTarget("CSS.setRuleSelector",                  Map("styleSheetId"->styleSheetId, "range"->range, "selector"->selector)      ) map (Json.unpackFromString[{def selectorList: SelectorList}](_).selectorList)
      def setKeyframeKey                  (styleSheetId: StyleSheetId, range: SourceRange, keyText: String):     Future[Value]                            = sendMessageToTarget("CSS.setKeyframeKey",                   Map("styleSheetId"->styleSheetId, "range"->range, "keyText"->keyText)        ) map (Json.unpackFromString[{def keyText: Value}](_).keyText)
      def setStyleTexts                   (edits: Array[StyleDeclarationEdit]):                                  Future[Vector[CSSStyle]]                 = sendMessageToTarget("CSS.setStyleTexts",                    Map("edits"->edits)                                                          ) map (Json.unpackFromString[{def styles: Vector[CSSStyle]}](_).styles)
      def setMediaText                    (styleSheetId: StyleSheetId, range: SourceRange, text: String):        Future[CSSMedia]                         = sendMessageToTarget("CSS.setMediaText",                     Map("styleSheetId"->styleSheetId, "range"->range, "text"->text)              ) map (Json.unpackFromString[{def media: CSSMedia}](_).media)
      def createStyleSheet                (frameId: FrameId):                                                    Future[StyleSheetId]                     = sendMessageToTarget("CSS.createStyleSheet",                 Map("frameId"->frameId)                                                      ) map (Json.unpackFromString[{def styleSheetId: StyleSheetId}](_).styleSheetId)
      def addRule                         (styleSheetId: StyleSheetId, ruleText: String, location: SourceRange): Future[CSSRule]                          = sendMessageToTarget("CSS.addRule",                          Map("styleSheetId"->styleSheetId, "ruleText"->ruleText, "location"->location)) map (Json.unpackFromString[{def rule: CSSRule}](_).rule)
      def forcePseudoState                (nodeId: NodeId, forcedPseudoClasses: Traversable[String]):            Future[_]                                = sendMessageToTarget("CSS.forcePseudoState",                 Map("nodeId"->nodeId, "forcedPseudoClasses"->forcedPseudoClasses)            )
      def getMediaQueries                 (medias: Vector[CSSMedia]):                                            Future[Vector[CSSMedia]]                 = sendMessageToTarget("CSS.getMediaQueries",                  Map("medias"->medias)                                                        ) map (Json.unpackFromString[{def medias: Vector[CSSMedia]}](_).medias)
      def setEffectivePropertyValueForNode(nodeId: NodeId, propertyName: String, value: String):                 Future[_]                                = sendMessageToTarget("CSS.setEffectivePropertyValueForNode", Map("nodeId"->nodeId)                                                        )
/*ev*/def getBackgroundColors             (nodeId: NodeId):                                                      Future[BackgroundColors]                 = sendMessageToTarget("CSS.getBackgroundColors",              Map("nodeId"->nodeId)                                                        ) map (Json.unpackFromString[BackgroundColors](_))
      def startRuleUsageTracking          ():                                                                    Future[_]                                = sendMessageToTarget("CSS.startRuleUsageTracking",           Map()                                                                        )
      def takeCoverageDelta               ():                                                                    Future[Vector[RuleUsage]]                = sendMessageToTarget("CSS.takeCoverageDelta",                Map()                                                                        ) map (Json.unpackFromString[{def coverage: Vector[RuleUsage]}](_).coverage)
      def stopRuleUsageTracking           ():                                                                    Future[Vector[RuleUsage]]                = sendMessageToTarget("CSS.stopRuleUsageTracking",            Map()                                                                        ) map (Json.unpackFromString[{def ruleUsage: Vector[RuleUsage]}](_).ruleUsage)
    }

    object Input {
      def setIgnoreInputEvents    (ignore: Boolean): Future[_] = sendMessageToTarget("Input.setIgnoreInputEvents", Map("ignore"->ignore))
      def dispatchKeyEvent       (`type`:                String,                           // keyDown | keyUp | rawKeyDown | char
                                  modifiers:             Int                    = 0,       // Alt=1, Ctrl=2, Meta/Command=4, Shift=8
                                  timestamp:             Option[TimeSinceEpoch] = None,
                                  text:                  String                 = "",
                                  unmodifiedText:        String                 = "",
                                  keyIdentifier:         String                 = "",
                                  code:                  String                 = "",
                                  key:                   String                 = "",
                                  windowsVirtualKeyCode: Int                    = 0,
                                  nativeVirtualKeyCode:  Int                    = 0,
                                  autoRepeat:            Boolean                = false,
                                  isKeypad:              Boolean                = false,
                                  isSystemKey:           Boolean                = false,
                                  location:              Int                    = 0
                                 ): Future[_] =
        sendMessageToTarget("Input.dispatchKeyEvent",         Map( "type"                  -> `type`
                                                                 , "modifiers"             -> modifiers
                                                                 , "text"                  -> text
                                                                 , "unmodifiedText"        -> unmodifiedText
                                                                 , "keyIdentifier"         -> keyIdentifier
                                                                 , "code"                  -> code
                                                                 , "key"                   -> key
                                                                 , "windowsVirtualKeyCode" -> windowsVirtualKeyCode
                                                                 , "nativeVirtualKeyCode"  -> nativeVirtualKeyCode
                                                                 , "autoRepeat"            -> autoRepeat
                                                                 , "isKeypad"              -> isKeypad
                                                                 , "isSystemKey"           -> isSystemKey
                                                                 , "location"              -> location
                                                                 ) ++ timestamp.map("timestamp" -> _))

      def dispatchMouseEvent     (`type`:                String,                           // mousePressed | mouseReleased | mouseMoved | mouseWheel
                                  x:                     Int,                              // coordinate of the event relative to the main frame's viewport in CSS pixels.
                                  y:                     Int,
                                  modifiers:             Int                    = 0,       // Alt=1, Ctrl=2, Meta/Command=4, Shift=8
                                  timestamp:             Option[TimeSinceEpoch] = None,
                                  button:                String                 = "none",  // none | left | middle | right
                                  clickCount:            Int                    = 0,
                                  deltaX:                Double                 = 0,
                                  deltaY:                Double                 = 0) = ???
      def dispatchTouchEvent     (`type`:                String,                           // touchStart | touchEnd | touchMove | touchCancel
                                  touchPoints:           Traversable[TouchPoint],          // TouchEnd and TouchCancel must not contain any touch points, while TouchStart and TouchMove must contains at least one
                                  modifiers:             Int                    = 0,       // Alt=1, Ctrl=2, Meta/Command=4, Shift=8
                                  timestamp:             Option[TimeSinceEpoch] = None) = ???
      def synthesizeScrollGesture(x:                     Double, // X coordinate of the start of the gesture in CSS pixels.
                                  y:                     Double,
                                  xDistance:             Option[Double] = None,     // The distance to scroll along the X axis (positive to scroll left).
                                  yDistance:             Option[Double] = None,     // The distance to scroll along the Y axis (positive to scroll up).
                                  xOverscroll:           Option[Double] = None,     // The number of additional pixels to scroll back along the X axis, in addition to the given distance.
                                  yOverscroll:           Option[Double] = None,
                                  preventFling:          Boolean        = true,
                                  speed:                 Int            = 800,
                                  gestureSourceType:     String         = "default", // :GestureSourceType(default|touch|mouse). Which type of input events to be generated (which queries the platform for the preferred input type).
                                  repeatCount:           Int            = 0,         // The number of times to repeat the gesture (default: 0).
                                  repeatDelayMs:         Int            = 250,       // The number of milliseconds delay between each repeat.
                                  interactionMarkerName: String         = ""         // The name of the interaction markers to generate, if not empty
                                 ): Future[_] =
        sendMessageToTarget("Input.synthesizeScrollGesture",  Map( "x"->x
                                                                 , "y"->y
                                                                 , "preventFling"          -> preventFling
                                                                 , "speed"                 -> speed
                                                                 , "gestureSourceType"     -> gestureSourceType
                                                                 , "repeatCount"           -> repeatCount
                                                                 , "repeatDelayMs"         -> repeatDelayMs
                                                                 , "interactionMarkerName" -> interactionMarkerName
                                                                 ) ++ xDistance.map("xDistance"->_) ++
                                                                      yDistance.map("yDistance"->_) ++
                                                                      xOverscroll.map("xOverscroll"->_) ++
                                                                      yOverscroll.map("yOverscroll"->_))

    }

    object Console {
      case class  messageAdded(message: ConsoleMessage) extends SessionEvent
      def enable():          Future[_] = sendMessageToTarget("Console.enable" ,       Map())
      def disable():         Future[_] = sendMessageToTarget("Console.disable",       Map())
      def clearMessages():   Future[_] = sendMessageToTarget("Console.clearMessages", Map())
    }

    object Debugger {
      case class  scriptParsed       (scriptId: String, url: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int,
                                      executionContextId: ExecutionContextId, hash: String, executionContextAuxData: Option[Json.Keep],
                                      isLiveEdit: Boolean=false, sourceMapURL: Option[String], hasSourceURL: Boolean=false, isModule: Boolean=false, length: Option[Int], stackTrace: Option[StackTrace])                           extends SessionEvent
      case class  scriptFailedToParse(scriptId: String, url: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int,
                                      executionContextId: ExecutionContextId, hash: String, executionContextAuxData: Option[Json.Keep],
                                                                 sourceMapURL: Option[String], hasSourceURL: Boolean=false, isModule: Boolean=false, length: Option[Int], stackTrace: Option[StackTrace])                           extends SessionEvent
      case class  breakpointResolved (breakpointId: BreakpointId, location: Location)                                                                                                                                               extends SessionEvent
      case class  paused             (callFrames: Vector[/*DebuggerCallFrame*/Json.Keep], reason: String, data: Option[Json.Keep], hitBreakpoints: Vector[String] = Vector.empty, asyncStackTrace: Option[StackTrace], scheduledAsyncTaskId: Option[AsyncTaskId]) extends SessionEvent
      case object resumed                                                                                                                                                                                                           extends SessionEvent

      def enable                ():                                                                  Future[_]              = sendMessageToTarget("Debugger.enable" , Map())
      def disable               ():                                                                  Future[_]              = sendMessageToTarget("Debugger.disable", Map())
      def setBreakpointsActive  () = ???
      def setSkipAllPauses      () = ???
      def setBreakpointByUrl    () = ???
      def setBreakpoint         () = ???
      def removeBreakpoint      () = ???
      def getPossibleBreakpoints() = ???
      def continueToLocation    () = ???
      def pauseOnAsyncTask      () = ???
      def stepOver              () = ???
      def stepInto              () = ???
      def stepOut               () = ???
      def pause                 () = ???
      def scheduleStepIntoAsync () = ???
      def resume                () = ???
      def searchInContent       () = ???
      def setScriptSource       () = ???
      def restartFrame          () = ???
      def getScriptSource       () = ???
      def setPauseOnExceptions  () = ???
      def evaluateOnCallFrame   () = ???
      def setVariableValue      () = ???
      def setReturnValue        () = ???
      def setAsyncCallStackDepth() = ???
      def setBlackboxPatterns   () = ???
      def setBlackboxedRanges   () = ???
    }

    object Network {
      case class resourceChangedPriority           (requestId: RequestId, newPriority: String, timestamp: MonotonicTime)                                                                            extends SessionEvent
      case class requestWillBeSent                 (requestId: RequestId, loaderId: LoaderId, documentURL: String, request: Request, timestamp: MonotonicTime, wallTime: TimeSinceEpoch, initiator: Initiator, redirectResponse: Option[Response], `type`: Option[ResourceType], frameId: Option[FrameId]) extends SessionEvent
      case class requestServedFromCache            (requestId: RequestId)                                                                                                                           extends SessionEvent
      case class responseReceived                  (requestId: RequestId, loaderId: LoaderId, timestamp: MonotonicTime, `type`: ResourceType, response: Response, frameId: Option[FrameId])         extends SessionEvent
      case class dataReceived                      (requestId: RequestId, timestamp: MonotonicTime, dataLength: Int, encodedDataLength: Int)                                                        extends SessionEvent
      case class loadingFinished                   (requestId: RequestId, timestamp: MonotonicTime, encodedDataLength: Int)                                                                         extends SessionEvent
      case class loadingFailed                     (requestId: RequestId, timestamp: MonotonicTime, `type`: ResourceType, errorText: String, canceled: Boolean=false, blockedReason: Option[String])extends SessionEvent
      case class webSocketWillSendHandshakeRequest (requestId: RequestId, timestamp: MonotonicTime, wallTime: TimeSinceEpoch, request: Json.Keep/*WebSocketRequest*/)                               extends SessionEvent
      case class webSocketHandshakeResponseReceived(requestId: RequestId, timestamp: MonotonicTime, response: Json.Keep/*WebSocketResponse*/)                                                       extends SessionEvent
      case class webSocketCreated                  (requestId: RequestId, url: String, initiator: Option[Initiator])                                                                                extends SessionEvent
      case class webSocketClosed                   (requestId: RequestId, timestamp: MonotonicTime)                                                                                                 extends SessionEvent
      case class webSocketFrameReceived            (requestId: RequestId, timestamp: MonotonicTime, response: Json.Keep/*WebSocketFrame*/)                                                          extends SessionEvent
      case class webSocketFrameError               (requestId: RequestId, timestamp: MonotonicTime, errorMessage: String)                                                                           extends SessionEvent
      case class webSocketFrameSent                (requestId: RequestId, timestamp: MonotonicTime, response: Json.Keep/*WebSocketFrame*/)                                                          extends SessionEvent
      case class eventSourceMessageReceived        (requestId: RequestId, timestamp: MonotonicTime, eventName: String, eventId: String, data: String)                                               extends SessionEvent
      case class requestIntercepted                (interceptionId: InterceptionId, request: Request, frameId: FrameId, resourceType: ResourceType, isNavigationRequest: Boolean, redirectHeaders: Option[Map[String,String]], redirectStatusCode: Option[Int], redirectUrl: Option[String], authChallenge: Option[Json.Keep/*AuthChallenge*/]) extends SessionEvent

      def enable                     ():                                        Future[_]              = sendMessageToTarget("Network.enable" ,                     Map())
      def disable                    ():                                        Future[_]              = sendMessageToTarget("Network.disable",                     Map())
      def setUserAgentOverride       (ua: String):                              Future[_]              = sendMessageToTarget("Network.setUserAgentOverride",        Map("userAgent"->ua))
      def setExtraHTTPHeaders        (headers: Map[String,String]):             Future[_]              = sendMessageToTarget("Network.setExtraHTTPHeaders",         Map("headers"->headers))
      def setBlockedURLs             (urls: Traversable[String]):               Future[_]              = sendMessageToTarget("Network.setBlockedURLs",              Map("urls"->urls))
      def replayXHR                  (requestId: RequestId):                    Future[_]              = sendMessageToTarget("Network.replayXHR",                   Map("requestId"->requestId))
      def clearBrowserCache          ():                                        Future[_]              = sendMessageToTarget("Network.clearBrowserCache",           Map())
      def clearBrowserCookies        ():                                        Future[_]              = sendMessageToTarget("Network.clearBrowserCookies",         Map())
      def deleteCookies              (name: String, url: Option[String]=None, domain: Option[String]=None, path: Option[String]=None):
                                                                                Future[_]              = sendMessageToTarget("Network.deleteCookies",               Map("name"->name) ++ url.map("url"->_) ++ domain.map("domain"->_) ++ path.map("path"->_))
      def emulateNetworkConditions   (offline: Boolean, latency: Double, downloadThroughput: Double, uploadThroughput: Double, connectionType: Option[String]):
                                                                                Future[_]              = sendMessageToTarget("Network.emulateNetworkConditions",    Map("offline"->offline, "latency"->latency, "downloadThroughput"->downloadThroughput, "uploadThroughput"->uploadThroughput) ++ connectionType.map("connectionType"->_))
      def setCacheDisabled           (cacheDisabled: Boolean):                  Future[_]              = sendMessageToTarget("Network.setCacheDisabled",            Map("cacheDisabled"->cacheDisabled))
      def setBypassServiceWorker     (bypass: Boolean):                         Future[_]              = sendMessageToTarget("Network.setBypassServiceWorker",      Map("bypass"->bypass))
      def setDataSizeLimitsForTest   (maxTotalSize: Int, maxResourceSize: Int): Future[_]              = sendMessageToTarget("Network.setDataSizeLimitsForTest",    Map("maxTotalSize"->maxTotalSize, "maxResourceSize"->maxResourceSize))
      def getResponseBody            (requestId: RequestId):                    Future[Array[Byte]]    = sendMessageToTarget("Network.getResponseBody",             Map("requestId"->requestId)                   ) map { json => val r=Json.unpackFromString[{def body: String; def base64Encoded: Boolean}](json); if (r.base64Encoded) Base64.decodeBase64(r.body) else r.body.getBytes("UTF-8") }
      def canClearBrowserCache       ():                                        Future[Boolean]        = sendMessageToTarget("Network.canClearBrowserCache",        Map()                                         ) map (Json.unpackFromString[{def result: Boolean           }](_).result     )
      def canClearBrowserCookies     ():                                        Future[Boolean]        = sendMessageToTarget("Network.canClearBrowserCookies",      Map()                                         ) map (Json.unpackFromString[{def result: Boolean           }](_).result     )
      def canEmulateNetworkConditions():                                        Future[Boolean]        = sendMessageToTarget("Network.canEmulateNetworkConditions", Map()                                         ) map (Json.unpackFromString[{def result: Boolean           }](_).result     )
      def getCertificate             (origin: String):                          Future[Vector[String]] = sendMessageToTarget("Network.getCertificate",              Map("origin"->origin)                         ) map (Json.unpackFromString[{def tableNames: Vector[String]}](_).tableNames )
      def getCookies                 (urls: Traversable[String]=Nil):           Future[Vector[Cookie]] = sendMessageToTarget("Network.getCookies",                  if (urls.isEmpty) Map() else Map("urls"->urls)) map (Json.unpackFromString[{def cookies: Vector[Cookie]   }](_).cookies    )
      def getAllCookies              ():                                        Future[Vector[Cookie]] = sendMessageToTarget("Network.getAllCookies",               Map()                                         ) map (Json.unpackFromString[{def cookies: Vector[Cookie]   }](_).cookies    )
      def setCookie                  (name: String, value: String, url: Option[String]=None, domain: Option[String]=None, path: Option[String]=None, secure: Boolean=false, httpOnly: Boolean=false, sameSite: Option[String]=None, expires: Option[TimeSinceEpoch]=None):
                                                                                Future[Boolean]        = sendMessageToTarget("Network.deleteCookies",               Map("name"->name, "value"->value) ++ url.map("url"->_) ++ domain.map("domain"->_) ++ path.map("path"->_) ++ (if (secure) List("secure"->true) else Nil) ++ (if (httpOnly) List("httpOnly"->true) else Nil) ++ sameSite.map("sameSite"->_) ++ expires.map("expires"->_)
                                                                                                                                                                                                                  ) map (Json.unpackFromString[{def success: Boolean}](_).success)

// <- serialize case class to json as dict with field names
//    def setCookies              (cookies: Traversable[CookieParam]):             Future[_] = sendMessageToTarget("Network.setCookies", "cookies"->cookies)
//    def setRequestInterception  (patterns: Traversable[RequestPattern]):         Future[_] = ???
//    def continueInterceptedRequest(interceptionId: InterceptionId, errorReason: Option[String]=None, rawResponse: Option[String]=None, url: Option[String]=None, method: Option[String]=None, postData: Option[String]=None, headers: Option[Map[String,String]]=None, authChallengeResponse: Option[AuthChallengeResponse]) = ???
    }

  }

  object Inspector {
    case class  detached(reason: String) extends CdpEvent
    case object targetCrashed            extends CdpEvent
//  def enable():          Future[_] = sendMessageToTarget("Inspector.enable" , Map())
//  def disable():         Future[_] = sendMessageToTarget("Inspector.disable", Map())
  }

  case class Target(targetId: String) {
    def attachToTarget():         Future[Session] = send("Target.attachToTarget", Map("targetId"->targetId)) map { json => Session(Json.unpackFromString[{def sessionId: String}](json).sessionId) }
  }

  def createTarget(url: String):  Future[Target]  = send("Target.createTarget",   Map("url"->url))           map { json => Target(Json.unpackFromString[{def targetId: String}](json).targetId) }
}
