#!/usr/bin/python
import sys
import os

if "/home/belka/qt4/lib/" in os.environ.get("LD_LIBRARY_PATH",""):
  # export LD_LIBRARY_PATH=/home/belka/qt4/lib/
  #  print "Init Precompiled qt4 version (WebSecurityEnabled patch)"
  sys.path = ["/home/belka/PyQt4/lib",] + sys.path[:]
  from PyQt4.QtCore import QT_VERSION_STR
  assert(QT_VERSION_STR == "4.8.7")

import argparse
from PyQt4.QtGui import QApplication,QImage,QPainter,qRgba,QRegion,QMouseEvent,QKeyEvent
from PyQt4.QtNetwork import QNetworkProxy,QNetworkAccessManager,QNetworkCookie, QNetworkCookieJar
import pickle

from PyQt4 import QtCore, QtGui, QtWebKit
from PyQt4.QtWebKit import QWebPage

from PyQt4.QtCore import QByteArray,QUrl,QObject,QThread,pyqtSignal,pyqtSlot,QSettings,QVariant,QSize,pyqtProperty,QPoint,QRect,QTimer,QEvent,Qt,QDateTime,QString
#from PyQt4.QtGui import *

import threading
import os, sys, signal
import time 


class ControllerPage(QWebPage):
    evaljsSignalSent = pyqtSignal(str)
    avaljsSignalSent = pyqtSignal(str)
    
    def __init__(self, parent,contentPage,args):
      super(ControllerPage, self).__init__(parent)
      self.phantom = parent
      self.contentPage = contentPage
      self.evaljsSignalSent.connect(self.onEval)
      self.avaljsSignalSent.connect(self.onAval)
      self.mainFrame().addToJavaScriptWindowObject('page', self)
      self.m_scrollPosition = QPoint()

      self.contentFrame = self.contentPage.mainFrame()

#
    #def shouldInterruptJavaScript(self):
    #    QApplication.processEvents(QEventLoop.AllEvents, 42)
    #    return False
#
    #def javaScriptAlert(self, originatingFrame, msg):
    #    self.parent().javaScriptAlertSent.emit(msg)
#
    def javaScriptConsoleMessage(self, message, lineNumber, sourceID):
      self.parent().javaScriptConsoleMessageSent.emit(message, lineNumber, sourceID)

    def onEval(self,data):
      a = unicode(data).split(" ",1)

      if ( len(a) != 2 ):
        self.phantom.error("EVAL format error len(a)=%d" % len(a))
      else:
        (eid,cmd) = a
        self.lastError = None
        javascript = "try { %s } catch(e) {page.setLastError(e);}" % cmd
        retVal = self.mainFrame().evaluateJavaScript(javascript).toPyObject()

        if self.lastError:
          self.phantom.errret(eid,self.lastError)
        else:
          self.phantom.ret(eid,retVal)

    def onAval(self,data):
      a = unicode(data).split(" ",1)

      if ( len(a) != 2 ):
        self.phantom.error("EVAL format error len(a)=%d" % len(a))
      else:
        (eid,cmd) = a
        self.lastError = None
        javascript = "try { var AVAL=function (eid){ %s };AVAL(%s); } catch(e) {page.setLastError(e);}" % (cmd,eid)
        retVal = self.mainFrame().evaluateJavaScript(javascript).toPyObject()
        if self.lastError:
          self.phantom.errret(eid,self.lastError)
        #else:
        #  self.phantom.ret(eid,retVal)

    
    @pyqtSlot(str, result='QVariant')
    def setLastError(self, code):
      self.lastError = code
      return None

    @pyqtSlot(str, result='QVariant')
    def render(self,fname):
      contentsSize = self.contentFrame.contentsSize()
      contentsSize -= QSize(self.m_scrollPosition.x(), self.m_scrollPosition.y())
      frameRect = QRect(QPoint(0, 0), contentsSize)
      #if not self.m_clipRect.isEmpty():
      #    frameRect = self.m_clipRect

      viewportSize = self.contentPage.viewportSize()
      self.contentPage.setViewportSize(contentsSize)

      image = QImage(frameRect.size(), QImage.Format_ARGB32)
      image.fill(qRgba(255, 255, 255, 0))

      painter = QPainter()

      # We use tiling approach to work-around Qt software rasterizer bug
      # when dealing with very large paint device.
      # See http://code.google.com/p/phantomjs/issues/detail?id=54.
      tileSize = 4096
      htiles = (image.width() + tileSize - 1) / tileSize
      vtiles = (image.height() + tileSize - 1) / tileSize
      for x in range(htiles):
          for y in range(vtiles):
              tileBuffer = QImage(tileSize, tileSize, QImage.Format_ARGB32)
              tileBuffer.fill(qRgba(255, 255, 255, 0))

              # Render the web page onto the small tile first
              painter.begin(tileBuffer)
              painter.setRenderHint(QPainter.Antialiasing, True)
              painter.setRenderHint(QPainter.TextAntialiasing, True)
              painter.setRenderHint(QPainter.SmoothPixmapTransform, True)
              painter.translate(-frameRect.left(), -frameRect.top())
              painter.translate(-x * tileSize, -y * tileSize)
              self.contentFrame.render(painter, QRegion(frameRect))
              painter.end()

              # Copy the tile to the main buffer
              painter.begin(image)
              painter.setCompositionMode(QPainter.CompositionMode_Source)
              painter.drawImage(x * tileSize, y * tileSize, tileBuffer)
              painter.end()

      self.contentPage.setViewportSize(viewportSize)

      image.save(fname)

      return True

    #@pyqtSlot(str, str,result='QVariant')

    @pyqtSlot(str,result='QVariant')
    def evaluateJavaScript(self, code):
      #args = []
      #for arg in arguments:
      #  arg = arg.toPyObject()
      #  tp = type(arg).__name__
      #  print arg,tp
      #  if tp == 'float':
      #    arg = unicode(arg)
      #  elif tp == "NoneType":
      #    arg = "null"
      #  elif tp == "QString":
      #    arg = unicode('"' + arg.replace("\"","\\\"") +'"')
      #  else:
      #    self.error("unkwnon type %s" % tp)
      #    self.lastError = "unkwnon type %s" % tp
      #    return
      #  args.append(arg)
      #
      #args = ",".join(args)

      function = '(%s)()' % ( code)
      
      self.phantom.debug("EVAL " + function)

      ret = self.contentPage.mainFrame().evaluateJavaScript(function)
      return ret

    @pyqtSlot()
    def stop(self):
      self.contentPage.triggerAction(QWebPage.Stop)
    @pyqtSlot(str, 'QVariant', 'QVariant')
    def sendEvent(self, type_, arg1, arg2):
      type_ = str(type_).lower()



      if type_ in ('mousedown', 'mouseup', 'mousemove'):
          if type(arg1).__name__ == "QVariant":
            arg1 = arg1.toInt()[0]
      
          if type(arg2).__name__ == "QVariant":
            arg2 = arg2.toInt()[0]

          eventType = QMouseEvent.Type(QEvent.None)
          button = Qt.MouseButton(Qt.LeftButton)
          buttons = Qt.MouseButtons(Qt.LeftButton)

          if type_ == 'mousedown':
              eventType = QEvent.MouseButtonPress
          elif type_ == 'mouseup':
              eventType = QEvent.MouseButtonRelease
          elif type_ == 'mousemove':
              eventType = QEvent.MouseMove
              button = buttons = Qt.NoButton

          assert eventType != QEvent.None

          event = QMouseEvent(eventType, QPoint(arg1, arg2), button, buttons, Qt.NoModifier)
          QApplication.postEvent(self.contentPage, event)
          QApplication.processEvents()

          return

      if type_ == 'click':
          if type(arg1).__name__ == "QVariant":
            arg1 = arg1.toInt()[0]
      
          if type(arg2).__name__ == "QVariant":
            arg2 = arg2.toInt()[0]
          self.sendEvent('mousedown', arg1, arg2)
          self.sendEvent('mouseup', arg1, arg2)
          return

      if type_ == 'keypress':
          if type(arg1).__name__ == "QVariant":
            arg1 = str(arg1.toPyObject())
      
          #if type(arg2).__name__ == "QVariant":
          #  arg2 = Qt.KeyboardModifiers(arg2.toInt()[0])

          for c in arg1:
            if c.islower():
              self.sendEvent('keydown', QVariant(c), QVariant(0))
              self.sendEvent('keyup', QVariant(c), QVariant(0))
            else:
              self.sendEvent('keydown', QVariant(c), QVariant(0x02000000))
              self.sendEvent('keyup', QVariant(c), QVariant(0x02000000))
          
          return

      if type_ in ('keydown', 'keyup' ):
        if type_ == "keydown":
          keyEventType = QKeyEvent.KeyPress
        else:
          keyEventType = QKeyEvent.KeyRelease
        
        if type(arg1).__name__ == "QVariant":
          text = QString(arg1.toString().at(0))
          key = text.at(0).toUpper().unicode()
        else:
          key = arg1
          text = arg1

        if type(arg2).__name__ == "QVariant":
          arg2 = Qt.KeyboardModifiers(arg2.toInt()[0])


        event = QKeyEvent(keyEventType, key, arg2,text)
        QApplication.postEvent(self.contentPage, event)
        QApplication.processEvents()






    @pyqtProperty('QVariantMap')
    def viewportSize(self):
        vps = self.contentPage.viewportSize()
        result = {
            'height': vps.height(),
            'width': vps.width()
        }
        return result

    @pyqtProperty('QVariantMap')
    def scrollPosition(self):
        scroll = self.m_scrollPosition
        result = {
            'left': scroll.x(),
            'top': scroll.y()
        }
        return result

    @scrollPosition.setter
    def scrollPosition(self, size):
        positions = {'left': 0, 'top': 0}
        for item in positions:
            try:
                positions[item] = int(size[item])
                if positions[item] < 0:
                    positions[item] = 0
            except (KeyError, ValueError):
                positions[item] = self.scrollPosition[item]
        self.m_scrollPosition = QPoint(positions['left'], positions['top'])
        self.m_mainFrame.setScrollPosition(self.m_scrollPosition)


class CookieJar(QNetworkCookieJar):
    def __init__(self, parent, cookiesFile):
      super(CookieJar, self).__init__(parent)
      self.phantom = parent.phantom
      self.m_cookiesFile = cookiesFile
      self.load()

    def log(self,msg):
      self.phantom.debug(msg)
    def load(self):
      try:
        ds = pickle.load(open(self.m_cookiesFile, 'rb'))
      except:
        ds = []
      qs = []
      t = int(time.time())
      for d in ds:
        if d['expirationDate'] > t and not d['isSessionCookie']:
          c = QNetworkCookie(d['name'],d['value'])
          c.setDomain(d['domain'])
          c.setExpirationDate(QDateTime.fromTime_t(d['expirationDate']))
          c.setHttpOnly(d['isHttpOnly'])
          c.setSecure(d['isSecure'])
          c.setPath(d['path'])
          qs.append(c)

      #for c in cs
      self.setAllCookies(qs)
    def save(self):
      cs = []
      t = int(time.time())      
      for c in self.allCookies():
        if not c.isSessionCookie() and c.expirationDate().toTime_t() > t:
          d = {
            'domain' : c.domain(),
            'expirationDate' : c.expirationDate().toTime_t(),
            'isHttpOnly' : c.isHttpOnly(),
            'isSecure' : c.isSecure(),
            'isSessionCookie' : c.isSessionCookie(),
            'name' : c.name(),
            'path' : c.path(),
            'value' : c.value()
          }
          cs.append(d)

      pickle.dump(cs,open(self.m_cookiesFile, 'wb'))
      #self.m_cookieStorage.setValue("point",QNetworkCookie(QByteArray("bbb"), QByteArray("aaa") ))
      #self.m_cookieStorage.setValue("cookies",self.allCookies())

    def setCookiesFromUrl(self, cookieList, url):
      super(CookieJar, self).setCookiesFromUrl(cookieList,url)
      self.save()
      return False

      settings = QSettings(self.m_cookiesFile, QSettings.IniFormat)
      
      settings.beginGroup(url.host())
      
      for cookie in cookieList:
          settings.setValue(unicode(cookie.name()), unicode(cookie.value()))
      
      settings.sync()
      
      return True
    
    def __del__(self):
      self.save()
    #def cookiesForUrl(self, url):
    #  cookieList = super(CookieJar, self).cookiesForUrl(url)
    #  self.log("cookiesForUrl0 %s %s" % (url.host(),[ (c.name(),c.value()) for c in cookieList ]))
    #  #return cookieList
    #  
    #  settings = QSettings(self.m_cookiesFile, QSettings.IniFormat)
    #  cookieList = []
##
    #  settings.beginGroup(url.host())
##
    #  for cname in settings.childKeys():
    #    v = unicode(settings.value(cname).toString())
    #    cookieList.append(QNetworkCookie(QByteArray(unicode(cname)), QByteArray(v) ))
##
    #  self.log("cookiesForUrl %s %s" % (url.host(),[ (c.name(),c.value()) for c in cookieList ]))
    #  
    #  return cookieList

class NetworkAccessManager(QNetworkAccessManager):
  def __init__(self, parent,args):
    super(NetworkAccessManager, self).__init__(parent)    
    self.args = args
  def createRequest(self, op, req, outgoingData):
    for k in self.args:
      req.setRawHeader(k, self.args[k] )

    reply = QNetworkAccessManager.createRequest(self, op, req, outgoingData)
    return reply

class ContentPage(QWebPage):
  openSignalSent = pyqtSignal(str)

  def __init__(self, parent,phantom,args):
    super(ContentPage, self).__init__(parent)
   
    self.parent = parent    
    self.phantom = phantom
    self.openSignalSent.connect(self.onOpen)
    self.loadFinished.connect(self.onLoadFinished)
    self.waitLoading = False
    self.pageId = 0
    self.userAgent = args.userAgent

    nam = NetworkAccessManager(self,{
      'Accept-Language' : 'en-US,en;q=0.8,ru;q=0.6,de;q=0.4',
      'Accept' :'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8,huye=a'
    })

    if args.cookies_file:
      #self.networkAccessManager().setCookieJar(CookieJar(self, args.cookies_file))
      nam.setCookieJar(CookieJar(self, args.cookies_file))

    #print self.networkAccessManager().cookieJar()

    #
    self.setNetworkAccessManager(nam)
    
    self.setViewportSize(QSize(args.viewportWidth, args.viewportHeight))

    self._extension_handlers = {
      #QWebPage.ErrorPageExtension: self._handle_errorpage,
      QtWebKit.QWebPage.ChooseMultipleFilesExtension: self._handle_multiple_files,
    }

  def supportsExtension(self, ext):
    return ext in self._extension_handlers

  def extension(self, ext, opt, out):
    handler = self._extension_handlers[ext]
    return handler(opt, out)


  def onLoadFinished(self,status):
    if self.waitLoading:
      self.waitLoading = False
      if status:
        self.phantom.out("OPN:" + str(self.pageId) + ":success") 
      else:
        self.phantom.out("OPN:" + str(self.pageId) + ":fail") 
    
  def _handle_multiple_files(self, info, files):
    print "_handle_multiple_files"
    result = self.chooseFile(None,None)
    if result:
      files.fileNames = [result]
  
    return True
  def chooseFile(self, originatingFrame, oldFile):
    print "chooseFile"
    oldFile = "/tmp/t.png"
    result = self.phantom.controllerPage.mainFrame().evaluateJavaScript("""if(page.onFilePicker) page.onFilePicker(); else null""").toPyObject()
    if result:
      return result
    else:
      return oldFile

  def onOpen(self,url):
    self.pageId += 1
    if self.waitLoading:
      self.phantom.out("OPN:"+str(self.pageId) + ":faileddup") 
    else:
      self.waitLoading = True
      self.mainFrame().load( QUrl(url))

  def javaScriptConsoleMessage(self, message, lineNumber, sourceID):
    if message == "$$INJECT":
      self.injectQueryFunction()
    else:
      self.phantom.javaScriptConsoleMessageSent.emit("CLT:" + message, lineNumber, sourceID)

  def userAgentForUrl(self, url):
    return self.userAgent

  def injectQueryFunction(self):

    self.mainFrame().evaluateJavaScript("""
var _query = function(selector) {
    var idx = null;
    var text2re = null;
    var ds = [];
    var exists = function (a,b) {
      var r = a.indexOf(b);
      return r  != -1 && r !== undefined;
    };
    if (selector instanceof Array) {
      if (selector[1] == '=') {
        ds = _query(selector[0]);
        if (ds.length > selector[2]) {
          ds = [ ds[ selector[2] ] ];
        } else {
          ds = [];
        }
      } else if (selector[1] == 'op') {
        var ds0 = _query(selector[0]);
        for ( var i = 0; i < ds0.length ; i++) {
          var ds1 = ds0[i].offsetParent;
          if ( ! exists(ds,ds1) ) ds.push(ds1);
        }
      } else if (selector[1] == 'up') {
        var ds0 = _query(selector[0]);
        for ( var i = 0; i < ds0.length ; i++) {
          var ds1 = ds0[i].parentNode;
          if ( ! exists(ds,ds1)  ) ds.push(ds1);
        }
      } else if (selector[1] == 'ns') {
        var ds0 = _query(selector[0]);
        for ( var i = 0; i < ds0.length ; i++) {
          var ds1 = ds0[i].nextElementSibling;
          if ( ! exists(ds,ds1)  ) ds.push(ds1);
        }
      } else if (selector[1] == 'ps') {
            var ds0 = _query(selector[0]);
            for ( var i = 0; i < ds0.length ; i++) {
                var ds1 = ds0[i].previousElementSibling;
                if ( ! exists(ds,ds1)  ) ds.push(ds1);
            }
        } else if (selector[1] == '>') {
        var ds0 = _query(selector[0]);
        for ( var i = 0; i < ds0.length ; i++) {
          var ds1 = ds0[i].children;
          for ( var j = 0; j < ds1.length ; j++) {
            if ( ! exists(ds,ds1[j])  ) ds.push(ds1[j]);
          }
        }
      } else if (selector[1] == '>>') {
        var ds0 = _query(selector[0]);
        for ( var i = 0; i < ds0.length ; i++) {
          var ds1 = ds0[i].querySelectorAll(selector[2]);
          for ( var j = 0; j < ds1.length ; j++) {
            if ( ! exists(ds,ds1[j])  ) ds.push(ds1[j]);
          }
        }

      } else if (selector[1] == 're' ) {
        var ds0 = _query(selector[0]);
      var re   = RegExp(selector[2]);
        for ( var i = 0; i < ds0.length ; i++) {
          var ds1 = ds0[i].innerText;
          if (ds1.match(re)) ds.push(ds0[i]);
        }
      } else {
        ds = [];
      }
    } else {
      var ds = [];
      var ds0 = document.querySelectorAll(selector);
      for ( var i = 0 ; i < ds0.length ; i++) ds.push(ds0[i]);
      var frms = document.getElementsByTagName("iframe");
      for ( var i = 0 ; i < frms.length ; i++) {
        if (frms[i].contentDocument) {
          var ds0 = frms[i].contentDocument.querySelectorAll(selector) ;
          if ( ds0 ) for ( var j = 0 ; j < ds0.length ; j++) {
            ds.push(ds0[j]);
          }
        }
      }
    }
    return ds;
}      
    """)
  
  

class StdIn(QThread):
  def __init__(self,controllerPage,phantom,contentPage):
    QThread.__init__(self)
    self.controllerPage = controllerPage
    self.phantom = phantom
    self.contentPage = contentPage
  
  def run(self):
    while True:
      line = sys.stdin.readline()
      line = line.rstrip()
    
      t = line.split(" ",1)
      if len(t) == 2:
        (cmd,data) = t
      else:
        (cmd,data) = (t[0],"")
      if cmd == 'QUIT' or cmd == '':
        self.phantom.info("QUIT")
        QApplication.instance().exit(0)
        break
      elif line.startswith("OPEN"):
        self.contentPage.openSignalSent.emit(data)
      elif line.startswith("EVAL"):
        self.controllerPage.evaljsSignalSent.emit(data)
      elif line.startswith("AVAL"):
        self.controllerPage.avaljsSignalSent.emit(data)


class Phantom(QObject):
  javaScriptConsoleMessageSent = pyqtSignal(str, int, str)
  startedSent = pyqtSignal()

  def __init__(self, parent, args):
    super(Phantom, self).__init__(parent)
    
    self.returnValue = 0
    self.verbose = args.verbose

    if args.proxy is not None:
      (host,port) = args.proxy.split(":")
      if args.proxy_type == 'socks5':
        proxy = QNetworkProxy(QNetworkProxy.Socks5Proxy, host, int(port))
      else:
        proxy = QNetworkProxy(QNetworkProxy.HttpProxy, host, int(port))
      QNetworkProxy.setApplicationProxy(proxy)
    
    #self.m_webPage.setNetworkAccessManager(self)

    self.javaScriptConsoleMessageSent.connect(self.printConsoleMessage)
    self.startedSent.connect(self.onStarted)


    if not args.silent:
      self.view = QtWebKit.QWebView()
      self.contentPage = ContentPage(self.view,self,args)
      self.contentPage.settings().setAttribute(QtWebKit.QWebSettings.DeveloperExtrasEnabled, True)
      self.view.setPage(self.contentPage)
      self.view.show()
    else:
      self.contentPage = ContentPage(self,self,args)

    #self.contentPage.settings().setAttribute(QtWebKit.QWebSettings.WebSecurityEnabled, False)

    self.controllerPage = ControllerPage(self,self.contentPage, args)
    self.controllerPage.mainFrame().addToJavaScriptWindowObject('phantom', self)

    self.stdin = StdIn(self.controllerPage,self,self.contentPage)
    
    QTimer.singleShot(0, lambda: self.onStarted())

    self.controllerPage and self.controllerPage.mainFrame().evaluateJavaScript("""
      var fs = { write: function() {}  };
      function ret(id,msg) {phantom.ret(id,msg)};
      function errret(id,msg) {phantom.errret(id,msg)};
      function quoteString(str) {
          var c, i, l = str.length, o = '"';
          for (i = 0; i < l; i += 1) {
              c = str.charAt(i);
              if (c >= ' ') {
                  if (c === '\\\\' || c === '"') {
                      o += '\\\\';
                  }
                  o += c;
              } else {
                  switch (c) {
                  case '\\b':
                      o += '\\\\b';
                      break;
                  case '\\f':
                      o += '\\\\f';
                      break;
                  case '\\n':
                      o += '\\\\n';
                      break;
                  case '\\r':
                      o += '\\\\r';
                      break;
                  case '\\t':
                      o += '\\\\t';
                      break;
                  default:
                      c = c.charCodeAt();
                      o += '\\\\u00' + Math.floor(c / 16).toString(16) +
                          (c % 16).toString(16);
                  }
              }
          }
          return o + '"';
      }
      function detectType(value) {
          var s = typeof value;
          if (s === 'object') {
              if (value) {
                  if (value instanceof Array) {
                      s = 'array';
                  } else if (value instanceof RegExp) {
                      s = 'regexp';
                  } else if (value instanceof Date) {
                      s = 'date';
                  }
              } else {
                  s = 'null';
              }
          }
          return s;
      }
     
      page.evaluate = function (func, args) {
        var str, arg, argType, i, l;
        if (!(func instanceof Function || typeof func === 'string' || func instanceof String)) {
            throw "Wrong use of WebPage#evaluate";
        }
        str = 'function() { return (' + func.toString() + ')(';
        for (i = 1, l = arguments.length; i < l; i++) {
            arg = arguments[i];
            argType = detectType(arg);

            switch (argType) {
            case "object":      //< for type "object"
            case "array":       //< for type "array"
                str += JSON.stringify(arg) + ","
                break;
            case "date":        //< for type "date"
                str += "new Date(" + JSON.stringify(arg) + "),"
                break;
            case "string":      //< for type "string"
                str += quoteString(arg) + ',';
                break;
            default:            // for types: "null", "number", "function", "regexp", "undefined"
                str += arg + ',';
                break;
            }
        }
        str = str.replace(/,$/, '') + '); }';
        return this.evaluateJavaScript(str);
    };
    """)
  
  def onStarted(self):
    self.info("STARTED")
    self.stdin.start()

  def printConsoleMessage(self, message, lineNumber, source):
    #if source: message = '%s:%d %s' % (source, lineNumber, message)
    sys.stderr.write( message  + "\n")
    

  def info(self,msg):
    sys.stderr.write( "INF:"+ msg  + "\n")
  
  def out(self,msg):
    sys.stderr.write( msg  + "\n")

  def convert(self,val):
    tp = type(val).__name__
    if tp == "QVariant":
      val = val.toPyObject()
      tp = type(val).__name__
    if tp == "QString":
      val = unicode(val)
      val = "\"" + val.replace("\"","\\\"") + "\""
    elif tp == "dict":
      tp = []
      for k in val:
        v = self.convert(val[k])
        k = unicode(k)
        tp.append(u'"' + k + u'": ' + unicode(v))
      val = u"{"+ u",".join(tp) + u"}"
    elif tp == "float" or tp== "int":
      val = val
    elif tp == 'bool':
      if val:
        val = u'true'
      else:
        val = u'false'
    else:
      val = unicode(val)
    return val

  @pyqtSlot(int,QVariant, result='QVariant')
  def ret(self,eid,msg):
    msg = self.convert(msg)
    sys.stderr.write( "RET:" + str(eid) + ":" + unicode(msg) + "\n")

  @pyqtSlot(int,str, result='QVariant')
  def errret(self,eid,msg):
    msg = self.convert(msg)
    sys.stderr.write("ERT:" + unicode(eid) + ":" + unicode(msg.replace("\n"," ")) + "\n")
  
  def error(self,msg):
    msg = self.convert(msg)
    sys.stderr.write(u"ERR:" + unicode(msg) + u"\n")

  def debug(self,msg):
    msg = self.convert(msg)
    if self.verbose:
      #msg = msg.decode('utf-8')
      try:
        sys.stderr.write(u"DBG:" + msg + u"\n")
      except:
        sys.stderr.write(u"DBG:unicode encoding error\n")
  #view.load(QtCore.QUrl('test.html'))
  #view.load(QtCore.QUrl("https://www.blogger.com/blogger.g?blogID=3210038986647032047#editor/src=sidebar"))
  


def parseArgs(arguments):
  class YesOrNoAction(argparse.Action):
      '''Converts yes or no arguments to True/False respectively'''
      def __call__(self, parser, namespace, value, option_string=None):
          answer = True if value == 'yes' else False
          setattr(namespace, self.dest, answer)

  #class TrueOrFalseAction(argparse.Action):
  #    '''Converts yes or no arguments to True/False respectively'''
  #    def __call__(self, parser, namespace, value, option_string=None):
  #        answer = True if value == 'true' else False
  #        setattr(namespace, self.dest, answer)

  parser = argparse.ArgumentParser(
    description='PyQT web browser',
    usage='%(prog)s',
    formatter_class=argparse.RawTextHelpFormatter
  )

  #parser.add_argument('--ignore-ssl-errors', default='yes', action=YesOrNoAction,
  #    choices=['yes', 'no'],
  #    help='Ignore SSL errors (default: yes)'
  #)

 #parser.add_argument('--web-security', default='yes', action=TrueOrFalseAction,
 #    choices=['true', 'false'],
 #    help='Web Security'
 #)  

  parser.add_argument('--proxy', metavar='address:port',
      help='Set the network proxy'
  )

  parser.add_argument('--proxy-type', default='http', metavar='type',
      help='Set the network proxy type (default: http)' 
  )

  parser.add_argument('--cookies-file', metavar='/path/to/cookies.txt',
    help='Sets the file name to store the persistent cookies'
  )
  #Mozilla/5.0 (Windows NT 6.0;) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36
  parser.add_argument('--userAgent',default ="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36",
    help='userAgent'
  )

  parser.add_argument('--viewportHeight',default = 1050,type=int,
    help='viewportHeight'
  )

  parser.add_argument('--viewportWidth',default = 1680,type=int,
    help='viewportWidth'
  )

  parser.add_argument('--verbose', action='store_true',
      help='Show verbose debug messages'
  )

  parser.add_argument('--silent', 
      default='yes', action=YesOrNoAction,
      choices=['yes', 'no'],
      help='Create Browser Automation Window'
  )

  args = parser.parse_known_args(arguments)[0]

  #file_check = (args.cookies_file, )
  #for file_ in file_check:
  #    if file_ is not None and not os.path.exists(file_):
  #        sys.exit("No such file or directory: '%s'" % file_)

  return args


def main(arguments):
  args = parseArgs(arguments)

  app = QApplication([sys.argv[0]] + arguments)

  phantom = Phantom(app, args)
  
  app.exec_()
    
  return phantom.returnValue

if __name__ == '__main__':
  sys.exit(main(sys.argv[1:]))
