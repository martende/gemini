#gemini

PhantomJs Akka bridge. 
Brings Jquery like DOM traversal to Scala. Avoid callbacks hell with fututres.


###Usage:
```scala

import akka.gemini.{Page,Selector,PhantomExecutor}
import scala.concurrent.duration._

// Create Browser Session
val p:Page = PhantomExecutor(isDebug=false) 
// Opens page and returns future , that should be completed after successfull page retriveal

val openFuture:Future[Boolean]  = p.open("https://github.com/martende/gemini/") 
// or make it synchrone


scala.concurrent.Await.result(openFuture,10 seconds) // should be true or throws Exception

// Click Somewere on page
p.$("button.submitButton").click()   // synchrone operation produces exception if element not found

// Wait for some reaction on page: future produces success if element p.$("#someElement")  exists

scala.concurrent.Await.result(p.waitForSelector(p.$("#someElement") , pageLoadTimeout * 2 ),pageLoadTimeout)

// Data traversal using full Traversable power

val elementMap = for ( el <- p.$("#route .suggestcontainer > div") ) yield {
el.$(".title").innerText -> el.$(".description").innerText
}

// Change Element value 
p.$("input[name=outboundDate]").value = "123"

// Use inherited selectors 

val form:Selector = p.$("form.mainForm")
if ( form.exists() ){
  val input1 = form.$("input[name=aaa]")
  val someOtherElement = form.parentNode.nextSibling
}

// Finish phantomjs session

p.close 


```

###Install:


asdasdasd
