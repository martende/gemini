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

Gemini uses pathched version of phantomjs with asyncIo api from https://github.com/execjosh/phantomjs.git and branch WIP-async-file-io
or https://github.com/martende/phantomjs/tree/ASYNC_1.9 patched 1.9 version.
**Warning !!!** - On current 1.9 phantomjs release gemini does not work.

For instalation do something like 
```
git clone https://github.com/execjosh/phantomjs.git
cd phantomjs
git checkout WIP-async-file-io
./build.sh
```

copy bin/phantomjs to your project and write phantom.bin - variable to application.conf.

To install from source use sbt on cloned project

```
git clone https://github.com/martende/gemini
cd gemini
sbt publish-local
```

and then in build.sbt write library deps 

```
libraryDependencies ++= Seq(
"com.github.martende" %% "gemini" % "0.1-SNAPSHOT"
)
```

###Configuiration:

Sample application.conf part (for default sbt projects src/main/resources )

```
phantom {
  // Path to binary phantomjs file - this repo produces 64bit compiled for ubuntu jessie bin
  // bin/phantomjs: ELF 64-bit LSB  executable, x86-64, version 1 (SYSV), dynamically linked (uses shared libs), for GNU/Linux 2.6.18, BuildID[sha1]=6d1ee78f9e4370d6a33ebcb2cf989be26d4961e5, not stripped

  bin = "./bin/phantomjs"   
  // Some Extra args for phantomjs 
  //args  =["--proxy=127.0.0.1:3128"]
  args = []
}
```

###Sample Sbt example project

#####/build.sbt
```
scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
"com.github.martende" %% "gemini" % "0.1-SNAPSHOT"
)

```

#####/src/main/scala/hello.scala
```
import akka.gemini.PhantomExecutor
import akka.actor.ActorSystem
import scala.concurrent.duration._

object Hi {
  def main(args: Array[String]) = {
  	implicit val system = ActorSystem("MySystem")
	val p = PhantomExecutor(isDebug=false)
	
	scala.concurrent.Await.result({
		p.open("http://www.google.com/")
	},10 seconds)

	p.render("screen.png")
	p.close()
	
	println("Screenshot screen.png done.")
	system.shutdown()
	
  }
}

```

#####src/main/resources/application.conf
```
akka {
	loglevel="DEBUG"
}

phantom {
  bin = "../gemini/bin/phantomjs"
  args = []
}
```
