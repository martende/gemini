var system = require('system');
var page = require('webpage').create();
var fs = require('fs');
var webserver = require('webserver');

var args = system.args;

var CONFIG = {
    debug: true,
    wsPort: 7578,
    viewportWidth: 1024,
    viewportHeight: 768,
    userAgent: "Mozilla/5.0 (Windows NT 6.0;) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2062.120 Safari/537.36"
};

for (var i =0;i< args.length;i++) {
    var t = args[i].split("=");
    if ( t.length == 2 ) {
        var k = t[0];var v = t[1];
        if ( k.length > 2) {
            k = k.substr(2);
            if (k in CONFIG) {
                switch (typeof CONFIG[k]) {
                    case "boolean": CONFIG[k] = !!v;break;
                    case "number":  CONFIG[k] = parseInt(v);break;
                    default: CONFIG[k] = v;
                }
            }
        }

    }
    
}

page.viewportSize = {
  width: CONFIG.viewportWidth,
  /*height: 3400*/
  height: CONFIG.viewportHeight
};

page.settings.userAgent = CONFIG.userAgent;

page.onConfirm = function(msg) {
	console.log("CLT:confirmClicked: " + msg);
  	return true; // `true` === pressing the "OK" button, `false` === pressing the "Cancel" button
};

page.onConsoleMessage = function(msg) {
	if (msg == "$$INJECT") {
		debug("injectClients");
		page.evaluate(function(_inj_query){
			eval("window._query = " + _inj_query + ";");
		},_query.toString());
		return;
	}
  	console.log("CLT:" +  msg);
};


page.onPageCreated = function(newPage) {
  console.log('A new child page was created! Its requested URL is not yet available, though.');
  // Decorate
  newPage.onClosing = function(closingPage) {
    console.log('A child page is closing: ' + closingPage.url);
  };

};

page.onResourceRequested = function(requestData, networkRequest) {
  console.log('CLT:Request (#' + requestData.id + '): ' + JSON.stringify(requestData));
};


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
    		var ds0 = frms[i].contentDocument.querySelectorAll(selector) ;
    		if ( ds0 ) for ( var j = 0 ; j < ds0.length ; j++) {
    			ds.push(ds0[j]);
    		}
    	}
    }
  	return ds;
}

//page.open("http://www.yahoo.com/",function(x) { console.log("bbbb",x)});

function debug(msg) {
	if (CONFIG.debug) console.log("DBG:" + msg);
}

function info(msg) {
	console.log("INF:" + msg);
}

function error(msg) {
	console.log("ERR:" + msg);
}

function ret(id,v) {
	console.log("RET:"+id+":" + v);
}

function errret(id,err) {
	console.log("ERT:"+id+":" + err);
}

var pageId=0;
var inOpen = false;
function openurl(url) {
	pageId++;
	debug("openurl '"+url+"' pageId="+pageId);
	if ( inOpen) {
		console.log("OPN:"+pageId+":faileddup");
	} else {
		inOpen = true;
		page.open(url,function(status) {
			inOpen = false;
			console.log("OPN:"+pageId+":"+status)}
		);
	}
}


function processCmd(cmd,data,cb,hasExit) {
    switch (cmd) {
        case 'QUIT':
            info("QUIT");
            if ( hasExit ) phantom.exit(0);
            return;
        case 'OPEN':
            var url = data;
            openurl(url);
            cb();
            return;
        case 'EVAL':
            var args = data.split(" ");
            if ( args.length < 2 ) error("EVAL format error"); else {
                var eid = args[0];
                var cmd = args.slice(1).join(" ");
                try {
                    var r = eval(cmd);
                    ret(eid,JSON.stringify(r));
                } catch(e) {
                    errret(args[0],e);
                }
            }
            cb();
            return;
        case 'AVAL':
            var args = data.split(" ");
            if ( args.length < 2 ) error("EVAL format error"); else {
                var eid = args[0];
                var cmd = args.slice(1).join(" ");
                try {
                    var js = "var AVAL=function (eid){"+cmd+"};AVAL("+eid+");";
                    eval(js);
                    //ret(eid,JSON.stringify(r));
                } catch(e) {
                    errret(args[0],e);
                }
            }
            cb();
            
            return;
    }
}

var onInput = function(err,data) {
	if ( err !== null ) {
		error("readAsync "+err);
		phantom.exit(0);
		return;
	}
	data=data.replace(/[\n\r]+/,'');
	var cmd = data.substr(0,4);
	data = data.substr(5);
    processCmd(cmd,data,function(){system.stdin.readLineAsync(onInput);},true);

};
/*
var server = webserver.create();

var wsStdin = server.listen(CONFIG.wsPort,{'keepAlive': true}, function(request, response) {
    console.log("EBAT'");
    var cmd = request.url.substr(1,5);
    console.log("CMD=",cmd);
    console.log("post=",request.postRaw);

    //response.statusCode = 200;
    //response.write('STARTED');
    //response.close();
    processCmd(cmd,request.postRaw,function(){},false);
    //phantom.exit();
});
*/

info("STARTED");
system.stdin.readLineAsync(onInput);
