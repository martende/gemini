var system = require('system');
var page = require('webpage').create();
var fs = require('fs');

var CONFIG = {
	debug: true,
};

page.viewportSize = {
  width: 800,
  height: 3400
};

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

var onInput = function(err,data) {
	if ( err !== null ) {
		error("readAsync "+err);
		phantom.exit(0);
		return;
	}
	data=data.replace(/[\n\r]+/,'');
	var cmd = data.substr(0,4);
	switch (cmd) {
		case 'QUIT':
			info("QUIT");
			phantom.exit(0);
			return;
		case 'OPEN':
			var url = data.substr(5);
			openurl(url);
			system.stdin.readLineAsync(onInput);
			return;
		case 'EVAL':
			var args = data.substr(5).split(" ");
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
			system.stdin.readLineAsync(onInput);
			return;
		case 'AVAL':
			var args = data.substr(5).split(" ");
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

			system.stdin.readLineAsync(onInput);
			return;
	}

	debug(data);
	try {
		eval(data);
	} catch(e) {
		error("Eval exception " + e);
	}


};

info("STARTED");
system.stdin.readLineAsync(onInput);
