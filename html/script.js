

function highlightCode () {
    setTimeout(function () {
	var es = document.getElementsByTagName("code");
	for (var i=0; i<es.length; i++) {
            hljs.highlightBlock(es[i]);
	}
    }, 10);
}



var lastKey = 0;

function getLastKey() {
    return lastKey;
}

function propagateEvent (e) {
    var x = document.getElementById("slide");

    lastKey = e.keyCode;

    var ne = new KeyboardEvent("keyup", {
	bubbles : true,
	cancelable : true,
	keyCode : e.keyCode
    });

    x.dispatchEvent(ne);
}

document.onkeydown = propagateEvent;

