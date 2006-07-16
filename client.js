dojo.require("dojo.rpc.JsonService");
dojo.require("dojo.animation.Timer");

dojo.require("dojo.widget.Manager");
dojo.require("dojo.widget.LayoutContainer");
dojo.require("dojo.widget.ContentPane");
dojo.require("dojo.widget.LinkPane");
dojo.require("dojo.widget.SplitContainer");
dojo.require("dojo.widget.TabContainer");

var jb = new dojo.rpc.JsonService("jukebox.smd");

var refresh_timer = new dojo.animation.Timer(5000);
refresh_timer.onTick = function () {
    jb.get_queue("dummy").addCallback(update_player_status);
    jb.get_history(5).addCallback(update_history);
}

function update_username(jbResp) {
    document.getElementById('username').value = jbResp[0];
}

function update_player_status(status) {
    var s = document.getElementById("statusatom");
    var n = document.getElementById("nowplaying");
    var d = document.getElementById("statuspanel");
    s.innerHTML = ""; s.appendChild(document.createTextNode(status.status));
    n.innerHTML = ""; n.appendChild(document.createTextNode(status.entry.url));
    display_search_results(status.queue, d);
}

function update_history(entries) {
    var h = document.getElementById("history");
    h.innerHTML = "";
    h.appendChild(document.createTextNode(dojo.json.serialize(entries)));
}

function change_username() {
    jb.login(document.getElementById('username').value).addCallback(update_username);
}

function do_logout() {
    jb.logout('dummy').addCallback(update_username);
}

function do_skip() {
    jb.skip('dummy').addCallback(update_player_status);
}

function do_clear_queue() {
    jb.clear_queue('dummy').addCallback(update_player_status);
}

function do_pause(shouldPause) {
    jb.pause(shouldPause).addCallback(update_player_status);
}

function do_enqueue(trackEntries) {
    jb.enqueue(trackEntries).addCallback(update_player_status);
}

function enqueuer_for(trackEntries) {
    return function () { do_enqueue(trackEntries); };
}

dojo.widget.registerWidgetPackage("jukebox");
dojo.widget.defineWidget("jukebox.TrackWidget", dojo.widget.HtmlWidget,
{
    widgetType: "TrackWidget",
    isContainer: true,

    templatePath: "TrackWidget.html",

    track: null,
});

function display_search_results(results, divnode) {
    var listnode = document.createElement("ol");
    for (var i = 0; i < results.length; i++) {
	var track = results[i];
	var itemnode = document.createElement("li");

	var enq = document.createElement("a");
	enq.href = "#";
	enq.onclick = enqueuer_for([track]);
	enq.appendChild(document.createTextNode("enq"));
	itemnode.appendChild(enq);

	var linknode = document.createElement("a");
	linknode.href = track.url;
	linknode.appendChild(document.createTextNode("(...)"));
	itemnode.appendChild(linknode);

	var urlParts = track.url.split("/");
	var partsnode = document.createElement("span");
	partsnode.className = "parts";
	itemnode.appendChild(partsnode);
	var startIndex = urlParts.length > 4 ? urlParts.length - 4 : 0;
	for (var j = startIndex; j < urlParts.length; j++) {
	    var partnode = document.createElement("span");
	    partnode.className = "p" + (j - startIndex);
	    partnode.appendChild(document.createTextNode(urlParts[j]));
	    partsnode.appendChild(document.createTextNode(" "));
	    partsnode.appendChild(partnode);
	}

	listnode.appendChild(itemnode);
    }

    divnode.innerHTML = "";
    divnode.appendChild(listnode);
}

function addMainTab(w) {
    var tc = dojo.widget.byId("mainTabContainer");
    tc.addChild(w);
    tc.selectTab(w);
}

function do_search() {
    var searchtext = document.getElementById("searchtext").value;
    var keys = searchtext.split(/ +/);
    jb.search(keys).addCallback
    (function (results) {
	 var p = dojo.widget.createWidget("ContentPane", {label: searchtext});
	 //p.extraArgs.onClose = function () { alert("hi"); return true; };
	 display_search_results(results, p.domNode);
	 addMainTab(p);
     });
}

function initClient() {
    jb.whoami('dummy').addCallback(update_username);

    refresh_timer.start();
    refresh_timer.onTick();
}
