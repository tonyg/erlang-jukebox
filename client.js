dojo.require("dojo.rpc.JsonService");
dojo.require("dojo.animation.Timer");

dojo.require("dojo.widget.Manager");
dojo.require("dojo.widget.Button");
dojo.require("dojo.widget.LayoutContainer");
dojo.require("dojo.widget.ContentPane");
dojo.require("dojo.widget.LinkPane");
dojo.require("dojo.widget.SplitContainer");
dojo.require("dojo.widget.TabContainer");
dojo.require("dojo.widget.TitlePane");
dojo.require("dojo.event.*");

var jb = new dojo.rpc.JsonService("jukebox.smd");

var refresh_timer = new dojo.animation.Timer(5000);
refresh_timer.onTick = function () {
    jb.get_queue("dummy").addCallback(update_player_status);
    refresh_history();
    refresh_volume();
}

function refresh_history() {
    jb.get_history(15).addCallback(update_history);
}

function refresh_volume() {
    jb.get_volume('dummy').addCallback(update_volume);
}

function update_username(jbResp) {
    document.getElementById('username').value = jbResp[0];
}

function button(actionfn, icon) {
    var b = document.createElement("img");
    b.onclick = actionfn;
    b.src = icon;
    return b;
}

function update_player_status(status) {
    var s = document.getElementById("statusatom");
    var n = document.getElementById("nowplaying");
    var d = document.getElementById("statuspanel");
    s.innerHTML = ""; s.appendChild(document.createTextNode("Now playing ("+status.status+")"));

    n.innerHTML = "";
    if (status.entry) {
	n.appendChild(dojo.widget.createWidget("TrackWidget", {track: status.entry}).domNode);
    } else {
	n.appendChild(document.createElement("br"));
    }

    var listnode = document.createElement("ol");
    for (var i = 0; i < status.queue.length; i++) {
	var track = status.queue[i];
	var itemnode = document.createElement("li");
	itemnode.appendChild(button(dequeuer_for(track), "icons/deq-icon.png"));
	itemnode.appendChild(dojo.widget.createWidget("TrackWidget", {track: track}).domNode);
	listnode.appendChild(itemnode);
    }

    d.innerHTML = "";
    d.appendChild(listnode);

    var deqAll = dojo.widget.createWidget("Button", {caption: "Dequeue all"}, d, "first");
    dojo.event.connect(deqAll, "onClick", do_clear_queue);
}

function update_history(entries) {
    var listnode = document.createElement("ol");

    for (var i = entries.length - 1; i >= 0; i--) {
	var entry = entries[i];
	var itemnode = document.createElement("li");

	var whonode = document.createElement("span");
	whonode.className = "who";
	whonode.appendChild(document.createTextNode(entry.who));

	var whatnode = document.createElement("span");
	whatnode.className = "what";
	whatnode.appendChild(document.createTextNode(entry.what + " "));
	if (entry.track) {
	    whatnode.appendChild(dojo.widget.createWidget("TrackWidget",
							  {track: entry.track}).domNode);
	}
	if (entry.message) {
	    whatnode.appendChild(document.createTextNode('"' + entry.message + '"'));
	}

	itemnode.appendChild(whonode);
	itemnode.appendChild(document.createTextNode(" "));
	itemnode.appendChild(whatnode);

	listnode.appendChild(itemnode);
    }

    var h = document.getElementById("history");
    h.innerHTML = "";
    h.appendChild(listnode);
}

var current_volume = 0;
function update_volume(result) {
    vol = result.volume;
    document.getElementById("volume").innerHTML = vol + "%";
    document.getElementById("volume-tick-" + current_volume).className = "inactive-volume-tick";
    document.getElementById("volume-tick-" + vol).className = "active-volume-tick";
    current_volume = vol;
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

function do_dequeue(track) {
    jb.dequeue(track).addCallback(update_player_status);
}

function dequeuer_for(track) {
    return function () { do_dequeue(track); };
}

dojo.widget.registerWidgetPackage("jukebox");
dojo.widget.defineWidget("jukebox.TrackWidget", dojo.widget.HtmlWidget,
{
    widgetType: "TrackWidget",

    track: null,

    buildRendering: function(args, frag) {
	this.domNode = document.createElement("span");
	this.domNode.className = "jukeboxTrack";

	var linknode = document.createElement("a");
	linknode.href = this.track.url;
	linknode.appendChild(document.createTextNode("(...)"));
	this.domNode.appendChild(linknode);

	var urlParts = this.track.url.split("/");

	var partstr = urlParts[urlParts.length - 1];
	partstr = unescape(partstr);
	partstr = partstr.replace(/_/g, ' ');

	var abbrnode = document.createElement("abbr");
	abbrnode.title = this.track.url;
	abbrnode.appendChild(document.createTextNode(partstr));
	abbrnode.appendChild(document.createTextNode(" "));

	var partnode = document.createElement("span");
	partnode.className = "finalUrlPart";
	partnode.appendChild(abbrnode);
	this.domNode.appendChild(partnode);

	if (this.track.username) {
	    this.domNode.appendChild(document.createTextNode(" (" + this.track.username + ")"));
	}
    },
});

function display_search_results(results, divnode) {
    var listnode = document.createElement("ol");
    for (var i = 0; i < results.length; i++) {
	var track = results[i];
	var itemnode = document.createElement("li");
	itemnode.appendChild(button(enqueuer_for([track]), "icons/enq-icon.png"));
	itemnode.appendChild(dojo.widget.createWidget("TrackWidget", {track: track}).domNode);
	listnode.appendChild(itemnode);
    }

    divnode.innerHTML = "";
    divnode.appendChild(listnode);

    var enqAll = dojo.widget.createWidget("Button", {caption: "Enqueue all"}, divnode, "first");
    dojo.event.connect(enqAll, "onClick", enqueuer_for(results));
}

function do_search() {
    var searchtext = document.getElementById("searchtext").value;
    var keys = searchtext.split(/ +/);

    var p = document.getElementById("searchResults");
    p.innerHTML = "Searching...";

    jb.search(keys).addCallback(function (results) {
				    display_search_results(results, p);
				});
    return false;
}

function send_chat() {
    var n = document.getElementById("chatMessage");
    jb.chat(n.value).addCallback(refresh_history);
    n.value = "";
}

function volume_setter_for(i) {
    return function () {
	jb.set_volume(i).addCallback(update_volume);
    };
}

function build_volume_ticks() {
    var container = document.getElementById("volume-ticks");
    for (var i = 0; i <= 100; i++) {
	var link = document.createElement("span");
	link.id = "volume-tick-" + i;
	link.className = "inactive-volume-tick";
	link.onclick = volume_setter_for(i);
	link.innerHTML = "|";
	container.appendChild(link);
    }
}

function initClient() {
    build_volume_ticks();

    var username = document.location.search.match(/username=([^&]+)/);
    if (username) { username = unescape(username[1]); }

    if (username) {
	document.getElementById('username').value = username;
	change_username();
    } else {
	jb.whoami('dummy').addCallback(update_username);
    }

    refresh_timer.start();
    refresh_timer.onTick();
}
