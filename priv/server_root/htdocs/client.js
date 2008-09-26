var jb = null;
var currentUsername = "unknown";

var refresh_timer = null; // will be result of setTimeout below.
function refresh_timer_tick() {
    jb.get_queue()
    .addCallback(function (status)
    {
	update_player_status(status);
	refresh_history();
	refresh_volume();
	// Only rearm the timer once we know the server's answering requests.
	arm_refresh_timer();
    });
}
function arm_refresh_timer() {
    refresh_timer = setTimeout(refresh_timer_tick, 5000);
}

function refresh_history() {
    jb.get_history(15).addCallback(update_history);
}

function refresh_volume() {
    jb.get_volume().addCallback(update_volume);
}

function update_username(newName) {
    currentUsername = newName;
    document.getElementById('username').value = newName;
}

function button(actionfn, text, maybeClass, maybeTitle) {
    var b = document.createElement("button");
    b.className = "action-span";
    if (maybeClass) { b.className += " " + maybeClass; }
    if (maybeTitle) { b.title = maybeTitle; }
    b.onclick = actionfn;
    b.innerHTML = text;
    return b;
}

function textSpan(text, maybeClass) {
    var n = document.createElement("span");
    if (maybeClass) n.className = maybeClass;
    n.appendChild(document.createTextNode(text));
    return n;
}

function spacerText(text) {
    return textSpan(text, "spacerText");
}

function prependChild(node, child) {
    return node.insertBefore(child, node.firstChild);
}

function update_player_status(status) {
    var s = document.getElementById("statusatom");
    var n = document.getElementById("nowplaying");
    var d = document.getElementById("statuspanel");
    var pausedString = status.paused ? ", paused" : "";
    s.innerHTML = "";
    s.appendChild(document.createTextNode("Now playing ("+status.status+pausedString+")"));

    n.innerHTML = "";
    if (status.entry) {
	n.appendChild(new TrackWidget(status.entry).domNode);
    } else {
	n.appendChild(document.createElement("br"));
    }

    var currentDownloads = {};
    for (var i = 0; i < status.downloads.length; i++) {
	currentDownloads[status.downloads[i]] = 1;
    }

    var listnode = document.createElement("ol");
    for (var i = 0; i < status.queue.length; i++) {
	var track = status.queue[i];
	var itemnode = document.createElement("li");
	itemnode.appendChild(button(dequeuer_for(track), "dequeue",
				    "imageButton dequeueButton",
				    "Dequeue track"));
	itemnode.appendChild(spacerText(" "));
	itemnode.appendChild(button(raiser_for(track), "up",
				    "imageButton upButton",
				    "Move track earlier in queue"));
	itemnode.appendChild(spacerText("/"));
	itemnode.appendChild(button(lowerer_for(track), "down",
				    "imageButton downButton",
				    "Move track later in queue"));
	itemnode.appendChild(new TrackWidget(track).domNode);
	if (currentDownloads[track.url]) {
	    itemnode.appendChild(textSpan(" (caching)", "cachingIndicator"));
	}
	listnode.appendChild(itemnode);
    }

    d.innerHTML = "";
    d.appendChild(listnode);

    var deqAll = new ButtonWidget("Dequeue all").domNode;
    prependChild(d, deqAll);
    Event.observe(deqAll, 'click', do_clear_queue);
}

function historiesEqual(h1, h2) {
    // Cheat, using JSON text-equivalence as the equivalence we're after.
    return JSON.stringify(h1) == JSON.stringify(h2);
}

var previousHistoryEntries = [];
function update_history(entries) {
    if (historiesEqual(entries, previousHistoryEntries)) {
	return;
    }

    previousHistoryEntries = entries;

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
	    whatnode.appendChild(new TrackWidget(entry.track).domNode);
	}
	if (entry.message) {
	    whatnode.appendChild(document.createTextNode('"' + entry.message + '"'));
	}
	if (entry.error) {
	    whatnode.appendChild(document.createTextNode(JSON.stringify(entry.error)));
	}

	itemnode.appendChild(whonode);
	itemnode.appendChild(document.createTextNode(" "));
	itemnode.appendChild(whatnode);

	listnode.appendChild(itemnode);
    }

    var h = document.getElementById("history");
    h.innerHTML = "";
    h.appendChild(listnode);
    h.scrollTop = 10000; // a big number - meaning "as far as possible"
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
    update_username(document.getElementById('username').value);
}

function do_skip() {
    jb.skip(currentUsername).addCallback(update_player_status);
}

function do_clear_queue() {
    jb.clear_queue(currentUsername).addCallback(update_player_status);
}

function do_pause(shouldPause) {
    jb.pause(shouldPause).addCallback(update_player_status);
}

function do_enqueue(trackEntries, atTop) {
    jb.enqueue(currentUsername, trackEntries, atTop).addCallback(update_player_status);
}

function enqueuer_for(trackEntries, atTop) {
    return function () { do_enqueue(trackEntries, atTop); };
}

function raiser_for(track) {
    return function () {
	jb.raise(track).addCallback(update_player_status);
    };
}

function lowerer_for(track) {
    return function () {
	jb.lower(track).addCallback(update_player_status);
    };
}

function do_dequeue(track) {
    jb.dequeue(currentUsername, track).addCallback(update_player_status);
}

function dequeuer_for(track) {
    return function () { do_dequeue(track); };
}

function ButtonWidget(caption) {
    this.caption = caption;

    this.domNode = document.createElement("a");
    this.domNode.className = "action-span";
    this.domNode.innerHTML = caption;
}

function TrackWidget(track) {
    this.track = track;

    this.domNode = document.createElement("span");
    this.domNode.className = "jukeboxTrack";

    var linknode = document.createElement("a");
    linknode.className = "trackUrlLink";
    linknode.href = this.track.url;
    linknode.appendChild(document.createTextNode("(...)"));
    this.domNode.appendChild(linknode);

    var urlParts = this.track.url.split("/");

    var partstr = urlParts[urlParts.length - 1];
    partstr = unescape(partstr);
    partstr = partstr.replace(/_/g, ' ');

    var abbrnode = document.createElement("abbr");
    abbrnode.title = unescape(this.track.url);
    abbrnode.appendChild(document.createTextNode(partstr));
    abbrnode.appendChild(document.createTextNode(" "));

    var partnode = document.createElement("span");
    partnode.className = "finalUrlPart";
    partnode.appendChild(abbrnode);
    this.domNode.appendChild(partnode);

    if (this.track.username) {
	this.domNode.appendChild(textSpan(" (" + this.track.username + ")", "trackUsername"));
    }
}

function group_by_folder(results) {
    var groups = [];
    var current = null;
    var acc = [];
    for (var i = 0; i < results.length; i++) {
	var track = results[i];
	var folder = track.url.match(/(.*\/)[^\/]*/)[1];
	if (folder != current) {
	    if (current != null) {
		groups.push({folder: current, results: acc});
		acc = [];
	    }
	    current = folder;
	}
	acc.push(track);
    }
    groups.push({folder: current, results: acc});
    return groups;
}

function display_search_results(ungrouped_results, divnode) {
    var groups = group_by_folder(ungrouped_results);

    divnode.innerHTML = "";

    for (var groupIndex = 0; groupIndex < groups.length; groupIndex++) {
	var listnode = document.createElement("ul");
	var group = groups[groupIndex];

	for (var i = 0; i < group.results.length; i++) {
	    var track = group.results[i];
	    var itemnode = document.createElement("li");
	    itemnode.appendChild(button(enqueuer_for([track], false), "enqueue",
					"imageButton enqueueButton",
					"Append track to queue"));
	    itemnode.appendChild(spacerText(" "));
	    itemnode.appendChild(button(enqueuer_for([track], true), "@top",
					"imageButton atTopButton",
					"Prepend track to queue"));
	    itemnode.appendChild(new TrackWidget(track).domNode);
	    listnode.appendChild(itemnode);
	}

	var enqF = document.createElement("a");
	enqF.onclick = enqueuer_for(group.results, false);
	enqF.innerHTML = "(enqueue)";

	var folderName = document.createElement("a");
	folderName.className = "folderLink";
	folderName.href = group.folder;
	folderName.appendChild(document.createTextNode(unescape(group.folder)));

	var heading = document.createElement("div");
	heading.className = "folderHeading";
	heading.appendChild(enqF);
	heading.appendChild(document.createTextNode(" "));
	heading.appendChild(folderName);

	divnode.appendChild(heading);
	divnode.appendChild(listnode);
    }

    var enqAll = new ButtonWidget("Enqueue all").domNode;
    prependChild(divnode, enqAll);
    Event.observe(enqAll, "click", enqueuer_for(ungrouped_results, false));
}

function do_search() {
    var searchtext = document.getElementById("searchtext").value;
    var keys = searchtext.split(/ +/);

    var p = document.getElementById("searchResults");
    p.innerHTML = "Searching...";

    jb.search(keys)
    .addCallback(function (results) {
		     display_search_results(results, p);
		 })
    .addErrorCallback(function (err) {
			  p.innerHTML = JSON.stringify(err);
		      });
    return false;
}

function do_random(count) {
    var p = document.getElementById("searchResults");
    p.innerHTML = "Finding approximately " + count + " random tracks...";

    jb.randomtracks(count)
    .addCallback(function (results) {
		     display_search_results(results, p);
		 })
    .addErrorCallback(function (err) {
			  p.innerHTML = JSON.stringify(err);
		      });
    return false;
}

function send_chat() {
    var n = document.getElementById("chatMessage");
    jb.chat(currentUsername, n.value).addCallback(refresh_history);
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
	var link = document.createElement("a");
	link.id = "volume-tick-" + i;
	link.className = "inactive-volume-tick";
	link.onclick = volume_setter_for(i);
	link.innerHTML = "|";
	link.title = i;
	link.onmouseover = build_volume_tick_closure_show(i);
	link.onmouseout = build_volume_tick_closure_hide(i);
	container.appendChild(link);
    }
}

function build_volume_tick_closure_show(vol) {
    return function() {
        document.getElementById("volume-indicator").innerHTML = vol + "%";
    }
}

function build_volume_tick_closure_hide(vol) {
    return function() {
        document.getElementById("volume-indicator").innerHTML = "";
    }
}

function initClient() {
    jb = new JsonRpcService("/rpc/jukebox", onReady);
    build_volume_ticks();
    document.getElementById('searchtext').focus();

    function onReady() {
	jb.options.timeout = 30000; /* milliseconds */
	var username = document.location.search.match(/username=([^&]+)/);
	if (username) {
	    username = username[1].replace(/\+/g, " ");
	    username = unescape(username);
	}

	if (username) {
	    document.getElementById('username').value = username;
	    change_username();
	} else {
	    jb.get_caller_hostname().addCallback(update_username);
	}

	refresh_timer_tick();
    }
}
