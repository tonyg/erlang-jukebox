var jb = null;
var currentUsername = "unknown";

var refresh_timer = null; // will be result of setTimeout below.
var clock_timer = null; // will be result of setTimeout below.
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

var elapsedTime;
var totalTime;
var state;
var paused;

function clock_timer_tick() {
    if (state == "playing" && !paused) {
	elapsedTime ++;
	if (elapsedTime > totalTime) {
	    elapsedTime = totalTime;

	    // Clear the timeout before calling
	    // refresh_timer_tick, since that sets it.
	    clearTimeout(refresh_timer);
	    refresh_timer_tick();
	}
	update_time();
    }
    
    arm_clock_timer();
}

function arm_clock_timer() {
    clock_timer = setTimeout(clock_timer_tick, 1000);
}

function update_time() {
    var s = document.getElementById("statusatom");
    var pausedString = paused ? ", paused" : "";
    var timeString =
	state == "playing"
	? (" " + timeFormat(elapsedTime) + " / " + timeFormat(totalTime))
	: "";

    s.innerHTML = "";
    s.appendChild(document.createTextNode("Now playing (" + state + 
    					  pausedString + timeString + ")"));
}

function update_player_status(status) {
    state = status.status;
    paused = status.paused;
    elapsedTime = status.elapsedTime;
    totalTime = status.info ? status.info.totalTime : 0;
    update_time();

    var n = document.getElementById("nowplaying");
    var d = document.getElementById("statuspanel");

    n.innerHTML = "";
    if (status.entry) {
	n.appendChild(new LargeTrackWidget(status.entry, status.info, "large").domNode);
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
	var info = status.queueInfo[i];
	var itemnode = document.createElement("li");
	var span = document.createElement("span");
	span.className = "queue-buttons";
	span.appendChild(button(dequeuer_for(track), "dequeue",
				"imageButton dequeueButton",
				"Dequeue track"));
	span.appendChild(spacerText(" "));
	span.appendChild(button(raiser_for(track), "up",
				"imageButton upButton",
				"Move track earlier in queue"));
	span.appendChild(spacerText("/"));
	span.appendChild(button(lowerer_for(track), "down",
				"imageButton downButton",
				"Move track later in queue"));
	itemnode.appendChild(span);
	itemnode.appendChild((i < 3)
			     ? new LargeTrackWidget(track, info, "small").domNode
			     : new TrackWidget(track, info).domNode);
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

function timeFormat(seconds) {
    function pad(num) {
        return num < 10 ? "0" + num : num;
    }

    seconds = Math.floor(seconds);

    return (seconds >= 3600 ? pad(Math.floor(seconds / 3600)) + ":" : "")
	+ pad(Math.floor(seconds / 60)) + ":"
	+ pad(seconds % 60);
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

	var whennode = document.createElement("span");
	whennode.className = "when";
    var date = new Date();
    date.setTime(entry.when * 1000); // JS wants millis, erlang:now() gives seconds
	whennode.appendChild(document.createTextNode(format_date(date)));

	var whonode = document.createElement("span");
	whonode.className = "who";
	whonode.appendChild(document.createTextNode(entry.who));

	var whatnode = document.createElement("span");
	whatnode.className = "what";

	var whatHTML;

	if (entry.message) {
	    whatHTML = entry.what + ' "' + entry.message + '"';

        if (entry.track) {
	        whatHTML += '<span class="while-listening"> while listening to '
                     + new TrackWidget(entry.track, entry.info).domNode.innerHTML
                     + '</span>';
        }

	} else if (entry.what == 'skip') {
	    whatHTML = ' <span class="skip">skip</span> ' 
                 + new TrackWidget(entry.track, entry.info).domNode.innerHTML;

	} else if (entry.error) {
	    whatHTML = '<span class="error">' + JSON.stringify(entry.error) + '</span>';

	} else if (entry.http_error) {
	    whatHTML = '<span class="http-error">' + entry.http_error.response_code + ' downloading ' + entry.http_error.url + '</span>';
	}

    whatnode.innerHTML = whatHTML;

	itemnode.appendChild(whennode);
	itemnode.appendChild(document.createTextNode(" "));
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

function format_date(date) {
    var time = date.toLocaleTimeString();

    var now = new Date();
    if (date.getDate() != now.getDate() || 
        date.getMonth() != now.getMonth() || 
        date.getFullYear() != now.getFullYear()) {
        time += " (" + date.toLocaleDateString() + ")";
    }

    return time;
}

var current_volume = 0;
function update_volume(result) {
    vol = result.volume;
    document.getElementById("volume").innerHTML = vol + "%";
    document.getElementById("volume-who").innerHTML = result.who == "" ? "" : "(" + result.who + " " + result.direction + ")";
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

function LargeTrackWidget(track, info, size) {
    info = info || {};

    this.track = track;

    this.domNode = document.createElement("span");
    this.domNode.className = "jukeboxTrack jukebox-track-" + size;

    var partHtml = '<span class="finalUrlPart">';    
    partHtml += '<span class="img-holder">';

    if (info.albumArt) {
        partHtml += '<img src="cache/' + info.cacheHash + '.jpeg"/>';
    }
    
    partHtml += '</span>';
    partHtml += '<abbr title="' + unescape(this.track.url) + '">';
    
    if (info.trackName) {
        partHtml += '<b>' + info.trackName + '</b>';
        if (info.artistName) partHtml += ' - ' + info.artistName;
        partHtml += '<br/><small>';
        if (info.trackNumber) partHtml += 'Track ' + info.trackNumber;
        if (info.albumTitle) partHtml +=' from the album "' + info.albumTitle + '"';
        partHtml += '<a href="' + this.track.url + '" class="trackUrlLink">(...)</a></small>';
    } else {
        partHtml += short_url(this.track.url);
        partHtml += '<a href="' + this.track.url + '" class="trackUrlLink">(...)</a>';
    }

    partHtml += '</abbr>';
    partHtml += '</span>';

    this.domNode.innerHTML = partHtml;

    if (this.track.username) {
	this.domNode.appendChild(textSpan(" (" + this.track.username + ")", "trackUsername"));
    }
}

function TrackWidget(track, info) {
    info = info || {};

    this.track = track;

    this.domNode = document.createElement("span");
    this.domNode.className = "jukeboxTrack";

    var partHtml = '<span class="finalUrlPart">';
    partHtml += '<abbr title="' + unescape(this.track.url) + '">';
    
    if (info.trackName) {
        partHtml += info.trackName;
        if (info.artistName) partHtml += ' - ' + info.artistName;
        partHtml += '<a href="' + this.track.url + '" class="trackUrlLink">(...)</a>';
    } else {
        partHtml += short_url(this.track.url);
        partHtml += '<a href="' + this.track.url + '" class="trackUrlLink">(...)</a>';
    }

    partHtml += '</abbr>';
    partHtml += '</span>';

    this.domNode.innerHTML = partHtml;

    if (this.track.username) {
	this.domNode.appendChild(textSpan(" (" + this.track.username + ")", "trackUsername"));
    }
}

function short_url(url) {
    var urlParts = url.split("/");

    var partstr = urlParts[urlParts.length - 1];
    partstr = unescape(partstr);
    return partstr.replace(/_/g, ' ');
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
	    itemnode.appendChild(new TrackWidget(track, null).domNode);
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
	jb.set_volume(currentUsername, i).addCallback(update_volume);
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
	clock_timer_tick();
    }
}
