-module(jukeboxsession).
-behaviour(gen_server).

-include("tqueue.hrl").

-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    rfc4627_jsonrpc:register_service
      (Pid,
       rfc4627_jsonrpc:service(<<"jukebox">>,
			       <<"urn:uuid:e97721d2-6439-4981-8527-e1f82edd0023">>,
			       <<"1.0.0">>,
			       [{<<"get_caller_hostname">>, []},
				{<<"search">>, [{"keys", arr}]},
				{<<"randomtracks">>, [{"approxcount", num}]},
				{<<"enqueue">>, [{"who", str},
						 {"entrylist", arr},
						 {"attop", bit}]},
				{<<"dequeue">>, [{"who", str},
						 {"entry", obj}]},
				{<<"raise">>, [{"entry", obj}]},
				{<<"lower">>, [{"entry", obj}]},
				{<<"get_queue">>, []},
				{<<"skip">>, [{"who", str}]},
				{<<"pause">>, [{"pause", bit}]},
				{<<"clear_queue">>, [{"who", str}]},
				{<<"get_history">>, [{"entrycount", num}]},
				{<<"chat">>, [{"who", str},
					      {"message", str}]},
				{<<"get_volume">>, []},
				{<<"set_volume">>, [{"newvol", num}]}])),
    {ok, Pid}.

default_name(IpAddr) ->
    case inet:gethostbyaddr(IpAddr) of
	{ok, Hostent} when element(1, Hostent) == hostent ->
	    Hostname = element(2, Hostent),
	    [Shortname | _] = string:tokens(Hostname, "."),
	    Shortname;
	_ ->
	    if
		is_tuple(IpAddr) ->
		    Digits = tuple_to_list(IpAddr),
		    lists:flatten(io_lib:format("~p.~p.~p.~p", Digits));
		is_list(IpAddr) ->
		    IpAddr;
		true ->
		    "unknown"
	    end
    end.

summary_to_json({StateSymbol, Q, Entry, IsPaused, ElapsedTime}) ->
    CurrentDownloads = urlcache:current_downloads(),
    {obj, [{"status", list_to_binary(atom_to_list(StateSymbol))},
	   {"entry", tqueue:entry_to_json(Entry)},
	   {"info",  urlcache:info_to_json(info_for_entry(Entry))},
	   {"queue", tqueue:to_json(Q)},
       {"queueInfo",  urlcache:queue_info_json(Q)},
	   {"paused", IsPaused},
	   {"elapsedTime", ElapsedTime},
	   {"downloads", lists:map(fun erlang:list_to_binary/1, CurrentDownloads)}]}.

info_for_entry(null) ->
    null;
info_for_entry(_Entry=#entry{url=Url}) ->
    urlcache:get_info(Url).

history_to_json(H) ->
    lists:map(fun ({Who, {What, Entry}, When}) ->
		      {obj, [{"who", list_to_binary(Who)},
			     {"what", list_to_binary(atom_to_list(What))},
			     {"when", When}
			     | Entry]}
	      end, H).

log(Who, What, JsonFields) ->
    history:record(history, Who, {What, JsonFields}).

init(_Args) ->
    {ok, no_state}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call({jsonrpc, <<"get_caller_hostname">>, RequestInfo, []}, _From, State) ->
    Name = case rfc4627:get_field(RequestInfo, "remote_peername", undefined) of
	       undefined -> "unknown";
	       NameBin -> default_name(binary_to_list(NameBin))
	   end,
    {reply, {result, list_to_binary(Name)}, State};

handle_call({jsonrpc, <<"search">>, _RequestInfo, [Keys]}, _From, State) ->
    Tracks = trackdb:search_tracks(lists:map(fun binary_to_list/1, Keys)),
    {reply, {result, tqueue:to_json(Tracks)}, State};

handle_call({jsonrpc, <<"randomtracks">>, _RequestInfo, [ApproxCount]}, _From, State) ->
    Tracks = trackdb:random_tracks(ApproxCount),
    {reply, {result, tqueue:to_json(Tracks)}, State};

handle_call({jsonrpc, <<"enqueue">>, _RequestInfo, [Who, EntryList, AtTop]}, _From, State) ->
    Q = tqueue:from_json(EntryList),
    player:enqueue(binary_to_list(Who), AtTop, Q),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"dequeue">>, _RequestInfo, [_Who, Entry]}, _From, State) ->
    player:dequeue(tqueue:entry_from_json(Entry)),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"raise">>, _RequestInfo, [Entry]}, _From, State) ->
    player:raise(tqueue:entry_from_json(Entry)),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"lower">>, _RequestInfo, [Entry]}, _From, State) ->
    player:lower(tqueue:entry_from_json(Entry)),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"get_queue">>, _RequestInfo, []}, _From, State) ->
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"skip">>, _RequestInfo, [Who]}, _From, State) ->
    {ok, SkippedTrack, NewState} = player:skip(),
    log(binary_to_list(Who), skip, [{track, tqueue:entry_to_json(SkippedTrack)}]),
    {reply, {result, summary_to_json(NewState)}, State};

handle_call({jsonrpc, <<"pause">>, _RequestInfo, [Pause]}, _From, State) ->
    {reply, {result, summary_to_json(player:pause(Pause))}, State};

handle_call({jsonrpc, <<"clear_queue">>, _RequestInfo, [_Who]}, _From, State) ->
    {reply, {result, summary_to_json(player:clear_queue())}, State};

handle_call({jsonrpc, <<"get_history">>, _RequestInfo, [EntryCount]}, _From, State) ->
    {reply, {result, history_to_json(history:retrieve(history, EntryCount))}, State};

handle_call({jsonrpc, <<"chat">>, _RequestInfo, [Who, Message]}, _From, State) ->
    {_StateSymbol, _Q, Entry, _IsPaused, _ElapsedTime} = player:get_queue(),
    log(binary_to_list(Who), says, [{message, Message},
                                    {track, tqueue:entry_to_json(Entry)},
                                    {info,  urlcache:info_to_json(info_for_entry(Entry))}]),
    {reply, {result, true}, State};

handle_call({jsonrpc, <<"get_volume">>, _RequestInfo, []}, _From, State) ->
    {reply, {result, {obj, [{"volume", volume:get()}]}}, State};

handle_call({jsonrpc, <<"set_volume">>, _RequestInfo, [NewVol]}, _From, State) ->
    {reply, {result, {obj, [{"volume", volume:set(NewVol)}]}}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
