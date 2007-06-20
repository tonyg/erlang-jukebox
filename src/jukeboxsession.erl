-module(jukeboxsession).
-behaviour(gen_server).

-include("tqueue.hrl").

-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    mod_jsonrpc:register_service
      (Pid,
       mod_jsonrpc:service(<<"jukebox">>,
                           <<"urn:uuid:e97721d2-6439-4981-8527-e1f82edd0023">>,
                           <<"1.0.0">>,
                           [{<<"get_caller_hostname">>, []},
			    {<<"search">>, [{"keys", arr}]},
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

default_name(Ip4Addr) ->
    case inet:gethostbyaddr(Ip4Addr) of
	{ok, Hostent} when element(1, Hostent) == hostent ->
	    Hostname = element(2, Hostent),
	    [Shortname | _] = string:tokens(Hostname, "."),
	    Shortname;
	_ ->
	    if
		is_tuple(Ip4Addr) ->
		    Digits = tuple_to_list(Ip4Addr),
		    lists:flatten(io_lib:format("~p.~p.~p.~p", Digits));
		is_list(Ip4Addr) ->
		    Ip4Addr;
		true ->
		    "unknown"
	    end
    end.

summary_to_json({idle, Q}) ->
    {obj, [{"status", <<"idle">>},
	   {"entry", null},
	   {"queue", tqueue:to_json(Q)}]};
summary_to_json({{Status, Entry}, Q}) ->
    {obj, [{"status", list_to_binary(atom_to_list(Status))},
	   {"entry", tqueue:entry_to_json(Entry)},
	   {"queue", tqueue:to_json(Q)}]}.

history_to_json(H) ->
    lists:map(fun ({Who, {What, Entry}}) ->
		      {obj, [{"who", list_to_binary(Who)},
			     {"what", list_to_binary(atom_to_list(What))}
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

handle_call({jsonrpc, <<"get_caller_hostname">>, ModData, []}, _From, State) ->
    {init_data, {_PortNumber, Ip4Addr}, _Resolve} = element(2, ModData),
    Name = default_name(Ip4Addr),
    {reply, {result, list_to_binary(Name)}, State};

handle_call({jsonrpc, <<"search">>, _ModData, [Keys]}, _From, State) ->
    Tracks = trackdb:search_tracks(lists:map(fun binary_to_list/1, Keys)),
    {reply, {result, tqueue:to_json(Tracks)}, State};

handle_call({jsonrpc, <<"enqueue">>, _ModData, [Who, EntryList, AtTop]}, _From, State) ->
    Q = tqueue:from_json(EntryList),
    player:enqueue(binary_to_list(Who), AtTop, Q),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"dequeue">>, _ModData, [_Who, Entry]}, _From, State) ->
    player:dequeue(tqueue:entry_from_json(Entry)),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"raise">>, _ModData, [Entry]}, _From, State) ->
    player:raise(tqueue:entry_from_json(Entry)),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"lower">>, _ModData, [Entry]}, _From, State) ->
    player:lower(tqueue:entry_from_json(Entry)),
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"get_queue">>, _ModData, []}, _From, State) ->
    {reply, {result, summary_to_json(player:get_queue())}, State};

handle_call({jsonrpc, <<"skip">>, _ModData, [Who]}, _From, State) ->
    {ok, SkippedTrack, NewState} = player:skip(),
    log(binary_to_list(Who), skip, [{track, tqueue:entry_to_json(SkippedTrack)}]),
    {reply, {result, summary_to_json(NewState)}, State};

handle_call({jsonrpc, <<"pause">>, _ModData, [Pause]}, _From, State) ->
    {reply, {result, summary_to_json(player:pause(Pause))}, State};

handle_call({jsonrpc, <<"clear_queue">>, _ModData, [_Who]}, _From, State) ->
    {reply, {result, summary_to_json(player:clear_queue())}, State};

handle_call({jsonrpc, <<"get_history">>, _ModData, [EntryCount]}, _From, State) ->
    {reply, {result, history_to_json(history:retrieve(history, EntryCount))}, State};

handle_call({jsonrpc, <<"chat">>, _ModData, [Who, Message]}, _From, State) ->
    log(binary_to_list(Who), says, [{message, Message}]),
    {reply, {result, true}, State};

handle_call({jsonrpc, <<"get_volume">>, _ModData, []}, _From, State) ->
    {reply, {result, {obj, [{"volume", volume:get()}]}}, State};

handle_call({jsonrpc, <<"set_volume">>, _ModData, [NewVol]}, _From, State) ->
    {reply, {result, {obj, [{"volume", volume:set(NewVol)}]}}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
