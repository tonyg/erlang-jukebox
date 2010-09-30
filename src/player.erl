-module(player).
-include("tqueue.hrl").
-include("settings.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([supports_extension/1]).
-export([enqueue/3, dequeue/1, raise/1, lower/1, get_queue/0, skip/0, pause/1, clear_queue/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, player}, player, [], []).

%---------------------------------------------------------------------------

supports_extension(Extension) ->
    case player_mapping(Extension) of
	{ok, _CommandLine} -> true;
	playlist -> true;
	_ -> false
    end.

player_mapping(E) -> player_mapping1(http_util:to_lower(E)).

-define(VLC_PLAYER_MAPPING, {ok, ?VLC_COMMAND }).

player_mapping1(".ogg") -> ?VLC_PLAYER_MAPPING;
player_mapping1(".mp3") -> ?VLC_PLAYER_MAPPING;
player_mapping1(".m4a") -> ?VLC_PLAYER_MAPPING;
player_mapping1(".wav") -> ?VLC_PLAYER_MAPPING;
player_mapping1(".flac") -> ?VLC_PLAYER_MAPPING;
player_mapping1(".wma") -> ?VLC_PLAYER_MAPPING;
player_mapping1(".m3u") -> playlist;
player_mapping1(_) -> not_playable.

enqueue(Username, AtTop, QUrls) ->
    gen_server:call(player, {enqueue, AtTop, tqueue:chown(Username, QUrls)}).
dequeue(QEntry) -> gen_server:call(player, {dequeue, QEntry}).
raise(QEntry) -> gen_server:call(player, {raise, QEntry}).
lower(QEntry) -> gen_server:call(player, {lower, QEntry}).
get_queue() -> gen_server:call(player, get_queue).
skip() -> gen_server:call(player, skip).
pause(On) -> gen_server:call(player, {pause, On}).
clear_queue() -> gen_server:call(player, clear_queue).

%---------------------------------------------------------------------------

-record(state, {status, is_paused, current_entry, queue, elapsed_acc, last_et_event}).

reset_play_time(State) ->
    State#state{elapsed_acc = 0,
		last_et_event = erlang:now()}.

act_on(State=#state{status = idle, is_paused = IsPaused, queue = TQ}) ->
    case queue:out(TQ) of
	{empty, _} -> State;
	{{value, Entry=#entry{url = Url}}, TQ1} ->
	    reset_play_time(State#state{status = cache(Url, IsPaused),
					current_entry = Entry,
					queue = TQ1})
    end;
act_on(State) -> State.

elapsed_time(#state{is_paused = true,
		    elapsed_acc = ElapsedAcc}) ->
    ElapsedAcc;
elapsed_time(#state{is_paused = false,
		    elapsed_acc = ElapsedAcc,
		    last_et_event = LastEtEvent}) ->
    ElapsedAcc + (timer:now_diff(erlang:now(), LastEtEvent) / 1000000).

summarise_state(State = #state{queue = Q, current_entry = Entry, is_paused = IsPaused}) ->
    StatusSymbol = case State#state.status of
		       idle -> idle;
		       {Other, _PlayerDetails} -> Other
		   end,
    {StatusSymbol, Q, Entry, IsPaused, elapsed_time(State)}.

act_and_reply(State) ->
    State1 = act_on(State),
    {reply, summarise_state(State1), State1}.


cache(Url, IsPaused) ->
    Extension = filename:extension(Url),
    {ok, Template} = player_mapping(Extension),
    CacheRef = make_ref(),
    urlcache:cache(Url, self(), CacheRef),
    receive
	{urlcache, ok, CacheRef, LocalFileName} ->
		{obj,Keys} = urlcache:info_to_json(urlcache:get_info(Url)),
		Artist = binary_to_list(lastfm:get_field("artistName", Keys)),
		Title = binary_to_list(lastfm:get_field("trackName", Keys)),
		lastfm:scrobble(?LASTFM_USER, ?LASTFM_PASSWORD,[{artist,Artist},{track,Title}]),
	    play(Template, LocalFileName, IsPaused)
    after 100 ->
	    {caching, {Template, CacheRef}}
    end.

play(Template, LocalFileName, IsPaused) ->
    [Program | CommandLine] = lists:map(fun
					    (url) -> LocalFileName;
					    (Part) -> Part
					end, Template),
    PlayerPid = execdaemon:run(Program, CommandLine),
    send_pause(PlayerPid, IsPaused),
    {playing, PlayerPid}.


expand_m3us_and_cache(List) ->
    Urls = expand_m3us(queue:to_list(List), []),
    ok = lists:foreach(fun (#entry{url = Url}) -> urlcache:cache(Url) end, Urls),
    queue:from_list(Urls).

expand_m3us([], Acc) ->
    lists:flatten(lists:reverse(Acc));
expand_m3us([TQEntry|Tail], Acc) ->
    Url = TQEntry#entry.url,
    TQEntry2 = case string:right(Url, 4) of
		   ".m3u" ->
		       fetch_m3u(Url, TQEntry#entry.username);
		   _Else ->
		       TQEntry
	       end,
    expand_m3us(Tail, [TQEntry2|Acc]).

fetch_m3u(Url, Username) ->
    case spider:retrieve(Url) of
	{ok, "2"++_CodeRest, _Headers, Body} ->
	    Entries = lists:filter(fun(E) -> "#" /= string:left(E,1) end, string:tokens(Body, "\r\n")),
	    {ok, Base, _Count} = regexp:sub(Url, "/[^/]*$", "/" %% " emacs balancer
					   ),
	    CurriedResolveRelative = fun(Relative) ->
					     UrlEncRel = spider:url_encode_path(Relative),
					     spider:resolve_relative(Base, UrlEncRel)
				     end,
	    CorrectUrls = lists:map(CurriedResolveRelative, Entries),
	    lists:map(fun (U) -> tqueue:tqueue_entry(U, Username) end, CorrectUrls);
	_Else ->
	    []
    end.

send_pause(PlayerPid, IsPaused) ->
    execdaemon:command(PlayerPid, sendsig,
		       case IsPaused of
			   true -> "STOP";
			   false -> "CONT"
		       end),
    ok.

stop_current_playback(State = #state{current_entry = Entry}, WaitForTermination) ->
    case State#state.status of
	{playing, PlayerPid} ->
	    execdaemon:command(PlayerPid, sendsig, "KILL"),
	    case WaitForTermination of
		true -> execdaemon:wait_for_event(PlayerPid);
		false -> ok
	    end;
	_ -> ok
    end,
    {Entry, make_idle(State)}.

make_idle(State) ->
    State#state{status = idle,
		current_entry = null}.

save_state(#state{queue = Q}) ->
    file:write_file("ejukebox.state", rfc4627:encode({obj, [{"queue", tqueue:to_json(Q)}]})).

clean_state() ->
    make_idle(reset_play_time(#state{is_paused = false,
				     queue = queue:new()})).

load_state() ->
    State = case file:read_file("ejukebox.state") of
		{ok, PickledStateStr} ->
		    {ok, PickledState, _Remainder} = rfc4627:decode(PickledStateStr),
		    case rfc4627:get_field(PickledState, "queue") of
			{ok, JsonQ} ->
			    (clean_state())#state{queue = tqueue:from_json(JsonQ)};
			not_found ->
			    clean_state()
		    end;
		_ ->
		    clean_state()
	    end,
    State#state{is_paused = not(queue:is_empty(State#state.queue))}.

init(_Args) ->
    process_flag(trap_exit, true), %% so that we get an opportunity to run terminate().
    {ok, act_on(load_state())}.

handle_call({enqueue, AtTop, Q}, _From, State) ->
    Q1 = expand_m3us_and_cache(Q),
    Q2 = case AtTop of
	     true -> queue:join(Q1, State#state.queue);
	     false -> queue:join(State#state.queue, Q1)
	 end,
    act_and_reply(State#state{queue=Q2});
handle_call({dequeue, QEntry}, _From, State) ->
    act_and_reply(State#state{queue=tqueue:dequeue(QEntry, State#state.queue)});
handle_call({raise, QEntry}, _From, State) ->
    act_and_reply(State#state{queue=tqueue:raise(QEntry, State#state.queue)});
handle_call({lower, QEntry}, _From, State) ->
    act_and_reply(State#state{queue=tqueue:lower(QEntry, State#state.queue)});
handle_call(get_queue, _From, State) ->
    act_and_reply(State);
handle_call(skip, _From, State) ->
    {SkippedEntry, IdleState} = stop_current_playback(State, true),
    NewState = act_on(IdleState),
    {reply, {ok, SkippedEntry, summarise_state(NewState)}, NewState};
handle_call({pause, On}, _From, State) ->
    case State#state.status of
	{playing, PlayerPid} -> send_pause(PlayerPid, On);
	_ -> ok
    end,
    act_and_reply(State#state{is_paused = On,
			      elapsed_acc = elapsed_time(State),
			      last_et_event = erlang:now()});
handle_call(clear_queue, _From, State) ->
    act_and_reply(State#state{queue = queue:new()}).

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({execdaemon_event, PlayerPid, _Code, _Aux}, State) ->
    execdaemon:terminate(PlayerPid),
    {noreply, State};
handle_info({execdaemon_eof, _PlayerPid}, State) ->
    {noreply, act_on(make_idle(State))};
handle_info({urlcache, ok, ReceivedRef, LocalFileName},
	    State = #state{status = {caching, {Template, CacheRef}},
			   is_paused = IsPaused})
  when ReceivedRef =:= CacheRef ->
    {noreply, act_on(reset_play_time(State#state{status = play(Template,
							       LocalFileName,
							       IsPaused)}))};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Subprocess: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    {InterruptedEntry, IdleState} = stop_current_playback(State, false),
    OldQ = IdleState#state.queue,
    StateForStartup = IdleState#state{queue = case InterruptedEntry of
						  null -> OldQ;
						  E -> queue:in_r(E, OldQ)
					      end},
    save_state(StateForStartup),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
