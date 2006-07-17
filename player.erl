-module(player).
-include("tqueue.hrl").
-behaviour(gen_server).

-export([start/1]).
-export([supports_extension/1]).
-export([enqueue/2, dequeue/1, raise/1, lower/1, get_queue/0, skip/0, pause/1, clear_queue/0]).
-export([get_volume/0, set_volume/1]).
-export([init/1, handle_call/3, handle_info/2]).

%---------------------------------------------------------------------------

start(Sconf) ->
    io:format("Starting player.~n"),
    gen_server:start_link({local, player}, player, [], []).

%---------------------------------------------------------------------------

supports_extension(Extension) ->
    case player_mapping(Extension) of
	{ok, _CommandLine} -> true;
	_ -> false
    end.

player_mapping(E) -> player_mapping1(http_util:to_lower(E)).

player_mapping1(".ogg") -> {ok, ["/usr/bin/env", "ogg123", "-q", url]};
player_mapping1(".mp3") -> {ok, ["/usr/bin/env", "mpg123", "-q", url]};
player_mapping1(_) -> not_playable.

enqueue(Username, QUrls) -> gen_server:call(player, {enqueue, tqueue:chown(Username, QUrls)}).
dequeue(QEntry) -> gen_server:call(player, {dequeue, QEntry}).
raise(QEntry) -> gen_server:call(player, {raise, QEntry}).
lower(QEntry) -> gen_server:call(player, {lower, QEntry}).
get_queue() -> gen_server:call(player, get_queue).
skip() -> gen_server:call(player, skip).
pause(On) -> gen_server:call(player, {pause, On}).
clear_queue() -> gen_server:call(player, clear_queue).

get_volume() -> gen_server:call(player, get_volume).
set_volume(NewVol) -> gen_server:call(player, {set_volume, NewVol}).

%---------------------------------------------------------------------------

-record(state, {status, queue}).

init(_Args) ->
    {ok, #state{status = idle, queue = queue:new()}}.

act_on(State=#state{status = idle, queue = TQ}) ->
    case queue:out(TQ) of
	{empty, _} -> State;
	{{value, Entry=#entry{url = Url}}, TQ1} ->
	    State#state{status = {playing, Entry, play(Url)}, queue = TQ1}
    end;
act_on(State) -> State.

summarise_state(State) ->
    Q = State#state.queue,
    case State#state.status of
	idle -> {idle, Q};
	{Other, Entry, _PlayerDetails} -> {{Other, Entry}, Q}
    end.

act_and_reply(State) ->
    State1 = act_on(State),
    {reply, summarise_state(State1), State1}.

handle_call({enqueue, Q}, _From, State) ->
    act_and_reply(State#state{queue=queue:join(State#state.queue, Q)});
handle_call({dequeue, QEntry}, _From, State) ->
    act_and_reply(State#state{queue=tqueue:dequeue(QEntry, State#state.queue)});
handle_call({raise, QEntry}, _From, State) ->
    act_and_reply(State#state{queue=tqueue:raise(QEntry, State#state.queue)});
handle_call({lower, QEntry}, _From, State) ->
    act_and_reply(State#state{queue=tqueue:lower(QEntry, State#state.queue)});
handle_call(get_queue, _From, State) ->
    act_and_reply(State);
handle_call(skip, _From, State) ->
    Entry = case State#state.status of
		idle -> null;
		{_Other, CurrentEntry, PlayerPid} ->
		    execdaemon:command(PlayerPid, sendsig, "KILL"),
		    execdaemon:wait_for_event(PlayerPid),
		    CurrentEntry
	    end,
    NewState = act_on(State#state{status = idle}),
    {reply, {ok, Entry, summarise_state(NewState)}, NewState};
handle_call({pause, On}, _From, State) ->
    case State#state.status of
	idle -> act_and_reply(State);
	{_Other, Entry, PlayerPid} ->
	    NewState = case On of
			   true -> execdaemon:command(PlayerPid, sendsig, "STOP"),
				   paused;
			   false -> execdaemon:command(PlayerPid, sendsig, "CONT"),
				    playing
		       end,
	    act_and_reply(State#state{status = {NewState, Entry, PlayerPid}})
    end;
handle_call(clear_queue, _From, State) ->
    act_and_reply(State#state{queue = queue:new()});
handle_call(get_volume, _From, State) ->
    {reply, hmix_get_volume(), State};
handle_call({set_volume, NewVol}, _From, State) ->
    {reply, hmix_set_volume(NewVol), State}.

handle_info({execdaemon_event, PlayerPid, _Code, _Aux}, State) ->
    execdaemon:terminate(PlayerPid),
    {noreply, State};
handle_info({execdaemon_eof, _PlayerPid}, State) ->
    {noreply, act_on(State#state{status = idle})};
handle_info(Msg, State) ->
    io:format("Subprocess: ~p~n", Msg),
    {noreply, State}.

play(Url) ->
    Extension = filename:extension(Url),
    {ok, Template} = player_mapping(Extension),
    [Program | CommandLine] = lists:map(fun
					    (url) -> Url;
					    (Part) -> Part
					end, Template),
    execdaemon:run(Program, CommandLine).

hmix_get_volume() ->
    Out = os:cmd("hmix | grep MASTER"),
    case lists:prefix("MASTER", Out) of
	true ->
	    {match, Start, Length} = regexp:match(Out, "[0-9]+"),
	    VolStr = string:substr(Out, Start, Length),
	    list_to_integer(VolStr);
	false ->
	    unavailable
    end.

hmix_set_volume(NewVol) ->
    os:cmd("hmix -master " ++ integer_to_list(NewVol)),
    hmix_get_volume().
