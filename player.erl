-module(player).
-behaviour(gen_server).

-export([start/1]).
-export([supports_extension/1]).
-export([enqueue/1, get_queue/0, skip/0, pause/1, clear_queue/0]).
-export([init/1, handle_call/3, handle_info/2]).

-export([join/2]).

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

enqueue(Urls) -> gen_server:call(player, {enqueue, Urls}).
get_queue() -> gen_server:call(player, get_queue).
skip() -> gen_server:call(player, skip).
pause(On) -> gen_server:call(player, {pause, On}).
clear_queue() -> gen_server:call(player, clear_queue).

%---------------------------------------------------------------------------

-record(state, {status, queue}).

init(_Args) ->
    {ok, #state{status = idle, queue = queue:new()}}.

act_on(State=#state{status = idle, queue = TQ}) ->
    case queue:out(TQ) of
	{empty, _} -> State;
	{{value, Entry={_QID,Url}}, TQ1} ->
	    State#state{status = {playing, Entry, play(Url)}, queue = TQ1}
    end;
act_on(State) -> State.

summarise_state(State) ->
    Q = queue:to_list(State#state.queue),
    case State#state.status of
	idle -> {idle, Q};
	{Other, Entry, _PlayerDetails} -> {{Other, Entry}, Q}
    end.

act_and_reply(State) ->
    State1 = act_on(State),
    {reply, summarise_state(State1), State1}.

handle_call({enqueue, Urls}, _From, State) ->
    act_and_reply(State#state{queue=queue:join(State#state.queue, tqueue:from_list(Urls))});
handle_call(get_queue, _From, State) ->
    act_and_reply(State);
handle_call(skip, _From, State) ->
    case State#state.status of
	idle ->
	    ok;
	{_Other, _Entry, {UnixPid, Port}} ->
	    os:cmd("kill -KILL " ++ integer_to_list(UnixPid)),
	    receive
		{Port, {exit_status, _Code}} -> ok
	    end
    end,
    act_and_reply(State#state{status = idle});
handle_call({pause, On}, _From, State) ->
    case State#state.status of
	idle -> act_and_reply(State);
	{_Other, Entry, {UnixPid, Port}} ->
	    NewState = case On of
			   true -> os:cmd("kill -TSTP " ++ integer_to_list(UnixPid)), paused;
			   false -> os:cmd("kill -CONT " ++ integer_to_list(UnixPid)), playing
		       end,
	    act_and_reply(State#state{status = {NewState, Entry, {UnixPid, Port}}})
    end;
handle_call(clear_queue, _From, State) ->
    act_and_reply(State#state{queue = queue:new()}).

handle_info({Port, {exit_status, Code}}, State) when is_port(Port) ->
    {noreply, act_on(State#state{status = idle})};
handle_info(Msg, State) ->
    {noreply, State}.

join([], _) -> "";
join([X], _) -> X;
join([X | XS], Sep) -> X ++ Sep ++ join(XS, Sep).

play(Url) ->
    Extension = filename:extension(Url),
    {ok, Template} = player_mapping(Extension),
    CommandLine = lists:map(fun (url) ->
				    Url;
				(Part) -> Part
			    end, Template),
    Cmd = join(["./wrapper.sh" | CommandLine], " "),
    Port = open_port({spawn, Cmd}, [exit_status]),
    receive
	{Port, {data, UnixPid}} ->
	    {list_to_integer(string:strip(UnixPid, right, $\n)), Port}
    end.
