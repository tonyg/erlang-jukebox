-module(player).
-behaviour(gen_server).

-export([start/1]).
-export([supports_extension/1]).
-export([enqueue/1, get_queue/0, skip/0, clear_queue/0]).
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
clear_queue() -> gen_server:call(player, clear_queue).

%---------------------------------------------------------------------------

-record(state, {status, queue}).

init(_Args) ->
    {ok, #state{status = idle, queue = []}}.

queue_entry(Url) ->
    {{node(), now()}, Url}.

act_on(#state{status = idle, queue = [Entry | Queue]}) ->
    {_QID, Url} = Entry,
    #state{status = {playing, Entry, play(Url)}, queue = Queue};
act_on(State) -> State.

summarise_state(State) ->
    Q = State#state.queue,
    case State#state.status of
	idle -> {idle, Q};
	{playing, Entry, _PlayerDetails} -> {{playing, Entry}, Q}
    end.

act_and_reply(State) ->
    State1 = act_on(State),
    {reply, summarise_state(State1), State1}.

handle_call({enqueue, Urls}, _From, State) ->
    act_and_reply(State#state{queue=(State#state.queue ++ lists:map(fun queue_entry/1, Urls))});
handle_call(get_queue, _From, State) ->
    act_and_reply(State);
handle_call(skip, _From, State) ->
    case State#state.status of
	idle ->
	    ok;
	{playing, _Entry, {UnixPid, Port}} ->
	    os:cmd("kill " ++ integer_to_list(UnixPid)),
	    receive
		{Port, {exit_status, _Code}} -> ok
	    end
    end,
    act_and_reply(State#state{status = idle});
handle_call(clear_queue, _From, State) ->
    act_and_reply(State#state{queue = []}).

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
