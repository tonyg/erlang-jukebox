-module(jukeboxsession).
-include("tqueue.hrl").
-export([initial_state/1, handler/3]).

-record(session, {username, ip}).

default_name(Session) ->
    Digits = tuple_to_list(Session#session.ip),
    lists:flatten(io_lib:format("~p.~p.~p.~p", Digits)).

initial_state(IP) ->
    #session{username = default_name(#session{ip = IP}),
	     ip = IP}.

user_id(Session) ->
    {Session#session.username, Session#session.ip}.

r_user_id(Session) ->
    {U,IP} = user_id(Session),
    {array,[U,{array, tuple_to_list(IP)}]}.

summary_to_json({idle, Q}) ->
    {struct, [{status, "idle"},
	      {entry, null},
	      {queue, tqueue:to_json(Q)}]};
summary_to_json({{Status, Entry}, Q}) ->
    {struct, [{status, atom_to_list(Status)},
	      {entry, tqueue:entry_to_json(Entry)},
	      {queue, tqueue:to_json(Q)}]}.

history_to_json(H) ->
    {array, lists:map(fun ({Who, {What, Entry}}) ->
			      {struct, [{who, Who},
					{what, atom_to_list(What)}
					| Entry]}
		      end, H)}.

log(Session, What, JsonFields) ->
    history:record(history, Session#session.username, {What, JsonFields}).

%% handler(State which we mostly ignore, Request = {call, Method, Arglist}, Session)
handler(State, Request, undefined) ->
    handler(State, Request, State);
handler(_, {call, login, [NewName]}, Session) ->
    NewSession = Session#session{username = NewName},
    if
	Session#session.username /= NewName ->
	    log(NewSession, login, []);
	true -> ok
    end,
    {true, 0, NewSession, {response, r_user_id(NewSession)}};
handler(_, {call, whoami, _}, Session) ->
    {false, {response, r_user_id(Session)}};
handler(_, {call, logout, _}, Session) ->
    log(Session, logout, []),
    NewSession = Session#session{username = default_name(Session)},
    {true, 0, NewSession, {response, r_user_id(NewSession)}};
handler(_, {call, search, [{array, Keys}]}, _Session) ->
    Tracks = trackdb:search_tracks(Keys),
    {false, {response, tqueue:to_json(Tracks)}};
handler(_, {call, enqueue, [EntryList]}, Session) ->
    Q = tqueue:from_json(EntryList),
    player:enqueue(Session#session.username, Q),
    {false, {response, summary_to_json(player:get_queue())}};
handler(_, {call, dequeue, [Entry]}, _Session) ->
    player:dequeue(tqueue:entry_from_json(Entry)),
    {false, {response, summary_to_json(player:get_queue())}};
handler(_, {call, get_queue, _}, _) ->
    {false, {response, summary_to_json(player:get_queue())}};
handler(_, {call, skip, _}, Session) ->
    {ok, SkippedTrack, NewState} = player:skip(),
    log(Session, skip, [{track, tqueue:entry_to_json(SkippedTrack)}]),
    {false, {response, summary_to_json(NewState)}};
handler(_, {call, pause, [P]}, _) ->
    {false, {response, summary_to_json(player:pause(P))}};
handler(_, {call, clear_queue, _}, _) ->
    {false, {response, summary_to_json(player:clear_queue())}};
handler(_, {call, get_history, [N]}, _) ->
    {false, {response, history_to_json(history:retrieve(history, N))}};
handler(_, {call, chat, [Message]}, Session) ->
    log(Session, says, [{message, Message}]),
    {false, {response, true}};
handler(_, {call, get_volume, _}, _) ->
    {false, {response, {struct, [{volume, volume:get()}]}}};
handler(_, {call, set_volume, [NewVol]}, _) ->
    {false, {response, {struct, [{volume, volume:set(NewVol)}]}}}.
