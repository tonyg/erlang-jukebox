-module(jukeboxsession).
-export([initial_state/1, handler/3]).

-record(session, {username, ip}).

default_name() ->
    "Anonymous Coward".

initial_state(IP) ->
    #session{username = default_name(),
	     ip = IP}.

user_id(Session) ->
    {Session#session.username, Session#session.ip}.

r_user_id(Session) ->
    {U,IP} = user_id(Session),
    {array,[U,{array, tuple_to_list(IP)}]}.

handler(State, Request, undefined) ->
    handler(State, Request, State);
handler(_, {call, login, [NewName]}, Session) ->
    NewSession = Session#session{username = NewName},
    {true, 0, NewSession, {response, r_user_id(NewSession)}};
handler(_, {call, whoami, _}, Session) ->
    {false, {response, r_user_id(Session)}};
handler(_, {call, logout, _}, Session) ->
    NewSession = Session#session{username = default_name()},
    {true, 0, NewSession, {response, r_user_id(NewSession)}}.
