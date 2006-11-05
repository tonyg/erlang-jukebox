-module(jrpc).
-include("yaws_api.hrl").
-export([serve/2]).

update_prop(_K, _Fun, []) ->
    [];
update_prop(K, Fun, [E | L]) when is_tuple(E), element(1, E) == K ->
    [Fun(E) | L];
update_prop(K, Fun, [E | L]) ->
    [E | update_prop(K, Fun, L)].

serve(A, Modulename) ->
    {ok, {IP, _Port}} = inet:peername(A#arg.clisock),
    A2 = A#arg{state = apply(Modulename, initial_state, [IP])},
    %% Workaround to set the MIME type of the response.
    update_prop(content, fun ({Key, _MimeType, Body}) -> {Key, "application/json", Body} end,
		yaws_rpc:handler_session(A2, {Modulename, handler})).
