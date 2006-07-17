-module(volume).
-behaviour(gen_server).

-export([start/1]).
-export([get/0, set/1]).
-export([init/1, handle_call/3]).

%---------------------------------------------------------------------------

start(Sconf) ->
    io:format("Starting volume control.~n"),
    gen_server:start_link({local, volume}, volume, [], []).

%---------------------------------------------------------------------------

get() -> gen_server:call(volume, get).
set(NewVol) -> gen_server:call(volume, {set, NewVol}).

%---------------------------------------------------------------------------

init(_Args) ->
    {ok, unknown}.

handle_call(Request, From, unknown) ->
    handle_call(Request, From, hmix_get_volume());
handle_call(get, _From, Volume) ->
    {reply, Volume, Volume};
handle_call({set, VolArg}, _From, _Volume) ->
    NewVol = hmix_set_volume(VolArg),
    {reply, NewVol, unknown}.

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
