-module(volume).
-behaviour(gen_server).

-export([start_link/0]).
-export([get/0, set/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, volume}, volume, [], []).

%---------------------------------------------------------------------------

get() -> gen_server:call(volume, get).
set(NewVol) -> gen_server:call(volume, {set, NewVol}).

%---------------------------------------------------------------------------

hmix_get_volume() ->
    Out = os:cmd("hmix | grep MASTER"),
    case lists:prefix("MASTER", Out) of
	true ->
	    {match, Start, Length} = regexp:match(Out, "[0-9]+"),
	    VolStr = string:substr(Out, Start, Length),
	    list_to_integer(VolStr);
	false ->
	    null
    end.

hmix_set_volume(NewVol) ->
    os:cmd("hmix -master " ++ integer_to_list(NewVol)),
    hmix_get_volume().

init(_Args) ->
    {ok, unknown}.

handle_call(Request, From, unknown) ->
    handle_call(Request, From, hmix_get_volume());
handle_call(get, _From, Volume) ->
    {reply, Volume, Volume};
handle_call({set, VolArg}, _From, _Volume) ->
    NewVol = hmix_set_volume(VolArg),
    {reply, NewVol, unknown}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
