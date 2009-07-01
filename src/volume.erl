-module(volume).
-behaviour(gen_server).

-export([start_link/0]).
-export([get/0, set/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, volume}, volume, [], []).

%---------------------------------------------------------------------------

get() -> gen_server:call(volume, get).
set(Who, NewVol) -> gen_server:call(volume, {set, Who, NewVol}).

%---------------------------------------------------------------------------

% TODO Parse the Limits: line to find MAX_VOL, configure DEVICE sensibly

% Typical desktop machine
%-define(MAX_VOL, 65536).
%-define(DEVICE, "Master").

% USB audio device on the real jukebox.
-define(MAX_VOL, 44).
-define(DEVICE, "Speaker").

alsa_get_volume() -> 
    Out = string:strip(os:cmd("amixer sget " ++ ?DEVICE ++ " | grep 'Front Left: Playback'")),
    case lists:prefix("Front Left: Playback", Out) of
	true ->
	    {match, Start, Length} = regexp:first_match(Out, "[0-9]+"),
	    VolStr = string:substr(Out, Start, Length),
	    round(list_to_integer(VolStr) * 100 / ?MAX_VOL);
	false ->
	    null
    end.

alsa_set_volume(NewVol) ->
    os:cmd("amixer sset " ++ ?DEVICE ++ " " ++ integer_to_list( round(NewVol * ?MAX_VOL / 100) )),
    alsa_get_volume().

init(_Args) ->
    {ok, {unknown, "", up}}.

handle_call(Request, From, {unknown, Who, Direction}) ->
    handle_call(Request, From, {alsa_get_volume(), Who, Direction});
handle_call(get, _From, State) ->
    {reply, State, State};
handle_call({set, Who, VolArg}, _From, {OldVol, _Who, _Direction}) ->
    NewVol = alsa_set_volume(VolArg),
    Direction = 
        case VolArg > OldVol of
            true -> up;
            false -> down
        end,
    {reply, {NewVol, Who, Direction}, {unknown, Who, Direction}}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
