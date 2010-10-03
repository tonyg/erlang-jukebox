-module(volume).
-behaviour(gen_server).
-include("settings.hrl").

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

% PulseAudio standard max value
-define(MAX_VOL, 65535).

pactl_get_volume() -> 
	Get_vol = "pactl list " ++ ?PACTL_EXTRA_ARGS,
	%io:format("~p~n", [Get_vol]), 
	Out = string:strip(os:cmd(Get_vol)),
	case regexp:first_match(Out, "Sink #" ++ ?PACTL_SINK ++ "\n(\t[^\n]+\n)+") of
		{match, SinkStart, SinkLength} ->
			SinkStr = string:substr(Out, SinkStart, SinkLength),
			case regexp:first_match(SinkStr, "Volume: 0:[0-9\t ]+") of
				{match, Start, Length} ->
					VolLen = string:len("Volume: 0:"),
					VolStr = string:strip(string:substr(SinkStr, Start + VolLen, Length-VolLen)),
					list_to_integer(VolStr);
				nomatch ->
					'Can\'t find sink ' ++ ?PACTL_SINK ++ ' in pactl output'
			end;
		nomatch ->
			'Install pactl please'
	end.

pactl_set_volume(NewVol) ->
	Set_vol = "pactl set-sink-volume 0 " ++ integer_to_list( round(NewVol * ?MAX_VOL / 100)) ++ ?PACTL_EXTRA_ARGS,
	%io:format("~p~n", [Set_vol]), 
	os:cmd(Set_vol),
    pactl_get_volume().

init(_Args) ->
    {ok, {unknown, "", up}}.

handle_call(Request, From, {unknown, Who, Direction}) ->
    handle_call(Request, From, {pactl_get_volume(), Who, Direction});
handle_call(get, _From, State) ->
    {reply, State, State};
handle_call({set, Who, VolArg}, _From, {OldVol, _Who, _Direction}) ->
    NewVol = pactl_set_volume(VolArg),
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
