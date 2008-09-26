-module(jukebox).
-behaviour(application).

-export([start/2, stop/1, log_error/2]).
-export([start/0, stop/0, stop_and_halt/0]).

start() ->
    application:start(jukebox).

stop() ->
    application:stop(jukebox).

stop_and_halt() ->
    spawn(fun () ->
                  SleepTime = 1000,
                  timer:sleep(SleepTime),
                  halt(0)
          end),
    case catch stop() of _ -> ok end.

start(normal, []) ->
    {ok, _SupPid} = jukebox_supervisor:start_link().

stop(_State) ->
    ok.

log_error(WhoStr, JsonFields) ->
    history:record(history, WhoStr,
		   {error, [{"error", {obj, JsonFields}}]}).
