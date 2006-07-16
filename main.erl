-module(main).
-export([start/1]).

start(Sconf) ->
    spider:start(Sconf),
    trackdb:start(Sconf),
    player:start(Sconf),
    history:start(history, 100).
