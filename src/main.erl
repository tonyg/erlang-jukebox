-module(main).
-export([start/1]).

start(Sconf) ->
    spider:start(Sconf),
    trackdb:start(Sconf),
    player:start(Sconf),
    volume:start(Sconf),
    history:start(history, 100).
