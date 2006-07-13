-module(main).
-export([start/1]).

start(Sconf) ->
    spider:start(Sconf),
    trackdb:start(Sconf).
