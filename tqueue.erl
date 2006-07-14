-module(tqueue).

-export([from_list/1]).

tqueue_entry(Url) ->
    {{node(), now()}, Url}.

from_list(Urls) ->
    queue:from_list(lists:map(fun tqueue_entry/1, Urls)).
