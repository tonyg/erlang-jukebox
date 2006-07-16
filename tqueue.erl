-module(tqueue).

-export([from_list/1, entry_to_json/1, entry_from_json/1, to_json/1, from_json/1]).
-export([raise/2, lower/2, dequeue/2]).

tqueue_entry(Url) ->
    {{node(), now()}, Url}.

from_list(Urls) ->
    queue:from_list(lists:map(fun tqueue_entry/1, Urls)).

entry_to_json({{Node, Stamp}, Url}) ->
    {struct, [{id, {array, [atom_to_list(Node),
			    {array, tuple_to_list(Stamp)}]}},
	      {url, Url}]}.

entry_from_json(J) ->
    {array, [NodeStr, {array, StampList}]} = json:obj_fetch(id, J),
    Url = json:obj_fetch(url, J),
    {{list_to_atom(NodeStr), list_to_tuple(StampList)}, Url}.

to_json(Q) ->
    {array, lists:map(fun entry_to_json/1, queue:to_list(Q))}.

from_json({array, Entries}) ->
    queue:from_list(lists:map(fun entry_from_json/1, Entries)).

raise(E, Q) -> queue:from_list(raise1(E, queue:to_list(Q))).
lower(E, Q) -> queue:from_list(lower1(E, queue:to_list(Q))).
dequeue(E, Q) -> queue:from_list(lists:delete(E, queue:to_list(Q))).

raise1(_E, []) ->
    [];
raise1({K,_}, [Y, X={XK,_} | XS]) when K == XK ->
    [X, Y, XS];
raise1(E, [X | XS]) ->
    [X | raise1(E, XS)].

lower1(_E, []) ->
    [];
lower1({K,_}, [X={XK,_}, Y | XS]) when K == XK ->
    [Y, X, XS];
lower1(E, [X | XS]) ->
    [X | lower1(E, XS)].
