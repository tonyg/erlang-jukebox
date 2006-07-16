-module(tqueue).
-include("tqueue.hrl").

-export([from_list/2, entry_to_json/1, entry_from_json/1, to_json/1, from_json/1]).
-export([search/2, search/3, chown/2, finish_search/1]).
-export([raise/2, lower/2, dequeue/2]).

tqueue_entry(Url, Username) ->
    #entry{id = {node(), now()}, url = Url, username = Username}.

from_list(Urls, Username) ->
    queue:from_list(lists:map(fun (U) -> tqueue_entry(U, Username) end, Urls)).

entry_to_json(#entry{id = {Node, Stamp}, url = Url, username = Username}) ->
    {struct, [{id, {array, [atom_to_list(Node),
			    {array, tuple_to_list(Stamp)}]}},
	      {url, Url},
	      {username, Username}]}.

entry_from_json(J) ->
    {array, [NodeStr, {array, StampList}]} = json:obj_fetch(id, J),
    Url = json:obj_fetch(url, J),
    Username = json:obj_fetch(username, J),
    #entry{id = {list_to_atom(NodeStr), list_to_tuple(StampList)},
	   url = Url,
	   username = Username}.

to_json(Q) ->
    {array, lists:map(fun entry_to_json/1, queue:to_list(Q))}.

from_json({array, Entries}) ->
    queue:from_list(lists:map(fun entry_from_json/1, Entries)).

search(Keys, Q) ->
    search(Keys, Q, []).

search(Keys0, Q, Acc0) ->
    Keys = lists:map(fun http_util:to_lower/1, Keys0),
    lists:foldl(fun (Entry=#entry{url = TrackUrl}, Acc) ->
			case matches_all(Keys, http_util:to_lower(TrackUrl)) of
			    true -> [Entry | Acc];
			    false -> Acc
			end
		end, Acc0, queue:to_list(Q)).

chown(Username, Q) ->
    queue:from_list(lists:map(fun (Entry) ->
				      Entry#entry{username = Username}
			      end, queue:to_list(Q))).

finish_search(Acc) ->
    queue:from_list(lists:keysort(3, Acc)). %% 3 = index of url - TODO find a better way

matches_all(Keys, Candidate) ->
    lists:all(fun (Key) ->
		      case string:str(Candidate, Key) of
			  0 -> false;
			  _ -> true
		      end
	      end, Keys).

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
