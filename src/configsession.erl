-module(configsession).
-export([initial_state/1, handler/3]).

initial_state(_IP) -> ok.

handler(_, {call, current_rescans, _}, _) ->
    {false, {response, {array, trackdb:current_rescans()}}};
handler(_, {call, all_roots, _}, _) ->
    {false, {response, {array, trackdb:all_roots()}}};
handler(_, {call, remove_root, [Url]}, _) ->
    trackdb:remove_root(Url),
    {false, {response, {array, trackdb:all_roots()}}};
handler(_, {call, rescan_root, [Url]}, _) ->
    trackdb:rescan_root(Url),
    {false, {response, {array, trackdb:current_rescans()}}};
handler(_, {call, snapshot, _}, _) ->
    trackdb:snapshot(),
    {false, {response, true}}.
