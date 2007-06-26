-module(configsession).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    mod_jsonrpc:register_service
      (Pid,
       mod_jsonrpc:service(<<"config">>,
                           <<"urn:uuid:a4b32870-fac5-4d7f-bacc-187e77db928c">>,
                           <<"1.0.0">>,
                           [{<<"current_rescans">>, []},
                            {<<"all_roots">>, []},
                            {<<"remove_root">>, [{"url", str}]},
                            {<<"rescan_root">>, [{"url", str}]},
                            {<<"snapshot">>, []}])),
    {ok, Pid}.

%---------------------------------------------------------------------------

lists_to_binaries([]) ->
    [];
lists_to_binaries([H|Rest]) ->
    [list_to_binary(H) | lists_to_binaries(Rest)].

roots_to_json([]) ->
    [];
roots_to_json([{Url,QLen}|Rest]) ->
    [{obj, [{"url", list_to_binary(Url)},
	    {"count", QLen}]} | roots_to_json(Rest)].

init(_Args) ->
    {ok, no_state}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call({jsonrpc, <<"current_rescans">>, _ModData, []}, _From, State) ->
    {reply, {result, lists_to_binaries(trackdb:current_rescans())}, State};

handle_call({jsonrpc, <<"all_roots">>, _ModData, []}, _From, State) ->
    {reply, {result, roots_to_json(trackdb:all_roots())}, State};

handle_call({jsonrpc, <<"remove_root">>, _ModData, [Url]}, _From, State) ->
    trackdb:remove_root(binary_to_list(Url)),
    {reply, {result, roots_to_json(trackdb:all_roots())}, State};

handle_call({jsonrpc, <<"rescan_root">>, _ModData, [Url]}, _From, State) ->
    trackdb:rescan_root(binary_to_list(Url)),
    {reply, {result, lists_to_binaries(trackdb:current_rescans())}, State};

handle_call({jsonrpc, <<"snapshot">>, _ModData, []}, _From, State) ->
    trackdb:snapshot(),
    {reply, {result, true}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
