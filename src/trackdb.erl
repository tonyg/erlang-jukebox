-module(trackdb).
-behaviour(gen_server).

-export([start_link/0]).

-export([all_roots/0, current_rescans/0, remove_root/1, rescan_root/1, update_root/2]).
-export([search_tracks/1, random_tracks/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Client interface

%% new_guid() ->  {node(), erlang:now()}.

start_link() ->
    gen_server:start_link({local, trackdb}, trackdb, [], []).

all_roots() -> gen_server:call(trackdb, all_roots).
current_rescans() -> gen_server:call(trackdb, current_rescans).
remove_root(Url) -> gen_server:call(trackdb, {remove_root, Url}).
rescan_root(Url) -> gen_server:call(trackdb, {rescan_root, Url}).
update_root(Url, TrackUrls) -> gen_server:call(trackdb, {update_root, Url, TrackUrls}).

search_tracks(Keys) -> gen_server:call(trackdb, {search_tracks, Keys}, 10000).
random_tracks(ApproxCount) -> gen_server:call(trackdb, {random_tracks, ApproxCount}, 10000).

% Server-side

-record(v1, {roots}). % roots :: dict(url, tqueue)
-record(v2, {roots}). % roots :: dict(url, {count, tqueue})

upgrade_state(State=#v2{}) ->
    State;
upgrade_state(#v1{roots = OldRoots}) ->
    upgrade_state(#v2{roots=dict:map(fun(_Url, Q) ->
					     {queue:len(Q), Q}
				     end, OldRoots)});
upgrade_state(State) -> %% unlabeled version.
    dict:fetch_keys(State), %% ensure it's a dictionary
    upgrade_state(#v1{roots=State}).

searcher(From, Keys, Roots) ->
    Matches = dict:fold(fun (_Url, {_QLen, Q}, Acc) -> tqueue:search(Keys, Q, Acc) end, [], Roots),
    gen_server:reply(From, tqueue:finish_search(Matches)).

randomizer(From, ApproxCount, Roots) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    Entries = dict:to_list(Roots),
    TotalTrackCount = lists:sum(lists:map(fun ({_Url, {QLen, _Q}}) -> QLen end, Entries)),
    Proportion = ApproxCount / TotalTrackCount,
    Selected = lists:foldl(fun ({_Url, {_QLen, Q}}, AccOuter) ->
				   lists:foldl(fun (Entry, Acc) ->
						       V = random:uniform(),
						       if
							   V < Proportion ->
							       [Entry | Acc];
							   true ->
							       Acc
						       end
					       end, AccOuter, queue:to_list(Q))
			   end, [], Entries),
    gen_server:reply(From, tqueue:finish_search(Selected)).

rescanner(Url) ->
    update_root(Url, tqueue:from_list(spider:spider(Url), null)).

read_db() ->
    case file:read_file("state/ejukebox.db") of
	{ok, State} -> {ok, {[], upgrade_state(binary_to_term(State))}};
	{error, enoent} -> {ok, {[], #v2{roots = dict:new()}}}
    end.

init(_Args) ->
	case file:make_dir("state") of
		ok -> read_db();
		{error, eexist} -> read_db()
	end.

handle_call(all_roots, _From, S={_, State}) ->
    {reply,
     lists:map(fun ({Url, {QLen, _Q}}) -> {Url, QLen} end,
	       dict:to_list(State#v2.roots)),
     S};

handle_call(current_rescans, _From, S={Rescans, _}) ->
    {reply, Rescans, S};

handle_call({remove_root, Url}, _From, {Rescans, State}) ->
    {reply, ok, {Rescans, State#v2{roots = dict:erase(Url, State#v2.roots)}}};

handle_call({rescan_root, Url}, _From, {Rescans, State}) ->
    {reply, spawn(fun () -> rescanner(Url) end), {[Url | Rescans], State}};

handle_call({update_root, Url, Q}, _From, {Rescans, State}) ->
    NewState = State#v2{roots = dict:store(Url, {queue:len(Q), Q}, State#v2.roots)},
    file:write_file("ejukebox.db", term_to_binary(NewState)),
    {reply, ok, {lists:delete(Url, Rescans), NewState}};

handle_call({search_tracks, Keys}, From, S={_, State}) ->
    spawn(fun () -> searcher(From, Keys, State#v2.roots) end),
    {noreply, S};

handle_call({random_tracks, ApproxCount}, From, S={_, State}) ->
    spawn(fun () -> randomizer(From, ApproxCount, State#v2.roots) end),
    {noreply, S}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
