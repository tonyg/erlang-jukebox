-module(trackdb).
-behaviour(gen_server).

-export([start/1]).

-export([snapshot/0]).
-export([all_roots/0, remove_root/1, rescan_root/1, update_root/2]).
-export([search_tracks/1]).

-export([init/1, handle_call/3]).

% Client interface

%% new_guid() ->  {node(), erlang:now()}.

start(Sconf) ->
    io:format("Starting trackdb.~n"),
    gen_server:start_link({local, trackdb}, trackdb, [], []).

snapshot() -> gen_server:call(trackdb, snapshot).
all_roots() -> gen_server:call(trackdb, all_roots).
remove_root(Url) -> gen_server:call(trackdb, {remove_root, Url}).
rescan_root(Url) -> gen_server:call(trackdb, {rescan_root, Url}).
update_root(Url, TrackUrls) -> gen_server:call(trackdb, {update_root, Url, TrackUrls}).

search_tracks(Keys) -> gen_server:call(trackdb, {search_tracks, Keys}).

% Server-side

init(_Args) ->
    case file:read_file("ejukebox.db") of
	{ok, Data} -> {ok, binary_to_term(Data)};
	{error, enoent} -> {ok, dict:new()}
    end.

handle_call(snapshot, _From, Roots) ->
    {reply, file:write_file("ejukebox.db", term_to_binary(Roots)), Roots};

handle_call(all_roots, _From, Roots) ->
    {reply, dict:fetch_keys(Roots), Roots};

handle_call({remove_root, Url}, _From, Roots) ->
    {reply, ok, dict:erase(Url, Roots)};

handle_call({rescan_root, Url}, _From, Roots) ->
    {reply, spawn(fun () -> rescanner(Url) end), Roots};

handle_call({update_root, Url, TrackUrls}, _From, Roots) ->
    {reply, ok, dict:store(Url, TrackUrls, Roots)};

handle_call({search_tracks, Keys}, From, Roots) ->
    spawn(fun () -> searcher(From, Keys, Roots) end),
    {noreply, Roots}.


matches_all(Keys, Candidate) ->
    lists:all(fun (Key) ->
		      case string:str(Candidate, Key) of
			  0 -> false;
			  _ -> true
		      end
	      end, Keys).

searcher(From, Keys0, Roots) ->
    Keys = lists:map(fun http_util:to_lower/1, Keys0),
    Matches = dict:fold(fun (_Url, TrackUrls, Sofar0) ->
				lists:foldl(fun (TrackUrl, Sofar1) ->
						    case matches_all(Keys,
								     http_util:to_lower(TrackUrl))
							of
							true -> [TrackUrl | Sofar1];
							false -> Sofar1
						    end
					    end, Sofar0, TrackUrls)
			end, [], Roots),
    gen_server:reply(From, Matches).

rescanner(Url) ->
    update_root(Url, spider:spider(Url)).
