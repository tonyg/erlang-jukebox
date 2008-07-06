-module(urlcache).
-behaviour(gen_server).

-define(CACHE_DIR, "ejukebox_cache").
-define(CACHE_LIMIT_K, (1048576 * 2)).

-export([start_link/0]).
-export([cache/1, cache/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, urlcache}, ?MODULE, [], []).

cache(Url) ->
    gen_server:cast(urlcache, {cache, Url}).

cache(Url, Pid, Ref) ->
    gen_server:cast(urlcache, {cache, Url, Pid, Ref}).

%%---------------------------------------------------------------------------

start_caching(Url) ->
    prune_cache(),
    Filename = local_name_for(Url),
    case get({downloader, Url}) of
	undefined ->
	    DownloaderPid = spawn_link(fun () -> download_and_cache(Filename, Url) end),
	    put({downloader, Url}, DownloaderPid);
	_DownloaderPid ->
	    ok
    end,
    Filename.

local_name_for(Url) ->
    ?CACHE_DIR ++ "/" ++ hexify(binary_to_list(crypto:sha(Url))) ++ ".cachedata".

hexify([]) ->
    "";
hexify([B | Rest]) ->
    [hex_digit((B bsr 4) band 15), hex_digit(B band 15) | hexify(Rest)].

hex_digit(X) when X < 10 ->
    X + $0;
hex_digit(X) ->
    X + $A - 10.

download_and_cache(Filename, Url) ->
    case filelib:is_file(Filename) of
	true ->
	    ok;
	false ->
	    PartFilename = Filename ++ ".part",
	    {ok, "2"++_CodeRest, _, BodyList} = spider:retrieve(Url),
	    ok = file:write_file(PartFilename, BodyList),
	    ok = file:rename(PartFilename, Filename),
	    ok
    end.

prune_cache() ->
    filelib:ensure_dir(?CACHE_DIR ++ "/."),
    [SizeStr | _] = string:tokens(os:cmd("du -sk " ++ ?CACHE_DIR), "\t "),
    Size = list_to_integer(SizeStr),
    if
	Size > ?CACHE_LIMIT_K ->
	    prune_candidate(string:tokens(os:cmd("ls -tr " ++ ?CACHE_DIR), "\r\n")),
	    prune_cache();
	true ->
	    ok
    end.

prune_candidate([]) ->
    no_candidate_available;
prune_candidate([C | Rest]) ->
    case lists:reverse(C) of
	"atadehcac." ++ _ -> %% ".cachedata" backwards
	    file:delete(?CACHE_DIR ++ "/" ++ C),
	    {deleted_candidate, C};
	_ ->
	    prune_candidate(Rest)
    end.

wait_for_completion(LocalFileName, Pid, Ref) ->
    spawn(fun () ->
		  link(Pid),
		  wait_for_completion1(LocalFileName, Pid, Ref)
	  end),
    ok.

wait_for_completion1(LocalFileName, Pid, Ref) ->
    case filelib:is_file(LocalFileName) of
	true ->
	    Pid ! {urlcache, ok, Ref, LocalFileName};
	false ->
	    timer:sleep(200),
	    wait_for_completion1(LocalFileName, Pid, Ref)
    end.

%%---------------------------------------------------------------------------

init(_Args) ->
    {ok, none}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({cache, Url}, State) ->
    start_caching(Url),
    {noreply, State};
handle_cast({cache, Url, Pid, Ref}, State) ->
    LocalFileName = start_caching(Url),
    wait_for_completion(LocalFileName, Pid, Ref),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
