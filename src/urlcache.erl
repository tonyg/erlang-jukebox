-module(urlcache).
-behaviour(gen_server).
-include("tqueue.hrl").

-define(CACHE_DIR, "ejukebox_cache").
-define(CACHE_LIMIT_K, (1048576 * 2)).

-export([start_link/0]).
-export([cache/1, cache/3, current_downloads/0]).
-export([get_info/1, info_to_json/1, queue_info_json/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, urlcache}, ?MODULE, [], []).

cache(Url) ->
    gen_server:cast(urlcache, {cache, Url}).

cache(Url, Pid, Ref) ->
    gen_server:cast(urlcache, {cache, Url, Pid, Ref}).

current_downloads() ->
    gen_server:call(urlcache, current_downloads).

get_info(null) -> null;
get_info(Url) ->
    MetadataFilename = local_metadata_name_for(Url),
    case file:read_file(MetadataFilename) of
        {error,_} -> null;
        {ok, File} ->
            [StatusLine | Lines] = string:tokens(binary_to_list(File), "\r\n"),
	    case StatusLine of
		"+" ++ _ ->
            dict:from_list(tupleise(Lines, []));
		"-" ++ _ ->
            jukebox:log_error("urlcache",
                      [{"metadata_error", list_to_binary(Lines)}]),
		    dict:new()
	    end
    end.

tupleise([], List) -> List;
tupleise([Name, Value | Rest], List) -> 
    tupleise(Rest, [{Name, Value} | List]).

info_to_json(null) -> null;
info_to_json(Info) ->
    {obj, dict_to_json(Info, dict:fetch_keys(Info), [])}.

dict_to_json(_Dict, [], Acc) ->
    Acc;
dict_to_json(Dict, [Key | Keys], Acc) ->
    dict_to_json(Dict, Keys, [{Key, list_to_binary(dict:fetch(Key, Dict))} | Acc]).

queue_info_json(Q) ->
    UrlList = lists:map(fun (_ = #entry{url = Url}) -> Url end, queue:to_list(Q)),
    lists:map(fun (Url) -> info_to_json(get_info(Url)) end, UrlList).

%%---------------------------------------------------------------------------

start_caching(Url) ->
    prune_cache(),
    Filename = local_name_for(Url),
    MetadataFilename = local_metadata_name_for(Url),
    case get({downloader, Url}) of
	undefined ->
	    CachePid = self(),
	    DownloaderPid = spawn_link(fun () -> download_and_cache(CachePid, Filename, Url) end),
	    put({downloader, Url}, DownloaderPid);
	_DownloaderPid ->
	    ok
    end,
    {Filename, MetadataFilename}.

local_name_prefix(Url) ->
    ?CACHE_DIR ++ "/" ++ hexify(binary_to_list(crypto:sha(Url))).

local_name_for(Url) ->
    local_name_prefix(Url) ++ ".cachedata".

local_metadata_name_for(Url) ->
    local_name_prefix(Url) ++ ".metadata".

hexify([]) ->
    "";
hexify([B | Rest]) ->
    [hex_digit((B bsr 4) band 15), hex_digit(B band 15) | hexify(Rest)].

hex_digit(X) when X < 10 ->
    X + $0;
hex_digit(X) ->
    X + $A - 10.

try_rename(Source, Target, 0, PrevError) ->
    exit({could_not_rename, Source, Target, PrevError});
try_rename(Source, Target, N, _PrevError) ->
    case file:rename(Source, Target) of
	ok -> ok;
	E = {error, _} ->
	    timer:sleep(200),
	    try_rename(Source, Target, N - 1, E)
    end.

quote_for_shell(S) ->
    "'" ++ quote_for_shell1(S).

quote_for_shell1("") ->
    "'";
quote_for_shell1("'" ++ S) ->
    "'\"'\"'" ++ quote_for_shell1(S);
quote_for_shell1([Ch | S]) ->
    [Ch | quote_for_shell1(S)].

download_and_cache(CachePid, Filename, Url) ->
    case filelib:is_file(Filename) of
	true ->
	    %% TODO: touch the file, to avoid needless
	    %% deletion-and-redownloading on cache prune
	    ok;
	false ->
	    PartFilename = Filename ++ ".part",
	    CommandString = "curl -g -s -S -C - -o "++PartFilename++" "++quote_for_shell(Url),
	    case os:cmd(CommandString) of
		"" ->
		    ok = try_rename(PartFilename, Filename, 5, no_previous_error);
		ErrorText ->
		    ok = jukebox:log_error("urlcache",
					   [{"curl_command", list_to_binary(CommandString)},
					    {"curl_error", list_to_binary(ErrorText)}])
	    end,
	    CommandString2 = jukebox:priv_dir() ++ "/metadata/get_metadata.py " ++ 
                         filename:extension(Url) ++ " " ++ Filename ++ " " ++ 
                         local_name_prefix(Url),
	    case os:cmd(CommandString2) of
		"" -> ok;
		MetadataOutput ->
		    jukebox:log_error("urlcache",
				      [{"metadata_command", list_to_binary(CommandString2)},
				       {"metadata_error", list_to_binary(MetadataOutput)}])
	    end
    end,
    gen_server:cast(CachePid, {download_done, Url}),
    ok.

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

wait_for_completion(LocalFileName, MetadataFilename, Pid, Ref) ->
    spawn(fun () ->
		  link(Pid),
		  wait_for_completion1(LocalFileName, MetadataFilename, Pid, Ref)
	  end),
    ok.

wait_for_completion1(LocalFileName, MetadataFilename, Pid, Ref) ->
    case filelib:is_file(MetadataFilename) of
	true ->
	    Pid ! {urlcache, ok, Ref, LocalFileName};
	false ->
	    timer:sleep(200),
	    wait_for_completion1(LocalFileName, MetadataFilename, Pid, Ref)
    end.

collect_current_downloads([], Urls) ->
    Urls;
collect_current_downloads([{{downloader, Url}, _Pid} | Rest], Urls) ->
    collect_current_downloads(Rest, [Url | Urls]);
collect_current_downloads([_Other | Rest], Urls) ->
    collect_current_downloads(Rest, Urls).

%%---------------------------------------------------------------------------

init(_Args) ->
    {ok, none}.

handle_call(current_downloads, _From, State) ->
    {reply, collect_current_downloads(get(), []), State}.

handle_cast({cache, Url}, State) ->
    start_caching(Url),
    {noreply, State};
handle_cast({cache, Url, Pid, Ref}, State) ->
    {LocalFileName, MetadataFilename} = start_caching(Url),
    wait_for_completion(LocalFileName, MetadataFilename, Pid, Ref),
    {noreply, State};
handle_cast({download_done, Url}, State) ->
    erase({downloader, Url}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
