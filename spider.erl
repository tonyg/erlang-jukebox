-module(spider).

-export([start/1]).
-export([spider/1, spider/2]).

start(Sconf) ->
    io:format("Starting spider.~n"),
    ibrowse:start().

spider(Url) -> spider(Url, false).

spider(Url, WantDebug) ->
    spider([Url], [], WantDebug).

debug_out(true, Fmt, Args) ->
    io:format(Fmt, Args);
debug_out(false, _, _) ->
    ok.

spider([], Results, _WantDebug) ->
    Results;
spider([Url | Work], Results, WantDebug) ->
    {ok, RE} = regexp:parse("[hH][rR][eE][fF]=\"([^\"]*)\""),
    debug_out(WantDebug, "Spidering ~p~n", [Url]),
    case ibrowse:send_req(Url, [], get, [], [{http_vsn, {1, 0}}]) of
	{ok, "3" ++ _CodeRest, Headers, _Body} ->
	    case lists:keysearch("Location", 1, Headers) of
		{value, {_, Replacement}} ->
		    debug_out(WantDebug, "Redirecting to ~p~n", [Replacement]),
		    spider([Replacement | Work], Results, WantDebug);
		_ ->
		    debug_out(WantDebug, "Redirection without replacement! Ignoring~n", []),
		    spider(Work, Results, WantDebug)
	    end;
	{ok, "2" ++ _CodeRest, _Headers, Body} ->
	    %%debug_out(WantDebug, "Body length ~p~n", [length(Body)]),
	    {match, Matches} = regexp:matches(Body, RE),
	    {NewWork, NewResults} =
		lists:foldl(fun ({Start, Length}, State) ->
				    process_result_url(Url,
						       string:substr(Body, Start + 6, Length - 7),
						       State)
			    end, {Work, Results}, Matches),
	    spider(NewWork, NewResults, WantDebug);
	Response ->
	    debug_out(WantDebug, "Odd response ~p~n", [Response]),
	    spider(Work, Results, WantDebug)
    end.

html_decode(S0) ->
    {ok, S1, _} = regexp:gsub(S0, "&lt;", "<"),
    {ok, S2, _} = regexp:gsub(S1, "&gt;", ">"),
    {ok, S3, _} = regexp:gsub(S2, "&quot;", "\""),
    {ok, S4, _} = regexp:gsub(S3, "&amp;", "\\&"),
    S4.

resolve_relative(Base, Url) ->
    case Url of
	"http://" ++ _Rest ->
	    Url;
	"//" ++ Rest ->
	    "http://" ++ Rest;
	"/" ++ AbsPath ->
	    {match, Start, Length} = regexp:match(Base, "http://[^/]+/"),
	    Prefix = string:substr(Base, Start, Length),
	    Prefix ++ AbsPath;
	RelPath ->
	    string:strip(Base, right, $/) ++ "/" ++ RelPath
    end.

avoid_extension(E) -> avoid_extension1(http_util:to_lower(E)).

avoid_extension1(".flac") -> true;
avoid_extension1(".wav") -> true;
avoid_extension1(".m3u") -> true;
avoid_extension1(".rm") -> true;
avoid_extension1(".ram") -> true;
avoid_extension1(".wmv") -> true;
avoid_extension1(".wma") -> true;
avoid_extension1(".mov") -> true;
avoid_extension1(".jpg") -> true;
avoid_extension1(".m4a") -> true;
avoid_extension1(".m4p") -> true;
avoid_extension1(_) -> false.

url_path(Url) ->
    case string:rchr(Url, $?) of
	0 -> Url;
	N -> string:substr(Url, 1, N - 1)
    end.

process_result_url(BaseUrl, HtmlEncodedUrl, {Work, Results}) ->
    RelUrl = html_decode(HtmlEncodedUrl),
    case lists:nth(1, RelUrl) of
	$. ->
	    {Work, Results};
        _ ->
	    Url = resolve_relative(BaseUrl, RelUrl),
	    Extension = filename:extension(Url),
	    case avoid_extension(Extension) of
		true -> {Work, Results};
		false ->
		    case player:supports_extension(Extension) of
			true -> {Work, [Url | Results]};
			false ->
			    case lists:nth(length(Url), Url) of
				$/ ->
				    BasePath = url_path(BaseUrl),
				    UrlPath = url_path(Url),
				    BaseIsPrefix = lists:prefix(BasePath, UrlPath),
				    if
					BasePath /= UrlPath, BaseIsPrefix ->
					    {[Url | Work], Results};
					true -> {Work, Results}
				    end;
				_ ->
				    {Work, Results}
			    end
		    end
	    end
    end.
