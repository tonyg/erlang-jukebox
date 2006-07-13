-module(spider).

-export([start/1]).
-export([spider/1]).

start(Sconf) ->
    io:format("Starting spider.~n"),
    ibrowse:start().

spider(Url) ->
    spider([Url], []).

spider([], Results) ->
    Results;
spider([Url | Work], Results) ->
    {ok, RE} = regexp:parse("[hH][rR][eE][fF]=\"([^\"]*)\""),
    case ibrowse:send_req(Url, [], get) of
	{ok, "3" ++ _CodeRest, Headers, _Body} ->
	    case lists:keysearch("Location", 1, Headers) of
		{value, {_, Replacement}} ->
		    spider([Replacement | Work], Results);
		_ -> spider(Work, Results)
	    end;
	{ok, "2" ++ _CodeRest, _Headers, Body} ->
	    {match, Matches} = regexp:matches(Body, RE),
	    {NewWork, NewResults} =
		lists:foldl(fun ({Start, Length}, State) ->
				    process_result_url(Url,
						       string:substr(Body, Start + 6, Length - 7),
						       State)
			    end, {Work, Results}, Matches),
	    spider(NewWork, NewResults);
	_ -> spider(Work, Results)
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
    Url = resolve_relative(BaseUrl, html_decode(HtmlEncodedUrl)),
    Extension = filename:extension(Url),
    case avoid_extension(Extension) of
	true -> {Work, Results};
	false ->
	    case player:supports_extension(Extension) of
		true -> {Work, [Url | Results]};
		false ->
		    BasePath = url_path(BaseUrl),
		    UrlPath = url_path(Url),
		    BaseIsPrefix = lists:prefix(BasePath, UrlPath),
		    if
			BasePath /= UrlPath, BaseIsPrefix -> {[Url | Work], Results};
			true -> {Work, Results}
		    end
	    end
    end.
