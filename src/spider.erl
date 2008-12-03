-module(spider).

-export([start_link/0]).
-export([spider/1, spider/2]).
-export([url_encode_path/1, url_encode/1]).
-export([resolve_relative/2, retrieve/1, retrieve/2]).

start_link() ->
    ibrowse:start_link().

spider(Url) -> spider(Url, false).

spider(Url, WantDebug) ->
    spider([Url], [], WantDebug).

debug_out(true, Fmt, Args) ->
    io:format(Fmt, Args);
debug_out(false, _, _) ->
    ok.

retrieve(Url) ->
    retrieve(Url, 30000).

retrieve(Url, Timeout) ->
    case ibrowse:send_req(Url, [], get, [], [{http_vsn, {1, 0}}], Timeout) of
	Result when element(1, Result) == ok ->
	    Result;
	_ -> ibrowse:send_req(Url, [], get, [], [{http_vsn, {1, 1}}], Timeout)
    end.

spider([], Results, _WantDebug) ->
    Results;
spider([Url | Work], Results, WantDebug) ->
    {ok, RE} = regexp:parse("[hH][rR][eE][fF]=\"([^\"]*)\""),
    debug_out(WantDebug, "Spidering ~p~n", [Url]),
    case retrieve(Url) of
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

%% Like url_encode, but leaves any "reserved characters" alone in
%% the string as well.
url_encode_path(Str) when list(Str) ->
    url_encode_path_char(lists:reverse(Str), []).

url_encode_path_char([X | T], Acc) when X >= $a, X =< $z ->
    url_encode_path_char(T, [X | Acc]);
url_encode_path_char([X | T], Acc) when X >= $A, X =< $Z ->
    url_encode_path_char(T, [X | Acc]);
url_encode_path_char([X | T], Acc) when X >= $0, X =< $9 ->
    url_encode_path_char(T, [X | Acc]);
url_encode_path_char([X | T], Acc)
  when X == $-; X == $_; X == $.; X == $~;
       X == $!; X == $*; X == $'; X == $(;
       X == $); X == $;; X == $:; X == $@;
       X == $&; X == $=; X == $+; X == $$;
       X == $,; X == $/; X == $?; X == $%;
       X == $#; X == $[; X == $] ->
    url_encode_path_char(T, [X | Acc]);
url_encode_path_char([X | T], Acc) ->
    url_encode_path_char(T, [$%, d2h(X bsr 4), d2h(X band 16#0f) | Acc]);
url_encode_path_char([], Acc) ->
    Acc.

%% url_encode is lifted from ibrowse, and modified to include digits
%% and tilde.
url_encode(Str) when list(Str) ->
    url_encode_char(lists:reverse(Str), []).

url_encode_char([X | T], Acc) when X >= $a, X =< $z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $A, X =< $Z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $0, X =< $9 ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X == $-; X == $_; X == $.; X == $~ ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) ->
    url_encode_char(T, [$%, d2h(X bsr 4), d2h(X band 16#0f) | Acc]);
url_encode_char([], Acc) ->
    Acc.

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.

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

avoid_extension1(".rm") -> true;
avoid_extension1(".ram") -> true;
avoid_extension1(".wmv") -> true;
avoid_extension1(".wma") -> true;
avoid_extension1(".mov") -> true;
avoid_extension1(".jpg") -> true;
avoid_extension1(".m4p") -> true;
avoid_extension1(_) -> false.

url_path(Url) ->
    case string:rchr(Url, $?) of
	0 -> Url;
	N -> string:substr(Url, 1, N - 1)
    end.

is_current_or_parent_link("." ++ _) -> true;
is_current_or_parent_link(RelUrl) ->
    string:str(RelUrl, "/./") /= 0 orelse
	string:str(RelUrl, "/../") /= 0.

process_result_url(BaseUrl, HtmlEncodedUrl, {Work, Results}) ->
    RelUrl = html_decode(HtmlEncodedUrl),
    case is_current_or_parent_link(RelUrl) of
	true ->
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
