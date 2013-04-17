-module(lastfm).
-export([start/0, artist/1, album/1, track/1, scrobble/3, get_field/2]).

-include("settings.hrl").

-define(CLIENT_ID, "tst").
-define(CLIENT_VERSION, "1.0").

-include_lib("xmerl/include/xmerl.hrl").

start() -> 
	io:format("~p~n", [scrobble(?LASTFM_USER, ?LASTFM_PASSWORD, [{artist,"pixies"},{track,"debaser"}])]).

get_field(_, [])  ->
	[];
get_field(Match, [{Token, Stuff}|_]) when Match == Token ->
	Stuff;
get_field(Match, [_|T]) ->
	get_field(Match,T).

getUnixTimestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}}).


postKeys({X,Y}) when is_atom(X) -> atom_to_list(X) ++ "=" ++ Y;
postKeys({X,Y}) -> X ++ "=" ++ Y.

buildRequest(Keys) ->
		lists:foldr(
			fun(E, Acc) -> E ++ "&" ++ Acc end,
			[],
			lists:map(fun({X,Y}) -> postKeys({X,Y}) end,Keys)		
		).

doRequest(Keys) ->
	doRequest(Keys,"http://ws.audioscrobbler.com/2.0/").

doRequest(Keys, URL) ->
	Request = buildRequest(Keys),
	io:format("~p~n", [Request]),
	httpc:request(post, {URL ++ "?",[], "application/x-www-form-urlencoded",Request}, [], []).

scrobble(Username, Password, Args) ->
	case Username of
		"" ->
			{error, "Need to set username in settings.hrl"};
		_ ->
			Auth = md5:md5_hex(Username ++ md5:md5_hex(Password)),
			case lastfm_call(auth.getMobileSession, [{username, Username}, {authToken, Auth}], [], ['session'], ['subscriber','key'], true) of
				{error, Error} ->
					{error, Error};
				[{session, Keys}] ->
					Key = get_field(key, Keys),
					Timestamp = erlang:integer_to_list(getUnixTimestamp(erlang:now())),
					Token = md5:md5_hex(?LASTFM_SHARED_SECRET ++ Timestamp),
					case doRequest([{hs,"true"},{c, ?CLIENT_ID},{p,"1.2.1"},{v,?CLIENT_VERSION},{u, Username},{t,Timestamp},{a,Token},{api_key,?LASTFM_API_KEY},{sk,Key}],"http://post.audioscrobbler.com/") of
						{ok, {{_, 200, _}, _, Body}} ->
							io:format("~p~n", [Body]),
							["OK", SessionID, NowPlaying, Submission] = string:tokens(Body,"\n"),
							Length = 100, % FIXME: right number
							case doRequest([{s,SessionID},{"a[0]",edoc_lib:escape_uri(get_field(artist,Args))},{"i[0]",Timestamp},{"t[0]",edoc_lib:escape_uri(get_field(track,Args))},{"o[0]","P"},{"l[0]",integer_to_list(Length)},{"r[0]",""},{"b[0]",""},{"n[0]",""},{"m[0]",""}],Submission) of
								{ok, {{_, 200, _}, _, "OK\n"}} ->
									{ok, "Submitted fine"};
								{error, Error} ->
									{error, Error}
							end;
						{error, Error} ->
							{error, Error}
						end
					end
			end.

api_sig(Args) ->
	SortedArgs = lists:keysort(1, Args),
	%io:format("~p~n", [SortedArgs]),
	ToHash = 
		lists:foldr(
			fun(E, Acc) -> E ++ Acc end,
			[],
			lists:map(fun({X,Y}) -> atom_to_list(X)++Y end,SortedArgs)		
		) ++ ?LASTFM_SHARED_SECRET,
	md5:md5_hex(ToHash).

artist(Name) ->
	lastfm_call(artist.search, [{artist,Name}], ['artistmatches'], ['artist'], []).

album(Name) ->
	lastfm_call(album.search, [{album,Name}], ['albummatches'], ['album'], ['artist']).

track(Name) ->
	lastfm_call(track.search, [{track,Name}], ['trackmatches'], ['track'], ['artist','listeners']).

% Jump are ones to jump inside and only store the internal structure of
% Structure are ones to store and jump inside
% Text are ones that will contain a single text element
% FIXME: better bloody names
lastfm_call(Func, Args, Jump, Structure, Text) ->
	lastfm_call(Func, Args, Jump, Structure, Text, false).

lastfm_call(Func, Args, Jump, Structure, Text, Sign) ->
	FullArgs = [{api_key, ?LASTFM_API_KEY},{method,atom_to_list(Func)}] ++ Args,
	case Sign of
		false ->
			SignedArgs = FullArgs;
		true ->
			%SignedArgs = [{api_sig, api_sig(++FullArgs)}] ++ FullArgs
			SignedArgs = [{api_sig, api_sig(FullArgs)}] ++ FullArgs
	end,
	inets:start(),
	case doRequest(SignedArgs) of
		{ok, {{_, 200, _}, _, Body}} ->
			%io:format("~p~n", [Body]),
			case xmerl_scan:string(Body) of
				{E, _} when is_record(E, xmlElement) ->
					case catch decode_element(E, Jump, Structure, Text) of
					{'EXIT', Reason} -> exit(Reason);
					{error, Reason} -> {error, Reason};
					Result -> lists:reverse(collapse(Result))
					end;
				{error, Reason} -> {error, Reason}
			end;
		{ok, StatusLine} ->
			{error, StatusLine};
		{error, Reason} ->
			{error, Reason}
		end.

collapse([[T]]) -> collapse(T);
collapse([H|T]) -> collapse([H|T],[]);
collapse(E) -> E.

collapse([H|T], Rest) -> 
	CH = collapse(H),
	case CH of
		[] -> collapse(T, Rest);
		_  -> collapse(T, [CH|Rest])
	end;
collapse([], Rest) -> Rest.

get_attribute([H|T], K) -> 
	case (H#xmlAttribute.name) of
		K -> H#xmlAttribute.value;
		_ -> get_attribute(T,K)
	end;
get_attribute([], K) -> "No such attribute "++K.

get_text([H|T]) when is_record(H, xmlText) -> string:join([get_text(H),get_text(T)],"");
get_text([]) -> "";
get_text(E) when is_record(E, xmlText) -> E#xmlText.value;
get_text(E) -> {error,{bad_text,E}}.

decode_element(E, Jump, Structure, Text) when is_record(E, xmlElement) -> 
	case (E#xmlElement.name) of
		'lfm' ->
			decode_element(E#xmlElement.content, Jump, Structure, Text);
		'results' ->
			decode_element(E#xmlElement.content, Jump, Structure, Text);
		'id' ->
			{id, get_text(E#xmlElement.content)};
		'name' ->
			{name, get_text(E#xmlElement.content)};
		'mbid' ->
			{mbid, get_text(E#xmlElement.content)};
		'url' ->
			{url, get_text(E#xmlElement.content)};
		'image' ->
			{image, get_attribute(E#xmlElement.attributes, size), get_text(E#xmlElement.content)};
		'streamable' ->
			[];
		'opensearch:Query' ->
			[];
		'opensearch:totalResults' ->
			[];
		'opensearch:startIndex' ->
			[];
		'opensearch:itemsPerPage' ->
			[];
		 _ ->
			IsJump = lists:member(E#xmlElement.name, Jump),
			IsStructure = lists:member(E#xmlElement.name, Structure),
			IsText = lists:member(E#xmlElement.name, Text),
			if 
				IsStructure ->
					{E#xmlElement.name, decode_element(E#xmlElement.content, Jump, Structure, Text)};
				IsJump ->
					decode_element(E#xmlElement.content, Jump, Structure, Text);
				IsText ->
					{E#xmlElement.name, get_text(E#xmlElement.content)};
				true ->
					{error, {decode_type, E#xmlElement.name}}
			end
	end;
decode_element([T|Rest], Jump, Structure, Text) when is_record(T, xmlText) ->
    case only_whitespace(T#xmlText.value) of
	yes -> decode_element(Rest, Jump, Structure, Text);
	no ->
		{error, {unexpected_text, T#xmlText.value}}
    end;
decode_element([T|Rest], Jump, Structure, Text) when is_record(T, xmlElement) ->
	DT = decode_element(T, Jump, Structure, Text),
	case DT of
		[] -> decode_element(Rest, Jump, Structure, Text);
		_  -> [DT|decode_element(Rest, Jump, Structure, Text)]
	end;
decode_element([], _, _, _) -> [];
decode_element(E, _, _, _) -> {error, {bad_element, E}}.

only_whitespace([]) -> yes;
only_whitespace([$ |Rest]) -> only_whitespace(Rest);
only_whitespace([$\n|Rest]) -> only_whitespace(Rest);
only_whitespace([$\t|Rest]) -> only_whitespace(Rest);
only_whitespace(_) -> no.

