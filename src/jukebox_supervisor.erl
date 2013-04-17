-module(jukebox_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10},
	  [{rfc4627_jsonrpc, {rfc4627_jsonrpc, start_link, []},
	    transient, 5000, worker, [rfc4627_jsonrpc]},
	   {httpd, {inets, start, [httpd,
                                   [{file, "priv/server_root/conf/httpd.conf"}],
                                   stand_alone]},
	    transient, infinity, supervisor, [httpd_instance_sup]},

	   {urlcache, {urlcache, start_link, []}, permanent, 5000, worker, [urlcache]},
	   {spider, {spider, start_link, []}, permanent, 5000, worker, [ibrowse]},
	   {trackdb, {trackdb, start_link, []}, transient, 5000, worker, [trackdb]},
	   {player, {player, start_link, []}, transient, 5000, worker, [player]},
	   {volume, {volume, start_link, []}, transient, 5000, worker, [volume]},

	   {configsession, {configsession, start_link, []},
	    transient, 5000, worker, [configsession]},
	   {jukeboxsession, {jukeboxsession, start_link, []},
	    transient, 5000, worker, [jukeboxsession]},

	   {history, {history, start_link, [history, 100]},
	    transient, 5000, worker, [history]}]}}.
