-module(jukebox_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10},
	  [{mod_jsonrpc, {mod_jsonrpc, start_link, []},
	    transient, 5, worker, [mod_jsonrpc]},
	   {httpd, {httpd, start_link, ["priv/server_root/conf/httpd.conf"]},
	    transient, infinity, supervisor, [httpd_instance_sup]},

	   {spider, {spider, start_link, []}, permanent, 5, worker, [ibrowse]},
	   {trackdb, {trackdb, start_link, []}, transient, 5, worker, [trackdb]},
	   {player, {player, start_link, []}, transient, 5, worker, [player]},
	   {volume, {volume, start_link, []}, transient, 5, worker, [volume]},

	   {configsession, {configsession, start_link, []}, transient, 5, worker, [configsession]},

	   {history, {history, start_link, [history, 100]},
	    transient, 5, worker, [history]}]}}.
