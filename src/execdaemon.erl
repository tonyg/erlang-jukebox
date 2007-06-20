-module(execdaemon).

-export([open/0, terminate/1, command/3, read_from/1, wait_for_event/1, run/2]).

open() ->
    Pid = spawn(fun startup/0),
    Pid ! {subscribe, self()},
    receive
	{subscribed, Pid2} when Pid == Pid2 -> Pid
    end.

priv_dir() ->
    case code:priv_dir(jukebox) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

startup() ->
    Port = open_port({spawn, priv_dir() ++ "/execdaemon/execdaemon"}, [stream, use_stdio, eof]),
    mainloop(Port, 1, [], [], "").

mainloop(Port, ReqNum, Requests, Subscribers, Acc) ->
    receive
	{subscribe, Pid} ->
	    Pid ! {subscribed, self()},
	    mainloop(Port, ReqNum, Requests, [Pid | Subscribers], Acc);
	execdaemon_terminate ->
	    port_command(Port, [0]),
	    mainloop(Port, ReqNum, Requests, Subscribers, Acc);
	{execdaemon_command, Pid, Command, Arg} ->
	    port_command(Port, lists:flatten([integer_to_list(ReqNum), ":",
					      atom_to_list(Command), ",", Arg, [0]])),
	    mainloop(Port, ReqNum + 1, [{ReqNum, Pid} | Requests], Subscribers, Acc);
	{_Port1, eof} ->
	    {_, Pids} = lists:unzip(Requests),
	    send_to_subs(Pids ++ Subscribers, {execdaemon_eof, self()}),
	    port_close(Port);
	{_Port1, {data, Data}} ->
	    {NewRequests, NewAcc} = process_received(Subscribers, Requests, Acc, Data),
	    mainloop(Port, ReqNum, NewRequests, Subscribers, NewAcc)
    end.

send_to_subs(Subscribers, Msg) ->
    lists:foreach(fun (Subscriber) -> Subscriber ! Msg end, Subscribers).

process_received(_Subscribers, Requests, Acc, []) ->
    {Requests, Acc};
process_received(Subscribers, Requests, Acc, [0 | Rest]) ->
    {ReqNum, Code, Aux} = split_response(lists:reverse(Acc)),
    if
	ReqNum == 0 ->
	    send_to_subs(Subscribers, {execdaemon_event, self(), Code, Aux}),
	    process_received(Subscribers, Requests, [], Rest);
	true ->
	    case proplists:get_value(ReqNum, Requests, none) of
		none -> process_received(Subscribers, Requests, [], Rest);
		Pid ->
		    Pid ! {execdaemon_response, self(), Code, Aux},
		    process_received(Subscribers, proplists:delete(ReqNum, Requests), [], Rest)
	    end
    end;
process_received(Subscribers, Requests, Acc, [Ch | Rest]) ->
    process_received(Subscribers, Requests, [Ch | Acc], Rest).

split_response(Str) ->
    {ReqNum, ":" ++ CmdArg} = lists:split(string:chr(Str, $:) - 1, Str),
    {Cmd, "," ++ Arg} = lists:split(string:chr(CmdArg, $,) - 1, CmdArg),
    {list_to_integer(ReqNum), list_to_atom(Cmd), Arg}.

terminate(Pid) ->
    Pid ! execdaemon_terminate.

command(Pid, Command, Arg) ->
    Pid ! {execdaemon_command, self(), Command, Arg},
    read_from(Pid).

read_from(Pid) ->
    receive
	{execdaemon_response, Pid1, Code, Aux} when Pid == Pid1 ->
	    {Code, Aux};
	{execdaemon_eof, Pid1} when Pid == Pid1 ->
	    eof
    end.

wait_for_event(Pid) ->
    receive
	{execdaemon_event, Pid1, Code, Aux} when Pid == Pid1 ->
	    {Code, Aux};
	{execdaemon_eof, Pid1} when Pid == Pid1 ->
	    eof
    end.

run(Program, Argvec) ->
    Pid = open(),
    command(Pid, program, Program),
    command(Pid, argc, integer_to_list(length(Argvec))),
    lists:foldl(fun (Arg, Count) ->
			command(Pid, arg, [integer_to_list(Count), ",", Arg]),
			Count + 1
		end, 0, Argvec),
    command(Pid, execv, ""),
    Pid.
