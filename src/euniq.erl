%%#!/usr/bin/env escript
-module(euniq).
-export([main/1, uniq/2]).

-define(BUFSIZ, 1024).

usage() ->
	io:format("usage: euniq [-c|-d|-u] [file1]~n"),
	io:format("-c\t\tcount occurences of each line~n"),
	io:format("-d\t\twrite only duplicated lines~n"),
	io:format("-u\t\twrite only unique lines~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $c, flag, count_repeats },
		{ $d, flag, only_dups },
		{ $u, flag, only_uniq }
	]) of
	{ok, Options, ArgsN} ->
		process(Options, ArgsN);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.

process(Opts, []) ->
	io:setopts(standard_io, [binary]),
	uniq(standard_io, Opts);
process(Opts, ["-"]) ->
	io:setopts(standard_io, [binary]),
	uniq(standard_io, Opts);
process(Opts, [File | _]) ->
	try
		process_file(Opts, File)
	catch
		throw:{error, Reason} ->
			io:format(standard_error, "euniq: ~s: ~s~n", [File, str:error(Reason)]),
			halt(1)
	end.

process_file(Opts, Filename) ->
	case file:open(Filename, [read, binary, {read_ahead, ?BUFSIZ}]) of
	{ok, Fp} ->
		uniq(Fp, Opts),
		file:close(Fp);
	Error ->
		throw(Error)
	end.

uniq(Fp, Opts) ->
	case file:read_line(Fp) of
	{error, Reason} ->
		throw({error, Reason});
	{ok, Line} ->
		uniq(Fp, Line, which_filter(Opts), 1);
	eof ->
		ok
	end.

uniq(Fp, PrevLine, Filter, Count) ->
	case file:read_line(Fp) of
	{error, Reason} ->
		throw({error, Reason});
	{ok, Line} ->
		Tally = case compare_lines(PrevLine, Line) of
		false ->
			Filter(PrevLine, Count), 1;
		true ->
			Count + 1
		end,
		uniq(Fp, Line, Filter, Tally);
	eof ->
		Filter(PrevLine, Count)
	end.

compare_lines(LineA, LineB) ->
	str:rchop(LineA) == str:rchop(LineB).

filter(PrevLine, _Count) ->
	file:write(standard_io, PrevLine).

filter_count(PrevLine, Count) ->
	io:format("~B ~s", [Count, PrevLine]).

filter_dups(PrevLine, Count) ->
	if
	Count > 1 ->
		file:write(standard_io, PrevLine);
	Count == 1 ->
		ok
	end.

filter_uniq(PrevLine, Count) ->
	if
	Count > 1 ->
		ok;
	Count == 1 ->
		file:write(standard_io, PrevLine)
	end.

which_filter(Options) ->
	Count_repeats = proplists:get_value(count_repeats, Options, false),
	Only_dups = proplists:get_value(only_dups, Options, false),
	Only_uniq = proplists:get_value(only_uniq, Options, false),
	case {Count_repeats, Only_dups, Only_uniq} of
	{false, false, false} ->
		fun filter/2;
	{true, false, false} ->
		fun filter_count/2;
	{false, true, false} ->
		fun filter_dups/2;
	{false, false, true} ->
		fun filter_uniq/2;
	_ ->
		usage()
	end.

