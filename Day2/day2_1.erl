-module(day2_1).

-export([eval_all/1, main/0, main_with_file/0, test/0,
	 to_map/1]).

to_code(1) -> add;
to_code(2) -> mul;
to_code(99) -> halt;
to_code(_) -> error.

get_indirect(Idx, Map) ->
    case maps:find(Idx, Map) of
      {ok, SndIdx} -> maps:find(SndIdx, Map);
      error -> error
    end.

eval(InstP, Program) ->
    {ok, CurrInst} = maps:find(InstP, Program),
    case to_code(CurrInst) of
      add ->
	  {ok, L} = get_indirect(InstP + 1, Program),
	  {ok, R} = get_indirect(InstP + 2, Program),
	  {ok, TargetIndex} = maps:find(InstP + 3, Program),
	  {run, Program#{TargetIndex => L + R}};
      mul ->
	  {ok, L} = get_indirect(InstP + 1, Program),
	  {ok, R} = get_indirect(InstP + 2, Program),
	  {ok, TargetIndex} = maps:find(InstP + 3, Program),
	  {run, Program#{TargetIndex => L * R}};
      halt -> {halt, Program};
      error -> error
    end.

eval_all(Program) -> eval_all(run, 0, Program).

eval_all(run, InstP, Program) ->
    {State, NewProg} = eval(InstP, Program),
    eval_all(State, InstP + 4, NewProg);
eval_all(halt, _, Program) -> {ok, Program};
eval_all(error, _, _) -> error.

% Convert list to map
to_map(List) ->
    lists:foldl(fun (Elem, {Idx, Map}) ->
			{Idx + 1, Map#{Idx => Elem}}
		end,
		{0, #{}}, List).

% simple test case
test() ->
    {_, TestIn1} = to_map([1, 9, 10, 3, 2, 3, 11, 0, 99, 30,
			   40, 50]),
    {_, TestOut1} = to_map([1, 9, 10, 70, 2, 3, 11, 0, 99,
			    30, 40, 50]),
    {_, Out1} = eval(0, TestIn1),
    if Out1 /= TestOut1 ->
	   io:write(Out1),
	   io:nl(),
	   io:write(TestOut1),
	   io:nl(),
	   error;
       true -> ok
    end.

main() ->
    ok = test(),
    Raw = [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3,
	   2, 10, 1, 19, 1, 19, 5, 23, 1, 23, 9, 27, 2, 27, 6, 31,
	   1, 31, 6, 35, 2, 35, 9, 39, 1, 6, 39, 43, 2, 10, 43, 47,
	   1, 47, 9, 51, 1, 51, 6, 55, 1, 55, 6, 59, 2, 59, 10, 63,
	   1, 6, 63, 67, 2, 6, 67, 71, 1, 71, 5, 75, 2, 13, 75, 79,
	   1, 10, 79, 83, 1, 5, 83, 87, 2, 87, 10, 91, 1, 5, 91,
	   95, 2, 95, 6, 99, 1, 99, 6, 103, 2, 103, 6, 107, 2, 107,
	   9, 111, 1, 111, 5, 115, 1, 115, 6, 119, 2, 6, 119, 123,
	   1, 5, 123, 127, 1, 127, 13, 131, 1, 2, 131, 135, 1, 135,
	   10, 0, 99, 2, 14, 0, 0],
    {_, Program} = to_map(Raw),
    AlarmStateProgram = maps:merge(Program,
				   #{1 => 12, 2 => 2}),
    {ok, Result} = eval_all(AlarmStateProgram),
    maps:find(0, Result).

% same as main/0 but reading the data from a file
main_with_file() ->
    ok = test(),
    {ok, FileData} = file:read_file("input2_1.txt"),
    Raw = [binary_to_integer(X)
	   || X
		  <- string:split(binary:replace(FileData, <<"\n">>,
						 <<"">>),
				  <<",">>, all)],
    {_, Program} = to_map(Raw),
    AlarmStateProgram = maps:merge(Program,
				   #{1 => 12, 2 => 2}),
    {ok, Result} = eval_all(AlarmStateProgram),
    maps:find(0, Result).
