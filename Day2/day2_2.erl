-module(day2_2).

-import(day2_1, [eval_all/1, to_map/1]).

-export([main/0]).

find_in_for_out(BaseProg, ExpectedOut,
		[{Noun, Verb} | Rest]) ->
    Prog = maps:merge(BaseProg, #{1 => Noun, 2 => Verb}),
    {ok, #{0 := Result}} = eval_all(Prog),
    if ExpectedOut == Result -> {Noun, Verb};
       true -> find_in_for_out(BaseProg, ExpectedOut, Rest)
    end;
find_in_for_out(_, _, [H | _]) ->
    {howdidthishappenweresmarterthanthis, H};
find_in_for_out(_, _, []) -> error.

main() ->
    {ok, FileData} = file:read_file("input2_1.txt"),
    Raw = [binary_to_integer(X)
	   || X
		  <- string:split(binary:replace(FileData, <<"\n">>,
						 <<"">>),
				  <<",">>, all)],
    {_, Program} = to_map(Raw),
    {Noun, Verb} = find_in_for_out(Program, 19690720,
				   [{N, V}
				    || N <- lists:seq(0, 99),
				       V <- lists:seq(0, 99)]),
    100 * Noun + Verb.
