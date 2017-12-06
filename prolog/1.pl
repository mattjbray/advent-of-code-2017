go_inner(Result, [], _) :- Result is 0.
go_inner(Result, [First], First) :- Result is First.
go_inner(Result, [_], _) :- Result is 0.
go_inner(Result, [Next, Next | T], First) :-
    go_inner(Acc, [Next | T], First),
    Result is (Next + Acc).
go_inner(Result, [_ | T], First) :-
    go_inner(Result, T, First).

%% go splits off the first element and calls go_inner.
go(Result, []) :- Result is 0.
go(Result, Input) :-
    Input = [First|_],
    go_inner(Result, Input, First).

codes_to_numbers(Result, []) :- Result = [].
%% Ignore the newline (code 10).
codes_to_numbers(Result, [10]) :- Result = [].
codes_to_numbers(Result, [Code|Codes]) :-
    number_codes(Num, [Code]),
    codes_to_numbers(Acc, Codes),
    Result = [Num|Acc].

?- go(3, [1,1,2,2]).
?- go(0, [1,2,1,2]).
?- go(4, [1,1,1,1]).
?- go(0, [1,2,3,4]).
?- go(9, [9,1,2,1,2,1,2,9]).

solution(Result) :-
    read_file_to_codes("../resources/day_1.txt", Terms, []),
    codes_to_numbers(Numbers, Terms),
    go(Result, Numbers).

?- solution(1341).
