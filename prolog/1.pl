go_inner(0, [], _).
go_inner(First, [First], First).
go_inner(0, [_], _).
go_inner(Result, [Next, Next | T], First) :-
    go_inner(Acc, [Next | T], First),
    Result is (Next + Acc).
go_inner(Result, [_ | T], First) :-
    go_inner(Result, T, First).

%% go splits off the first element and calls go_inner.
go(0, []).
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

input(Numbers) :-
    read_file_to_codes("../resources/day_1.txt", Terms, []),
    codes_to_numbers(Numbers, Terms).

solution(Result) :-
    input(Numbers),
    go(Result, Numbers).

?- solution(1341).

%% Part 2

wrap_index(I,Length,I) :-
    Max is Length - 1,
    between(0,Max,I).
wrap_index(I,Length,Wrapped) :-
    Wrapped is I + Length,
    I < 0.
wrap_index(I,Length,Wrapped) :-
    Wrapped is I - Length,
    I >= Length.

checksum_n([], _, _, _, _, 0).

checksum_n([H|T], Length, N, I, Orig, Result) :-
    Unwrapped is N + I,
    wrap_index(Unwrapped,Length,Wrapped),
    nth0(Wrapped,Orig,H),
    J is I + 1,
    checksum_n(T, Length, N, J, Orig, Acc),
    Result is H + Acc.

checksum_n([_|T], Length, N, I, Orig, Result) :-
    J is I + 1,
    checksum_n(T, Length, N, J, Orig, Result).


part_2(List, Result) :-
    length(List,Length),
    Halfway is div(Length,2),
    checksum_n(List,Length,Halfway,0,List,Result).

?- part_2([1,2,1,2], 6).
?- part_2([1,2,2,1], 0).
?- part_2([1,2,3,1,2,3], 12).
?- part_2([1,2,1,3,1,4,1,5], 4).

?- input(Numbers), part_2(Numbers, 1348).
