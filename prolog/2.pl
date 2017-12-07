checksum_row(Row, Checksum) :-
    min_list(Row, Min),
    max_list(Row, Max),
    Checksum is Max - Min.

checksum([], 0).
checksum([Row|Rows], Result) :-
    checksum_row(Row, RowChecksum),
    checksum(Rows, Acc),
    Result is RowChecksum + Acc.

example_spreadsheet([[5,1,9,5], [7,5,3], [2,4,6,8]]).

?- example_spreadsheet(Spreadsheet), checksum(Spreadsheet, 18).

partition_on(List, C, Result) :-
    partition_on_inner(List, C, [], Result).

partition_on_inner([], _, [], []).
partition_on_inner([], _, Group, [Group]).
partition_on_inner([H|T], H, Group, [Group|Acc]) :-
    partition_on_inner(T, H, [], Acc).
partition_on_inner([H|T], C, Group, Result) :-
    append(Group,[H],NewGroup),
    partition_on_inner(T, C, NewGroup, Result).

?- partition_on([1,2,0,3,4], 0, [[1,2],[3,4]]).

row_codes([], []).
row_codes([Codes|T], [Number|Acc]) :-
    number_codes(Number, Codes),
    row_codes(T, Acc).

rows_codes([], []).
rows_codes([RowCodes|T], [Row|Acc]) :-
    char_code('\t', Tab),
    partition_on(RowCodes, Tab, Grouped),
    row_codes(Grouped, Row),
    rows_codes(T, Acc).

read_spreadsheet(Rows) :-
    read_file_to_codes("../resources/day_2.txt", Codes, []),
    char_code('\n', Newline),
    partition_on(Codes, Newline, RowCodes),
    rows_codes(RowCodes, Rows).

?- read_spreadsheet(Rows), checksum(Rows, 45972).

% Part 2

dividend(_,[],0).
dividend(Dividend, [Divisor|_], Quotient) :-
    Dividend =\= Divisor,
    divmod(Dividend, Divisor, Quotient, 0).
dividend(Dividend, [_|T], Quotient) :-
        dividend(Dividend, T, Quotient).

checksum_2_row_inner(Row, [Dividend|_], Quotient) :-
    dividend(Dividend, Row, Quotient),
    0 =\= Quotient.
checksum_2_row_inner(Row, [_|Rest], Quotient) :-
    checksum_2_row_inner(Row, Rest, Quotient).

checksum_2_row(Row, Checksum) :-
    checksum_2_row_inner(Row, Row, Checksum).

?- checksum_2_row([5,9,2,8], 4).
?- checksum_2_row([9,4,7,3], 3).
?- checksum_2_row([3,8,6,5], 2).

checksum_2([], 0).
checksum_2([Row|Rows], Result) :-
    checksum_2_row(Row, Acc1),
    checksum_2(Rows, Acc2),
    Result is Acc1 + Acc2.

example_spreadsheet_2([[5,9,2,8], [9,4,7,3], [3,8,6,5]]).

?- example_spreadsheet_2(Rows), checksum_2(Rows, 9).
?- read_spreadsheet(Rows), checksum_2(Rows, 326).
