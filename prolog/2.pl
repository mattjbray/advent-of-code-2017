checksum_row(Row, Checksum) :-
    min_list(Row, Min),
    max_list(Row, Max),
    Checksum is Max - Min.

checksum([], 0).
checksum([Row|Rows], Result) :-
    checksum_row(Row, RowChecksum),
    checksum(Rows, Acc),
    Result is RowChecksum + Acc.

?- checksum([[5,1,9,5], [7,5,3], [2,4,6,8]], 18).

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
