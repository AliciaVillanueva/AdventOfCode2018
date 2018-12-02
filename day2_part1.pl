day2_part1(File, CheckSum) :-
%% removes facts from previous runs
    retractall(twos(_)),
    retractall(threes(_)),
    retractall(row(_)),
%% read the file
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( assertz(row(Codes)), fail )
    ; close(Stream), !,
%% collects in LStrings all lists R stored in facts row(R) 
    findall(R,row(R),LStrings), 
%% initializes counters for twos and threes (add facts twos(0) and threes(0) to the DB)
    assertz(twos(0)), assertz(threes(0)),
    computeRep(LStrings), 
%% consults the final value of twos and thress and computes checsum
    twos(Twos), threes(Threes), CheckSum is Twos * Threes
    ).

%% for each string in the list calls hasTwosThrees
computeRep([]).
computeRep([String|Rest]) :-
   hasTwosThrees(String), computeRep(Rest).

%% updates the database when twos or threes are found
hasTwosThrees(String) :-
%% orders the list keeping the repeated elements
   msort(String,OrderedList),
   writeln(OrderedList),
%% updates the twos counter
   checkTwos(OrderedList),
%% updates the threes counter
   checkThrees(OrderedList).

%% base cases, nothing to do
checkTwos([]) :- !.
checkTwos([_]) :- !.
%% the last two elements are the same, so we update the counter and finish (no recursive call)
checkTwos([X,X]) :-  !, retract(twos(N)), NN is N + 1, assertz(twos(NN)). 
%% the first character in the list is different from the following one, thus no twos or threes, we keep searching from Y (recursive call)
checkTwos([X,Y|Rest]) :- X \== Y, !, checkTwos([Y|Rest]).
%% we've found exactly two equal elements (Y is different), thus we update counter and finish (no recursive call)
checkTwos([X,X,Y|_]) :- X \== Y, !, retract(twos(N)), NN is N + 1, assertz(twos(NN)). 
%% otherwise (more than two equal elements, we keep searching for twos.
checkTwos([X|Tail]) :- delete([X|Tail],X,Rest), !, checkTwos(Rest).
 
%% base cases, nothing to do
checkThrees([]) :- !.
checkThrees([_]) :- !.
%% the first character in the list is different from the following one, thus no twos or threes, we keep searching from Y (recursive call)
checkThrees([X,Y|Rest]) :- X \== Y, !, checkThrees([Y|Rest]).
%% the last three elements are the same, so we update the counter and finish (no recursive call)
checkThrees([X,X,X]) :-  !, retract(threes(N)), NN is N + 1, assertz(threes(NN)). 
%% two equal elements, the third different,  threes, we keep searching from Y (recursive call)
checkThrees([X,X,Y|Rest]) :- X \== Y, !, checkThrees([Y|Rest]).
%% we've found exactly three equal elements (Y is different), thus we update counter and finish (no recursive call)
checkThrees([X,X,X,Y|_]) :- X \== Y, !, retract(threes(N)), NN is N + 1, assertz(threes(NN)).
%% otherwise (more than three equal elements, we keep searching for threes.
checkThrees([X|Tail]) :- delete([X|Tail],X,Rest), !, checkThrees(Rest).
   
