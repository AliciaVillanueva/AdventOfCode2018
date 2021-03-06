day1_part1(File, Frequency) :-
%% Read from file
    open(File, read, Stream),
    repeat,
%% each time a line is read, it is asserted as a row in the DB.
%% fail is used to repeat reading the file
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( parse(Codes,Num),
          assertz(row(Num)), fail )
    ; close(Stream), !, 
%% Here starts solution. finall creates a list LNums with all the numbers read
    findall(R,row(R),LNums), writeln(LNums), 
%% we pass the list of numbers to computeFrequency that just sums them up. 
%% Frequency is instantiated with the final result which is "returned" when we run the call ?- day1_part1("input.txt",Freq).
    computeFrequency(LNums,Frequency)
    ).

%%%%% PARSIN THE INPUT

parse([],[]) :- !.
% removes blanks
parse([32|LCodes],Num) :- !, parse(LCodes,Num).
% removes tabs
parse([9|LCodes],Num) :- !, parse(LCodes,Num).
% remove +
parse([43|LCodes],Num) :- !, parse(LCodes,Num).
% deal with -
parse([45|LCodes],Num) :- !, parse(LCodes,N), Num is 0-N.
parse(LCodes,Num) :- !, number_codes(Num,LCodes).

number([],Acc,N,[]) :- !, atom_codes(N,Acc).
number([X|XS],Acc,N,Rest) :- X >= 48, X =< 57, !, append(Acc,[X],Acc1), number(XS,Acc1,N,Rest).
number([X|XS],Acc,N,[X|XS]) :- atom_codes(N,Acc), !.

%%%%% END PARSING
computeFrequency(LNums,Frequency) :- sum(LNums,Frequency).

sum([],0).
sum([X|R],S) :- sum(R,SR), S is SR + X.
