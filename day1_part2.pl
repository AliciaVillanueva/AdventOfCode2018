day1_part2(File, Frequency) :-
%% removes facts from previous runs
    retractall(row(_)),
    retractall(lfreq(_)),
    retractall(f(_)),
%% read file and add a row(Number). fact for each line read
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( parse(Codes,Num),
          assertz(row(Num)), fail )
    ; close(Stream), !, 
%% creates the list of numbers LNums from the read numbers in the database 
    findall(R,row(R),LNums), 
%% stores list of nums (you can also pass it as an immutable param to lookForFrequency
    assertz(lfreq(LNums)), 
%% frequency 0 has already appeared (there will be an f(Freq) for each computed Freq 
    assertz(f(0)), 
%% starts the cycle (and Frequency will be instantiated to the first repeated freq)
    lookForFrequency(0,Frequency)
    ).
    
%%% FOR INPUT PARSING
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

% extract from first argument a number in third argument. fourth argument is the remaining list of codes

number([],Acc,N,[]) :- !, atom_codes(N,Acc).
number([X|XS],Acc,N,Rest) :- X >= 48, X =< 57, !, append(Acc,[X],Acc1), number(XS,Acc1,N,Rest).
number([X|XS],Acc,N,[X|XS]) :- atom_codes(N,Acc), !.

%%% END PARSING

lookForFrequency(Ini,FFound) :-
%% consults the list of movements
    lfreq(ListN),
%% starts with the first iteration, starting frequency Ini, upon iteration end, Last will be the last computed frequency, 
%% ListN is the remaining changes for this iteration (initially the whole list), FF is the repeated frequency if Found = 1, otherwise it 
%% remains uninstantiated
    oneIte(Ini,Last,ListN,FF,Found), 
%% after the first iteration, if Found = 1 (and thus maybe without completing it),  FF has the result, 
%% otherwise (Found=0), another iteration starts
    (Found = 1 -> FF = FFound ; lookForFrequency(Last,FFound)).

%% if there are no changes to process ([]), iteration ends with Found=0 (last parameter)
oneIte(Ini,Ini,[],_,0).
%% we've found the element (last parameter = 1) and NF is the result (forth parameter)
oneIte(Ini,NF,[N|_],NF,1) :- 
%% compute new frequency
    NF is Ini + N,
%% checks wether the frequency already exists. If f(NF) is satisfied is because there is a fact with such frequency!
    f(NF),!.
%% otherwise, we compute the new frequency, add to the DB with assertz and keep processing the rest of changes (recursive call)
oneIte(Ini,Last,[N|Rest],FF,Found) :- 
    NF is Ini + N,
    assertz(f(NF)),
    oneIte(NF,Last,Rest,FF,Found).
