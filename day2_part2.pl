:- use_module(library(lists)), use_module(library(ordsets)).

day2_part2(File, Res) :-
    retractall(row(_)),
    open(File, read, Stream),
    repeat,
    read_line_to_codes(Stream, Codes),
    (   Codes \= end_of_file
    ->  ( assertz(row(Codes)), fail )
    ; close(Stream), !, 
    %% collect in LString all the lists in the input file
    findall(R,row(R),LStrings), 
    %% look for similar strings and returns Intersection
    findMatch(LStrings,Intersect), 
    %% transform from ascii codes (Intersec) to characters (Res)
    atom_codes(Res,Intersect)
    ).
    
%% for each list in the input file
findMatch([Head|LStrings],Result) :-
   %% compares the list with the rest of lists and, if a similar list is found, it is returned in Intersect
   compareEach(Head,LStrings,Intersect,Found),
   %% checks whether the similar list has been found and, in that case, unifies Result with the Intersect list, otherwise keeps searching
   (Found = 1 -> Result = Intersect ; findMatch(LStrings,Result)).

%% if there are no more lists to be compared to, it finishes and Found = 0 (last parameter)
compareEach(_List,[],_,0).
%% if is_similar succeeds, we've found the element and return the Intersect list
compareEach(List,[One|_Tail],Intersect,1) :-
   %% checks whether is_similar to the first list
   is_similar(List,One,Intersect), !.
%% otherwise, compares List with the rest of lists in Tail (recursive call)
compareEach(List,[_One|Tail],Match,Found) :-
   compareEach(List,Tail,Match,Found).

%% is the first elements of each list are equal, we keep checking and X is added to the intersection list
is_similar([X|XRest], [X|YRest], [X|ResRes]) :- !, is_similar(XRest,YRest,ResRes).
%% otherwise (the first elements are not equal, the two tails must be exactly the same (since we've found the only element that can differ)
is_similar([_X|XRest], [_Y|YRest],XRest) :- XRest = YRest.
