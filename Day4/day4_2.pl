
remove_duplicates(In, Out) :- sort(In, Out).

any(_, []) :- fail.
any(Pred, [A|As]) :- call(Pred, A); any(Pred, As).

has_length_two((_, L)) :- L == 2.

% helper for run_length_encode/2
rle_sub(NewElem, [], NewState) :-
    NewState = [(NewElem, 1)].
rle_sub(NewElem, [(Elem, Length)|T], NewState) :-
    (Elem == NewElem)
    -> (NewLength is Length + 1, NewState = [(Elem, NewLength)|T])
    ; NewState = [(NewElem, 1)|[(Elem, Length)|T]].

% returns a run-length encoded version of in *reversed* order
% use reverse/2 to reverse if needed
run_length_encode(In, Out) :-
    foldl(rle_sub, In, [], Out).

contains_double_nt(L) :-
    run_length_encode(L, L_Rle),
    any(has_length_two, L_Rle).

password(X) :-
    between(124075, 580769, X),
    number_chars(X, Digits),
    sort(0, @=<, Digits, Digits),
    contains_double_nt(Digits),
    length(Digits, 6).

:-
    findall(Password, password(Password), Passwords),
    remove_duplicates(Passwords, PwWithoutDuplicates),
    length(PwWithoutDuplicates, Length),
    write('Length: '),
    write(Length),
    write('\n'),
    halt.
