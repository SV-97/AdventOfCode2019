
remove_duplicates(In, Out) :- sort(In, Out).

contains_double([]) :- fail.
contains_double([H1|[H2|T]]) :- H1 == H2; contains_double([H2|T]).

password(X) :-
    between(124075, 580769, X),
    number_chars(X, Digits),
    sort(0, @=<, Digits, Digits),
    contains_double(Digits),
    length(Digits, 6).

:-
    findall(Password, password(Password), Passwords),
    remove_duplicates(Passwords, PwWithoutDuplicates),
    length(PwWithoutDuplicates, Length),
    write('Length: '),
    write(Length),
    write('\n'),
    halt.
