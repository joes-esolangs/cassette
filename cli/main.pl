:- module(main, [main/1]).
:- use_module('../backend/run').
% compile with swipl -o cassette.exe -c main.pl --goal=main

% FIXME: repl doesnt work doesnt work
main([Command]) :-
    (   Command = "r"
    ;   Command = "repl"),
    read(In),
    atom_string(In, Code),
    print_term(Code, []),
    cassette_repl(Code).

main([File]) :-
    open(File, read, Stream),
    read_string(Stream, 10000000, Text),
    close(Stream),
    string_codes(Text, Code), run(Code).

cassette_repl(Input) :-
    (   run2(Input)
    ;   writeln("code failed.")),
    main(["r"]).

