:- module(main, [main/1]).
:- use_module('../backend/run').
% compile with swipl -o cassette.exe -c main.pl --goal=main

% FIXME
main([File]) :-
    gtrace, open(File, read, Stream),
    read_string(Stream, 10000000, Text),
    close(Stream),
    string_codes(Text, Code), run(Code).
