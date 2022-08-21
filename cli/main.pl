:- module(main, [main/1]).
% compile with swipl -o cassette.exe -c main.pl --goal=main

main(Args) :- print_term(Args, []).
