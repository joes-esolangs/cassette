:- module(pretty_print, [pretty_print/1]).
% for pretty printing cassette terms for the print command.

pretty_print(lit(X)) :- print_term(X, []).
