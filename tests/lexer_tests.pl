:- module(lexer_tests, [test1/0]).
:- use_module(lexer).

test1() :-
    Code = `
    fn plus :: a b + end
    fn main :: 6 7 plus "hello world" out out end
    `, pretty_print(Code).
