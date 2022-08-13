:- module(lexer, [tokenize/2, plex/1]).

plex(Code) :-
    tokenize(Code, Out),
    print_term(Out, []).

tokenize(In, Out) :- tokenize(In, Out, 1).

tokenize([], [], _LineNo).

% increment lineno on newline
tokenize([In|T], Out, LineNo) :-
    code_type(In, newline),
    NextLineNo is LineNo + 1, !,
    tokenize(T, Out, NextLineNo).

% ignore whitespace
tokenize([In|T], Out, LineNo) :-
    code_type(In, space),
    tokenize(T, Out, LineNo).

% ignore comments
tokenize([0'%|T], Out, LineNo) :-
    consume_until(T, 0'\n, Remain, _),
    NextLineNo is LineNo + 1, !,
    tokenize(Remain, Out, NextLineNo).

% block comments dont work
tokenize([0'%, 0'%|T], Out, LineNo) :-
    consume_until(T, [0'%, 0'%], Remain, _),
    NextLineNo is LineNo + 1, !,
    tokenize(Remain, Out, NextLineNo).

% numbers
tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, digit),
    consume_type([In|T_i], digit, [0'.|Next], WholeDigits),
    consume_type(Next, digit, Remain, FractionDigits),
    append(WholeDigits, [0'.|FractionDigits], DigitList),
    number_codes(Value, DigitList),
    Out = lit_t(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, digit),
    consume_type([In|T_i], digit, Remain, DigitList),
    number_codes(Value, DigitList),
    Out = lit_t(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

% keywords
% TODO: organize keywords better
tokenize([0'f, 0'n|T_i], ['fn'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'c, 0'o, 0'n, 0'd|T_i], ['cond'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'c, 0'a, 0's, 0'e|T_i], ['case'(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'l, 0'o, 0'o, 0'p|T_i], ['loop'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'i, 0'f|T_i], ['if'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'e, 0'l, 0's, 0'e|T_i], ['else'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'm, 0'o, 0'd|T_i], ['mod'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'u, 0's, 0'e|T_i], ['use'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'l, 0'a, 0'm, 0'b|T_i], ['lam'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'l, 0'a, 0'm|T_i], ['lam'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'a, 0's|T_i], ['as'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'e, 0'n, 0'd|T_i], ['end'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0':, 0'-|T_i], [':-'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0':, 0':|T_i], ['::'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'-, 0'>|T_i], ['->'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'i, 0'n|T_i], ['in'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0':, 0'>|T_i], [':>'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'<, 0':|T_i], ['<:'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'(|T_i], ['('(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0')|T_i], [')'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'{|T_i], ['{'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'}|T_i], ['}'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'p, 0'a, 0's, 0's|T_i], ['pass'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo). % a do nothing method. does nothing
tokenize([0't, 0'r, 0'u, 0'e|T_i], ['true'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'f, 0'a, 0'l, 0's, 0'e|T_i], ['false'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0':|T_i], [':'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo). % module access
tokenize([0'||T_i], ['|'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'w, 0'h, 0'e, 0'n|T_i], ['when'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0',|T_i], [','(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'[|T_i], ['['(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0']|T_i], [']'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'~|T_i], ['~'(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

% strings
tokenize([0'"|T_i], [Out|T_o], LineNo) :-
    consume_until(T_i, 0'", Remain, Codes),
    string_codes(Value, Codes),
    Out = lit_t(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

% symbols
tokenize([0''|T_i], [Out|T_o], LineNo) :-
    consume_customtype([0''|T_i], ident_type, Remain, Codes),
    string_codes(Value, Codes),
    Out = lit_t(atom(Value), LineNo),
    tokenize(Remain, T_o, LineNo).

% identifiers
tokenize([In|T_i], [Out|T_o], LineNo) :-
    ident_type(In, first),
    consume_customtype([In|T_i], ident_type, Remain, CharList),
    string_codes(Name, CharList),
    Out = sym_t(Name, LineNo),
    tokenize(Remain, T_o, LineNo).

% utils
ident_type(Char, Type) :-
    Char \= 0'(,
    Char \= 0'),
    Char \= 0'[,
    Char \= 0'],
    Char \= 0'{,
    Char \= 0'},
    Char \= 0'",
    Char \= 0',,
    Char \= 0':,
    (code_type(Char, punct);
    (Type = first, code_type(Char, csymf)
    ;   code_type(Char, csym))).

consume_type([], _, [], []).
consume_type([Char|In], Type, Remain, [Char|Out]) :-
    code_type(Char, Type),
    consume_type(In, Type, Remain, Out).
consume_type([Char|In], Type, [Char|In], []) :-
    \+ code_type(Char, Type).

consume_customtype([], _, [], []).
consume_customtype([Char|In], Type, Remain, [Char|Out]) :-
    call(Type, Char, notf),
    consume_customtype(In, Type, Remain, Out).
consume_customtype([Char|In], Type, [Char|In], []) :-
    \+ call(Type, Char, notf).

consume_until([], _, [], []).
consume_until([TargetChar|In], TargetChar, In, []).
consume_until([Char|In], TargetChar, Remain, [Char|Out]) :-
    consume_until(In, TargetChar, Remain, Out).
