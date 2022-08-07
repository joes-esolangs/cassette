:- module(parser, [parse/2, ast/1]).
:- use_module(lexer).
% add pattern matching for arguments

parse(Code, AST) :-
    tokenize(Code, Out),
    instructions(AST, Out, []).

ast(Code) :-
    parse(Code, AST),
    print_term(AST, []).

pat_list([Pattern|Rest])         --> pattern(Pattern), ([','(_)], pat_list(Rest); {Rest = []}).
pattern(pat_lit(Value))     --> [lit_t(Value, _)].
pattern(pat_var(Name))     --> [sym_t(Name, _)].
pattern(pat_cons(Head, Tail)) --> ['['(_)], pat_list(Head), [sym_t("<:",_)], pattern(Tail), [']'(_)].
pattern(pat_snoc(F, L)) --> ['['(_)], pattern(F), [sym_t(":>",_)], pat_list(L), [']'(_)].
pattern(pat_list(L))    --> ['['(_)], (pat_list(L); {L = []}), [']'(_)].

branch(branch(Expressions, Instructions), Type, NoPipe) --> ({NoPipe = true}; ['|'(_)]), ({Type = case}, pat_list(Expressions); {Type = cond}, expr_list(Expressions)), ['->'(_)], instructions(Instructions, all).
branches([Branch|Rest], Type) --> ({Pipe = true; Pipe = false}, branch(Branch, Type, Pipe)), (branches(Rest, Type); {Rest = []}).

fn(fn(Name, Args, Instructions)) --> ['fn'(_)], (arg_list(Args); {Args = []}), [sym_t(Name, _)], block(Instructions).

cond(cond(Branches, Else)) --> ['cond'(_)], branches(Branches, cond), (['|'(_), '->'(_)], instructions(Else, all); {Else = []}), ['end'(_)].

case(case(Values, Branches, Else)) --> ['case'(_)], (elems(Values); {Values = []}), [':-'(_)], branches(Branches, case), (['|'(_), '->'(_)], instructions(Else, case); {Else = []}), ['end'(_)].

block(Instructions)    --> [':-'(_)], instructions(Instructions), ['end'(_)]; ['->'(_)], instruction(Instructions, all).

% Root symbol
instructions([Node|Rest]) --> instruction(Node, all), (instructions(Rest, all); {Rest = []}).

instruction(Node, all) --> fn(Node); lam(Node); lit(Node); sym(Node); seq(Node); pass(Node); tracer; as(Node); loop(Node); if(Node); case(Node); cond(Node).
instructions([Node|Rest], Type) --> instruction(Node, Type), (instructions(Rest, Type); {Rest = []}).

tracer --> [sym_t("$trace",_)], {trace}.

lam(lam(Args, Instructions)) --> ['lam'(_)], (arg_list(Args); {Args = []}), block(Instructions).
lam(lam([], Instructions)) --> ['('(_)], instructions(Instructions), [')'(_)].

as(as(Args)) --> ['as'(_)], arg_list(Args), ['->'(_)].

loop(ntimes(N, L)) --> lam(L), ['{'(_)], lit(N), ['}'(_)].

if(if(Condition, If, Else)) --> ['if'(_)], (expr_list(Condition); {Condition = []}), block(If), (['else'(_)], block(Else); {Else = []}).

pass(pass) --> ['pass'(_)].

lit(lit(Value)) --> [lit_t(Value, _)].
sym(sym(Name)) --> [sym_t(Name, _)].

% expr(Node) --> lit(Node); sym(Node); seq(Node); lam(Node); loop(Node);
% if(Node); as(Node).

elem_group(Node)     --> instruction(Node, all).
elem_group([Node|Rest])     --> instruction(Node, all), (elem_group(Rest); {Rest = []}).
elems([Node|Rest])          --> elem_group(Node), ([','(_)], elems(Rest); {Rest = []}).
seq(cons(Head, Tail))       --> ['['(_)], elems(Head), [sym_t("<:",_)], elem_group(Tail), [']'(_)].
seq(snoc(Beginning, Last))       --> ['['(_)], elem_group(Beginning), [sym_t(":>",_)], elems(Last), [']'(_)].
seq(tape(Elements))          --> ['['(_)], (elems(Elements); {Elements = []}), [']'(_)]. % circular doubly linked list

arg_list([Pattern|Rest]) --> pattern(Pattern), (arg_list(Rest); {Rest = []}).
expr_list([Expr|Rest]) --> instruction(Expr, all), (expr_list(Rest); {Rest = []}).



