# Cassette
Cassette is a new programming based off of the languages metatape, elixir, and joy, evolving syntax and semantics from each. Cassette is 
- Dynamically typed
- Interpreted (for now)
- Homoiconic 
- Functional
- Concatenive
- Tape Based
- Metaprogrammable (in the future).
 
# how does it work? 
If you are familier with stack langs, such as Forth, Joy, or Factor, it is common knowledge that the whole program is a stack. In Cassette it's very similar, but instead it uses a tape, which is a [Circular Doubly Linked List](https://en.wikipedia.org/wiki/Doubly_linked_list#Circular_doubly_linked_lists). Internally, Cassette represents it with two stacks (look into [tape.pl](backend/tape.pl) for more details).
 
This means that we can shift a tape `left` or `right`. Take for instance this tape:
 
`1 2 3 4 5`

If we shift the tape left, we get this new tape.
 
`2 3 4 5 1`
 
Similarly, we can move it right.
 
`5 1 2 3 4`

In Cassette you have this functionality for the whole program, quotes, and first class tapes. 

Even though its a tape language, it works very much like a stack lang, using the left list as the main stack. All the pushing and popping happens there. 
 
# examples

reversing a tape:
```erlang
fn [] reverse -> [] 
fn [x <: xs] reverse -> [xs reverse :> x] % examples of cons and snoc for pattern matching

fn main ::
    [1, 2, 3] reverse out
end
```

common stack functions:
```erlang
fn x dup :: x x end
fn x y swap :: y x end
fn x y pop -> x

21 dup
5 6 swap
7 8 9 pop
```
or as quotes
```erlang
(as x -> x x) as dup ->
(as x y -> y x) as swap ->
(as x y -> x) as pop ->

21 dup ~> % ~> evaluates a quote on the tape
5 6 swap ~>
7 8 9 pop ~>
```
