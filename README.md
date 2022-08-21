# cassette
Cassette is a new, evolutionary programming based off of brainf**k, being almost entirely different except for the tapes. Cassette is dynamically typed, homoiconic, and metaprogrammable. 
 
# how does it work? 
If you are familier with stack langs, such as forth, joy, factor, etc, you know that the whole program is a stack. In cassette its very similar, but instead it uses a tape, which is a [Circular Doubly Linked List](https://en.wikipedia.org/wiki/Doubly_linked_list#Circular_doubly_linked_lists). Internally, Cassette represents it with two stacks (look into [tape.pl](backend/tape.pl) for more detail).
 
This means that we can shift this tape `left` and `right`. Take for instance this tape:
 
`1 2 3 4 5`

If we shift the tape left, we get the new tape.
 
`2 3 4 5 1`
 
Similarly, we can move it right.
 
`5 1 2 3 4`

In Cassette you have this functionality for the whole program, quotes, and first class tapes. 

Even though its a tape language, it works very much like a stack lang, using the left list as the main stack. All the pushing and popping happens there. 
 
# examples

reversing a tape:
```elixir
fn [] reverse -> [] 
fn [x <: xs] reverse -> [xs reverse :> x] % examples of cons and snoc for pattern matching

fn main ::
    [1, 2, 3] reverse out
end
```

common stack functions:
```elixir
fn x dup :: x x end
fn x y swap :: y x end
fn x y pop -> x

21 dup
5 6 swap
7 8 9 pop
```
or as quotes
```elixir
(as x -> x x) as dup ->
(as x y -> y x) as swap ->
(as x y -> x) as pop ->

21 dup ~> % ~> evaluates a quote on the tape
5 6 swap ~>
7 8 9 pop ~>
```
