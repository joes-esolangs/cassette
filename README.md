# cassette
 a new revolutionary highlevel and readable tape language (unlike brainf)

# examples

reversing a tape:
```elixir
fn reverse_tape [] :: [] end
fn reverse_tape [x <: xs] :: [xs reverse_tape :> x] end

fn main ::
    [1, 2, 3] reverse_tape out
end
```
