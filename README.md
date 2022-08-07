# cassette
 a new revolutionary highlevel and readable tape language (unlike brainf), written entirely in prolog
# examples

reversing a tape:
```elixir
fn reverse [] -> [] 
fn reverse [x <: xs] -> [xs reverse :> x] 

fn main ::
    [1, 2, 3] reverse out
end
```
