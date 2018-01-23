# Typed Ruby
Very much a work-in-progress. I have no idea what I'm doing.

## Example
### Input
```ruby
foo
y = [1,2,3]
hsh = { "key": true }
x = 4
FooBar = 4
def sum() end
def sum2(thing1, thing2) end
```

### Output
```ocaml
(* val foo : Any *)
foo = ?

(* val y : Array<Any> *)
y = [1, 2, 3]

(* val hsh : Hash *)
hsh = { "key": true }

(* val x : Integer *)
x = 4

(* const FooBar : Constant<Integer> *)
FooBar = 4

(* fun sum : () -> Any *)
fun sum([]) -> Any

(* fun sum2 : (val thing1 : Any, val thing2 : Any) -> Any *)
fun sum2([thing1 = ?, thing2 = ?]) -> Any
```
