# Typed Ruby
Very much a work-in-progress. I have no idea what I'm doing.

## Example
### Input
```ruby
foo
45
y = [1,2,3]
hsh = { "key": true }
@x = 4
FooBar = 151.56
def sum0() end
def sum1(thing) end
def sum2(thing1, thing2) end
```

### Output
```ocaml
val foo : 'a = ?
val ORPHAN : int = 45
val y : array<'a> = [1, 2, 3]
val hsh : hash = { "key": true }
val @x : int = 4
val FooBar : const<float> = 151.560000
val sum0 : fun () -> 'a = fun { ... }
val sum1 : fun ('a) -> 'b = fun { ... }
val sum2 : fun ('a, 'b) -> 'c = fun { ... }
```
