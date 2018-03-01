# Typed Ruby
Very much a work-in-progress. I have no idea what I'm doing.

## Example
### Input
```ruby
foo
45

params = {
  "key": true
}

y = [1,2,3]
@x = 4
FooBar = 151.56

def sum0() end

def sum1(thing)
end

def sum2(thing1, thing2) end

false
```

### Output
```ocaml
val foo : 'a = ?
val (orphan) : int = 45
val params : hash = { "key": true }
val y : array<'a> = [1, 2, 3]
val @x : int = 4
val FooBar : const<float> = 151.560000
val sum0 : fun () -> 'a = fun { ... }
val sum1 : fun ('a) -> 'b = fun { ... }
val sum2 : fun ('a, 'b) -> 'c = fun { ... }
val (orphan) : bool = false
```
