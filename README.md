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

@x = :my_symbol
FooBar = 151.56

stmt1 = 3; stmt2 = 1

def sum0() end

def sum1(thing)
end

def sum2(thing1, thing2) end

false

y = [1,2,3]
y.first

func = -> { }
func = -> (local) { }
func = -> (local) { local.first }
func.call(y)
```

### Output
```
foo : 'a = ?
(orphan) : int = 45
params : hash = { "key": true }
@x : symbol = :my_symbol
FooBar : const<float> = 151.560000
stmt1 : int = 3
stmt2 : int = 1
sum0 : fun () -> 'a = fun { ... }
sum1 : fun ('a) -> 'b = fun { ... }
sum2 : fun ('a, 'b) -> 'c = fun { ... }
(orphan) : bool = false
y : array<'b> = [1, 2, 3]
(call) : 'b = y.first (...)
func : lambda () -> nil = -> { ... }
func : lambda ('a) -> nil = -> { ... }
func : lambda ('a) -> 'c = -> { ... }
(call) : 'c = func.call (...)
```
