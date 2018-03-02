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

def sum0(); false end

def sum1(thing)
  45
end

def sum2(thing1, thing2) end

false

y = [1,2,3]
y.first

func1 = -> { 45 }
func2 = -> (local) { }
func3 = -> (local, _x) { local.first }
func3.call(y)
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
sum0 : fun () -> (orphan) : bool = false
sum1 : fun (thing : 'a = ?) -> (orphan) : int = 45
sum2 : fun (thing1 : 'a = ?, thing2 : 'b = ?) -> (orphan) : 'c = ?
(orphan) : bool = false
y : array<'b> = [1, 2, 3]
(y : 'a = ?).first (...)
func1 : lambda () -> int = -> { ... }
func2 : lambda ('a) -> nil = -> { ... }
func3 : lambda ('a, 'b) -> any = -> { ... }
(func3 : 'a = ?).call (...)
```
