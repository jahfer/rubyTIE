# Typed Ruby
Very much a work-in-progress. I have no idea what I'm doing.

## Example
```ruby
foo                                     # foo : 'a = ?
45                                      # (orphan) : int = 45

params = {                              # params : hash = { "key": true }
  "key": true
}

@x = :my_symbol                         # @x : symbol = :my_symbol
FooBar = 151.56                         # FooBar : const<float> = 151.560000

stmt1 = 3; stmt2 = 1                    # stmt1 : int = 3
                                        # stmt2 : int = 1
def sum0(); false end                   # sum0 : fun () -> (orphan) : bool = false

def sum1(thing)                         # sum1 : fun (thing : 'a = ?) -> (orphan) : int = 45
  45
end

def sum2(thing1, thing2) end            # sum2 : fun (thing1 : 'a = ?, thing2 : 'b = ?) -> (orphan) : 'c = ?

false                                   # (orphan) : bool = false

y = [1,2,3]                             # y : array<'b> = [1, 2, 3]
y.first                                 # (y : 'a = ?).first (...)

func1 = -> { 45 }                       # func1 : lambda () -> int = -> { ... }
func2 = -> (local) { }                  # func2 : lambda ('a) -> nil = -> { ... }
func3 = -> (local, _x) { local.first }  # func3 : lambda ('a, 'b) -> any = -> { ... }
func3.call(y)                           # (func3 : 'a = ?).call (...)
```
