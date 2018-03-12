# Typed Ruby
Very much a work-in-progress. I have no idea what I'm doing.

## Example
**Input**
```ruby
# data/test_basic.rb

foo
45

params = {
  "key": true,
  "another": "value"
}

@x = :my_symbol
@x

FooBar = 151.56
FooBar

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

b = 3
a = b
```

```bash
$ jbuilder exec bin/cli.exe data/test_basic.rb
```

**Output**
```clj
        'a : (lvar :foo)
       int : (int 45)
      hash : (lvasgn :params (hash (pair (str "key") (true)) (pair (str "another") (str "value"))))
    symbol : (ivasgn @x (sym :my_symbol))
        'b : (ivar :@x)
     float : (casgn FooBar (float 151.560000))
 const<'c> : (const (nil) :FooBar)
       int : (lvasgn :stmt1 (int 3))
       int : (lvasgn :stmt2 (int 1))
      bool : (def :sum0 (args) (false))
       int : (def :sum1 (args (arg :thing)) (int 45))
       nil : (def :sum2 (args (arg :thing1) (arg :thing2)) (nil))
      bool : (false)
 array<'d> : (lvasgn :y (array (int 1) (int 2) (int 3)))
        'e : (send (lvar :y) :first)
lambda<int> : (lvasgn :func1 (block (lambda) (args) (int 45)))
lambda<nil> : (lvasgn :func2 (block (lambda) (args (arg :local)) (nil)))
lambda<'f> : (lvasgn :func3 (block (lambda) (args (arg :local) (arg :_x)) (send (lvar :local) :first)))
        'g : (send (lvar :func3) :call)
       int : (lvasgn :b (int 3))
        'h : (lvasgn :a (lvar :b))
```
