# Ruby Type Inference Engine (TIE)

Very much a work-in-progress. I have no idea what I'm doing.

## Examples

### Type Checking

**Input**

```
1| x = false
2| y = 3
3|
4| a = x
5| a = y
```

**Output**

```
(bool : lvasgn `x
  (bool : false))

(int : lvasgn `y
  (int : 3))

(bool : lvasgn `a
  (bool : lvar `x))

(bool|int : lvasgn `a
  (bool|int : lvar `y))
```

### Kitchen Sink

**Input**

```ruby
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

def maybe_sum(a, b, should_do_thing) end

false

y = [1,2,3]
y.first
z = y.first

func1 = -> { x = 45 }
func2 = -> (local) { }
func3 = -> (local, _x) {
  local.first
}
func3.call(y)

b = 3
a = b

sum1(5)
sum2(1,2)
maybe_sum(3, 5, false)

-> (local, _x) {
  local.first
}
```

```bash
$ dune exec bin/cli.exe data/test_basic.rb
```

**Output**

```clj
(T1 : lvar `foo)

(int : 45)

(hash : lvasgn `params
  (hash : { "key": true, "another": "value" }))

(symbol : ivasgn @x
  (symbol : :my_symbol))

(symbol : ivar `@x)

(float : casgn FooBar
  (float : 151.560000))

(float : const
  (T10 : nil) `FooBar)

(int : lvasgn `stmt1
  (int : 3))

(int : lvasgn `stmt2
  (int : 1))

(T17 : def `sum0 ()
  (T16 : false))

(T19 : def `sum1 (args (arg `thing))
  (T18 : 45))

(T21 : def `sum2 (args (arg `thing1) (arg `thing2))
  (T20 : nil))

(T23 : def `maybe_sum (args (arg `a) (arg `b) (arg `should_do_thing))
  (T22 : nil))

(bool : false)

(array<any> : lvasgn `y
  (array<any> : [1 2 3]))

(T31 : send
  (array<any> : lvar `y) `first)

(T31 : lvasgn `z
  (T31 : send
    (array<any> : lvar `y) `first))

(lambda<? -> any> : lvasgn `func1
  (lambda<? -> any> : lambda ()
    (int : lvasgn `x
      (int : 45))))

(lambda<? -> any> : lvasgn `func2
  (lambda<? -> any> : lambda (args (arg `local))
    (nil : nil)))

(lambda<? -> any> : lvasgn `func3
  (lambda<? -> any> : lambda (args (arg `local) (arg `_x))
    (T63 : send
      (T62 : lvar `local) `first)))

(T45 : send
  (lambda<? -> any> : lvar `func3) `call
    (array<any> : lvar `y))

(int : lvasgn `b
  (int : 3))

(int : lvasgn `a
  (int : lvar `b))

(T52 : send
  (nil : nil) `sum1
    (int : 5))

(T56 : send
  (nil : nil) `sum2
    (int : 1)
    (int : 2))

(T61 : send
  (nil : nil) `maybe_sum
    (int : 3)
    (int : 5)
    (bool : false))

(lambda<? -> any> : lambda (args (arg `local) (arg `_x))
  (T63 : send
    (T62 : lvar `local) `first))
```
