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
6|
7| z = a.foo
8| z
9| y

```

**Output**

```clj
(lvasgn `x
  (false : bool) : bool)

(lvasgn `y
  (3 : int) : int)

(lvasgn `a
  (lvar `x : bool) : int|bool)

(lvasgn `a
  (lvar `y : int) : int|bool)

(lvasgn `z
  (send
    (lvar `a : int|bool) `foo : T11) : T11)

(send
  (lvar `z : T11) `bar : T13)

(lvar `y : int)
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
(lvar `foo : T1)

(45 : int)

(lvasgn `params
  ({ "key": true, "another": "value" } : hash) : hash)

(ivasgn @x
  (:my_symbol : symbol) : symbol)

(ivar `@x : symbol)

(casgn FooBar
  (151.560000 : float) : float)

(const
  (nil : T10) `FooBar : float)

(lvasgn `stmt1
  (3 : int) : int)

(lvasgn `stmt2
  (1 : int) : int)

(def `sum0 ()
  (false : T16) : T17)

(def `sum1 (args (arg `thing))
  (45 : T18) : T19)

(def `sum2 (args (arg `thing1) (arg `thing2))
  (nil : T20) : T21)

(def `maybe_sum (args (arg `a) (arg `b) (arg `should_do_thing))
  (nil : T22) : T23)

(false : bool)

(lvasgn `y
  ([1 2 3] : array<T65>) : array<T65>)

(send
  (lvar `y : array<T65>) `first : T28)

(lvasgn `z
  (send
   (lvar `y : array<T65>) `first : T28) : T28)

(lvasgn `func1
  (lambda ()
    (lvasgn `x
      (45 : int) : int) : lambda<? -> T33>) : lambda<? -> T33>)

(lvasgn `func2
  (lambda (args (arg `local))
    (nil : nil) : lambda<? -> T36>) : lambda<? -> T36>)

(lvasgn `func3
  (lambda (args (arg `local) (arg `_x))
    (send
      (lvar `local : T39) `first : T40) : lambda<? -> T40>) : lambda<? -> T40>)

(send
  (lvar `func3 : lambda<? -> T40>) `call
    (lvar `y : array<T65>) : T45)

(lvasgn `b
  (3 : int) : int)

(lvasgn `a
  (lvar `b : int) : int)

(send
  (nil : nil) `sum1
    (5 : int) : T52)

(send
  (nil : nil) `sum2
    (1 : int)
    (2 : int) : T56)

(send
  (nil : nil) `maybe_sum
    (3 : int)
    (5 : int)
    (false : bool) : T61)

(lambda (args (arg `local) (arg `_x))
  (send
    (lvar `local : T39) `first : T40) : lambda<? -> T63>)
```
