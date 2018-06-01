# Typed Ruby
Very much a work-in-progress. I have no idea what I'm doing.

## Example
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

false

y = [1,2,3]
y.first
z = y.first

func1 = -> { x = 45 }
func2 = -> (local) { }
func3 = -> (local, _x) {
  local.first()
}
func3.call(y)

b = 3
a = b

sum1(5)
sum2(1,2)

-> (local, _x) {
  local.first()
}
```

```bash
$ jbuilder exec bin/cli.exe data/test_basic.rb
```

**Output**
```clj
 (T1 : lvar `foo)

[CONSTRAINT: Literal (T2 = int)]
 (T2 : (int 45))

[CONSTRAINT: Equality (T3 = T4)]
[CONSTRAINT: Literal (T3 = hash)]
 (T4 : lvasgn `params
  (T3 : (hash (pair (str "key") (true)) (pair (str "another") (str "value")))))

[CONSTRAINT: Equality (T5 = T6)]
[CONSTRAINT: Literal (T5 = symbol)]
 (T6 : ivasgn @x
  (T5 : (sym `my_symbol)))

 (T7 : ivar `@x)

[CONSTRAINT: Equality (T8 = T9)]
[CONSTRAINT: Literal (T8 = float)]
 (T9 : casgn FooBar
  (T8 : (float 151.560000)))

 (T11 : const
  (T10 : (nil)) `FooBar)

[CONSTRAINT: Equality (T12 = T13)]
[CONSTRAINT: Literal (T12 = int)]
 (T13 : lvasgn `stmt1
  (T12 : (int 3)))

[CONSTRAINT: Equality (T14 = T15)]
[CONSTRAINT: Literal (T14 = int)]
 (T15 : lvasgn `stmt2
  (T14 : (int 1)))

 (T17 : def `sum0 ()
  (T16 : (false)))

 (T19 : def `sum1 (args (arg `thing))
  (T18 : (int 45)))

 (T21 : def `sum2 (args (arg `thing1) (arg `thing2))
  (T20 : (nil)))

[CONSTRAINT: Literal (T22 = bool)]
 (T22 : (false))

[CONSTRAINT: Equality (T23 = T24)]
[CONSTRAINT: Literal (T23 = array<T25>)]
 (T24 : lvasgn `y
  (T23 : (array (int 1) (int 2) (int 3))))

[CONSTRAINT: FunctionApplication (() -> T27 =Fn T26[.first])]
 (T27 : send
  (T26 : lvar `y) `first)

[CONSTRAINT: Equality (T29 = T30)]
[CONSTRAINT: FunctionApplication (() -> T29 =Fn T28[.first])]
 (T30 : lvasgn `z
  (T29 : send
   (T28 : lvar `y) `first))

[CONSTRAINT: Equality (T31 = T32)]
[CONSTRAINT: Literal (T31 = int)]
[CONSTRAINT: Equality (T33 = T34)]
[CONSTRAINT: Literal (T33 = lambda<args -> any>)]
 (T34 : lvasgn `func1
  (T33 : lambda ()
   (T32 : lvasgn `x
    (T31 : (int 45)))))

[CONSTRAINT: Literal (T35 = nil)]
[CONSTRAINT: Equality (T36 = T37)]
[CONSTRAINT: Literal (T36 = lambda<args -> any>)]
 (T37 : lvasgn `func2
  (T36 : lambda (args (arg `local))
   (T35 : (nil))))

[CONSTRAINT: FunctionApplication (() -> T39 =Fn T38[.first])]
[CONSTRAINT: Equality (T40 = T41)]
[CONSTRAINT: Literal (T40 = lambda<args -> any>)]
 (T41 : lvasgn `func3
  (T40 : lambda (args (arg `local) (arg `_x))
   (T39 : send
    (T38 : lvar `local) `first)))

[CONSTRAINT: FunctionApplication (T43 -> T44 =Fn T42[.call])]
 (T44 : send
  (T42 : lvar `func3) `call
   (T43 : lvar `y))

[CONSTRAINT: Equality (T45 = T46)]
[CONSTRAINT: Literal (T45 = int)]
 (T46 : lvasgn `b
  (T45 : (int 3)))

[CONSTRAINT: Equality (T47 = T48)]
 (T48 : lvasgn `a
  (T47 : lvar `b))

[CONSTRAINT: Literal (T49 = nil)]
[CONSTRAINT: Literal (T50 = int)]
[CONSTRAINT: FunctionApplication (T50 -> T51 =Fn T49[.sum1])]
 (T51 : send
  (T49 : (nil)) `sum1
   (T50 : (int 5)))

[CONSTRAINT: Literal (T52 = nil)]
[CONSTRAINT: Literal (T53 = int)]
[CONSTRAINT: Literal (T54 = int)]
[CONSTRAINT: FunctionApplication (T53 -> T54 -> T55 =Fn T52[.sum2])]
 (T55 : send
  (T52 : (nil)) `sum2
   (T53 : (int 1))
   (T54 : (int 2)))

[CONSTRAINT: FunctionApplication (() -> T57 =Fn T56[.first])]
[CONSTRAINT: Literal (T58 = lambda<args -> any>)]
 (T58 : lambda (args (arg `local) (arg `_x))
  (T57 : send
   (T56 : lvar `local) `first))
```
