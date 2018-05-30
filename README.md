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
 (t/1001 : lvar `foo)

[CONSTRAINT: Literal (t/1002 = int)]
 (t/1002 : (int 45))

[CONSTRAINT: Equality (t/1003 = t/1004)]
[CONSTRAINT: Literal (t/1003 = hash)]
 (t/1004 : lvasgn `params
  (t/1003 : (hash (pair (str "key") (true)) (pair (str "another") (str "value")))))

[CONSTRAINT: Equality (t/1005 = t/1006)]
[CONSTRAINT: Literal (t/1005 = symbol)]
 (t/1006 : ivasgn @x
  (t/1005 : (sym `my_symbol)))

 (t/1007 : ivar `@x)

[CONSTRAINT: Equality (t/1008 = t/1009)]
[CONSTRAINT: Literal (t/1008 = float)]
 (t/1009 : casgn FooBar
  (t/1008 : (float 151.560000)))

 (t/1011 : const
  (t/1010 : (nil)) `FooBar)

[CONSTRAINT: Equality (t/1012 = t/1013)]
[CONSTRAINT: Literal (t/1012 = int)]
 (t/1013 : lvasgn `stmt1
  (t/1012 : (int 3)))

[CONSTRAINT: Equality (t/1014 = t/1015)]
[CONSTRAINT: Literal (t/1014 = int)]
 (t/1015 : lvasgn `stmt2
  (t/1014 : (int 1)))

 (t/1017 : def `sum0 ()
  (t/1016 : (false)))

 (t/1019 : def `sum1 (args (arg `thing))
  (t/1018 : (int 45)))

 (t/1021 : def `sum2 (args (arg `thing1) (arg `thing2))
  (t/1020 : (nil)))

[CONSTRAINT: Literal (t/1022 = bool)]
 (t/1022 : (false))

[CONSTRAINT: Equality (t/1023 = t/1024)]
[CONSTRAINT: Literal (t/1023 = array<t/1025>)]
 (t/1024 : lvasgn `y
  (t/1023 : (array (int 1) (int 2) (int 3))))

[CONSTRAINT: Member (t/1027.first -> t/1028)]
 (t/1027 : send
  (t/1026 : lvar `y) `first)

[CONSTRAINT: Equality (t/1030 = t/1031)]
[CONSTRAINT: Member (t/1030.first -> t/1032)]
 (t/1031 : lvasgn `z
  (t/1030 : send
   (t/1029 : lvar `y) `first))

[CONSTRAINT: Equality (t/1033 = t/1034)]
[CONSTRAINT: Literal (t/1033 = int)]
[CONSTRAINT: Equality (t/1035 = t/1036)]
 (t/1036 : lvasgn `func1
  (t/1035 : lambda ()
   (t/1034 : lvasgn `x
    (t/1033 : (int 45)))))

[CONSTRAINT: Literal (t/1037 = nil)]
[CONSTRAINT: Equality (t/1038 = t/1039)]
 (t/1039 : lvasgn `func2
  (t/1038 : lambda (args (arg `local))
   (t/1037 : (nil))))

[CONSTRAINT: Member (t/1041.first -> t/1044)]
[CONSTRAINT: Equality (t/1042 = t/1043)]
 (t/1043 : lvasgn `func3
  (t/1042 : lambda (args (arg `local) (arg `_x))
   (t/1041 : send
    (t/1040 : lvar `local) `first)))

[CONSTRAINT: Member (t/1047.call -> t/1048)]
 (t/1047 : send
  (t/1045 : lvar `func3) `call)

[CONSTRAINT: Equality (t/1049 = t/1050)]
[CONSTRAINT: Literal (t/1049 = int)]
 (t/1050 : lvasgn `b
  (t/1049 : (int 3)))

[CONSTRAINT: Equality (t/1051 = t/1052)]
 (t/1052 : lvasgn `a
  (t/1051 : lvar `b))

[CONSTRAINT: Literal (t/1053 = nil)]
[CONSTRAINT: Member (t/1055.sum1 -> nil)]
 (t/1055 : send
  (t/1053 : (nil)) `sum1)

[CONSTRAINT: Member (t/1057.first -> t/1059)]
 (t/1058 : lambda (args (arg `local) (arg `_x))
  (t/1057 : send
   (t/1056 : lvar `local) `first))
```
