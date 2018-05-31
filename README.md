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

-> (local, _x) {
  local.first()
}

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

[CONSTRAINT: Member (t/1026.first -> t/1027)]
 (t/1027 : send
  (t/1026 : lvar `y) `first)

[CONSTRAINT: Equality (t/1029 = t/1030)]
[CONSTRAINT: Member (t/1028.first -> t/1029)]
 (t/1030 : lvasgn `z
  (t/1029 : send
   (t/1028 : lvar `y) `first))

[CONSTRAINT: Equality (t/1031 = t/1032)]
[CONSTRAINT: Literal (t/1031 = int)]
[CONSTRAINT: Equality (t/1033 = t/1034)]
 (t/1034 : lvasgn `func1
  (t/1033 : lambda ()
   (t/1032 : lvasgn `x
    (t/1031 : (int 45)))))

[CONSTRAINT: Literal (t/1035 = nil)]
[CONSTRAINT: Equality (t/1036 = t/1037)]
 (t/1037 : lvasgn `func2
  (t/1036 : lambda (args (arg `local))
   (t/1035 : (nil))))

[CONSTRAINT: Member (t/1038.first -> t/1039)]
[CONSTRAINT: Equality (t/1040 = t/1041)]
 (t/1041 : lvasgn `func3
  (t/1040 : lambda (args (arg `local) (arg `_x))
   (t/1039 : send
    (t/1038 : lvar `local) `first)))

[CONSTRAINT: Member (t/1042.call -> t/1044)]
 (t/1044 : send
  (t/1042 : lvar `func3) `call)

[CONSTRAINT: Equality (t/1045 = t/1046)]
[CONSTRAINT: Literal (t/1045 = int)]
 (t/1046 : lvasgn `b
  (t/1045 : (int 3)))

[CONSTRAINT: Equality (t/1047 = t/1048)]
 (t/1048 : lvasgn `a
  (t/1047 : lvar `b))

[CONSTRAINT: Literal (t/1049 = nil)]
[CONSTRAINT: Member (t/1049.sum1 -> t/1051)]
 (t/1051 : send
  (t/1049 : (nil)) `sum1)

[CONSTRAINT: Member (t/1052.first -> t/1053)]
 (t/1054 : lambda (args (arg `local) (arg `_x))
  (t/1053 : send
   (t/1052 : lvar `local) `first))
```
