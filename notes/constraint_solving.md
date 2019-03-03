# Constraint Solving

### Constraint Generation

- [x] Completed

```
SubType  T1 < T2
Literal  T1 = bool
SubType  T4 < T10
Binding  x = T2
SubType  T3 < T4
Literal  T3 = int
Binding  y = T4
SubType  T5 < T6
SubType  T2 < T5
Binding  a = T6
SubType  T7 < T8
SubType  T4 < T7
SubType  T6 < T8
SubType  T6 < T9
```

### Simplification

#### Replace Literals in Subtypes

- [x] Completed

```diff
SubType  bool < T2
SubType  T4 < T10
Binding  x = T2
SubType  int < T4
Binding  y = T4
SubType  T5 < T6
SubType  T2 < T5
Binding  a = T6
SubType  T7 < T8
SubType  T4 < T7
SubType  T6 < T8
SubType  T6 < T9
```

#### Cosmetics: Insert Bindings in Subtypes

- [x] Completed

```diff
SubType  bool < T2 (x)
SubType  T4 (y) < T10
SubType  int < T4 (y)
SubType  T5 < T6 (a)
SubType  T2 (x) < T5
SubType  T7 < T8 (a)
SubType  T4 (y) < T7
SubType  T6 (a) < T8 (a)
SubType  T6 (a) < T9
```

#### Flatten Relationships

- [ ] Completed

```diff
                                 T6 (a) < T9
                                /
SubType  bool < T2 (x) < T5 < T6 (a) < T8 (a)
                              /
SubType  int < T4 (y) < T7 < T6 (a)
                \
                 T4 (y) < T10
```

#### Drop Intermediate Type Variables

- [ ] Completed

```diff
SubType (bool < x < a)
SubType (int < y < a)
```

### Unification

- [ ] Completed

```
x : bool
y : int
a : bool | int
```
