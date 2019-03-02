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

```diff
SubType  bool < x
SubType  y < T10
SubType  int < y
SubType  T5 < a
SubType  x < T5
Binding  a = a
SubType  T7 < T8
SubType  y < T7
SubType  a < T8
SubType  a < T9
```

#### Drop Types With Single Constraints (T9, T10)

```diff
SubType  bool < x
SubType  int < y
SubType  T5 < a
SubType  x < T5
SubType  T7 < T8
SubType  y < T7
SubType  a < T8
```

#### Flatten Relationships

```diff
SubType  bool < x < T5 < a < T8
SubType  int < y < T7 < a < T8
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
