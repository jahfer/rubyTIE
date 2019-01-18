# Constraint Solving

### Generate constraints

- [x] Completed

```
SubType (T1 < T2)
Literal (T1 = bool)
SubType (T4 < T10)
Binding (x = T2)
SubType (T3 < T4)
Literal (T3 = int)
Binding (y = T4)
SubType (T5 < T6)
SubType (T2 < T5)
Binding (a = T6)
SubType (T7 < T8)
SubType (T4 < T7)
Binding (a = T8)
SubType (T6 < T8)
SubType (T6 < T9)
```

### Fold bindings and subtypes

- [ ] Completed

```
SubType (bool < x < T5 < a)
SubType (int < y < T10 < T7 < a)
SubType (a < a)
SubType (a < T9)
```

### Drop intermediate types

- [ ] Completed

```
SubType (bool < x < a)
SubType (int < y < a)
SubType (a < T9)
```

### Unify constraints

- [ ] Completed

```
x = bool
y = int
a = bool | int
```
