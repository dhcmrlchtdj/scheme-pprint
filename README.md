# scheme pretty printer

---

```
$ make

--------oneline
(((call/cc (lambda (c) c)) (lambda (x) x)) (quote HEY!))

--------pretty
(((call/cc (lambda (c) c))
        (lambda (x) x))
    (quote HEY!))
```
