Draughts in erlang
===========================================
## How to run:
Run erlang REPL in distributed node:
```
erl -sname p1
```
Now compile module 'main':
```
c(main).
```
And now run draughts in one of 2 possible modes:

* Player vs CPU:
```
main:start(white).
```
or
```
main:start(black).
```

* CPU vs CPU:
```
main:fullAutoPlay().
```
