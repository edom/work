To run a file such as `test.rkt`:

```
PLTCOLLECTS=$(pwd): racket test.rkt
```

To generate HTML documentation from a file such as `main.scrbl`:

```
PLTCOLLECTS=$(pwd): scribble +m --dest _out main.scrbl
```
