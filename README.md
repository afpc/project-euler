# Project Euler in Haskell

**DISCLAIMER**

Real learning is an active process and seeing how it is done is a long way from 
experiencing that epiphany of discovery. Please do **NOT** just look at the
solutions straight away.

Only the first one-hundred problems can be shared publicly. These also have 
a wealth of helpful introductory teaching material on the 
[website](https://projecteuler.net/) itself.

---

### How is this repository structured?

Every problem has their corresponding file (i.e. `euler{xxx}.hs`). For some
problems there also exists a more optimal solution (i.e. `euler{xxx}_o.hs`).
I try to explain the method I used to solve every problem.

### How to get the solution?

I use `GHCi` to run every file.

```shell
ghci
> :l euler001.hs
> :main
```

You can also use `:set +s` to see the execution time of anything you run, along 
with memory usage.

