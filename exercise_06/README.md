# Restriction maps

### Task 1 - Double Digest Problem (DDP)
* In R, create a function for the brute-force DDP algorithm for one possible arrangement of fragments. 
* Modify it to work with all possible arrangements of fragments.


### Task 2 - Partial Digest Problem (PDP)
Implement recursive function for PDP algorithm according to following pseudocode:

```
PartialDigestProblem(deltaX)
1 width ← the maximum element from deltaX
2 delete the maximum element from deltaX
3 X ← {0, width}
4 Place(deltaX, X, width)

```

``` 
Place(deltaX, X, width)
1   if deltaX is empty
2     output X
3     return
4   y ← the maximum element from deltaX
5   if Δ(y, X) ⊆ deltaX
6     add y to X and remove the lengths Δ(y, X) from deltaX
7     Place(deltaX, X, width)
8     remove y from X and add the lengths Δ(y, X) to deltaX
9   if Δ(width - y, X) ⊆ deltaX
10    add width - y to X and remove the lengths Δ(width - y, X) from deltaX
11    Place(deltaX, X, width)
12    remove width - y from X and add the lengths Δ(width - y, X) to deltaX
13  return
```

Notes: 
* `deltaX` = `ΔX` = multiset of fragments lengths
* `Δ(y, X)` is a multiset of lengths between value `y` and all values in `X`.
* Hint: Create additional function `Remove()`, which removes from `deltaX` all used lengths `Δ(y, X)`.
