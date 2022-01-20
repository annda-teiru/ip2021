---
marp: true
---

# みだし

---

## 見出し2

```haskell
qsort :: Ord a => [a] -> [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lts ++ [x] ++ gts
    where
        (lts, gts) = partition (x >) xs
```

---

`qsort` の計算量は、平均 $O(n)$

O(n)