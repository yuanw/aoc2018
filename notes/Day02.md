https://adventofcode.com/2018/day/2

# Part I

We need to do

> * `abcdef` contains no letters that appear exactly two or three times.
> * `bababc` contains two `a` and three `b`, so it counts for both.
> * `abbcde` contains two `b`, but no letter appears exactly three times.
> * `abcccd` contains three `c`, but no letter appears exactly two times.
> * `aabcdd` contains two `a` and two `d`, but it only counts once.
> * `abcdee` contains two `e`.
> * `ababab` contains three `a` and three `b`, but it only counts once.

`Map` seems be a reasonable structure to represent the result.

`scan` function aims to do that transformation

```haskell
scan :: ID -> Map.Map Char Int
scan = foldr (\ k m -> Map.insert k (fromMaybe 0 (Map.lookup k m) + 1 ) m) Map.empty
```
