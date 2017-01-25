FastPush provides a monad (and a highly experimental monad transformer) that  allows you to push things onto a stack very quickly.

Under the hood, it's backed my a mutable ST vector. You can choose which kind of vector (Boxed, Unboxed, Storable, etc.) you want to use. 

https://hackage.haskell.org/package/FastPush

Install with "cabal install FastPush".
