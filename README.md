# cob-hs

Add the following incantation to `cabal.project` while this isn't on hackage/for using HEAD

```hs
source-repository-package
    type: git
    location: https://github.com/alt-romes/cob-hs.git

-- for cob-ui
source-repository-package
    type: git
    location: https://github.com/alt-romes/cob-hs.git
    subdir: cob-ui
```
# Interface

We provide
* A way (TH) to automatically derive instances of `Record` for your native
    Haskell datatypes, s.t. you can insert and query `RecordM` using your own
    Haskell idiomatic datatypes -- conversions to the `RecordM` format are done
    automatically behind the scenes.

* A way to construct RecordM queries which takes care of some usual details by
    reflecting on the types.

* Two interfaces to Cob:
    * One based on simple `mtl` constraints:
        * With support for RecordM (`Cob.RecordM`)
        * And UserM (`Cob.UserM`)
    * And another, a `Cob` free monad, which is basically a Cob DSL. It features:
        * An interpreter, `runCob`, which is implemented in terms of the first
            interface, and will run the DSL programs in the Cob instance specified by
            the `CobSession`
        * And a /mock interpreter/, `mockCob`, which is also implemented in terms of
            the first interface, and also runs the same DSL programs on the
            specified Cob instance -- but, afterwards, deletes all records and userm
            instances created.

* Examples! See the `examples` directory.
