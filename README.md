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
