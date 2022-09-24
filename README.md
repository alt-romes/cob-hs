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

Until this is patched upstream, and you require the `streamSearch`, you must
also add to `cabal.project`:

```hs
source-repository-package
    type: git
    location: https://github.com/alt-romes/servant.git
    tag: bc2008be82d1dbcf51faa6c61ceeaa66f3d5e8c5
    subdir: servant-client
```
