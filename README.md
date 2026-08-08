CoreML
======

このリポジトリは複数の章を含む Cabal workspace です。

```sh
# 全ての章をビルド
cabal build all

# 個別にビルドまたは実行
cabal build chap2
cabal run chap2
cabal build chap3
cabal run chap3
```

各章は `chap2/`・`chap3/` 以下の独立した Cabal パッケージです。追加する章は
`chapN/chapN.cabal` を作り、ルートの `cabal.project` の `packages` に加えます。
