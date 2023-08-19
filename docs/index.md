# オリジナルのプログラミング言語の処理系 (in F#)

## 言語の概要

- Rust 風の構文を採用した、TypeScript にコンパイルする静的型付け言語
- 判別共用体、レコード、パターンマッチ
- Rust の `trait` , `impl` を参考にしたメソッド定義と多相性の実現
    - メモリ管理は JavaScript ランタイムにおけるガベージコレクタに任せるので、所有権や借用チェッカーの概念はない
- `Option<T>` , `Result<T, E>` 等の関数型言語由来のデータ型を提供する

```
fn fact(n: int) -> int {
    if n <= 0 { 1 } else { n * fact(n - 1) }
}

console.print_int(fact(5)); // => 120
```

## リンク

- [実装中に書き残したメモ](implementation_note.md)

## 参考にしている言語

- Elm
- F#
- OCaml
- Rust
- TypeScript
