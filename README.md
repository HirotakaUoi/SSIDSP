SSIDSP
======

**Super Simple Introduction to the Denotational Semantics of Programs**

プログラムの表示的意味論（denotational semantics）を入門的に体験するための、小さな手続き型言語とその処理系です（Erlang 製、2013 年）。
プログラムの断片（式・文）を「環境（変数名 → 値）と入出力の状態を変換する関数」として定義し、それを合成してプログラム全体の意味を求めます。

## 仕組み

1. **字句解析** — Erlang 標準の `erl_scan` を使います。そのため演算子などの書き方は Erlang 風です（`=<`、`=/=`、`rem`、`%` でコメント、プログラムの終わりは `.`）。
2. **構文解析** — `grammerTest.yrl` から yecc で生成したパーサ（`grammerTest.erl`）で構文木を作ります。
3. **意味の計算** — `ssidsp*.erl` が構文木を評価します。`ssidsp1.erl` から `ssidsp8_1.erl` まで、段階的に機能を足した版を残しています。

後半の版では、スレッド（`thread` 文・`wait` 文）と実行キューのスケジューリングを模擬しています。

## 言語の例

```
{
	proc qsort(int ft, int ls) {
		int piv, i, j, temp;
		if (ft < ls) {
			piv=s[ls]; i=ft; j=ls-1;
			while (1) {
				while ( (i<ls) and (s[i]<piv) ) i=i+1;
				while ( (j>=ft) and (s[j]>piv) ) j=j-1;
				if (i>=j) break;
				temp = s[i]; s[i]=s[j]; s[j]=temp;
				i=i+1; j=j-1
			};
			temp = s[i]; s[i]=s[ls]; s[ls]=temp;
			thread qsort(ft, i-1);
			thread qsort(i+1, ls);
		}
	};
	int n=read();
	int s[n];
	for(int k=0; k<n; k=k+1) s[k] = random(1000);
	write(s[]);
	thread qsort(0,n-1);
	wait;
	write(s[]);
}
.
```

（`test.ssidsp` を短くしたもの）

主な構文: `int` の変数・配列の宣言、`proc`（手続き）、`if` / `elseif` / `else`、`while`、`for`、`switch` / `case` / `default`、`break`、`return`、`thread`、`wait`、`trace`。
組み込み: `read()`（入力リストから 1 つ読む）、`write(...)`（出力リストに書く）、`print(...)`（画面に表示）、`random(n)`、`printStatus()`。

## 実行方法

Erlang/OTP が必要です（OTP 24 で動作を確認。`erlang:now/0` と `random` モジュールについて非推奨の警告が出ます）。

```bash
erlc grammerTest.erl compTest.erl ssidsp8_1.erl
erl
```

```erlang
1> compTest:semInputByFileNamed().
FileName> test.ssidsp
Input List> [8].
```

入力リストは Erlang の項として `[8].` のように末尾に `.` を付けて入力します。結果として出力リストが表示されます。

- `compTest:semInputByTerminal()` — プログラムを 1 行で直接入力する
- `compTest:semInputByFileNamedRecompile()` — 意味の計算のモジュールを再コンパイルしてから実行する
- `compTest:inputByFileNamedReparse()` — `grammerTest.yrl` からパーサを作り直し、構文木だけを表示する
- 実行のログは `ssidsp.log` に書かれます。

使う意味の計算の版は `compTest.erl` の `semFilename()`（既定は `ssidsp8_1`）で切り替えます。
現在の版で動作を確認したサンプルは `test.ssidsp`（スレッドを使うクイックソート）と `nQueen4.ssidsp`（N-Queen）です。ほかのサンプルは古い版の文法向けのため、そのままでは動かないものがあります。

## ファイル

| ファイル | 内容 |
|---|---|
| `grammerTest.yrl` | 文法の定義（yecc） |
| `grammerTest.erl` | 上から生成したパーサ |
| `compTest.erl` | 字句解析・構文解析・実行をまとめて呼び出すドライバ |
| `ssidsp1.erl` 〜 `ssidsp8_1.erl`、`ssidsp_7_4.erl` | 意味の計算の各版 |
| `*.ssidsp` | サンプルプログラム（バブル・挿入・選択・クイックソート、N-Queen など） |
| `compTempTest.erl`、`listTest.erl`、`double.erl`、`ctemplate.erl` | 実験用のコード |
| `*.sublime-project`、`*.sublime-workspace` | Sublime Text の設定 |

関連: [NewSSIDSP](https://github.com/HirotakaUoi/NewSSIDSP)（同じ処理系の `ssidsp5_2` までの版を収めた別のリポジトリ）

## 背景のメモ

いわゆる「原始的」ソフトウエア
- キーボードから入力して、画面に「文字で」出力する
- ビットマップでも「描画命令を出力」と考えればよい

手続き型プログラム
- 実行が逐次的に行われる
- 条件判断、繰り返しがある
- GOTO（無条件分岐）…も？

表示的意味論
- プログラム（の断片）を「意味を表す」関数に変換する
- どんな関数か？
  - 環境（Environment）・文脈（Context）と呼ばれるもの
  - 変数（名）から その値（未定義も含む）への関数
- ボトムアップに変換する
  - 式、文といった断片から全体を合成する
