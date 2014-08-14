# コマンドラインMusic boxもどき

偽のコマンドラインmusic-boxを実装したF# 2.0のコードを書いてください。
fslex, fsyacc, FParsecを除いて、外部ライブラリの使用は控えてください。

このmusic-boxにおいては、複数のアルバムがテキストファイルに格納されているものとします。
それはjson, xml, S式のようなものでフォーマットされているとします。

曲を保管するために次の型を使ってください:

```fsharp
type Album = { Name: string; Year: int }
type Song = { Artist: string; Song: string; TimeInSec: int; Album: Album }
```

また、アルバムファイル内の曲をどのように選択/再生するのかを定義したプレイリストも、テキストファイルに格納されているものとします。

プレイリストの中身は次のようなものです:

* テキストファイルには一連のselect命令があり、1つのselect命令は以下を指示します:
    * Song/Album型の属性を用いて曲/アルバム曲を展開します。"<", ">", "=" 演算子を最低でもサポートしてください。
    * order/shuffle: Song/Album型の属性で曲を並べ替え/シャッフルします。
    * top X: 選択された結果から上位Xを選択します。

次はselect命令の例です:

```
select by Name = "Kirinji", Year > 2010 order Album, TimeInSec top 5
```

music-boxは次のコマンドをサポートします:

* --init [file]: ファイルに含まれたアルバムでmusic-boxを初期化します
* --load [file]: ファイルで定義されたプレイリストを読み込んで評価します
* --play:        再生される順番で、曲を印字します

以下は実行例です:

```
$ music-box --init albums
$ music-box --load my-favorite-aor
$ music-box --play

1. Daikanyama Eregy (6:04) - Kirinji (OMNIBUS 2002)
2. Ougon no Fune (4:20) - Kirinji (Ten 2013) 
3. I.G.Y. (6:03) - Donald Fagen (The Nightfly 1982)

$
```
