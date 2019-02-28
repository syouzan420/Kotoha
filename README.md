# Kotoha
今のところ このプログラムは 簡易計算機として機能してゐる  
例へば  
12 23  
と入力すると  
35  
が出力される

また  
10 3x4 2x3     
とすると     
28      
といふ結果を得る  

-5 -8 14      
であれば  
1  
が返る  

1/3 2/5  
とすると  
分數の計算がなされて  
11 / 15  
となる  

exit  
で終了する  

今のところ Kotof.hs は利用してゐないが 將來的に使ひたいと思つてゐる  

## 2019/2/13
34/2x6/3  
といつた計算でエラーが出てゐたので 修正した  

## 2019/2/14   
計算の中に succ pred といふ函數を入れ込めるやうにした  
例へば  
12 34 succ  
とすると  
57  
を得る  

## 2019/2/17
羃算を追加したのと 括弧を利用できるやうにした  
今のところ括弧の利用は限定的で  
18/(2x3/2)/5  
のやうに '/' の後でないと機能しないと思ふ

## 2019/2/17
(3x4)/(2x2)  
といつた表現も可能になつた  
様々な括弧のつけ方について まだテストが不十分なので  
不備がみつかるかも知れない  
また コードが煩雑になつてしまつたので まうすこし まとめられるやう  
考へる必要があると思ふ

## 2019/2/24
kotoha.hs と別に インラインで書いた命令を Haskellの記述に變換し  
すぐに實行する といふ シェルスクリプト kotoha.sh を書いた  
コマンドライン引數を受けとって 變換された文字列を返すConv.hs  
をコンパイルし Conv.exe を實行して 出力された文字列を  
catを使つて取り出し readで變數に讀み込んで それを  
ghc -e で實行させてゐる  
このとき モジュールや 自分の書いた函數を取り込みたかつたので  
.ghciファイルを使つて それを實現した  
例へば 今のところは  
\> 23 -32 4\*5  
と入力すると  
(+(23)) .> (+(-32)) .> (+(4\*5)) $ 0  
といふコードに變換され( .> は独自定義した函數)  
11  
が導かれる  
これを應用すれば おそらく 計算に限らず  
作つた函數を 前から順次實行していくやうなコードが作成できると思はれる

## 2019/2/25
分數 少數 が扱へるやうにした  
出力を變換することも可能だが(Func.hsのdisplayを使ふ)  
今のところは 變換しないで そのまま結果を表示するやうにした  
少數は 計算によつては 結果がおかしくなる  
例へば  
\> 3.56 4  
とすると  7.56 ではなく  
7.5600000000000005  
となる  
たしか この原因を 本のどこかで見たことがあつたので 復習してみるつもりだ

## 2019/2/28
リストとマップ操作を實現した  
括弧を使つて 複數引数をとる函數に對應してゐる  
例へば  
\[1,2,3] (map \*2)  
の結果は  
\[2,4,6]  
となる  
\[1,2,3] (map \*2) (map 4)  
であれば 最後 それぞれに 4が加へられて  
\[6,8,10]  
となる  
\[1,2,3] (map 3x5/2)  
とすると  
\[17 % 2,19 % 2,21 % 2]  
のやうに 分數の結果を得るが この結果をシェルスクリプト側で整形して  
\[17/2, 19/2, 21/2]  
などと示された方が 分かりやすいと思ふ  
今後の課題としておかう

## 2019/2/28
シェルスクリプトを改良して 分數の表記  
2 % 3  
といつたものを  
2/3  
と表示するやうにした  
また  
2/1  
のやうに 1で割るものについては  
2  
と 整数で表示されるやうにした  
途中かなり梃摺りコードも雑だと思ふが 一應實現はできた
