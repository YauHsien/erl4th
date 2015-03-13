# erl4th
Forth in Erlang

運用 Erlang 語言的特性，讓 Forth 語言可以運作。

要編譯 Forth ，一開始只要先把輸入的程式用空格分開，就已經是後序式了，而後序式也是一種二元樹或多元樹。然後， Forth 程式區分為三種符號：文字、值、區隔符號，所以做 lexical analysis 時，應該要這樣做：

* 文字應該要表示為〈文字，參數數目〉，因為文字是運算符號，有相對的參數，而參數數目表示為負整數。例如〈my-emit, -1〉，相當於在函數語言中有個函數名叫 my-emit/1 。
* 值應該要表示為〈值，消化參數數目〉。值的存在，可能之後會被運算符號消化掉，所以需要定義「消化參數數目」，去跟〈文字，參數數目〉做加總抵消。於是，如果有一個運算符號的「參數數目」沒有抵消為 0 ，就可以印出那個運算符號是 stack underflow error 。
* 區隔符號只能照樣存在 lexical analysis 的結果，因為之後做 semantic analysis 要用。

Syntax analysis 就不必了，因為 Forth 程式經 lexical analysis 之後，得到一串東西，就是語法樹。

至於 semantic analysis 的任務，是把 Forth 程式整理成一份字典和一個運算用的 stack 。字典中的每一筆，是一個文字定義，而且定義好的文字其實就是函數，例如 : star 42 emit ; 是函數 start/0 。在這一階段，要把每一筆文字的參數數目算出來，所以一筆文字表達成 (key, value) ，則是（〈star, 0〉，[42, emit]），本來 lexem value 是 [〈42, 1〉, 〈emit, -1〉] ，後來經由 semantic analysis 處理之後，要把第二個數字消掉，變成 [42, emit] ，之後可以直接拿去算。