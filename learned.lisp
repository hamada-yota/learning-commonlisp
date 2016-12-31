 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 式
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; (演算子 被演算子, ...)
 
 ; lispは前置記法
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 変数の種類
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; 文字列
 "afoo"
 ; ahoo
 
 ; シンボル
 'A
 ; A
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; インクリメント、デクリメント
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((x 3)) (incf x))
 (let ((x 3)) (decf x))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 関数型プログラミングスタイル
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 関数は引数か関数内で宣言された変数しか参照せず、
 値を返す以外の動作をしない
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; グローバル変数
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defparameter *x* 1)
 *x*
 ; 1
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ローカル変数
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let (
     (a 5) (b 6))
     (+ a b))
 ; 11
 
 ; 暗黙のprogn(複数の式を入れられる)
 (let (
     (a 5) (b 6))
     (+ a b)
     (- a b))
 ; -1
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ローカル関数
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (flet
     (
         (add1 (x) (+ x 1))
     )
     (add1 5))
 ; 6
 
 ; これはエラーとなる
 (flet
     (
         (foo (x) (+ x 1))
         (bar (x) (foo x))
     )
     (bar 5))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 同じスコープ内で他の関数を使う
 ; 再帰処理を書きたい場合にもlabelsを使う
 ; labels
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (labels
     (
         (foo (x) (+ x 1))
         (bar (x) (foo x))
     )
     (bar 5))
 ; 6
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 数の種類
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; 有理数を返す
 (format t "~A~%" (/ 2 3))
 
 ; 浮動小数点を返す
 (format t "~A~%" (/ 2 3.0))
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 例外処理、エラー処理
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; define condition
 (define-condition foo () ()
   (:report (lambda (condition stream)
                               (progn
                                 (princ "Stop FOOing around, numbskull!" stream)
                                 (print condition stream)))))
 ; define condition
 (define-condition bar () ()
   (:report (lambda (condition stream)
                                 (princ "Stop Baring around!" stream))))
 
 ; test function
 (defun bad-function ()
   (error 'bar))
 
 ; try, catch
 (handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))
 
 ; finally
 (unwind-protect (/ 1 0)
   (princ "I need to say 'flubyduby' matter what"))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 文字列、リストの相互変換
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (coerce "abc" 'list)
 ;(#\a #\b #\c)
 (coerce (coerce "abc" 'list) 'string)
 ;"abc"

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; httpクエリパラメータのパーセントデコード（ASCIIコードONLY）
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun http-char (c1 c2 &optional (default #\Space))
   (let ((code (parse-integer
                 (coerce (list c1 c2) 'string)
                 :radix 16
                 :junk-allowed t)))
     (if code
       (code-char code)
       default)))
 
 (defun decode-param (s)
   (labels ((f (lst)
               (when lst
                 (case (car lst)
                   (#\% (cons (http-char (cadr lst) (caddr lst))
                              (f (cdddr lst))))
                   (otherwise (cons (car lst) (f (cdr lst))))))))
     (coerce (f (coerce s 'list)) 'string)))
 
 (decode-param "%4a=12&%43=5621")
 ;"J=12&C=5621"
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; httpクエリパラメータのパーセントデコード（日本語対応）
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun http-byte (c1 c2 &optional (default #.(char-code #\space)))
   (let ((code (parse-integer
                 (coerce (list (code-char c1) (code-char c2)) 'string)
                 :radix 16
                 :junk-allowed t)))
     (or code default)))
 (defun decode-param (s)
   (labels ((f (lst)
               (when lst
                 (case (car lst)
                   (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
                                            (f (cdddr lst))))
                   (#.(char-code #\+) (cons #.(char-code #\space) (f (cdr lst))))
                   (otherwise (cons (car lst) (f (cdr lst))))))))
     (ext:convert-string-from-bytes
       (coerce (f (coerce (ext:convert-string-to-bytes s charset:utf-8) 'list))
               'vector)
       charset:utf-8)))
 ; Break 1 [4]> (decode-param "%e6%97%a5%e6%9c%ac%e8%aa%9e")
 ; "日本語"
 ;
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; クエリパラメータをキーとバリューに分解する
 ; このコードかっっけええええええええ
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun parse-params (s)
   (let ((i1 (position #\= s))
         (i2 (position #\& s)))
     (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                           (decode-param (subseq s (1+ i1) i2)))
                     (and i2 (parse-params (subseq s (1+ i2))))))
           ((equal s "") nil)
           (t s))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; データモード
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; クォートするという
 '(expt 2 3)
 
 ; アンクォートする
 `(+ 1 2 =,(+ 1 2)!)
 ; (+ 1 2 = 3 !)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; べき乗
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (expt 2 3)
 ; 8
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 標準出力
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; 可読性の高い形で出力する
 (princ 'a)
 ; A
 
 ; 改行付き、また値の後ろに空白を出力する
 (print "a")
 ; "a"
 
 ; シンボルだけはクォートしないと変数とみなされるのでこうする
 (print 'a)
 
 ; 改行なし(１行に収まるからprin1)
 (progn (prin1 "a") (prin1 "b") (prin1 "c"))
 ; "a""b""c"
 
 ; 次に現れる文字が改行の後ろに表示されるようにする
 (fresh-line)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; クォートの略記法
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;以下は同じ意味
 (quote sekfjlsejkfl)
 'sekfjlsejkfl

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ファイル書き出し
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (with-open-file (my-stream "data.txt" :direction :output) (print "my data" my-stream))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ファイル読み出し
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (with-open-file (my-stream "data.txt" :direction :input) (read my-stream))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ファイル読み書き
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (with-open-file (my-stream "animal-noises.txt" :direction :output)
        (print animal-noises my-stream)))
 ;((DOG . WOOF) (CAT . MEOW))
 (with-open-file (my-stream "animal-noises.txt" :direction :input)
       (read my-stream))                                                   
 ;((DOG . WOOF) (CAT . MEOW))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; シンボルのリストを文字列に変換する
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (prin1-to-string '(walk upstairs))
 "(WALK UPSTAIRS)"
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 文字列を文字のリストに展開する
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (coerce "uhhouuho" 'list)
 ; (#\u #\h #\h #\o #\u #\u #\h #\o)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 文字のリストを文字列にする
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (coerce '(#\u #\h #\h #\o #\u #\u #\h #\O) 'string)
 ; "uhhouuhO"
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 文字リテラル
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; #\newline
 ; #\tab
 ; #\space
 
 (progn (princ "This sentence will be interrupted")
        (princ #\newline)
        (princ "by an annoying newline character."))
 ; This sentence will be interrupted
 ; by an annoying newline character.
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; キー読み取り(入力)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun say-hello ()
   (princ "please type your name:")
   (let ((name (read)))
     (princ "nice to meet you, ")
     (princ name)))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; コンスセル
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; nilで終わっていないと.がつく
 (cons 'foo 'bar)
 ; (FOO . BAR)
 
 (cons 'a (cons 'foo 'bar))
 ; (A FOO . BAR)
 
 (cons 'a (cons 'foo nil))
 ; (A FOO)
 
 ; nilの出力は省略される
 (cons 'bar nil)
 ; (BAR)
 
 ; リストとコンスセルは同じもの
 ; コンスセルのつながりがリストだ
 ; コンスセルは２つのポインタを持つ箱にすぎない
 '(a b c)
 (A B C)
 
 ; シンボル'aをリストにコンスする
  (cons 'a '(a b c))
 ; (A A B C)
 
 ; データモードでドットを使うとリストをかける
 '(a  (b))
 ; (A (B))
 '(a . (b))
 ; (A B) ;nilで終わっているのでドットがない
 '(a . b)
 ; (A . B)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; リスト(数珠つなぎのコンスセル)の作成、追加
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; リストを作る
 (list 'foo 'bar 'baz)
 ; (FOO BAR BAZ)
 
 ; これでも同じ
 '(foo bar baz)
 ; (FOO BAR BAZ)
 
 ; リストにコンスする
 (cons 'foo '(bar baz))
 ; (FOO BAR BAZ)
 
 ; リストを保持する変数の先頭に要素を追加する
 (let ((fruits '(orange banana)))
     (push 'apple fruits))
 ; (APPLE ORANGE BANANA)
 
 ; consでも同じことができる
 (let ((fruits '(orange banana)))
     (setf fruits (cons 'apple fruits)))
 ; (APPLE ORANGE BANANA)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 要素の取り出し
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; リストの最初
 ; カーする
 (car (cons 'foo '(bar baz)))
 ; FOO
 
 ; リストの2つめ（とそれ以降のコンスセルのつながり）
 ; = リストの最初の要素を取り除くことと同じ
 ; クダーする
 (cdr (cons 'foo '(bar baz)))
 ; (BAR BAZ)
 
 ; car cdrを同時に行う(４段階までは用意されている）
 (cdar '((peas carrots tomatoes) (pork beef chicken)))
 ; (CARROTS TOMATOES)
 
 (let (
         (tmp '((peas carrots tomatoes) (pork beef chicken) duck))
     )
     (format t "~A~%" tmp)
     (format t "~A~%" (cddr tmp))
     (format t "~A~%" (cadr tmp))
     (format t "~A~%" (caddr tmp)))
 ; ((PEAS CARROTS TOMATOES) (PORK BEEF CHICKEN) DUCK)
 ; (DUCK)
 ; (PORK BEEF CHICKEN)
 ; DUCK
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 比較
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (equal (cons 'foo 'bar) (cons 'foo 'bar))
 ; T
 
 (equal (cons 'foo "bar") (cons 'foo 'bar))
 ; NIL
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; assoc
 ; リストを連想配列（キーと値のペアをリストにしたもの = alistという）として扱い
 ; キーが一致するものを返す
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((assoc-list '((a (1 2 3))
                     (b (4 5 6))
                     (c (7 8 9)))))
     (assoc 'b assoc-list))
 ; (B (4 5 6))
 
 ; 値をとるにはこうだ
 (let ((assoc-list '((a (1 2 3))
                     (b (4 5 6))
                     (c (7 8 9)))))
     (cadr (assoc 'b assoc-list)))
 ; ((4 5 6)) リストの中にはいったままか
 
 ; では値だけをとるにはこうだ
 (let ((assoc-list '((a (1 2 3))
                     (b (4 5 6))
                     (c (7 8 9)))))
     (cadr (assoc 'b assoc-list)))
 ; (4 5 6)
 
 ; キーが重複していたら最初に一致しているものを返す
 (let ((assoc-list '((a (1 2 3))
                     (a (4 5 6))
                     (c (7 8 9)))))
     (assoc 'a assoc-list))
 ; (A (1 2 3))
 
 ; だから、pushで先頭に要素を追加し、assocで要素を
 ; 取得することによって履歴の保持を兼ねることができる
 ; これはよく使われるテクニックらしい
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 関数ポインタ
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (function car)
 ; #<SYSTEM-FUNCTION CAR>
 
 ; 略記できる
 #'car
 ; #<SYSTEM-FUNCTION CAR>
 
 (mapcar #'sqrt (mapcar #'car '((1 2)(3 4))))
 ;(1 1.7320508)
             
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; mapcar
 ; 関数を適用する
 ;
 ; 他の関数を引数として受け取る関数を 高階関数 という
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((tmp-list '(1 2 3)))
     (mapcar #'sqrt tmp-list))
 ; (1 1.4142135 1.7320508)
 
 ; これは便利
 (let ((tmp-list '((1 2 3)
                   (4 5 6)
                   (7 8 9)
                   )))
     (mapcar #'car tmp-list))
 ; (1 4 7)
 
 (let ((tmp-list '((1 2 3)
                   (4 5 6)
                   (7 8 9)
                   )))
     (mapcar #'cdr tmp-list))
 ; ((2 3) (5 6) (8 9))
 
 ; mapcarの例
 (let ((tmp '(facebook google apple twitter microsoft)))
   (flet ((appreciate (x)
                      (format t "~A is the gratest company in the world !~%" x)))
     (mapcar #'appreciate tmp)))
 ; FACEBOOK is the gratest company in the world !
 ; GOOGLE is the gratest company in the world !
 ; APPLE is the gratest company in the world !
 ; TWITTER is the gratest company in the world !
 ; MICROSOFT is the gratest company in the world !
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; apply
 ; 引数を全て渡す
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((tmp-list '((1 2 3) (4 5 6))))
     (apply #'append tmp-list))
 ;(1 2 3 4 5 6)
 
 (append '(1 2 3) '(4 5 6))
 ;(1 2 3 4 5 6)
 
 (let ((tmp-list '(1 2 3)))
     (apply #'+ tmp-list))
 ;6
 ;(+ 1 2 3)と同じ
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 標準出力に改行を出力する
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (terpri)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 参考(パスカルの三角形)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun pascal (n)
   (let ((buff (make-array (1+ n) :initial-element 0)))
     (setf (aref buff 1) 1)
     (dotimes (i n)
       (do ((j (1+ i) (1- j)))
           ((zerop j))
         (format t "  ~3D" (setf (aref buff j)
                                 (+ (aref buff j) (aref buff (1- j))))))
       (terpri))))
 (pascal 10)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 配列
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((buff (make-array 3 :initial-element 0)))
   (dotimes (i 3)
     (princ (setf (aref buff i) (+ i 10)))
     (terpri))
   (princ "done!")
   (terpri))
 ;10
 ;11
 ;12
 ;done!
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; リストの検索(find)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (find 'west '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)) :key #'cadr)
 ; (GARDEN WEST DOOR)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; eval
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defparameter *foo* '(+ 1 2))
 (eval *foo*)
 ; 3
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; タイマー
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((a 0))
   (loop
     (setf a (+ 1 a))
     (print a)
     (sleep 0.1)))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 文字列結合
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (concatenate 'string "a2" "a")
 ; "a2a"
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; ラムダ (lambda)
 ; 無名関数を作る
 ; 本物の関数ではなく、マクロの一部らしい
 ;
 ; Lispはラムダ算法という数学的な概念から直接
 ; 導かれたプログラミング言語である
 ; それはlambdaを唯一のコマンドとする理論的な
 ; プログラミング言語のようなものだ
 ; lambda形式はLispシステムの中で最も根源的なコマンドであり
 ; 他の関数はlambdaの概念を元に導かれている
 ;
 ; 関数を引数として受け取る関数を高階関数という。
 ; 高階関数を使うのは、高階プログラミングという
 ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (mapcar (lambda (n) (/ n 2)) '(1 2 3 4 5))
 ; (1/2 1 3/2 2 5/2)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 循環リスト
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (setf *print-circle* t)
 (defparameter foo (list 1 2 3))
 ; FOO
 (setf (cdddr foo) foo)
 ; #1=(1 2 3 . #1#)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; alist
 ; 連想配列
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defparameter *drink-order* '((bill . coffee)
                               (lisa . soda)
                               (john . tea)))
 (assoc 'lisa *drink-order*)
 ; (LISA . SODA)
 
 ; 注文を変える
 (push '(lisa milk) *drink-order*)
 (assoc 'lisa *drink-order*)
 ; (LISA . MILK)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; assoc
 ; 連想配列
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (defparameter *test* '((a . 111) (b . 222)))
 ;*TEST*
 (assoc 'a *test*)
 ;(A . 111)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; loop
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (loop named main
       for x = 0
       then (cond
              ((eq x 10)(return-from main 'bye))
              (t (setf x (+ x 1)) (princ x))))
 ;12345678910
 ;BYE

 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 木構造
 ; 通常、リストの先頭にその構造が何を
 ; 表すかを示すシンボルが置かれる
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defparameter *house* '((walls (mortar (cement)
                                        (water)
                                        (sand))
                                (bricks))
                         (windows (glass)
                                  (frame)
                                  (curtains))
                         (roof (shingles)
                               (chimney))))
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; array 配列
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (make-array 3)
 ;#(NIL NIL NIL)
 (defparameter x (make-array 3))
 ;X
 (setf (aref x 1) 'foo)
 ;FOO
 x
 ;#(NIL FOO NIL)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; hash
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((tab (make-hash-table)))
   (push 1 (gethash 'foo tab))
   (push 2 (gethash 'foo tab))
   (push 3 (gethash 'bar tab))
   tab)
 ;#S(HASH-TABLE :TEST FASTHASH-EQL (BAR . (3)) (FOO . (2 1)))
 (let ((tab (make-hash-table)))
   (setf (gethash 'foo tab) 1)
   (setf (gethash 'foo tab) 2)
   (setf (gethash 'bar tab) 3)
   tab)
 ;#S(HASH-TABLE :TEST FASTHASH-EQL (BAR . 3) (FOO . 2))
 
 ;change test function
 (print (cons 10 100))
 (print (cons 10 (cons 100 nil)))
 (print '(10 100))
 (let ((x (cons 1 5))
       (hash (make-hash-table :test #'equal)))
   (setf (gethash (cons 1 5) hash) "hash-test")
   (if (gethash x hash) (print "it's t") (print "it's nil"))
   (print (gethash x hash))
   (print hash))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 再帰 ランダムピックのサンプル
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (loop repeat 100 collect (let ((xlist '(5 9 3 2 18)))
    (let ((x (random (apply #'+ xlist))))
        (labels ((pick (y xlist) (let ((z (- y (car xlist)))) (if (< z 0) (car xlist) (pick z (cdr xlist))))))
             (pick x xlist)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 登場回数をカウントしてハッシュに突っ込むサンプル
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((x '(12 10 uho uho uho 1 8 10 29 1 8 1))
       (ret (make-hash-table :test #'eq)))
   (loop for y in x collect (setf (gethash y ret)
                                  (+ 1 (if (gethash y ret)
                                         (gethash y ret) 0))))
   (print ret))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; シーケンス関数
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (reduce (lambda (best item)
           (if (and (evenp item) (> item best))
             item
             best))
         '(7 4 5 6 2)
         :initial-value 0)
 ;6
 (reduce (lambda (best item)
           (if (and (evenp item) (> item best))
             item
             best))
         '(7 4 5 6 2))
 ;7 -> BAD RESULT!
 (reduce '+ '(1 2 3 4 5))
 ;15
 
 ;mapcarはリストにしか使えない
 (mapcar (lambda (x) (+ 10 x)) '(1 2 3 4 5))
 ;(11 12 13 14 15)
 (mapcar (lambda (x) (+ 10 x)) (make-array 5 :initial-contents '(1 2 3 4 5)))
 ;*** - MAPCAR: A proper list must not end with #(1 2 3 4 5)
 
 ;mapなら配列にも使える
 ;'listは帰り値の型を指定する
 (map 'list (lambda (x) (+ 10 x)) (make-array 5 :initial-contents '(1 2 3 4 5)))
 ;(11 12 13 14 15)
 (map 'array (lambda (x) (+ 10 x)) (make-array 5 :initial-contents '(1 2 3 4 5)))
 ;#(11 12 13 14 15)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; リストからn番目の値を得る(ジェネリックなシーケンス関数ではない）
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (nth 3 (make-array 5 :initial-contents '(1 2 3 4 5)))
 ; error
 (nth 3 (append () '(1 2 3 4 5)))
 ; 4
 (nth 3 '(1 2 3 4 5))
 ; 4
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; loop
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (loop for i in '(0 2 4 6)
       never (oddp i))
 ;T
 
 (loop for i below 5
       nconc (list 'Z i))
 ;(Z 0 Z 1 Z 2 Z 3 Z 4)
 
 (loop for i below 5
       append (list 'Z i))
 ;(Z 0 Z 1 Z 2 Z 3 Z 4)
 
 (loop for i below 5
       if (oddp i)
       do (print i)
       else
       do
       (print "w00t"))
 ;"w00t" 
 ;1 
 ;"w00t" 
 ;3 
 ;"w00t" 
 ;NIL
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 合計を計算する
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun calc-sum (m)                           
   (let ((s))
     (labels ((do-calc (r)
                   (let ((rn (- r 1)))
                     (if (< rn 0)
                       0
                       (+ r (do-calc rn))))))
       (do-calc m))))
 (print (calc-sum 10))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; 合計を計算する
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (let ((x 1)) (loop for x from 0 for q across #(1 2 3) collect (progn (print (cons x (random 199))) )))
 
 ;(0 . 175) 
 ;(1 . 49) 
 ;(2 . 158) 
 ;((0 . 175) (1 . 49) (2 . 158))
 
