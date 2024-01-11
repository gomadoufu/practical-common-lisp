;; cdのデータを構築するヘルパー関数
(defun make-cd
    (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

;; cdをdbに追加するヘルパー関数
(defun add-record 
    (cd) 
    (push cd *db*))

;; dbの内容を再帰的に出力する関数
(defun dump-db 
    () 
    (dolist 
        (cd *db*) 
        (format t "~{~a:~10t~a~%~}~%" cd)))

;; レコードの初期値を入力するユーティリティ関数
(defun add-test-records 
    () 
    (add-record 
        (make-cd "Roses" "Kathy Mattea" 7 t)) 
    (add-record 
        (make-cd "Fly" "Dixie Chicks" 8 t)) 
    (add-record 
        (make-cd "Home" "Dixie Chicks" 9 t)) 
    (add-record 
        (make-cd "Greatest Hits" "Ricky Martin" 6 t)) 
    (add-record 
        (make-cd "Double Live" "Garth Brooks" 6 nil)))

;; 値を順々に入力してもらいながら、新たなCDを生成して追加する関数
(defun add-cds 
    () 
    (loop 
        (add-record 
            (prompt-for-cd))
        (if 
            (not 
                (y-or-n-p "Another? [y/n]: ")) 
            (return))))

;; add-cdsのためのインタフェース
(defun prompt-for-cd 
    ()
    (make-cd 
        (prompt-read "Title")
        (prompt-read "Artist")
        (or 
            (parse-integer 
                (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped [y/n]: ")))

;; 標準入力を読み込むヘルパー関数
(defun prompt-read 
    (prompt) 
    (format *query-io* "~a: " prompt) 
    (force-output *query-io*) 
    (read-line *query-io*))

;; dbをファイルに書き出す関数
(defun save-db 
    (filename) 
    (with-open-file 
        (out filename :direction :output :if-exists :supersede) 
        (with-standard-io-syntax 
            (print *db* out))))

;; ファイルからdbを読み出す関数
(defun load-db 
    (filename) 
    (with-open-file 
        (in filename) 
        (with-standard-io-syntax 
            (setf *db* 
                (read in)))))

;; dbからレコードを選択するクエリ
(defun select 
    (selector-fn) 
    (remove-if-not selector-fn *db*))

(defun artist-selector 
    (artist) #'
    (lambda 
        (cd) 
        (equal 
            (getf cd :artist) artist)))

(defun where 
    (&key title artist rating 
        (ripped nil ripped-p))
#'
    (lambda 
        (cd)
        (and
            (if title 
                (equal 
                    (getf cd :title) title ) t)
            (if artist 
                (equal 
                    (getf cd :artist) artist ) t)
            (if rating 
                (equal 
                    (getf cd :rating) rating ) t)
            (if ripped-p 
                (equal 
                    (getf cd :ripped) ripped ) t)
)))

;; 既存のレコードを更新する
(defun update 
    (selector-fn &key title artist rating 
        (ripped nil ripped-p)) 
    (setf *db* 
        (mapcar #'
            (lambda 
                (row) 
                (when 
                    (funcall selector-fn row) 
                    (if title 
                        (setf 
                            (getf row :title) title))
                    (if artist 
                        (setf 
                            (getf row :artist) artist))
                    (if rating 
                        (setf 
                            (getf row :rating) rating))
                    (if ripped-p 
                        (setf 
                            (getf row :ripped) ripped)))
row)*db*)))

;; 既存のレコードを削除する
(defun delete-rows 
    (selecctor-fn) 
    (setf *db* 
        (remove-if selector-fn *db*)))
