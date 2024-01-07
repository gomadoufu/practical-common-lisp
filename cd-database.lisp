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

(defun make-cd
    (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record 
    (cd) 
    (push cd *db*))

(defun dump-db 
    () 
    (dolist 
        (cd *db*) 
        (format t "~{~a:~10t~a~%~}~%" cd)))


;; 値を順々に入力してもらいながら、新たなCDのレコードを生成する
(defun add-cds 
    () 
    (loop 
        (add-record 
            (prompt-for-cd))
        (if 
            (not 
                (y-or-n-p "Another? [y/n]: ")) 
            (return))))

(defun prompt-for-cd 
    ()
    (make-cd 
        (prompt-read "Title")
        (prompt-read "Artist")
        (or 
            (parse-integer 
                (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped [y/n]: ")))

(defun prompt-read 
    (prompt) 
    (format *query-io* "~a: " prompt) 
    (force-output *query-io*) 
    (read-line *query-io*))

(defun save-db 
    (filename) 
    (with-open-file 
        (out filename :direction :output :if-exists :supersede) 
        (with-standard-io-syntax 
            (print *db* out))))

(defun load-db 
    (filename) 
    (with-open-file 
        (in filename) 
        (with-standard-io-syntax 
            (setf *db* 
                (read in)))))
