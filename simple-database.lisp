(defvar *db* nil)


;; input
(defun add-record (cd) (push cd *db*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; test input
;(add-record (make-cd "The Bends" "Radiohead" 6 nil))
;(add-record (make-cd "In Rainbows" "Radiohead" 10 1))
;(add-record (make-cd "The King of Limbs" "Radiohead" 9 nil))
;(add-record (make-cd "Nonagon Infinity" "King Gizzard and the Lizard Wizard" 10 t))
;(add-record (make-cd "RTJ4" "Run The Jewels" 9 t))

;; saving and loading
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; querying database
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-comparison-expr (field value)
  `(equal (getf row ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (row) (and ,@(make-comparison-list clauses))))

;; updating database
(defun make-setter-expr (field value)
  (if value `(setf (getf row ,field) ,value) value))

(defun make-setter-list (fields)
  (loop while fields collecting (make-setter-expr (pop fields) (pop fields))))

(defmacro update-row (selector-fn &rest clauses)
  `#'(lambda (row)
       (when (funcall ,selector-fn row)
         ,@(make-setter-list clauses))
       row))

;; Some how, through using two macros and expanding the clauses list, this works...
(defmacro update (selector-fn &rest clauses)
  `(setf *db* (mapcar (update-row ,selector-fn ,@clauses)
                      *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))




