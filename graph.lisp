(defmacro fn (name args &rest statements)
    `(defun ,name (,@args) ,@statements))

(fn graph (nodes)
    (defparameter *graph* (make-hash-table :test 'equal))
    (loop for n in nodes do
        (setf (gethash (nth 0 n) *graph*) (append (gethash (nth 0 n) *graph*) (list (nth 1 n))))
        (setf (gethash (nth 1 n) *graph*) (append (gethash (nth 1 n) *graph*) (list (nth 0 n)))))
    *graph*)

(fn connections (func query g &optional itself)
    ;; (if explain
    ;;     (princ (concatenate 'string "Retrieving " func " connections for " query '(#\Newline)))
    ;;     (princ (concatenate 'string "The following nodes are connected to " query '(#\Newline))))
    (cond 
        ((string= func "direct") (let ((result (gethash query g))) (loop for n in result collect (if (not (string= n itself)) n))))
        ((string= func "2nd-order") (loop for c in (gethash query g) collect (list c (connections "direct" c g query))))))

(fn isconnected (x y g)
    (let ((result (connections "direct" x g))) (loop for x in result do (if (string= x y) (return "yes")))))

(print
    ;; (connections "3rd-order" "https://jamesg.web.test"
    (isconnected "https://jamesg.blog" "https://jamesg.coffee"
        (graph
            (list
                (list "https://jamesg.blog" "https://jamesg.coffee")
                (list "https://jamesg.blog" "https://jamesg.web.test")
                (list "https://jamesg.web.test" "https://jamesg.tea")
                (list "https://jamesg.tea" "https://jamesg.test")
                (list "https://jamesg.tea" "https://jamesg.web.test")))))

;; (load "graph.lisp")