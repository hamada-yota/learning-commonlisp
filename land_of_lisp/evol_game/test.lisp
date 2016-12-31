
(print (cons 10 100))
(print (cons 10 (cons 100 nil)))
(print '(10 100))

(let ((x (cons 1 5))
      (hash (make-hash-table :test #'equal)))
  (setf (gethash (cons 1 5) hash) "hash-test")
  (if (gethash x hash) (print "it's t") (print "it's nil"))
  (print (gethash x hash))
  (print hash))

(defun calc-sum (m)
  (let ((s))
    (labels ((f (r)
                  (let ((rn (- r 1)))
                    (if (< rn 0)
                      0
                      (+ r (f rn))))))
      (f m))))
(print (calc-sum 10))

