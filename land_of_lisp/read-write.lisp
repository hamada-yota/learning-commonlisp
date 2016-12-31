(let ((animal-noises '((dog . woof) (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

 (with-open-file (my-stream "animal-noises.txt" :direction :input)
   (read my-stream))
