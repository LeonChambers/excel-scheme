(define (sequence start end lst)
  (if (= start end)
      (cons end lst)
      (sequence start (- end 1) (cons end lst))))

(define (get-subset keys alist)
  (if (null? keys)
      '()

      (let ((key (car keys)))
	(cons (assq key alist) (get-subset (cdr keys) alist)))))


