(load "load")

(define (main)
  (init-layout!
   (lambda (x y)
     (let ((val (cell-val (get-cell (list x y)))))
       (if val (number->string val) "")))
   (lambda (x y)
     (let* ((cell (get-cell (list x y)))
	    (func (cell-func cell))
	    (inputs (cell-inputs cell))
	    (val (cell-val cell)))
       (if func
	   (string-append "="
			  (with-output-to-string
			    (lambda ()
			      (write (cons func inputs)))))
	   (if val (number->string val) ""))))
   (lambda (x y val)
     (let ((cell (get-cell (list x y))))
       (if (eq? (string-ref val 0) #\=)
	   (let* ((parsed-val
		   (read (open-input-string (string-tail val 1))))
		  (func (car parsed-val))
		  (inputs (cdr parsed-val)))
	     (set-cell-func! cell func inputs))
	   (set-cell-val! cell (eval (read (open-input-string val))
				     (nearest-repl/environment))))))
   (lambda (cmd) (pp (eval cmd (nearest-repl/environment)))))
  (init-graphics!)
  (run-graphics))

(main)
