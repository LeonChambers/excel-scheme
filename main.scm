(load "load")

(define (main)
  (init-layout!
   (lambda (x y)
     (let ((val (get-value-coords (list x y))))
       (if val (number->string val) "")))
   (lambda (x y)
     (let* ((cell (get-dominant-cell (list x y)))
	    (func (get-func cell))
	    (inputs (get-inputs cell))
	    (val (get-value-coords (list x y))))
       (if (is-func? cell)
	   (string-append "="
			  (with-output-to-string
			    (lambda ()
			      (write (cons func inputs)))))
	   (if val (number->string val) ""))))
   (lambda (x y val)
     (if (eq? (string-ref val 0) #\=)
	 (let* ((parsed-val
		 (read (open-input-string (string-tail val 1))))
		(func (car parsed-val))
		(inputs (cdr parsed-val)))
	   (set-cell-func! x y func inputs))
	 (begin
	   (set-cell-val! x y (eval (read (open-input-string val))
				      (nearest-repl/environment))))))
   (lambda (cmd) (pp (eval cmd (nearest-repl/environment)))))
  (init-graphics!)  (run-graphics))
(main)


