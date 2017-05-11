; given a file returns the text of the file
(define (read-lines . args)
  (let ((p (cond ((null? args) (current-input-port))
                 ((port? (car args)) (car args))
                 ((string? (car args)) (open-input-file (car args)))
                 (else (error 'read-lines "bad argument")))))
    (let loop ((line (read-line p)) (lines (list)))
      (if (eof-object? line)
          (begin (if (and (pair? args) (string? (car args)))
                   (close-input-port p))
                 (reverse lines))
          (loop (read-line p) (cons line lines))))))

; Converts a file into a list of lists (rows)
(define (read-file file-name)
  (let ((text (read-lines file-name)))
    (define (helper txt lines)
      (if (null? txt)
	  lines
	  (let ((line (car txt))
		(rest (cdr txt)))
	    (helper rest (append lines `(,(read-next-line txt)))))))
    (helper text '())))

; Reads one line of the import text
(define (read-next-line text)
  (let ((text-lst (string->list (car text))))
    (define (helper unprocessed being-processed processed)
      (if (null? unprocessed)
	  (append processed `(,(list->string being-processed)))
	  (let ((next-element (car unprocessed))
		(rest (cdr unprocessed)))
	    (if (equal? next-element #\,)
		(helper rest
			'()
			(append processed `(,(list->string being-processed))))
		(helper rest
			(cons next-element being-processed)
			processed)))))
    (map string->number (helper text-lst '() '()))))

; Iterates through the list of lists
(define (read-the-cols y cols)
  (if (not (null? cols))
      (let* ((row-val (car cols)))
	(define (read-row x row)
	  (if (not (null? row))
	      (begin
		(add-cell (cons x y)
			  '()
			  (lambda args (car row)))
		(pp read-row)
		(read-row (+ x 1) (cdr row)))))
	(read-row 1 row-val)
	(read-the-cols (+ y 1) (cdr cols)))))

; Builds sheet from the list of lists
(define (make-board-from-file file-name)
  (let ((lines (read-file file-name)))
      (if (null? lines)
	  'done
	  (read-the-cols 1 lines)))
  (print-n-by-n 6))
		    
    

		  




