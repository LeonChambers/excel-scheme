#|

This file defines the basic cell structure of the excel system.

Cells are the atomic unit of the propagator network. These cells know

their coordinates, which could be defined to allow for rows or

columns, inputs and value. The cells also have knowledge of their

"func" which is only used to display the function that is used to

extract value, while eliding the aspects that ensure inputs are

calculated properly.


|# 
(define cells (make-equal-hash-table)) 


(define current-n 7)
(define-record-type <cell>
    (%make-cell coords inputs value func)
    cell?
  (coords %get-coords)
  (inputs get-inputs set-inputs!)
  (func get-func %set-func!)
  (value %get-value %set-value!))

; Default value of the default cell
(define default-value (lambda args 0))

; Default Value for a cell when no cell is found
(define default-cell
  (begin (hash-table/put! cells (cons 0 0)
			  (%make-cell (cons 0 0) '() default-value default-value))
	 (hash-table/get cells (cons 0 0) 0)))

; Returns the cell at x,y or default cell if none is found
; Note: This will not find a column cell that overlaps with a cell
; coordinates. Meaning if (1,1) is passed but only (1,y) exists then
; the default value will be returned.
(define (get-cell x y)
  (hash-table/get cells (cons x y) default-cell))

(define (get-cell-coords coords)
  (get-cell (car coords) (cdr coords)))

; Adds a cell to the "sheet" with the given values then returns that cell
(define (add-cell coords inputs func)
  (hash-table/put! cells coords (%make-cell coords inputs func func))
  (get-cell (car coords) (cdr coords)))

; Gets the coordinates of a cell, taking not of context
(define (get-coords cell . context)
  (let ((coords (%get-coords cell)))
    (if (not (null? context))
	(convert-coords coords (car context))
	(convert-coords coords))))

(define coord-handlers '())

(define (add-coord-handler tag converter deconverter)
  (cons (list tag converter deconverter) coord-handlers))

(add-coord-handler 'cell
		   (lambda (coords) coords)
		   (lambda (coords context) coords))
(add-coord-handler 'row
		   (lambda (coords) (cons 'x (cdr coords)))
		   (lambda (coords context)
		     (if (equal? (car coords) 'x)
			 (cons (car context) (cdr coords))
			 coords)))
(add-coord-handler 'col
		   (lambda (coords) (cons (car coords) 'y))
		   (lambda (coords context)
		     	(if (equal? (cdr coords) 'y)
			    (cons (car coords) (cdr context))
			    coords)))

(define (handler-tag handlers)
  (caar handlers))

(define (handler-converter handlers)
  (cadar handlers))

(define (handler-deconverter handlers)
  (caddar handlers))

(define (%get-dominant-cell coords)
  (define (helper handlers)
    (if (null? handlers)
	(get-cell-coords coords)
	(let* ((handler (handler-converter handlers))
	       (new-coords (handler coords))
	       (cell (get-cell-coords new-coords)))
	  (if (or (equal? cell default-cell)
		  (equal? (%get-value cell) default-value))
	      (helper coords (cdr helpers))
	      cell))))
  (helper coord-handlers))

(define (convert-coords coords . context)
  (if (null? context)
      coords
      (let ((context (car context)))
	(define (helper coords handlers)
	  (if (null? handlers)
	      coords
	      (let ((new-coords
		     ((helper-deconverter handlers) coords context)))
		(helper new-coords (cdr handlers)))))
	(let ((final-coords (helper coords coord-handler)))
	  (if (equal? finalcoords context)
	      'called-self
	      final-coords)))))

; Evaluates and returns a cells value
(define (get-value cell context)
  (let ((initial-val (%get-value cell)))
    (initial-val context)))

; Gets the value at a given coordinates
(define (get-value-coords coords)
    (let ((cell (%get-dominant-cell coords)))
    (get-value cell coords)))

; This gets the cell at this point, or if the cell doesn't exits then
; it creates a cell at the given location with the default value
(define (get-or-set-cell x y)
  (let (( retrieved (hash-table/get cells (cons x y) 0)))
    (if (equal? retrieved 0)
	(add-cell (cons x y) '() default-value)
	retrieved)))

; Returns a list of (func . inputs) of a given cell
(define (get-innards coords)
  (let ((cell (%get-dominant-cell coords)))
    (cons (get-func cell)
	  (get-inputs cell))))

; Given coordinates a function and inputs sets the given cell to have
; the given values
(define (set-innards! coords func . inputs)
  (set-cell! (car coords) (cdr coords) func inputs))


(define (%add-input! cell input-val label)
  (let ((old-inputs (get-inputs cell)))
    (if (assq label old-inputs)
	(del-assq! label old-inputs))
    (let ((new-inputs (cons (cons label input-val) old-inputs)))
      (hash-table/modify! cells
			  (get-coords cell)
			  'default
			  (lambda (cell)
			    (set-inputs! cell new-inputs)
			    cell)))))

; Adds an input to a cell with the given label
(define (add-input! x y x1 y1 label)
  (%add-input! (get-cell x y) (cons x1 y1) label)
  (print-n-by-n current-n))

; Iterates through inputs and adds them to the cell
(define (add-inputs! cell inputs)
  (if (null? inputs)
      '()
      (let ((label (car inputs))
	    (input (cons (cadr inputs) (caddr inputs)))
	    (rest (cdddr inputs)))
	(%add-input! cell input label)
	(cons label (add-inputs! cell rest)))))

; Sets the cell at x y to have the given value
(define (set-cell! x y value . args)
  (if (procedure? value)
      (set-lambda! x y value args)
      (set-value! x y value))
  (print-n-by-n current-n))


; Sets the value of a cell directly to the value given
(define (set-value! x y value)
  (let ((cell (get-or-set-cell x y)))
    (%set-value! cell (lambda args value))))

; Sets a cells value with a shorter name
(define (sv! x y value)
  (set-cell! x y value))

; Sets a cell at x y to have the function func with the arguments args
(define (set-lambda! x y func args)
  (let* ((cell (get-or-set-cell x y))
	 (arg-inputs (get-inputs cell)))
    (if (not (null? args))
	(let* ((args (car args)) 
	       (arg-labels (add-inputs! cell args))
	       (inputs (get-inputs cell)))
	  (set! arg-inputs (get-subset arg-labels inputs))))
    (let (( new-func (lambda (context)
		       (if (and (= 0 (length arg-inputs)) (not (null? args)))
			   0
			   (let ((cell (get-cell x y))) 
			     (if (null? args)
			       (set! arg-inputs (get-inputs cell)))
			     (let* ((input-coords (map cdr arg-inputs))
				  (convert-coords-context
				   (lambda (coords)
				     (convert-coords coords context)))
				  
				  (input-coords-final (map convert-coords-context
						     input-coords))
				  (input-values (map get-value-coords
						     input-coords-final)))
			     (apply func input-values)))))))
      (%set-value! cell new-func)
      (%set-func! cell func))))
  

(make-n-by-n 7)
(set-cell! 'x 1 + '(a x 2))
;(add-input! 1 1 1 2 'A)
;(add-input! 1 1 1 3 'B)
;(add-input! 1 1 1 4 'C)
