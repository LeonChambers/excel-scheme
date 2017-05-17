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
    (%make-cell coords inputs value func is-func)
    cell?
  (coords %get-coords)
  (inputs get-inputs set-inputs!)
  (value %get-value %set-value!)
  (func get-func %set-func!)
  (is-func is-func? %set-is-func))

; Default value of the default cell
(define default-value (lambda args 0))
(define default-func 0)


; Default Value for a cell when no cell is found
(define default-cell
  (begin (hash-table/put! cells (list 0 0)
			  (%make-cell (list 0 0) '() default-value default-func #f))
	 (hash-table/get cells (list 0 0) 0)))

; Returns the cell at x,y or default cell if none is found
; Note: This will not find a column cell that overlaps with a cell
; coordinates. Meaning if (1,1) is passed but only (1,y) exists then
; the default value will be returned.
(define (get-cell x y)
  (hash-table/get cells (list x y) default-cell))

(define (get-cell-coords coords)
  (get-cell (car coords) (cadr coords)))

; Adds a cell to the "sheet" with the given values then returns that cell
(define (add-cell coords inputs func)
  (hash-table/put! cells coords (%make-cell coords inputs func func #f))
  (get-cell (car coords) (cadr coords)))

; Gets the coordinates of a cell, taking not of context
(define (get-coords cell . context)
  (let ((coords (%get-coords cell)))
    (if (not (null? context))
	(convert-coords coords (car context))
	(convert-coords coords))))

(define coord-handlers '())

(define (add-coord-handler tag converter deconverter)
  (set! coord-handlers (cons (list tag converter deconverter) coord-handlers)))

(add-coord-handler 'cell
		   (lambda (coords) coords)
		   (lambda (coords context) coords))
(add-coord-handler 'row
		   (lambda (coords) (list 'x (cadr coords)))
		   (lambda (coords context)
		     (if (equal? (car coords) 'x)
			 (list (car context) (cadr coords))
			 coords)))
(add-coord-handler 'col
		   (lambda (coords) (list (car coords) 'y))
		   (lambda (coords context)
		     	(if (equal? (cadr coords) 'y)
			    (list (car coords) (cadr context))
			    coords)))

(define (handler-tag handlers)
  (caar handlers))

(define (handler-converter handlers)
  (cadar handlers))

(define (handler-deconverter handlers)
  (caddar handlers))

(define (get-dominant-cell coords)
  (define (helper handlers)
    (if (null? handlers)
	(get-cell-coords coords)
	(let* ((handler (handler-converter handlers))
	       (new-coords (handler coords))
	       (cell (get-cell-coords new-coords)))
	  (if (or (equal? cell default-cell)
		  (equal? (%get-value cell) default-value))
	      (helper (cdr handlers))
	      (begin
		cell)))))
  (helper coord-handlers))

(define (convert-coords coords . context)
  (if (null? context)
      coords
      (let ((context (car context)))
	(define (helper coords handlers)
	  (if (null? handlers)
	      coords
	      (let ((new-coords
		     ((handler-deconverter handlers) coords context)))
		(helper new-coords (cdr handlers)))))
	(let ((final-coords (helper coords coord-handlers)))
	  (if (equal? final-coords context)
	      'called-self
	      final-coords)))))

; Evaluates and returns a cells value
(define (get-value cell context)
  (let ((initial-val (%get-value cell)))
    (initial-val context)))

; Gets the value at a given coordinates
(define (get-value-coords coords)
  (let ((cell (get-dominant-cell coords)))
    (get-value cell coords)))

; This gets the cell at this point, or if the cell doesn't exits then
; it creates a cell at the given location with the default value
(define (get-or-set-cell x y)
  (let (( retrieved (hash-table/get cells (list x y) 0)))
    (if (equal? retrieved 0)
	(add-cell (list x y) '() default-value)
	retrieved)))

; Returns a list of (func . inputs) of a given cell
(define (get-innards coords)
  (let* ((cell (get-dominant-cell coords)))
    (list (get-func cell)
	  (get-inputs celll))))

; Given coordinates a function and inputs sets the given cell to have
; the given values
(define (set-innards! coords func . inputs)
  (set-cell! (car coords) (cadr coords) func inputs))


(define (%add-input! cell input-val label)
  (let ((old-inputs (get-inputs cell)))
    (if (assq label old-inputs)
	(del-assq! label old-inputs))
    (let ((new-inputs (cons(append (list label) input-val) old-inputs)))
      (hash-table/modify! cells (get-coords cell) 'default
			  (lambda (cell)
			    (set-inputs! cell new-inputs)
			    cell)))))

; Adds an input to a cell with the given label
(define (add-input! x y x1 y1 label)
  (%add-input! (get-cell x y) (list x1 y1) label))

; Iterates through inputs and adds them to the cell
(define (add-inputs! cell inputs)
  (if (null? inputs)
      '()
      (let ((label (caar inputs))
	    (input (cdar inputs))
	    (rest (cdr inputs)))
	(%add-input! cell input label)
	(cons label (add-inputs! cell rest)))))
; Sets the cell at x y to have the given value
(define (set-cell! x y value . args)
  (if (procedure? value)
      (set-cell-func! x y value args)
      (set-cell-val! x y value)))


; Sets the value of a cell directly to the value given
(define (set-cell-val! x y value)
  (let ((cell (get-or-set-cell x y)))
    (%set-is-func cell #f)
    (%set-value! cell (lambda args value))
    (%set-func! cell value)))

; Sets a cells value with a shorter name
(define (sv! x y value)
  (set-cell! x y value))

(define (set-cell-func! x y func args)
  (let* ((cell (get-or-set-cell x y))
	 (arg-labels (add-inputs! cell args))
	 (inputs (get-inputs cell))
	 (arg-inputs (get-subset arg-labels inputs)))
    (let ((new-func (lambda (context)
		      (let* ((input-coords (map cdr arg-inputs))
			     (input-values (map get-value-coords
						input-coords)))
			 (let* ((environment (extend-top-level-environment
					      (nearest-repl/environment)
					      arg-labels
					      input-values)))
			   (eval func environment))))))
      (%set-is-func cell #t)
      (%set-value! cell new-func)
      (%set-func! cell func))))

(define plus-str (read (open-input-string "(lambda args (apply + args))")))

;(make-n-by-n 7)
;(set-lambda! 'x 1 plus-str '((a x 2)))
;(add-input! 1 1 1 2 'A)
;(add-input! 1 1 1 3 'B)
;(add-input! 1 1 1 4 'C)
