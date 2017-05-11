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

(define-record-type <cell>
  (%make-cell coords)
  cell?
  (coords cell-coords)
  (inputs cell-inputs %set-cell-inputs!)
  (func cell-func %set-cell-func!)
  (val %cell-val %set-cell-val!)
  (version %cell-version %set-cell-version!))

;; Returns the cell with coordinated coords. If there is no cell with
;; those coordinates, it creates one and returns it
(define (get-cell coords)
  (let ((cell (hash-table/get cells coords #f)))
    (if (not cell)
	(let ((new-cell (%make-cell coords)))
	  (hash-table/put! cells coords cell)
	  new-cell)
	cell)))

(define (cell-val cell)
  (update-cell! cell)
  (%cell-val (get-cell (cell-coords cell))))

(define last-version-number 1)

(define (version-number)
  (let ((prev-val last-version-number))
    (set! last-version-number (+ prev-val 1))
    prev-val))

(let ((curr-num 0))
  (define (version-number)
    (set! curr-num (+ curr-num 1))
    curr-num))

(define (set-cell-val! cell val)
  (%set-cell-val! cell val)
  (%set-cell-func! cell #f)
  (%set-cell-inputs! cell #f)
  (%set-cell-version! cell (version-number))
  (hash-table/put! cells (cell-coords cell) cell))

(define (set-cell-func! cell func inputs)
  (%set-cell-val! cell #f)
  (%set-cell-func! cell func)
  (%set-cell-inputs! cell inputs)
  (%set-cell-version! cell 0)
  (hash-table/put! cells (cell-coords cell) cell))

;; If this cell doesn't have a function, does nothing. Otherwise,
;; recurses on all input cells, and then if necessary, recomputes the
;; value of this cell based on the new values of the input cells
(define (update-cell! cell)
  (let ((func (cell-func cell)))
    ;; If this is a static value, we don't have to do anything
    (if func
	(let* ((version (%cell-version cell))
	       (inputs (cell-inputs cell))
	       (input-cells (map get-cell (map cdr inputs)))
	       (max-input-version
		(reduce-left max 1 (map %cell-version input-cells))))
	  ;; Recurse on all input cells
	  (map update-cell! input-cells)
	  ;; If one of the inputs have been changed since the last
	  ;; time this cell was updated, we have to recompute the
	  ;; value of this cell
	  (if (> max-input-version version)
	      ;; Recompute the value of this cell
	      (let* ((environment (extend-top-level-environment
				   (nearest-repl/environment)
				   (map car inputs)
				   (map %cell-val input-cells)))
		     (new-val (eval func environment)))
		(%set-cell-val! cell new-val)
		(%set-cell-version! cell max-input-version)
		(hash-table/put! cells (cell-coords cell) cell)))))))
