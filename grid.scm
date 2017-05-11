;; A grid is a 2D array of cells that can can be drawn on a pane

;; Events triggered by grid elements

;; Should be triggered whenever a cell in a grid is selected. Events
;; of this type have 2 parameters. The first is the grid in which the
;; cell was selected. The second is the coordinates of the selected
;; cell
(define event-type:cell-selected 'cell-selected)

;; pane is the pane in which the grid is to be drawn
;; getter is a function of 2 parameters (the x and y valules of the
;; desired cell) and returns a string representing the value in that
;; cell
(define-record-type <grid>
  (%make-grid pane getter first-col first-row)
  grid?
  (pane grid-pane)
  (getter grid-getter)
  (first-col grid-first-col set-grid-first-col!)
  (first-row grid-first-row set-grid-first-row!)
  (row-heights grid-row-heights set-grid-row-heights!)
  (col-widths grid-col-widths set-grid-col-widths!)
  (selected-cell grid-selected-cell set-grid-selected-cell!))

(define (make-grid pane getter)
  (let ((grid (%make-grid pane getter 1 1)))
    (register-event-callback! event-type:draw
			      (lambda () (grid-draw grid)))
    (register-event-callback! event-type:layout-changed
			      (lambda ()
			        (grid-update-dimensions! grid)))
    (register-event-callback!
     event-type:mouse-click
     (lambda (window x y time)
       (if (pane-contains-point pane x y)
	   (begin
	     (grid-handle-click grid x y)
	     'swallow))))
    (register-event-callback!
     event-type:scroll-up
     (lambda (window x y time)
       (if (pane-contains-point pane x y)
	   (grid-scroll-up! grid))))
    (register-event-callback!
     event-type:scroll-down
     (lambda (window x y time)
       (if (pane-contains-point pane x y)
	   (grid-scroll-down! grid))))
    (register-event-callback!
     event-type:scroll-left
     (lambda (window x y time)
       (if (pane-contains-point pane x y)
	   (grid-scroll-left! grid))))
    (register-event-callback!
     event-type:scroll-right
     (lambda (window x y time)
       (if (pane-contains-point pane x y)
	   (grid-scroll-right! grid))))
    (register-event-callback!
     event-type:cell-selected
     (lambda (grid coords)
       (set-grid-selected-cell! grid coords)))
    grid))

(define (grid-scroll-up! grid)
  (set-grid-first-row! grid (+ (grid-first-row grid) 1))
  (grid-update-dimensions! grid))

(define (grid-scroll-down! grid)
  (let ((first-row (grid-first-row grid)))
    (if (> first-row 1)
	(begin
	  (set-grid-first-row! grid (- first-row 1))
	  (grid-update-dimensions! grid)))))

(define (grid-scroll-left! grid)
  (set-grid-first-col! grid (+ (grid-first-col grid) 1))
  (grid-update-dimensions! grid))

(define (grid-scroll-right! grid)
  (let ((first-col (grid-first-col grid)))
    (if (> first-col 1)
	(begin
	  (set-grid-first-col! grid (- first-col 1))
	  (grid-update-dimensions! grid)))))

(define (grid-update-row-heights! grid)
  (define (compute-row-heights height)
    (let lp ((remaining-height (- height padded-text-height)))
      (if (< remaining-height 0)
	  '()
	  (cons padded-text-height
		(lp (- remaining-height padded-text-height))))))
  (set-grid-row-heights!
   grid
   (compute-row-heights (pane-height (grid-pane grid)))))

(define (grid-update-col-widths! grid num-rows)
  (let ((width (pane-width (grid-pane grid)))
	(first-row (grid-first-row grid))
	(first-col (grid-first-col grid))
	(getter (grid-getter grid)))
    (define (compute-col-widths)
      ;; Returns the display lengths of a `num-chars`-character long
      ;; string
      (define (string-display-length num-chars)
	(+ (* 2 text-padding) (* char-width num-chars)))

      (define first-col-width
	(let* ((max-row-num (- (+ first-row num-rows) 1))
	       (longest-string (number->string max-row-num)))
	  (string-display-length (string-length longest-string))))

      (define (column-width col-num)
	(define column-string-width
	  (let lp ((max-width (string-length
			       (number->string col-num)))
		   (row-offset 0))
	    (if (= row-offset num-rows)
		max-width
		(lp (max (string-length
			  (getter col-num (+ first-row row-offset)))
			 max-width)
		    (+ row-offset 1)))))
	(string-display-length column-string-width))

      (define (partition-width remaining-width first-col)
	(if (< remaining-width 0)
	    '()
	    (let ((curr-col-width (column-width first-col)))
	      (cons curr-col-width
		    (partition-width (- remaining-width
					curr-col-width)
				     (+ first-col 1))))))

      (cons first-col-width
	    (partition-width
	     (- width first-col-width)
	     first-col)))

    (set-grid-col-widths!
     grid
     (compute-col-widths))))

(define (grid-update-dimensions! grid)
  (grid-update-row-heights! grid)
  (grid-update-col-widths! grid
			   (- (length (grid-row-heights grid)) 1)))

(define (grid-cell-at-location grid x y)
  (define (get-index x l)
    (let lp ((cumsum 0) (l l) (i 0))
      (if (null? l)
	  #f
	  (let ((new-cumsum (+ cumsum (car l))))
	    (if (and (> x cumsum) (< x new-cumsum))
		i
		(lp new-cumsum (cdr l) (+ i 1)))))))
  (let ((ix (get-index x (grid-col-widths grid)))
	(iy (get-index y (grid-row-heights grid))))
    (if (or (= ix 0) (= iy 0))
	#f ;; Header row/col
	(cons (+ (- ix 1) (grid-first-col grid))
	      (+ (- iy 1) (grid-first-row grid))))))

(define (grid-handle-click grid x y)
  (let ((clicked-cell
	 (grid-cell-at-location grid
				(- x (pane-x-left (grid-pane grid)))
				(- y (pane-y-top (grid-pane grid))))))
    (if clicked-cell
	(trigger-event (list event-type:cell-selected
			     grid
			     clicked-cell))
	(trigger-event (list event-type:cell-selected
			     grid
			     #f)))))

(define (grid-draw grid)
  (let ((pane (grid-pane grid))
	(col-widths (grid-col-widths grid))
	(row-heights (grid-row-heights grid))
	(first-row (grid-first-row grid))
	(first-col (grid-first-col grid))
	(selected-cell (grid-selected-cell grid))
	(getter (grid-getter grid)))
    (pane-draw-border pane)
    ;; Draw the header row
    (let lp ((c first-col)
	     (x-left (car col-widths))
	     (col-widths (cdr col-widths)))
      (if (not (null? col-widths))
	  (begin
	    (pane-draw-line pane x-left 0 x-left (pane-height pane))
	    (pane-draw-text pane
			    (+ x-left text-padding)
			    (- (car row-heights) text-padding)
			    (number->string c))
	    (lp (+ c 1) (+ x-left (car col-widths))
		(cdr col-widths)))))

    (define (draw-row r y-top height)
      (pane-draw-line pane 0 y-top (pane-width pane) y-top)
      (pane-draw-text pane text-padding
		      (- (+ y-top height) text-padding)
		      (number->string r))
      (let lp ((c first-col)
	       (x-left (car col-widths))
	       (col-widths (cdr col-widths)))
	(if (not (null? col-widths))
	    (begin
	      (if (and selected-cell
		       (= c (car selected-cell))
		       (= r (cdr selected-cell)))
		  (let ((x-right (+ x-left (car col-widths)))
			(y-bottom (+ y-top height)))
		    (x-graphics/set-foreground-color graphics "red")
		    (pane-draw-line pane x-left y-top x-right y-top)
		    (pane-draw-line pane x-right y-top x-right y-bottom)
		    (pane-draw-line pane x-right y-bottom x-left
				    y-bottom)
		    (pane-draw-line pane x-left y-bottom x-left
				    y-top)
		    (x-graphics/set-foreground-color graphics "black")))
	      (pane-draw-text pane
			      (+ x-left text-padding)
			      (- (+ y-top height) text-padding)
			      (getter c r))
	      (lp (+ c 1) (+ x-left (car col-widths))
		  (cdr col-widths))))))

    (let lp ((r first-row)
	     (y-top (car row-heights))
	     (row-heights (cdr row-heights)))
      (if (not (null? row-heights))
	  (begin
	    (draw-row r y-top (car row-heights))
	    (lp (+ r 1)
		(+ y-top (car row-heights))
		(cdr row-heights)))))))
