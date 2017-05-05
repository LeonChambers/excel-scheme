;; Dummy function to be replaced by actual backend
(define (get-cell-string x y)
  (string-append
   (number->string x)
   ","
   (number->string y)))


;;; Graphics setup

;; Initialize graphics device
(if (not (graphics-type-available? 'x))
    (error "X graphics not available"))
(define graphics (make-graphics-device 'x))

;; This allows X to push focus onto the graphics window
(x-graphics/set-input-hint graphics 1)

;; Redefine the graphics virtual coordinates so that the top left
;; corner of the window is (0, 0)
(graphics-set-coordinate-limits graphics 0 1 1 0)

;; Request the events we will actually use: The following are the mask
;; bits for the events we are interested in
;; #x0001 Mouse events
;; #x0004 Window resize events
;; #x0040 Key down events
(x-graphics/select-user-events graphics #x0045)

(define graphics-width-px)
(define graphics-height-px)

;; Reads the absolute dimensions of the graphics window
;; Should be called whenever the window is resized
(define (update-window-dimensions!)
  ((x-graphics/device-coordinate-limits graphics)
   (lambda (x-left y-bottom x-right y-top)
     (set! graphics-width-px (- x-right x-left))
     (set! graphics-height-px (- y-bottom y-top)))))

;; Conversions from absolute coordinates to virtual coordinates
(define (pixels-x x)
  (/ x graphics-width-px))

(define (pixels-y y)
  (/ y graphics-height-px))


;;; Pane abstraction

(define-record-type <pane>
  (make-pane x-left y-top x-right y-bottom click-handler)
  pane?
  (x-left pane-x-left set-pane-x-left!)
  (y-top pane-y-top set-pane-y-top!)
  (x-right pane-x-right set-pane-x-right!)
  (y-bottom pane-y-bottom set-pane-y-bottom!)
  (click-handler pane-click-handler))

(define (set-pane-dimensions! pane x-left y-top x-right y-bottom)
  (set-pane-x-left! pane x-left)
  (set-pane-y-top! pane y-top)
  (set-pane-x-right! pane x-right)
  (set-pane-y-bottom! pane y-bottom))

(define (pane-width pane)
  (- (pane-x-right pane) (pane-x-left pane)))

(define (pane-height pane)
  (- (pane-y-bottom pane) (pane-y-top pane)))

(define (pane-contains-point pane x y)
  (let ((x-left (pane-x-left pane))
	(x-right (pane-x-right pane))
	(y-top (pane-y-top pane))
	(y-bottom (pane-y-bottom pane)))
    (and (> x x-left)
	 (< x x-right)
	 (< y y-bottom)
	 (> y y-top))))

(define (pane-handle-click pane x y)
  ((pane-click-handler pane) x y))

(define (pane-draw-line pane x-start y-start x-end y-end)
  (graphics-draw-line graphics
		      (+ x-start (pane-x-left pane))
		      (+ y-start (pane-y-top pane))
		      (+ x-end (pane-x-left pane))
		      (+ y-end (pane-y-top pane))))

(define (pane-draw-arc pane x y radius-x radius-y angle-start
		       angle-sweep fill?)
  (x-graphics/draw-arc graphics (+ x (pane-x-left pane))
		     (+ y (pane-y-top pane)) radius-x radius-y
		     angle-start angle-sweep fill?))

(define (pane-draw-circle pane x y radius)
  (x-graphics/draw-circle graphics
			  (+ x (pane-x-left pane))
			  (+ y (pane-y-top pane))
			  radius))

(define (pane-draw-text pane x y string)
  (graphics-draw-text graphics
		      (+ x (pane-x-left pane))
		      (+ y (pane-y-top pane))
		      string))

(define (pane-draw-border pane)
  (let ((width (pane-width pane))
	(height (pane-height pane)))
    (pane-draw-line pane 0 0 width 0)
    (pane-draw-line pane width 0 width height)
    (pane-draw-line pane 0 height width height)
    (pane-draw-line pane 0 0 0 height)))


;;; System state variables

(define first-row 1)
(define first-col 1)
(define zoom-factor 1)
(define text-input-buffer "")


;;; Absolute text dimensions

(define default-text-padding-px 10)

(define default-text-height-px 18)

(define default-char-width-px 16)

(define text-padding-px)
(define (update-text-padding-px!)
  (set! text-padding-px (* zoom-factor default-text-padding-px)))

(define text-height-px)
(define (update-text-height-px!)
  (set! text-height-px (* zoom-factor default-text-height-px)))

(define char-width-px)
(define (update-char-width-px!)
  (set! char-width-px (* zoom-factor default-char-width-px)))

(define padded-text-height-px)
(define (update-padded-text-height-px!)
  (set! padded-text-height-px
	(+ text-height-px (* text-padding-px 2))))

(define (set-font-size! size)
  (x-graphics/set-font
   graphics
   (string-append
    ;; We arbitrarily chose one of the few available scalable
    ;; monospace fonts
    "-bitstream-courier 10 pitch-medium-r-normal-*-0-"
    (number->string (floor->exact (* 10 size)))
    "-0-0-m-0-ascii-0")))

(define (update-font-size!)
  (set-font-size! text-height-px))

(define (update-absolute-text-dimensions!)
  (update-text-padding-px!)
  (update-text-height-px!)
  (update-char-width-px!)
  (update-padded-text-height-px!)
  (update-font-size!))


;;; Virtual text dimensions

(define text-padding-x)

(define text-padding-y)

(define text-height)

(define padded-text-height)

(define char-width)

(define (update-text-dimensions!)
  (set! text-padding-x (pixels-x text-padding-px))
  (set! text-padding-y (pixels-y text-padding-px))
  (set! text-height (pixels-y text-height-px))
  (set! padded-text-height (pixels-y padded-text-height-px))
  (set! char-width (pixels-x char-width-px)))


;;; Grid dimensions

(define row-heights)

(define (compute-row-heights pane)
  (let lp ((remaining-height
	    (- (pane-height pane) padded-text-height)))
    (if (< remaining-height 0)
	'()
	(cons padded-text-height
	      (lp (- remaining-height padded-text-height))))))

(define (update-row-heights! pane)
  (set! row-heights (compute-row-heights pane)))

(define column-widths)

(define (compute-column-widths pane num-rows)
  ;; Returns the length of a `num-chars`-character long string
  (define (string-display-length num-chars)
    (+ (* 2 text-padding-x) (* char-width num-chars)))
  
  (define (first-column-width)
    (let* ((max-row-num (- (+ first-row num-rows) 1))
	   (num-chars (string-length (number->string max-row-num))))
      (string-display-length num-chars)))
  
  (define (column-width column-number)
    (define (column-string-width)
      (let lp ((max-width (string-length
			   (number->string column-number)))
	       (row-offset 0))
	(if (= row-offset num-rows)
	    max-width
	    (lp (max (string-length
		      (get-cell-string
		       (+ first-row row-offset)
		       column-number))
		     max-width)
		(+ row-offset 1)))))
    (string-display-length (column-string-width)))
  
  (define (partition-width remaining-width first-col)
    (if (< remaining-width 0)
	'()
	(let ((curr-column-width (column-width first-col)))
	  (cons curr-column-width
		(partition-width (- remaining-width curr-column-width)
				 (+ first-col 1))))))
  
  (let ((curr-first-column-width (first-column-width)))
    (cons curr-first-column-width
	  (partition-width
	   (- (pane-width pane) curr-first-column-width)
	   first-col))))

(define (update-column-widths! pane num-rows)
  (set! column-widths (compute-column-widths pane num-rows)))

(define (update-grid-dimensions!)
  (update-row-heights! grid-pane)
  (update-column-widths! grid-pane (- (length row-heights) 1)))

(define (cell-at-location x y)
  (define (get-index x l)
    (let lp ((cumsum 0) (l l) (i 0))
      (if (null? l)
	  #f
	  (let ((new-cumsum (+ cumsum (car l))))
	    (if (and (> x cumsum) (< x new-cumsum))
		i
		(lp new-cumsum (cdr l) (+ i 1)))))))
  (let ((ix (get-index x column-widths))
	(iy (get-index y row-heights)))
    (if (or (= ix 0) (= iy 0))
	#f ;; Header row/col
	(cons (+ (- iy 1) first-col)
	      (+ (- ix 1) first-row)))))


;;; Pane definitions

(define (grid-handle-click x y)
  ;; TODO: Handle the click
  (pp (cell-at-location (- x (pane-x-left grid-pane))
			(- y (pane-y-top grid-pane)))))

(define (zoom-in)
  (set! zoom-factor (* zoom-factor 1.1))
  (update-absolute-text-dimensions!)
  (update-text-dimensions!)
  (update-pane-layout!)
  (update-grid-dimensions!)
  (update-screen!))

(define (zoom-out)
  (set! zoom-factor (/ zoom-factor 1.1))
  (update-absolute-text-dimensions!)
  (update-text-dimensions!)
  (update-pane-layout!)
  (update-grid-dimensions!)
  (update-screen!))

(define grid-pane (make-pane 0 0.1 1 1 grid-handle-click))
(define text-input-pane (make-pane 0 0 0 0 (lambda (x y) unspecific)))
(define zoom-in-pane (make-pane 0 0 0 0
				(lambda (x y) (zoom-in))))
(define zoom-out-pane (make-pane 0 0 0 0
				 (lambda (x y) (zoom-out))))

(define (update-pane-layout!)
  (let* ((top-section-height-px
	  (* 2 (+ text-height-px text-padding-px)))
	 (top-section-height (pixels-y top-section-height-px))
	 (button-width (pixels-x top-section-height-px))
	 (out-button-start (- 1 button-width))
	 (in-button-start (- out-button-start button-width)))
    (set-pane-dimensions! grid-pane 0 top-section-height 1 1)
    (set-pane-dimensions! text-input-pane 0 0 in-button-start
			  top-section-height)
    (set-pane-dimensions! zoom-in-pane in-button-start 0
			  out-button-start top-section-height)
    (set-pane-dimensions! zoom-out-pane out-button-start 0 1
			  top-section-height)))


;;; UI drawing

(define (draw-grid)
  ;; Draw the header row
  (pane-draw-border grid-pane)
  (let lp ((c first-col)
	   (x-left (car column-widths))
	   (col-widths (cdr column-widths)))
    (if (not (null? col-widths))
	(begin
	  (pane-draw-line grid-pane x-left 0 x-left
			  (pane-height grid-pane))
	  (pane-draw-text grid-pane
			  (+ x-left text-padding-x)
			  (- (car row-heights)
			     text-padding-y)
			  (number->string c))
	  (lp (+ c 1) (+ x-left (car col-widths))
	      (cdr col-widths)))))
  
  (define (draw-row r y-top height)
    (pane-draw-line grid-pane 0 y-top (pane-width grid-pane) y-top)
    (pane-draw-text grid-pane
		    text-padding-x
		    (- (+ y-top height) text-padding-y)
		    (number->string r))
    (let lp ((c first-col)
	     (x-left (car column-widths))
	     (col-widths (cdr column-widths)))
      (if (not (null? col-widths))
	  (begin
	    (pane-draw-text grid-pane
			    (+ x-left text-padding-x)
			    (- (+ y-top height) text-padding-y)
			    (get-cell-string c r))
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
	      (cdr row-heights))))))

(define (draw-text-input)
  (pane-draw-text text-input-pane text-padding-x
		  (+ text-height text-padding-y)
		  text-input-buffer))

(define (pane-draw-magnifying-glass pane width height)
  (pane-draw-line pane (* 0.1 width) (* 0.9 height)
		  (* (- 0.9 (* 0.3 (+ 1 (/ (sqrt 2) 2)))) width)
		  (* (+ 0.1 (* 0.3 (+ 1 (/ (sqrt 2) 2)))) height))
  (pane-draw-arc pane (* 0.6 width) (* 0.4 height) (* 0.3 width)
		 (* 0.3 height) 0 360 #f))

(define (draw-zoom-in)
  (let ((width (pane-width zoom-in-pane))
	(height (pane-height zoom-in-pane)))
    (pane-draw-border zoom-in-pane)
    (pane-draw-magnifying-glass zoom-in-pane width height)
    (pane-draw-line zoom-in-pane (* 0.6 width) (* 0.2 height)
		    (* 0.6 width) (* 0.6 height))
    (pane-draw-line zoom-in-pane (* 0.4 width) (* 0.4 height)
		    (* 0.8 width) (* 0.4 height))))

(define (draw-zoom-out)
  (let ((width (pane-width zoom-out-pane))
	(height (pane-height zoom-out-pane)))
    (pane-draw-border zoom-out-pane)
    (pane-draw-magnifying-glass zoom-out-pane width height)
    (pane-draw-line zoom-out-pane (* 0.4 width) (* 0.4 height)
		    (* 0.8 width) (* 0.4 height))))

(define (update-screen!)
  (graphics-clear graphics)
  (draw-grid)
  (draw-text-input)
  (draw-zoom-in)
  (draw-zoom-out)
  (graphics-flush graphics))


;; UI Event handlers

(define (handle-click evt)
  (let ((x (vector-ref evt 2))
	(y (vector-ref evt 3)))
    (cond
     ((pane-contains-point grid-pane x y)
      (pane-handle-click grid-pane x y))
     ((pane-contains-point text-input-pane x y)
      (pane-handle-click text-input-pane x y))
     ((pane-contains-point zoom-in-pane x y)
      (pane-handle-click zoom-in-pane x y))
     ((pane-contains-point zoom-out-pane x y)
      (pane-handle-click zoom-out-pane x y))
     (else (pp evt)))))

(define (scroll-up)
  (set! first-row (+ first-row 1))
  (update-grid-dimensions!)
  (update-screen!))

(define (scroll-down)
  (if (> first-row 1)
      (begin
	(set! first-row (- first-row 1))
	(update-grid-dimensions!)
	(update-screen!))))

(define (scroll-left)
  (set! first-col (+ first-col 1))
  (update-grid-dimensions!)
  (update-screen!))

(define (scroll-right)
  (if (> first-col 1)
      (begin
	(set! first-col (- first-col 1))
	(update-grid-dimensions!)
	(update-screen!))))

(define (handle-key-press evt)
  (let ((key (vector-ref evt 2)))
    (set! text-input-buffer (string-append text-input-buffer key))
    (update-screen!)))

(define (main)
  (define (loop)
    (x-graphics/discard-events graphics)
    (let ((evt (x-graphics/read-user-event graphics)))
      (case (vector-ref evt 0)
	((0) ;; key press
	 (begin
	   (case (vector-ref evt 4)
	     ((0) (handle-click evt))
	     ((4) (scroll-up))
	     ((3) (scroll-down))
	     ((6) (scroll-left))
	     ((5) (scroll-right))
	     (else (pp evt)))
	   (loop)))
	((2) ;; window resize
	 (begin
	   (update-window-dimensions!)
	   (update-text-dimensions!)
	   (update-pane-layout!)
	   (update-grid-dimensions!)
	   (update-screen!)
	   (loop)))
	((6) ;; key press
	 (handle-key-press evt)
	 (loop))
	((10) ;; close-window
	 unspecific)
	(else
	 (begin
	   (pp evt)
	   (loop))))))
  (update-window-dimensions!)
  (update-absolute-text-dimensions!)
  (update-text-dimensions!)
  (update-pane-layout!)
  (update-grid-dimensions!)
  (update-screen!)
  (loop))

(main)
