;; A pane is a section of a window that can be drawn into. This allows
;; the rendering of individual components of a UI to be logically
;; separated in code

(define-record-type <pane>
  (make-pane x-left y-top x-right y-bottom)
  pane?
  (x-left pane-x-left set-pane-x-left!)
  (y-top pane-y-top set-pane-y-top!)
  (x-right pane-x-right set-pane-x-right!)
  (y-bottom pane-y-bottom set-pane-y-bottom!))

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
