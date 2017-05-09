;; UI buttons for zooming in and out

(define-record-type <zoom-button>
  (%make-zoom-button pane is-in)
  zoom-button?
  (pane zoom-button-pane)
  (is-in zoom-button-is-in))

(define (make-zoom-in-button pane)
  (make-zoom-button pane #t))

(define (make-zoom-out-button pane)
  (make-zoom-button pane #f))

(define (make-zoom-button pane is-in)
  (let ((button (%make-zoom-button pane is-in)))
    (register-event-callback!
     event-type:mouse-click
     (lambda (window x y time)
       (zoom-button-handle-click button x y)))
    (register-event-callback! event-type:draw
			      (lambda () (draw-zoom-button button)))))

(define (zoom-button-handle-click button x y)
  (if (pane-contains-point (zoom-button-pane button) x y)
      (begin
	(if (zoom-button-is-in button)
	    (zoom-in!)
	    (zoom-out!))
	swallow)))

(define (draw-zoom-button button)
  (let* ((pane (zoom-button-pane button))
	 (width (pane-width pane))
	 (height (pane-height pane)))
    (pane-draw-border pane)
    (pane-draw-line pane (* 0.1 width) (* 0.9 height)
		    (* (- 0.9 (* 0.3 (+ 1 (/ (sqrt 2) 2)))) width)
		    (* (+ 0.1 (* 0.3 (+ 1 (/ (sqrt 2) 2)))) height))
    (pane-draw-arc pane (* 0.6 width) (* 0.4 height) (* 0.3 width)
		   (* 0.3 height) 0 360 #f)
    (pane-draw-line pane (* 0.4 width) (* 0.4 height) (* 0.8 width)
		    (* 0.4 height))
    (if (zoom-button-is-in button)
	(pane-draw-line pane (* 0.6 width) (* 0.2 height)
			(* 0.6 width) (* 0.6 height)))))
