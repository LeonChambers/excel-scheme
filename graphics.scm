;; Global graphics device used to draw to the screen
(define graphics #f)

;; Events produced by the graphics layer

;; Should be triggered right after the absolute graphics dimensions
;; are changed. Events of this type have no parameters. The new
;; graphics dimensions will be available in the global namespace
(define event-type:absolute-dimensions-changed
  'absolute-dimensions-changed)

;; Should be triggered right after the virtual coordinates of the
;; window change for any reason. This could occur because either the
;; absolute dimensions of the window changed or the zoom level
;; changed. Events of this type have no parameters. The new graphics
;; dimensions will be available in the global namespace
(define event-type:dimensions-changed 'dimensions-changed)

;; Should be triggered to signal to all UI components to draw
;; themselves in the graphics window. Events of this type have no
;; parameters
(define event-type:draw 'draw)

;; Events of this type are produced by the graphics system when the
;; user clicks or scrolls on the graphics window. Events of this type
;; have 5 parameters. The first is the window in which the event
;; occurred. The second and third are the window coordinates at which
;; the event happened. The fourth is the time in milliseconds at which
;; the event happened.
(define event-type:mouse-event 0)

;; Events of this type are produced by the graphics system when the
;; window is resized. Events of this type have 3 parameters. The first
;; is the window in which the event occurred. The second and third are
;; the new dimensions of the window
(define event-type:window-resize 2)

;; Events of this type are produced by the graphics system when a key
;; is pressed while the graphics window is in focus. Events of this
;; type have 5 parameters. The first is the window that was in focus
;; when the key was pressed. The second is a string containing the key
;; that was pressed. The third is ???. The forth is the ascii code for
;; the button that was pressed. The fifth is the time in milliseconds
;; at which the event happened.
(define event-type:key-press 6)

;; Separate the different types of mouse events
(define event-type:mouse-click 'mouse-click)

(define event-type:scroll-up 'scroll-up)

(define event-type:scroll-down 'scroll-down)

(define event-type:scroll-left 'scroll-left)

(define event-type:scroll-right 'scroll-right)

(define mouse-event-map
  `((0 ,event-type:mouse-click) (4 ,event-type:scroll-up)
    (3 ,event-type:scroll-down) (6 ,event-type:scroll-left)
    (5 ,event-type:scroll-right)))

(register-event-callback!
 event-type:mouse-event
 (lambda (window x y sub-type-id time)
   (let ((sub-type (cadr (assq sub-type-id mouse-event-map))))
     (trigger-event (list sub-type window x y time)))))


;; Zoom is implemented through the virtual coordinates system in the
;; native graphics library so that objects being drawn to the screen
;; don't have to know about the zoom level

;; Ratio between absolute coordinates and virtual coordinates
(define zoom-level)

;; Text dimensions
(define text-padding 10)
(define text-height 18)
(define char-width 16)
(define padded-text-height (+ text-height (* 2 text-padding)))

;; Absolute dimensions of the window in pixels
(define absolute-graphics-width)
(define absolute-graphics-height)

;; Virtual dimensions of the window
(define graphics-width)
(define graphics-height)

;; Recompute the virtual window dimensions from the absolute
;; dimensions and zoom level
(define (update-dimensions!)
  (set! graphics-width (/ absolute-graphics-width zoom-level))
  (set! graphics-height (/ absolute-graphics-height zoom-level))
  (graphics-set-coordinate-limits graphics 0 graphics-height
				  graphics-width 0)
  (trigger-event (list event-type:dimensions-changed)))
(register-event-callback! event-type:absolute-dimensions-changed
			  update-dimensions!)

;; Set the absolute dimensions based on the (potentially updated)
;; window dimensions and update the virtual dimensions
(define (update-absolute-dimensions!)
  ((x-graphics/device-coordinate-limits graphics)
   (lambda (x-left y-bottom x-right y-top)
     (set! absolute-graphics-width (- x-right x-left))
     (set! absolute-graphics-height (- y-bottom y-top))
     (trigger-event (list event-type:absolute-dimensions-changed)))))
(register-event-callback! event-type:window-resize
			  (lambda args
			    (update-absolute-dimensions!)))

;; API for controlling the zoom level

(define (zoom-in!)
  (set! zoom-level (* zoom-level 1.1))
  (trigger-event (list event-type:absolute-dimensions-changed)))

(define (zoom-out!)
  (set! zoom-level (/ zoom-level 1.1))
  (trigger-event (list event-type:absolute-dimensions-changed)))

;; Set the font size based on the zoom level
(define (update-font-size!)
  (let ((size (* text-height zoom-level)))
    (x-graphics/set-font
     graphics
     (string-append
      ;; We arbitrarily chose one of the few available scalable
      ;; monospace fonts
      "-bitstream-courier 10 pitch-medium-r-normal-*-0-"
      (number->string (floor->exact (* 10 size)))
      "-0-0-m-0-ascii-0"))))
(register-event-callback! event-type:absolute-dimensions-changed
			  update-font-size!)

;; Initialize the global graphics device and set it up to receive user
;; input
(define (init-graphics!)
  ;; Create the graphics device
  (if (not (graphics-type-available? 'x))
      (error "X graphics not available"))
  (set! graphics (make-graphics-device 'x))

  ;; This allows X to push focus onto the graphics window
  (x-graphics/set-input-hint graphics 1)

  ;; Initialize the zoom level
  (set! zoom-level 1)

  ;; Initialize the coordinate systems
  (update-absolute-dimensions!)

  ;; Request only the events we will actually use: The following are
  ;; the mask bits for the events we are interested in
  ;; #x0001 Mouse events
  ;; #x0004 Window resize events
  ;; #x0040 Key down events
  (x-graphics/select-user-events graphics #x0045))

;; Runs an event loop that draws the screen, waits for an event,
;; handles the event, and then repeats
;; on-draw is a callback that should render content to the graphics
;; window
(define (run-graphics)
  (define (loop)
    (graphics-clear graphics)
    (trigger-event (list event-type:draw))
    (x-graphics/discard-events graphics)
    (let ((evt (x-graphics/read-user-event graphics)))
      ;; Break on window close events
      (if (not (= (vector-ref evt 0) 10))
	  (begin
	    (trigger-event (vector->list evt))
	    (loop)))))
  (update-absolute-dimensions!)
  (loop))
