;; A textbox is a field in which text can be entered

;; Events thrown by textbox elements

;; Should be triggered by the UI whenever a text field is
;; submitted. Events of this type have 2 parameters. The first is the
;; textbox from which the text was submitted. The second is the text
;; that was submitted
(define event-type:text-submitted 'text-submitted)

(define-record-type <textbox>
  (%make-textbox pane)
  textbox?
  (pane textbox-pane)
  (input-buffer textbox-input-buffer set-textbox-input-buffer!)
  (num-lines textbox-num-lines set-textbox-num-lines!)
  (chars-per-line textbox-chars-per-line set-textbox-chars-per-line!))

(define (make-textbox pane)
  (let ((textbox (%make-textbox pane)))
    (set-textbox-input-buffer! textbox "")
    (register-event-callback! event-type:draw
			      (lambda () (draw-textbox textbox)))
    (register-event-callback! event-type:layout-changed
			      (lambda ()
				(textbox-update-dimensions! textbox)))
    (register-event-callback! event-type:key-press
			      (lambda args
				(apply textbox-handle-keypress
				       (cons textbox args))))
    textbox))

(define (textbox-update-dimensions! textbox)
  (let ((width (pane-width (textbox-pane textbox)))
	(height (pane-height (textbox-pane textbox))))
    (set-textbox-num-lines!
     textbox
     (floor->exact (/ (- height (* 2 text-padding)) text-height)))
    (set-textbox-chars-per-line!
     textbox
     (floor->exact (/ width char-width)))))

(define (textbox-handle-keypress textbox window key . other)
  (let ((buffer (textbox-input-buffer textbox)))
    (cond
     ((string=? key "\177")
      (if (> (string-length buffer) 0)
	  (set-textbox-input-buffer!
	   textbox
	   (string-head buffer (- (string-length buffer) 1)))))
     ((string=? key "\r")
;;      (ignore-errors
      ;;       (lambda ()
      (begin
	 (trigger-event (list event-type:text-submitted textbox
			      buffer))
	 (set-textbox-input-buffer! textbox "")));))
     (else
      (set-textbox-input-buffer! textbox (string-append buffer key))))))

(define (draw-textbox textbox)
  (let ((pane (textbox-pane textbox))
	(buffer (textbox-input-buffer textbox))
	(chars-per-line (textbox-chars-per-line textbox))
	(num-lines (textbox-num-lines textbox)))
    (define (lines-to-draw buffer)
      (cond
       ((< (string-length buffer) chars-per-line)
	(list buffer))
       ((> (string-length buffer) (* num-lines chars-per-line))
	(lines-to-draw (string-tail buffer chars-per-line)))
       (else
	(cons (string-head buffer chars-per-line)
	      (lines-to-draw (string-tail buffer chars-per-line))))))
    (define (draw-lines . lines)
      (let lp ((y-top text-padding) (lines lines))
	(if (not (null? lines))
	    (begin
	      (pane-draw-text pane text-padding (+ y-top text-height)
			      (car lines))
	      (lp (+ y-top text-height) (cdr lines))))))
    (apply draw-lines (lines-to-draw buffer))))
