;; This file defines an event system that is used for message passing
;; in the frontend. A list of event types are defined and described
;; below. Callbacks will be registered for an event type, and whenever
;; an event of the corresponding type is triggered, all callbacks
;; registered for that event will be called with the parameters of the
;; event as arguments. If one of the callbacks returns the swallow
;; object, it swallows the event, and none of the remaining callbacks
;; are processed

(define swallow 'swallow)

(define event-callbacks '())

(define (register-event-callback! evt-type callback)
  (let* ((old-val (assq evt-type event-callbacks))
	 (new-callback
	  (if old-val
	      (lambda args
		(if (not (eq? (apply (cadr old-val) args) swallow))
		    (apply callback args)))
	      callback)))
    (set! event-callbacks
	  (cons (list evt-type new-callback) event-callbacks))))

(define (trigger-event evt)
  (let ((entry (assq (car evt) event-callbacks)))
    (if entry
	(apply (cadr entry) (cdr evt)))))
