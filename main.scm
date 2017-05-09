(load "load")

;; TODO: Dummy functions to be replaced by actual backend
(define (get-cell-string x y)
  (string-append
   (number->string x)
   ","
   (number->string y)))

(define (get-cell-innards x y)
  (string-append
   (number->string x)
   ","
   (number->string y)
   "innards"))

(define (set-cell-innards x y . innards)
  (pp (cons (list x y) innards)))

(define (run-global text)
  (pp (list 'global text)))

(define (main)
  (init-layout! get-cell-string get-cell-innards set-cell-innards run-global)
  (init-graphics!)
  (run-graphics))

(main)
