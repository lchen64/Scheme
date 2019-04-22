(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (define add-first (lambda (lst) (append (list first) lst)))
  (map add-first rests)
  )

(define (zip pairs)
  (define helper (lambda (pairs first second)
  (if (null? pairs) (list first second)
  (cons (cons (caar pairs) (car (zip (cdr pairs))))
    (cons (cons (car (cdar pairs)) (cadr (zip (cdr pairs)))) nil)))))
  (helper pairs nil nil))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define helper (lambda (s index)
  (if (null? s) nil
  (cons (cons index (cons (car s) nil)) (helper (cdr s) (+ 1 index))))))
  (helper s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond ((null? denoms) '(nil))
  ((< total (car denoms)) (list-change total (cdr denoms)))
  ((= total (car denoms)) (append `((,(car denoms))) (list-change total (cdr denoms))))
  (else (cons-all (car denoms) (list-change (- total (car denoms)) denoms)))))
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           `(,form ,params ,(car body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append `((lambda ,(car (zip values)) ,(car body))) (cadr (zip values)))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )))
