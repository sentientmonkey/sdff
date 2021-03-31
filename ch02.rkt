#lang racket/base

(require rackunit)
(require racket/list)

; 2.1.1
(define (compose f g)
   (λ  args
   (f (apply g args))))

(define (compose-b f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

(define (identity x) x)

(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (square x) (* x x))

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(define (spread-combine h f g)
  (let ((n (procedure-arity f)))
    (define (the-combination . args)
      (h (apply f (take args n))
         (apply g (drop args n))))
    the-combination))

(define (restrict-arity proc nargs)
  (hash-set! arity-table proc nargs)
  proc)

(define (assert e)
  (if e (error "assertion failed") #t))


(define (procedure-arity-min a)
        (min (procedure-arity a)))

(define (procedure-arity-max a)
        (max (procedure-arity a)))


(define (get-arity proc)
  (or (hash-ref arity-table proc #f)
      (let ((a (procedure-arity proc))) ;arity not in table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define arity-table (make-weak-hasheqv))

(define (spread-combine-with-err h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (h (apply f (take args n))
           (apply g (drop args n))))
      (restrict-arity the-combination t))))

(module+ test
  (require rackunit)

  (test-case
    "compose"
    [check-equal?
      ((compose (λ (x) (list 'foo x))
                (λ (x) (list 'bar x)))
       'z)
      '(foo (bar z))]

    [check-equal?
      ((compose-b (λ (x) (list 'foo x))
                  (λ (x) (list 'bar x)))
       'z)
      '(foo (bar z))])

  (test-case
    "iterate"
    (check-equal?
      (((iterate 3) square) 5)
      390625))

  (test-case
    "parallel-combine"
    (check-equal? 
      ((parallel-combine list
                         (λ (x y z) (list 'foo x y z))
                         (λ (u v w) (list 'bar u v w)))
       'a 'b 'c)
      '((foo a b c) (bar a b c))))

  (test-case
    "spread-combine"
    (check-equal?
      ((spread-combine list
                       (λ (x y) (list 'foo x y ))
                       (λ (u v w) (list 'bar u v w))) 
       'a 'b 'c 'd 'e)
      '((foo a b) (bar c d e)))
    (check-exn
      exn:fail?
      (λ ()
         ((spread-combine-with-err list
                                   (λ (x y) (list 'foo x y ))
                                   (λ (u v w) (list 'bar u v w))) 
          'a 'b 'c 'd 'e)))))
