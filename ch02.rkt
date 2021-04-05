#lang racket/base

(require rackunit)
(require racket/list)
(require racket/function)

; 2.1.1
(define (compose f g)
   (λ  args
   (f (apply g args))))

(define (compose-b f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

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
      '(foo (bar z))]))

(define (identity x) x)

(define ((iterate n) f)
  (if (= n 0)
    identity
    (compose f ((iterate (- n 1)) f))))

(define (square x) (* x x))

(module+ test
  (test-case
    "iterate"
    (check-equal?
      (((iterate 3) square) 5)
      390625)))

(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(module+ test
  (test-case
    "parallel-combine"
    (check-equal? 
      ((parallel-combine list
                         (λ (x y z) (list 'foo x y z))
                         (λ (u v w) (list 'bar u v w)))
       'a 'b 'c)
      '((foo a b c) (bar a b c)))

    (check-exn
      exn:fail:contract:arity?
      (λ ()
         ((parallel-combine list
                            (λ (x y) (list 'foo x y))
                            (λ (u v w) (list 'bar u v w)))
          'a 'b 'c 'd 'e)))))


(define (spread-combine h f g)
  (let ((n (procedure-arity f)))
    (define (the-combination . args)
      (h (apply f (take args n))
         (apply g (drop args n))))
    the-combination))

(define (restrict-arity proc nargs)
  (hash-set! arity-table proc nargs)
  proc)

; making this fail with a user error makes it easier to test
(define (assert e)
  (when (not e) (raise-user-error "assertion failed")))

; racket thinks about arity slightly different than MIT/GNU scheme
;
; In scheme, procedure-arity returns a dotted pair of min / max arity.
; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Arity.html
;
; In racket, procedure-arity returns a normalized-arity structure or a list of values
; https://docs.racket-lang.org/reference/procedures.html?q=procedure-arity#%28def._%28%28quote._~23~25kernel%29._procedure-arity%29%29

; Since no min/max functions exist, we can re-make them from inspecting the result.

(module+ test
  (test-case
    "arity-includes?"
    (check-true (arity-includes? (procedure-arity list) 0)))
  (test-case
    "arity-at-least-value"
    (check-equal? (arity-at-least-value (procedure-arity list)) 0)))

(define (procedure-arity-min a)
  (cond 
    [(arity-at-least? a) (arity-at-least-value a)]
    [(list? a) (apply min a)]
    [else a]))

(define (procedure-arity-max a)
  (cond 
    [(arity-at-least? a) (arity-at-least-value a)]
    [(list? a) (apply max a)]
    [else a]))

(module+ test
  (let ([f (case-lambda [(x) 0] [(x y) 1])]
        [g (λ (x y) (list 'foo x y))])
    (test-case
      "procedure-arity-min"
      [check-equal? (procedure-arity-min (procedure-arity list)) 0]
      [check-equal? (procedure-arity-min (procedure-arity f)) 1]
      [check-equal? (procedure-arity-min (procedure-arity g)) 2])
    (test-case
      "procedure-arity-min"
      [check-equal? (procedure-arity-max (procedure-arity list)) 0]
      [check-equal? (procedure-arity-max (procedure-arity f)) 2]
      [check-equal? (procedure-arity-min (procedure-arity g)) 2])))


(define (get-arity proc)
  (or (hash-ref arity-table proc #f)
      (let ([a (procedure-arity proc)]) ;arity not in table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define arity-table (make-weak-hasheqv))

(module+ test
  (test-case
    "get-arity"
    [check-equal? (get-arity list) 0]
    [check-equal? (get-arity list) 0]
    [check-equal? (get-arity (λ (x y) (list 'foo x y))) 2]
    [check-equal? (get-arity (λ (u v w) (list 'bar u v w))) 3]))

(define (spread-combine-with-err h f g)
  (let ([n (get-arity f)] [m (get-arity g)])
    (let ([t (+ n m)])
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (take args n))
           (apply g (drop args n))))
      (restrict-arity the-combination t))))

(module+ test
  (test-case
    "spread-combine"
    (check-equal?
      ((spread-combine list
                       (λ (x y) (list 'foo x y ))
                       (λ (u v w) (list 'bar u v w))) 
       'a 'b 'c 'd 'e)
      '((foo a b) (bar c d e)))

    (check-equal?
      ((spread-combine-with-err list
                       (λ (x y) (list 'foo x y ))
                       (λ (u v w) (list 'bar u v w))) 
       'a 'b 'c 'd 'e)
      '((foo a b) (bar c d e)))

    (check-exn
      exn:fail:user?
      (λ ()
         ((spread-combine-with-err list
                                   (λ (x y) (list 'foo x y ))
                                   (λ (u v w) (list 'bar u v w))) 
          'a 'b 'c 'd 'e 'extra)))))

; Exercise 2.1
; I'm not sure about scheme, but racket already checks arity when applying args to g.
; We can add our own check, but this seems silly. Maybe for better error messaging?
; I ended up fail:user to distingish between the existing contract arity errors.
(define (compose-with-err f g)
  (let ([n (get-arity f)]
        [m (get-arity g)])
    (assert (eqv? n m))
    (define (the-composition . args)
      (assert (eqv? (length args) n))
      (f (apply g args)))
    the-composition))

(module+ test
  (test-case
    "compose-with-err"
    (let ([f (λ (x) (list 'foo x))]
          [g (λ (x) (list 'bar x))]
          [gee (λ (x y) (list 'bar x y))])
      [check-equal? ((compose-with-err f g) 'z) '(foo (bar z))]
      [check-exn
        exn:fail:user?
        (λ ()
           ((compose-with-err f g) 'z 'bad))]
      [check-exn
        exn:fail:user?
        (λ ()
           ((compose-with-err f gee) 'z))])))

(define (parallel-combine-with-err h f g)
  (let ([n (get-arity f)]
        [m (get-arity g)])
    (assert (eqv? n m))
    (define (the-combination . args)
      (assert (= (length args) n))
      (h (apply f args) (apply g args)))
    the-combination))

(module+ test
  (test-case
    "parallel-combine-with-err"
    (let ([f (λ (x y z) (list 'foo x y z))]
          [g (λ (u v w) (list 'bar u v w))]
          [fuu (λ (x y) (list 'foo x y))])
      [check-equal? 
        ((parallel-combine-with-err list f g) 'a 'b 'c)
        '((foo a b c) (bar a b c)) ]
      [check-exn
        exn:fail:user?
        (λ ()
           ((parallel-combine-with-err list f g) 'a 'b 'c 'd 'e 'f))]
      [check-exn
        exn:fail:user?
        (λ ()
           ((parallel-combine-with-err list fuu g) 'a 'b 'c))])))
