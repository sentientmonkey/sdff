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


(define list-head take)
(define list-tail drop)

(define (spread-combine h f g)
  (let ((n (procedure-arity f)))
    (define (the-combination . args)
      (h (apply f (list-head args n))
         (apply g (list-tail args n))))
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

; Exercise 2.2
; In racket's case, procedure-arity works differently with the arity-at-least wrapper.
; Emulating this behavior was somewhat clunky.

(module+ test
  (test-case
    "procedure-arity-min/max"
    [check-true (arity=? (procedure-arity +) (arity-at-least 0))]
    [check-equal? (procedure-arity-min (procedure-arity +)) 0]
    [check-equal? (procedure-arity-max (procedure-arity +)) 0]; here scheme would return #f, not 0
    [check-true (arity=? (procedure-arity atan) '(1 2))]
    [check-equal? (procedure-arity-min (procedure-arity atan)) 1]
    [check-equal? (procedure-arity-max (procedure-arity atan)) 2])


  ; Racket has procedure-arity-includes?
  ; and procedure-arity-mask, which are better utility functions to help out.
  (test-case
    "procedure-arity-includes?"
    [check-true (procedure-arity-includes? + 1)]
    [check-true (procedure-arity-includes? + 0)]
    [check-true (procedure-arity-includes? atan 1)]
    [check-false (procedure-arity-includes? atan 0)])

  (test-case
    "procedure-arity-mask"
    [check-equal? (procedure-arity-mask +) -1]
    [check-equal? (procedure-arity-mask atan) 6]
    [check-equal? (procedure-arity-mask -) -2]
    [check-equal? (bitwise-and (procedure-arity-mask atan)
                               (procedure-arity-mask +)) 6]))

; Using a bitwise check we could hash arity then check when we need it.
(define (check-mask n m)
  (not (zero? (bitwise-and n m ))))

(define (check-args mask n)
  (bitwise-bit-set? mask n))

(define (check-arity-mask f g)
  (check-mask (procedure-arity-mask f)
              (procedure-arity-mask g)))

(module+ test
  (test-case
    "check-arity-mask"
    (let ([f (λ (x) x)]
          [g (λ (x y) (list x y))])
    [check-false (check-arity-mask f g)]
    [check-true (check-arity-mask + atan)]
    [check-true (check-arity-mask + f)]
    [check-true (check-arity-mask + g)]
    [check-true (check-arity-mask atan f)]
    [check-true (check-arity-mask atan g)]
    [check-false (check-arity-mask f g)])))

; not hashing here, but wouldn't be hard to add.
(define (compose-with-mask f g)
  (let ([n (procedure-arity-mask f)]
        [m (procedure-arity-mask g)])
    (assert (check-mask n m))
    (define (the-composition . args)
      (let ([l (length args)])
        (assert (check-args n (length args)))
        (assert (check-args m (length args)))
        (f (apply g args))))
    the-composition))

(module+ test
  (test-case
    "compose-with-err"
    (let ([f (λ (x) (list 'foo x))]
          [g (λ (x) (list 'bar x))]
          [gee (λ (x y) (list 'bar x y))])
      [check-equal? ((compose-with-mask f g) 'z) '(foo (bar z))]
      [check-exn
        exn:fail:user?
        (λ ()
           ((compose-with-mask f g) 'z 'bad))]
      [check-exn
        exn:fail:user?
        (λ ()
           ((compose-with-mask f gee) 'z))])))

; Multiple values
(define (spread-apply f g)
  (let* ([n (get-arity f)]
         [m (get-arity g)]
         [t (+ n m)])
    (define (the-combination . args)
      (assert (= (length args) t))
      (values (apply f (list-head args n))
              (apply g (list-tail args n))))
    (restrict-arity the-combination t)))

(define (compose-with-values f g)
  (define (the-composition . args)
    (call-with-values (λ () (apply g args))
      f))
  (restrict-arity the-composition (get-arity g)))

(module+ test
  (test-case
    "combine-with-values"
    [check-equal? ((compose-with-values (λ (a b)
                                           (list 'foo a b))
                                        (λ (x)
                                           (values (list 'bar x)
                                                   (list 'baz x))))
                   'z)
                  '(foo (bar z) (baz z))]))

(define (spread-combine-with-apply h f g)
  (compose-with-values h (spread-apply f g)))

(module+ test
  (test-case
    "spread-combine-with-apply"
    [check-equal? ((spread-combine-with-apply list
                                             (λ (x y) (list 'foo x y))
                                             (λ (u v w) (list 'bar u v w)))
                   'a 'b 'c 'd 'e)
                  '((foo a b) (bar c d e))]))

; let-values works slightly different in racket vs scheme, 
; which makes using these values with let-values kind of impossible
; https://stackoverflow.com/a/20556950

(define (spread-apply-with-values f g)
  (let* ([n (get-arity f)]
         [m (get-arity g)]
         [t (+ n m)])
    (define (the-combination . args)
      (assert (= (length args) t))
      (let ([fv (list (apply f (list-head args n)))]
            [gv (list (apply g (list-tail args n)))])
        (apply values (append fv gv))))
    (restrict-arity the-combination t)))

(define (spread-combine-with-values h f g)
  (compose-with-values h (spread-apply-with-values f g)))

;(module+ test
;  (test-case
;    "spread-apply-with-values"
;    [check-equal? ((spread-apply-with-values (λ (x y) (values x y))
;                                             (λ (u v w) (values w v u)))
;                   'a 'b 'c 'd 'e)
;                  '(a b e d c)]))
;


; errors out...
;(let ([x ((λ (x y) (values x y)) 'a 'b)])
;  (displayln x)) 

;(module+ test
;  (test-case
;    "spread-combine-with-values"
;    [check-equal? ((spread-combine-with-values list
;                                             (λ (x y) (values x y))
;                                             (λ (u v w) (values w v u)))
;                   'a 'b 'c 'd 'e)
;                  '(a b e d c)]))

