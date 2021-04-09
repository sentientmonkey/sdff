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

; Exercise 2.3
; nope, not gonna


(define (list-remove lst index)
  (let lp ([lst lst] [index index])
    (if (= index 0)
      (cdr lst)
      (cons (car lst) (lp (cdr lst) (- index 1))))))

(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (λ (f)
     (let ([m (+ (get-arity f) 1)])
       (define (the-combination . args)
         (assert (= (length args) m))
         (apply f (list-remove args i)))
       (assert (< i m))
       (restrict-arity the-combination m))))

(module+ test
  (test-case
  "discard-argument"
  [check-equal? (((discard-argument 2)
                  (λ (x y z) (list 'foo x y z)))
                 'a 'b 'c 'd)
                '(foo a b d)]))

(define (list-insert lst index value)
  (let lp ([lst lst] [index index])
    (if (= index 0)
      (cons value lst)
      (cons (car lst) (lp (cdr lst) (- index 1))))))

(define ((curry-argument i) . args)
  (λ (f)
     (assert (= (length args) (- (get-arity f) 1)))
     (λ (x)
        (apply f (list-insert args i x)))))

(module+ test
  (test-case
    "curry-argument"
    [check-equal? ((((curry-argument 2) 'a 'b 'c)
                    (λ (x y z w) (list 'foo x y z w)))
                   'd)
                  '(foo a b d c)]))


(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (λ (p) (list-ref lst p))
         permspec))
  the-permuter)

(define (permute-arguments . permspec)
  (let ([permute (make-permutation permspec)])
    (λ (f)
       (define (the-combination . args)
         (apply f (permute args)))
       (let ([n (get-arity f)])
         (assert (= n (length permspec)))
         (restrict-arity the-combination n)))))

(module+ test
  (test-case
    "permute-arguments"
    [check-equal? (((permute-arguments 1 2 0 3)
                    (λ (x y z w) (list 'foo x y z w)))
                   'a 'b 'c 'd)
                  '(foo b c a d)]))

; Exercise 2.4
; gonna skip multiple return values again...

; Exercise 2.5
; a.

; list-remove* removes all items given a list of indexes.
; indexes may be provided out of order.
(define (list-remove* lst indexes)
  (let lp ([lst lst]
           [indexes (sort indexes <)]
           [index 0])
    (cond
      [(empty? lst) lst]
      [(empty? indexes) lst]
      [(= index (car indexes))
       (lp (cdr lst) (cdr indexes) (add1 index))]
      [else 
        (cons (car lst) (lp (cdr lst) indexes (add1 index)))])))

(module+ test
  (test-case
    "list-remove-*"
    [check-equal? (list-remove* '(1 2 3) '(0 2))
                  '(2)])
    [check-equal? (list-remove* '(1 2 3) '(2 0))
                  '(2)]
    [check-equal? (list-remove* '(1 2 3) '())
                  '(1 2 3)]
    [check-equal? (list-remove* '(1 2 3) '(4))
                  '(1 2 3)])

(define (make-discard discardspec)
  (λ (lst) (list-remove* lst discardspec)))

(module+ test
  (test-case
    "make-discard"
    [check-equal? ((make-discard '(2)) '(a b c d))
                  '(a b d)]
    [check-equal? ((make-discard '(2 1)) '(a b c d))
                  '(a d)]))

(define (discard-argument* . discardspec)
  (let ([discard (make-discard discardspec)])
    (λ (f)
       (define (the-combination . args)
         (apply f (discard args)))
       (let ([m (+ (get-arity f) 1)])
         (assert (< (length discardspec) m))
         (restrict-arity the-combination m)))))
(module+ test
  (test-case
  "discard-argument*"
  [check-equal? (((discard-argument* 2)
                  (λ (x y z) (list 'foo x y z)))
                 'a 'b 'c 'd)
                '(foo a b d)]
  [check-equal? (((discard-argument* 2 1)
                  (λ (x y) (list 'foo x y)))
                 'a 'b 'c 'd)
                '(foo a d)]
  [check-equal? (((discard-argument* 0 1)
                  (λ (x y) (list 'foo x y)))
                 'a 'b 'c 'd)
                '(foo c d)]))

; 2.2 Regular Expressions
(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define chars-needing-quoting
  (list #\. #\[ #\\ #\^ #\$ #\*))

(define (r:quote string)
  (r:seq
    (list->string
      (append-map (λ (char)
                     (if (memv char chars-needing-quoting)
                       (list #\\ char)
                       (list char)))
                  (string->list string)))))

(module+ test
  (test-case
    "r:seq"
    [check-equal? (r:seq (r:quote "a") (r:dot) (r:quote "c"))
                  "\\(\\(a\\).\\(c\\)\\)"]))

(define (r:alt . exprs)
  (if (pair? exprs)
    (apply r:seq
           (cons (car exprs)
                 (append-map (λ (expr)
                                (list "\\|" expr))
                             (cdr exprs))))
    (r:seq)))

(module+ test
  (test-case
    "r:alt"
    [check-equal? (r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))
                  "\\(\\(foo\\)\\|\\(bar\\)\\|\\(baz\\)\\)"]))

(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond [(not max) (list expr "*")]
                       [(= max min) '()]
                       [else
                         (make-list (- max min)
                                    (r:alt expr ""))]))))

; !!!
; (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))


(define (bracket string procedure)
  (list->string
    (append '(#\])
            (procedure (string->list string))
            '(#\]))))

(define chars-needing-quoting-in-brackets
  (list #\] #\^ #\-))

(define (quoted-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove
            (λ (c)
               (memv c chars-needing-quoting-in-brackets))
            (optional #\^)
            (optional #\-))))

(define lset= member)

(define (r:char-from string)
  (case (string-length string)
    [(0) (r:seq)]
    [(1) (r:quote string)]
    [else
      (bracket string
               (λ (members)
                  (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quoted-bracketed-contents members))))]))

(define (r:char-not-from string)
  (bracket string
           (λ (members)
              (cons #\^ (quoted-bracketed-contents members)))))

(define (bourne-shell-quote-string string)
  (list->string
    (append (list #\')
            (append-map (λ (char)
                           (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                        (string->list string))
            (list #\'))))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))
