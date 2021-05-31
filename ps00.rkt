#lang racket/base

;; https://groups.csail.mit.edu/mac/users/gjs/6.945/psets/ps00/dh.pdf

;; Problem 1
(module+ test
  (require rackunit)
  (test-case 
    "mod"
    [check-equal? (modulo 13 8) 5]
    [check-equal? (remainder 13 8) 5]
    [check-equal? (modulo -13 8) 3]
    [check-equal? (remainder -13 8) -5]
    [check-equal? (modulo -13 -8) -5]
    [check-equal? (remainder -13 -8) -5]
    ))

;; modulo keeps the sign for the divisor
;; seems better for modulo math

(define (+mod a b n)
  (modulo (+ a b) n))

(module+ test
  (test-case
    "+mod"
    [check-equal? (+mod 7 5 8) 4]
    [check-equal? (+mod 10 10 3) 2]
    [check-equal? (+mod 99 99 100) 98]))

(define (-mod a b n)
  (modulo (- a b) n))

(module+ test
  (test-case
    "-mod"
    [check-equal? (-mod 5 12 2) 1]))


(define (*mod a b n)
  (modulo (* a b) n))

(module+ test
  (test-case
    "*mod"
    [check-equal? (*mod 6 6 9) 0]
    [check-equal? (*mod 50 -3 100) 50]))


(define (modular modulus op)
  (λ (a1 a2)
     (modulo (op a1 a2) modulus)))

(define +m12 (modular 12 +))
(define -m12 (modular 12 -))
(define *m12 (modular 12 *))

(module+ test
  (test-case
    "m12"
    [check-equal? (-m12 (*m12 (+m12 5 8) 3) 7) 8]))

(module+ test
  (test-case
    "modular"
    [check-equal? ((modular 17 +) 13 11) 7]
    [check-equal? ((modular 17 -) 13 11) 2]
    [check-equal? ((modular 17 *) 13 11) 7]))

;; Problem 2

(define (slow-exptmod n)
  (let [(*mod (modular n *))]
    (define (em a b)
      (if (= b 0)
        1
        (*mod a (em a (- b 1)))))
    em))

(module+ test
  (test-case
    "slow-exptmod"
    [check-equal? ((slow-exptmod 100) 3 0) 1]
    [check-equal? ((slow-exptmod 100) 3 1) 3]
    [check-equal? ((slow-exptmod 100) 3 2) 9]
    [check-equal? ((slow-exptmod 100) 3 3) 27]
    [check-equal? ((slow-exptmod 100) 3 4) 81]
    [check-equal? ((slow-exptmod 100) 3 5) 43]
    [check-equal? ((slow-exptmod 100) 3 6) 29]
    [check-equal? ((slow-exptmod 100) 3 7) 87]
    [check-equal? ((slow-exptmod 100) 3 8) 61]))


(define (exptmod p)
  (let [(mod* (modular p *))]
    (define (square x)
      (mod* x x))
    (define (em base exponent)
      (displayln (list base exponent))
      (cond 
        [(= exponent 0) 1]
        [(= exponent 1) base]
        [(= (modulo exponent 2) 0) (square (em base (/ exponent 2)))]
        [else #f]))
    em))

(module+ test
  (test-case
    "expt-mod"
    ;;[check-equal? ((exptmod 10) 3 0) 1]
    ;;[check-equal? ((exptmod 10) 3 1) 3]
    ;;[check-equal? ((exptmod 10) 3 2) 9]
    [check-equal? ((exptmod 10) 3 4) 81]
    ))

