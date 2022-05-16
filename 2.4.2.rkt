#lang sicp

(define (square x) (* x x))
(define (attach-tag tags content) (cons tags content))
(define (get-tag par) (cond ((pair? par) (car par))))
(define (get-content par) (cond ((pair? par) (cdr par))))

(define (is-zj k) (eq? `zj (get-tag k)))
(define (is-jz k) (eq? `jz (get-tag k)))

; 角度采用弧度制

; 直角坐标系
(define (zj-real-part k) (car (get-content k)))
(define (zj-imag-part k) (cdr (get-content k)))
(define (zj-magnitude k) (sqrt (+ (square (zj-real-part k)) (square (zj-imag-part k)))))
(define (zj-angle k) (atan (/ (zj-imag-part k) (zj-real-part k))))
(define (make-zj-from-real-imag real imag) (attach-tag `zj (cons real imag)))
(define (make-zj-from-mag-angle magnitude angle) (attach-tag `zj (cons (* magnitude (cos angle) (* magnitude (sin angle))))))

; 极坐标系
(define (jz-magnitude k) (car (get-content k)))
(define (jz-angle k) (cdr (get-content k)))
(define (jz-real-part k) (* (jz-magnitude k) (cos (jz-angle k))))
(define (jz-imag-part k) (* (jz-magnitude k) (sin (jz-angle k))))
(define (make-jz-from-real-imag real imag) (attach-tag `jz (cons (sqrt (+ (square real) (square imag))) (atan (/ imag real)))))
(define (make-jz-from-mag-angle magnitude angle) (attach-tag `jz (cons magnitude angle)))

(define (real-part k) (
    cond 
        ((is-zj k) (zj-real-part k))
        ((is-jz k) (jz-real-part k))
))
(define (imag-part k) (
    cond 
        ((is-zj k) (zj-imag-part k))
        ((is-jz k) (jz-imag-part k))
))
(define (magnitude k) (
    cond 
        ((is-zj k) (zj-magnitude k))
        ((is-jz k) (jz-magnitude k))
))
(define (angle k) (
    cond
        ((is-zj k) (zj-angle k))
        ((is-jz k) (jz-angle k))
))


(angle (make-zj-from-real-imag 1 1))
(magnitude (make-zj-from-real-imag 1 1))
(real-part (make-jz-from-mag-angle 1 1))
(imag-part (make-jz-from-mag-angle 1 1))