;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname subsets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;******************************
;;   CS 135 Fall 2016
;;   Xidong Chen (20683737)
;;   Assigment 10 Problem 02
;;******************************
;;



;;(a)

(define (subsets1 lon)
  (foldr (lambda (x y) (append (map (lambda (a) (cons x a)) y)
                               y))
         (list (list)) lon))


;;(b)
(define (subsets2 lon)
  (foldr (lambda (x y) (append (map (lambda (a) (cons x a)) y)
                               y))
         (list (list)) lon))