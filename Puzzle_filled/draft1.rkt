;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draft1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))



;; helper functions

(define (find-nth-element count lst)
  (cond [(empty? lst) empty]
        [(= count 0) (first lst)]
        [else (find-nth-element (sub1 count) (rest lst))]))

(define (locate my-grid my-pos)
  (find-nth-element (pos-x my-pos)
                    (find-nth-element (pos-y my-pos) my-grid)))

(define (replace-nth-element count new-item lst)
  (cond [(empty? lst) empty]
        [(= count 0) (cons new-item (rest lst))]
        [else
         (cons (first lst)
               (replace-nth-element (sub1 count)
                                    new-item (rest lst)))]))

(define (replace my-grid my-pos new-item)
  (replace-nth-element (pos-y my-pos)
                       (replace-nth-element
                        (pos-x my-pos) new-item
                        (find-nth-element (pos-y my-pos) my-grid))
                       my-grid))




;;;;;;;;;;;;;;;;;;;;;;
;; (lists-equiv? l1 l2)
;; Given two lists l1 and l2, determine whether the lists are essentially
;; the same up to reordering.  This function will work as long as one of
;; the two lists contains no duplicates.  Not really part of this assignment,
;; but generally useful in tests where we don't care about ordering.
;; lists-equiv?: (listof X) (listof X) -> Bool
;; Examples:
; (check-expect (lists-equiv? '("1" "2" "3") '("2" "3" "1")) true)
; (check-expect (lists-equiv? '(1 2 3 4) '(2 3 4 5)) false)

(define (lists-equiv? l1 l2)
  ;; The approach is a bit sneaky, but very succinct.  Check that
  ;; every element of l1 appears somewhere in l2 (in terms of equal?),
  ;; and that every elements of l2 appears somewhere in l1.
  (and (= (length l1) (length l2))
       (andmap (lambda (x1) (ormap (lambda (x2) (equal? x1 x2)) l2)) l1)
       (andmap (lambda (x2) (ormap (lambda (x1) (equal? x1 x2)) l1)) l2)))

;;(a)

;; (build-2dlist width height fn) produces a grid of width wide and height high
;;  starting from origin and apply fn to each element of the grid
;; build-2dlist: Nat Nat (Nat Nat -> Any) -> (listof (listof  Any))
;; examples:
(check-expect (build-2dlist 0 0 +) empty)
(check-expect (build-2dlist 1 2 *) (list (list 0) (list 0)))

(define (build-2dlist width height fn)
  (build-list height
              (lambda (y)
                (build-list width
                            (lambda (x)
                              (fn x y))))))

;;tests:
(check-expect (build-2dlist 0 0 -) empty)
(check-expect (build-2dlist 2 1 =)
              (list (list true false)))
(check-expect (build-2dlist 2 3 *)
              (list (list  0 0)
                    (list  0 1)
                    (list  0 2)))
(check-expect (build-2dlist 5 6 +)
              (list (list 0 1 2 3 4)
                    (list 1 2 3 4 5)
                    (list 2 3 4 5 6)
                    (list 3 4 5 6 7)
                    (list 4 5 6 7 8)
                    (list 5 6 7 8 9)))


;;(b)

;;(all-positions w h) produces all the posibile positions in a grid with
;; width of w and height of h
;; all-positions: Nat Nat -> (listof Posn)
;; examples:

(check-expect (lists-equiv? (all-positions 0 0) empty) true)
(check-expect (lists-equiv? (all-positions 1 1) (list (make-pos 0 0))) true)



(define (all-positions w h)
  (foldr append empty (build-2dlist w h make-pos)))

;;tests:
(check-expect (lists-equiv? (all-positions 0 0) empty) true)
(check-expect (lists-equiv? (all-positions 2 2)
                            (list (make-pos 0 0) (make-pos 1 0)
                                  (make-pos 0 1) (make-pos 1 1))) true)
(check-expect (lists-equiv? (all-positions 3 4)
                            (list (make-pos 0 0) (make-pos 1 0)
                                  (make-pos 2 0) (make-pos 0 1)
                                  (make-pos 1 1) (make-pos 2 1) 
                                  (make-pos 0 2) (make-pos 1 2)
                                  (make-pos 2 2) (make-pos 0 3)
                                  (make-pos 1 3) (make-pos 2 3))) true)


;;(2)

(define (all-orientations my-grid)
  (local [(define (reflect-left/right my-grid)
            (foldr (lambda (x y) (cons (reverse x) y)) empty my-grid))
          (define (reflect-up/down my-grid)
            (reverse my-grid))
          (define (rotate my-grid)
            (cond [(empty? my-grid) empty]
                  [(empty? (first my-grid)) empty]
                  [else (local [(define first-column (map first my-grid))
                                (define rest-column (map rest my-grid))]
                          (cons first-column
                                (rotate rest-column)))]))
          (define all-solutions
            (list my-grid
                  (reflect-left/right my-grid)
                  (reflect-up/down my-grid)
                  (reflect-left/right (reflect-up/down my-grid))
                  (rotate my-grid)
                  (reflect-left/right (rotate my-grid))
                  (reflect-up/down (rotate my-grid))
                  (reflect-left/right (reflect-up/down (rotate my-grid)))))]
    (foldr (lambda (x y) (cond [(member? x y) y]
                               [else (cons x y)])) empty all-solutions))) 



;;(3)



(define (first-empty-pos my-grid)
  (local
    [(define width (length (first my-grid)))
     (define height (length my-grid))
     (define flattened-grid
       (foldr append empty my-grid))
     (define assigned-grid
       (map list flattened-grid (all-positions width height)))
     (define results
       (filter (lambda (x) (member? #\. x)) assigned-grid))]
    (cond [(empty? results) empty]
          [else (second (first results))])))



;;(4)


(define (superimpose base top start-pos)
  (local
    [(define (add-pos pos1)
       (make-pos (+ (pos-x pos1) (pos-x start-pos))
                 (+ (pos-y pos1) (pos-y start-pos))))
     (define original-top-pos
       (all-positions (length (first top)) (length top)))
     (define new-top-pos
       (map add-pos original-top-pos))
     (define (update-grid base top lo-np lo-op)
       (cond [(empty? lo-op) base]
             [(char=? #\. (locate top (first lo-op)))
              (update-grid base top (rest lo-np) (rest lo-op))]
             [else
              (update-grid (replace base (first lo-np)
                                    (locate top (first lo-op)))
                           top (rest lo-np) (rest lo-op))]))]
    (update-grid base top new-top-pos original-top-pos)))

(define (neighbours-single base my-piece)
  (local
    [(define (find-first-non-empty loc) 
       (cond [(empty? loc) 0]
             [(not (char=? #\. (first loc))) 0]
             [else (add1 (find-first-non-empty (rest loc)))]))
     (define first-non-empty-my-piece (find-first-non-empty (first my-piece)))
     (define first-empty-pos-base (first-empty-pos base))
     (define difference (- (pos-x first-empty-pos-base)
                           first-non-empty-my-piece))
     (define start-pos (make-pos
                        (cond [(>= difference 0) difference]
                              [else 0])
                        (pos-y first-empty-pos-base)))
     (define (add-pos pos1)
       (make-pos (+ (pos-x pos1) (pos-x start-pos))
                 (+ (pos-y pos1) (pos-y start-pos))))
     (define old-pos
       (all-positions (length (first my-piece)) (length my-piece)))
     (define new-pos
       (map add-pos old-pos))
     (define (valid-move? base my-piece new-pos old-pos)
       (cond [(empty? new-pos) true]
             [(empty? (locate base (first new-pos))) false]
             [(or (char=? #\. (locate base (first new-pos)))
                  (and (not (char=? #\. (locate base (first new-pos))))
                       (char=? #\. (locate my-piece (first old-pos)))))
              (valid-move? base my-piece (rest new-pos) (rest old-pos))]
             [else false]))]
    (cond [(valid-move? base my-piece new-pos old-pos)
           (list (superimpose base my-piece start-pos))]
          [else empty])))


(define (neighbours my-state) 
  (local [(define base (state-puzzle my-state))
          (define lop (state-pieces my-state))
          (define (all-possible-state base my-piece)
            (foldr (lambda (x y) (append (neighbours-single base x) y))
                   empty (all-orientations my-piece)))]
    (foldr (lambda (x y) (append (all-possible-state base x) y))
           empty lop)))


(define (find-first-non-empty loc)
       (cond [(empty? loc) 0]
             [(not (char=? #\. (first loc))) 0]
             [else (add1 (find-first-non-empty (rest loc)))]))


(neighbours (make-state (list (list #\a #\a #\. #\.)
                              (list #\a #\a #\. #\.)
                              (list #\a #\a #\. #\.))
                        (list (list (list #\b #\b)
                                    (list #\b #\.)))))


> (neighbours (make-state (list (list #\a #\a #\. #\.)
                                (list #\a #\a #\. #\a)
                                (list #\a #\. #\. #\a))
                        (list (list (list #\b #\b #\.)
                                    (list #\. #\b #\.)
                                    (list #\. #\b #\b)))))  

                                    