#lang racket
(provide (all-defined-out))
(define (check_bst tree)
  (let ([root (car tree)]
        [left (car(cdr tree))]
        [right (car(cdr(cdr tree)))])
    (if (and (null? left) (null? right))
        #t
        (if (and (null? left) (not (null? right)))
            (< root (car right))
            (if (and (null? right) (not (null? left)))
                (> root (car left))
                (if (or (< root (car left)) (> root (car right)))
                    #f
                    (and (check_bst left)(check_bst right))))))))

(define (apply f bst)
  (let ([root (car bst)]
        [left (car(cdr bst))]
        [right (car(cdr(cdr bst)))])
    (if (null? root)
        '()
        (list (f root) (apply f left) (apply f right)))))

(define (addlist bst)
  (let ([root (car bst)]
        [left (car(cdr bst))]
        [right (car(cdr(cdr bst)))])
    (if (null? root)
        '()
        (if (and (null? left) (null? right))
            (list root)
            (if (and (null? right) (not (null? left)))
                (append (list root) (addlist left))
                (if (and (not(null? right)) (null? left))
                    (append (list root) (addlist right))
                    (append (list root) (addlist left) (addlist right))))))))

(define (equals bef aft)
  (let ([left (addlist bef)]
        [right (addlist aft)])
    (if (not(= (length left) (length right)))
        #f
        (for/and ([i left])
          (for/or ([j right])
            (if (= i j)
                #t
                #f))))))
