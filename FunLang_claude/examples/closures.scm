; Замыкания и каррирование

; Функция, возвращающая функцию (каррирование)
(define make-adder
  (lambda (n)
    (lambda (x) (+ n x))))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(display (add5 3))
(newline)
(display (add10 7))
(newline)

; Композиция функций
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define double (lambda (x) (* x 2)))
(define inc (lambda (x) (+ x 1)))

(define double-then-inc (compose inc double))
(display (double-then-inc 5))
(newline)
