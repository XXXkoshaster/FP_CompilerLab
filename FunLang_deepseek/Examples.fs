module FunLang.Examples

let factorialProgram = """
(letrec ((fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1)))))))
  (fact 5))
"""

let fibonacciProgram = """
(letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (fib 10))
"""

let listProgram = """
(let ((lst (list 1 2 3 4 5)))
  (car (cdr lst)))
"""

let mapProgram = """
(letrec ((map (lambda (f lst)
                (if (null? lst)
                    ()
                    (cons (f (car lst))
                          (map f (cdr lst)))))))
  (map (lambda (x) (* x x)) (list 1 2 3 4 5)))
"""