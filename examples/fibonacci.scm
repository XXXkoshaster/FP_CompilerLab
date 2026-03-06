; Числа Фибоначчи

(define fib
  (lambda (n)
    (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

; Вывести первые 10 чисел Фибоначчи
(define print-fibs
  (lambda (i n)
    (if (< i n)
      (begin
        (display (fib i))
        (newline)
        (print-fibs (+ i 1) n))
      0)))

(print-fibs 0 10)
