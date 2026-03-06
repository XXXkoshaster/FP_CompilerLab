; Операции со списками

; map — применить функцию к каждому элементу
(define map
  (lambda (f xs)
    (if (null? xs)
      (list)
      (cons (f (car xs)) (map f (cdr xs))))))

; filter — оставить элементы, для которых функция возвращает истину
(define filter
  (lambda (f xs)
    (if (null? xs)
      (list)
      (if (f (car xs))
        (cons (car xs) (filter f (cdr xs)))
        (filter f (cdr xs))))))

; fold — свёртка списка
(define fold
  (lambda (f acc xs)
    (if (null? xs)
      acc
      (fold f (f acc (car xs)) (cdr xs)))))

; reverse — перевернуть список
(define reverse
  (lambda (xs)
    (fold (lambda (acc x) (cons x acc)) (list) xs)))

; Примеры
(define nums (list 1 2 3 4 5))

(display (map (lambda (x) (* x x)) nums))
(newline)

(display (filter (lambda (x) (> x 2)) nums))
(newline)

(display (fold (lambda (acc x) (+ acc x)) 0 nums))
(newline)

(display (reverse nums))
(newline)
