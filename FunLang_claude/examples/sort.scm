; Сортировка вставками

(define insert
  (lambda (x xs)
    (if (null? xs)
      (list x)
      (if (<= x (car xs))
        (cons x xs)
        (cons (car xs) (insert x (cdr xs)))))))

(define isort
  (lambda (xs)
    (if (null? xs)
      (list)
      (insert (car xs) (isort (cdr xs))))))

(display (isort (list 5 3 8 1 9 2 7 4 6)))
(newline)
