; Ввод-вывод файлов

; Записать в файл
(write-file "test_output.txt" "Hello from Scheme!\nSecond line.")
(display "Файл записан")
(newline)

; Проверить существование
(display (file-exists? "test_output.txt"))
(newline)

; Прочитать обратно
(define content (read-file "test_output.txt"))
(display content)
(newline)

; Дописать в конец
(append-file "test_output.txt" "\nThird line.")
(display (read-file "test_output.txt"))
(newline)
