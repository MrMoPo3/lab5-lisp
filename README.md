<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Левчук Іван Володимирович група КВ-12</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 2 (14)
База даних: Виробництво дронів

Тип записів: Геш-таблиця

Таблиці: Виробники дронів, Дрони
  
## Лістинг реалізації завдання
```lisp
(defun split-string (string delimiter)
  (let ((result '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (progn
               (push (string-trim '(#\Space #\Tab #\Newline) (subseq string start pos)) result)
               (setf start (1+ pos)))
          finally (push (string-trim '(#\Space #\Tab #\Newline) (subseq string start)) result))
    (nreverse result)))

(defun hashtable-to-alist (hashtable)
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hashtable)
    (reverse alist)))

(defun make-line-separator (widths)
  (reduce (lambda (acc width)
            (concatenate 'string acc "+" (make-string (+ width 2) :initial-element #\-)))
          widths
          :initial-value "+"))

(defun make-row (values widths)
  (reduce (lambda (acc pair)
            (destructuring-bind (value width) pair
              (concatenate 'string acc " | " (format nil "~v@<~a~>" width (or value "")))))
          (mapcar #'list values widths)
          :initial-value "|"))

(defun read-csv (file-path &key (separator #\,))
  (let ((rows '()))
    (with-open-file (stream file-path :direction :input)
      (let ((headers (split-string (read-line stream) separator)))
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((values (split-string line separator)))
                   (let ((row (make-hash-table :test 'equal)))
                     (loop for header in headers
                           for value in values
                           do (setf (gethash header row) (string-trim '(#\Space #\Tab #\Newline) value)))
                     (push row rows))))))
    (reverse rows)))

(defun select (file-path &key (separator #\,))
  (let ((rows (read-csv file-path :separator separator)))
    (lambda (&key (filters '()))
      (let ((filtered-rows rows))
        (dolist (filter filters filtered-rows)
          (let ((key (car filter))
                (expected-value (cdr filter)))
            (setf filtered-rows
                  (remove-if-not (lambda (row)
                                   (string= (gethash key row) expected-value))
                                 filtered-rows))))
        filtered-rows))))

(defun write-csv (file-path rows &key (separator #\,))
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((headers (mapcar #'car (hashtable-to-alist (first rows)))))
      (format stream "~{~a~}~%" headers)
      (dolist (row rows)
        (let ((values (mapcar #'cdr (hashtable-to-alist row))))
          (format stream "~{~a~}~%" values))))))


```
  
### Тестові набори та утиліти
```lisp
(defun pretty-print-csv-table (filepath &key (separator #\,))
  (when (probe-file filepath)
    (with-open-file (stream filepath :direction :input)
      (let* ((lines (loop for line = (read-line stream nil nil)
                          while line
                          collect line))
             (headers (split-string (first lines) separator))
             (rows (mapcar (lambda (line) (split-string line separator)) (rest lines)))
             (column-widths (mapcar (lambda (col-index)
                                       (max (length (nth col-index headers))
                                            (loop for row in rows maximize (length (nth col-index row)))))
                                     (loop for i from 0 below (length headers) collect i))))

        (format t "~%~a~%" (make-line-separator column-widths))
        (format t "~a~%" (make-row headers column-widths))
        (format t "~a~%" (make-line-separator column-widths))

        (dolist (row rows)
          (format t "~a~%" (make-row row column-widths)))
        (format t "~a~%" (make-line-separator column-widths))))))

(defun print-hashtable (hashtable)
  (maphash (lambda (key value)
             (format t "~a: ~a~%" key value))
           hashtable))

(defun test-reading-data ()
  (format t "~%Reading all data from manufacturers.csv:~%")
  (pretty-print-csv-table "manufacturers.csv")

  (format t "~%Reading all data from drones.csv:~%")
  (pretty-print-csv-table "drones.csv")

  (format t "~%Filtering drones by Manufacturer = DJI~%")
  (let ((filtered-dji (funcall (select "drones.csv") :filters '(("Manufacturer" . "DJI")))))
    (dolist (row filtered-dji)
      (print-hashtable row)))

  (format t "~%Filtering drones by Flight_range(km) > 5~%")
  (let ((filtered-range (remove-if-not 
                         (lambda (row)
                           (let ((range (gethash "Flight_range(km)" row)))
                             (and range (> (parse-integer range) 5))))
                         (funcall (select "drones.csv")))))
    (dolist (row filtered-range)
      (print-hashtable row))))

(defun test-write-data-to-csv-file ()
  (format t "~%Writing filtered data to output.csv and displaying:~%")
  (let ((filtered-data 
         (funcall (select "drones.csv") :filters '(("Flight_range(km)" . "3")))))
    (when filtered-data
      (write-csv "output.csv" filtered-data)
      (pretty-print-csv-table "output.csv"))))

(defun test-convert-hashtable-to-alist ()
  (format t "~%Testing conversion from hashtable to alist~%")
  (let ((hash (make-hash-table :test 'equal)))
    (setf (gethash "Drone_id" hash) "1")
    (setf (gethash "Drone_name" hash) "Mamont")
    (setf (gethash "Manufacturer" hash) "Escadrone")
    (setf (gethash "Flight_range(km)" hash) "25")
    (format t "Original hash table:~%")
    (print-hashtable hash)
    (format t "Converted alist: ~a~%" (hashtable-to-alist hash))))

(defun test-pretty-printing ()
  (format t "~%Testing pretty printing for drones.csv:~%")
  (pretty-print-csv-table "drones.csv")

  (format t "~%Testing pretty printing for manufacturers.csv:~%")
  (pretty-print-csv-table "manufacturers.csv"))

(defun test-database ()
  (format t "~%Start testing database functions~%")
  (test-reading-data)
  (test-write-data-to-csv-file)
  (test-convert-hashtable-to-alist)
  (test-pretty-printing)
  (format t "~%End of tests~%"))
```
### Вміст тестових файлів №1 ()
```
ManufacturerI_D,Manufacturer_Name
1,DarwinFPV
2,GEPRC
3,iFlight
4,DJI
5,SJRC

```
### Вміст тестових файлів №2 ()
```
Drone_ID,Drone_name,Manufacturer,Flight_range(km)
10, DJI Mavic 3,DJI,15
20,SJRC F5S PRO+,SRJC,3
30,   DarwinFPV X9,DarwinFPV,3
40,MARK4 7,GEPRC,7
50, iFlight Chimera7,iFlight,5

```
### Тестування
```lisp
CL-USER> (test-database)

Start testing database functions

Reading all data from manufacturers.csv:

++-----------------+--------------------
| | ManufacturerI_D | Manufacturer_Name

++-----------------+--------------------
| | 1               | DarwinFPV
        
| | 2               | GEPRC
            
| | 3               | iFlight
          
| | 4               | DJI
              
| | 5               | SJRC
             
++-----------------+--------------------

Reading all data from drones.csv:

++----------+------------------+--------------+-------------------
| | Drone_ID | Drone_name       | Manufacturer | Flight_range(km)

++----------+------------------+--------------+-------------------
| | 10       | DJI Mavic 3      | DJI          | 15
              
| | 20       | SJRC F5S PRO+    | SRJC         | 3
               
| | 30       | DarwinFPV X9     | DarwinFPV    | 3
               
| | 40       | MARK4 7          | GEPRC        | 7
               
| | 50       | iFlight Chimera7 | iFlight      | 5
               
++----------+------------------+--------------+-------------------

Filtering drones by Manufacturer = DJI
Drone_ID: 10
Drone_name: DJI Mavic 3
Manufacturer: DJI
Flight_range(km)
: 15


Filtering drones by Flight_range(km) > 5:

Writing filtered data to output.csv and displaying:

Testing conversion from hashtable to alist
Original hash table:
Drone_id: 1
Drone_name: Mamont
Manufacturer: Escadrone
Flight_range(km): 25
Converted alist: ((Drone_id . 1) (Drone_name . Mamont)
                  (Manufacturer . Escadrone) (Flight_range(km) . 25))

Testing pretty printing for drones.csv:

++----------+------------------+--------------+-------------------
| | Drone_ID | Drone_name       | Manufacturer | Flight_range(km)

++----------+------------------+--------------+-------------------
| | 10       | DJI Mavic 3      | DJI          | 15
              
| | 20       | SJRC F5S PRO+    | SRJC         | 3
               
| | 30       | DarwinFPV X9     | DarwinFPV    | 3
               
| | 40       | MARK4 7          | GEPRC        | 7
               
| | 50       | iFlight Chimera7 | iFlight      | 5
               
++----------+------------------+--------------+-------------------

Testing pretty printing for manufacturers.csv:

++-----------------+--------------------
| | ManufacturerI_D | Manufacturer_Name

++-----------------+--------------------
| | 1               | DarwinFPV
        
| | 2               | GEPRC
            
| | 3               | iFlight
          
| | 4               | DJI
              
| | 5               | SJRC
             
++-----------------+--------------------

End of tests
NIL
```
