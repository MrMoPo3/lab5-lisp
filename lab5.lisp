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
