(defvar *x0* 0.110002 ) ; xn: (0, 1) 
(defvar *alpha*  3.5699456) ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *xn* *x0*)  ;n: 0,1,2,...

(defvar *plainfile* "data.txt")
(defvar *ciperfile* "ciper.txt")
(defvar *plainfile2* "data2.txt")
(defvar *decode* nil)

(defun main()
  ;main function
  (format t "Begin encrypt.....~%")
  (setf *decode* nil)
  (encry)
  ;(setf *decode* T)
  ;(encry)  (format t "Complete "))

(defun encry ()
  ;encryption the file
  (defvar content)
  (setf content "")
  (if *decode*
      (setf content (read-txt *ciperfile*))
      (setf content (read-txt *plainfile*)))
  (format t "~a ~%" *decode*)
  (format t "content: ~a ~%" content)
  (defvar M 2000)
  (loop for i from 1 to M
       do( loop-next ))
  (defvar curfile *ciperfile*)
  (if *decode*
      (setf curfile *plainfile2*)
      (setf curfile *ciperfile*))
  (with-open-file (stream curfile :direction :output
                          :if-exists :supersede)
    (loop for ciperchar in (coerce content 'list)
       do
         ;(format t "~a" ciperchar)
         (defvar x1* (mod (f246 *xn*) 256))
         (format stream "~a" 
                 (int-int-xor x1* (char-int ciperchar)))
         (loop-next))))

(defun int-int-xor (a b)
  (format t "a:~a b:~a~%" a b)
  (bit-vector->integer 
   (bit-xor ( expand-bit-array (integer->bit-vector a))
            ( expand-bit-array (integer->bit-vector b)) )))

(defun expand-bit-array(elm)
  ;(format t "bit array length: ~a~%" (length elm))
  (setf newa (make-array 8 :element-type 'bit :fill-pointer
                         (- 8 (length elm))))
  ;(format t "elm:~a~%" elm)
  (loop for i from 0 to (- (length elm) 1)
     do
       (vector-push (aref elm i) newa)
       ;(format t "~a newa: ~a insert ~a: ~a~%"
               ;elm newa i (aref elm i)))
  ;(format t "type of newa : ~a~%" (type-of newa))
       )
  newa
  )

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun integer->bit-vector (integer)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) 
                        (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'bit-vector)))

(defun f246(xi)
  (+ (* (truncate (mod (* xi 100) 10)) 100)
     (* (truncate (mod (* xi 10000) 10)) 10)
     (* (truncate (mod (* xi 1000000) 10)) 1)
     ))
    
(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*))))

(defun read-txt(filename)
  ;read file content
  (defvar content "")
  (let* ((in (open filename :if-does-not-exist nil)))
     (when in
       (loop for line = (read-line in nil)
          while line do (setf content (concatenate 'string content line)))
       (close in)))
  content
  )