(defvar *x0*) ; xn: (0, 1) 
(defvar *alpha*) ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *xn*)  ;n: 0,1,2,...
(defvar *plainfile*)
(defvar *ciperfile*)
(defvar *defile*)
(defvar *decode* nil)

(defun main()
  (setf *plainfile* "plain.bmp")
  (setf *ciperfile* "ciper.bmp")
  (setf *defile* "de.bmp")
  (setf *x0* 0.301000)
  (setf *alpha* 3.5946)
  (setf *decode* nil)
  (format t "Encryption~%")
  (bmp-encry)
  (setf *decode* T)
  (format t "De-cryption~%")
  (bmp-encry))

(defun bmp-encry ()
  ;encryption the file
  (setf *xn* *x0*)
  (defvar content nil)
  (if *decode*
      (setf content (read-bmp *ciperfile*))
      (setf content (read-bmp *plainfile*)))
  (defvar M 2000)
  (loop for i from 1 to M
       do( loop-next ))
  (defvar curfile *ciperfile*)
  (if *decode*
      (setf curfile *defile*)
      (setf curfile *ciperfile*))
  (with-open-file (stream curfile :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (loop for ciperbyte in content
       do
         (defvar x1*)
         (setf x1* (mod (f246 *xn*) 256))
         (write-byte (int-int-xor x1* ciperbyte) stream)
         (loop-next))))

(defun read-bmp(filename)
  (defvar byte-list nil)
  (let* ((in (open filename :element-type '(unsigned-byte 8)
                   :if-does-not-exist nil)))
     (when in
       (loop for byte = (read-byte in nil)
          while byte do
            (push byte byte-list)
           )
       (close in))
     byte-list)
)

(defun int-int-xor (a b)
  ;(format t "a:~a b:~a~%" a b)
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
  (+ (* (truncate (mod (* xi 10000) 10)) 100) ;4
     (* (truncate (mod (* xi 100000) 10)) 10) ;5
     (* (truncate (mod (* xi 1000000) 10)) 1) ;6
     ))
    
(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  )