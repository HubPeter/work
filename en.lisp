(defvar *x0* 0.3010198)  ; xn: (0, 1) 
(defvar *alpha* 3.5946) ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *plainfile* "plain.bmp")
(defvar *ciperfile* "ciper.bmp")
(defvar *defile* "de.bmp")
(defvar *decode*)
(defvar *xn*)

(defun main()
  (setf *decode* nil)
  (format t "Encryption~%")
  (bmp-encry)
  (setf *decode* T)
  (format t "De-cryption~%")
  (bmp-encry)
  (format t "Complete ")
  )

(defun bmp-encry ()
  ;encryption the file
  (setf *xn* *x0*)
  (format t "*Xn* ~a *x0* ~a~%" *xn* *x0*)
  (let* ((content nil)
         (curfile *ciperfile*) )
    (if *decode*
        (setf content (read-bmp *ciperfile*))
        (setf content (read-bmp *plainfile*)))
    ;(format t "bytes read: ~a~%" (length content))
    (loop for i from 1 to 2000
       do( loop-next ))
    (if *decode*
        (setf curfile *defile*)
        (setf curfile *ciperfile*))
    (with-open-file (stream curfile :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (let* ((i 0) (x1* 0))
        (loop for ciperbyte in content
           do
             (loop-next)
             (setf x1* (mod (f246 *xn*) 256))
                                        ;(if (< i 54)
                                        ;    (progn
                                        ;      (write-byte (expand-byte ciperbyte) stream)
                                        ;      (format t "~a --> ~a ~%" ciperbyte (expand-byte ciperbyte))
                                        ;      )
             (write-byte (boole boole-xor x1* ciperbyte) stream)
             (if (< i 4)
                 (format t "plainbyte ~a ciperbyte ~a ~%" ciperbyte (boole boole-xor x1* ciperbyte)))
                                        ;    )
             (incf i)
             )
        )
      )
    )
  )

(defun read-bmp(filename)
  (let* ((in (open filename :element-type '(unsigned-byte 8)
                   :if-does-not-exist nil))
         (byte-list nil)
         (byte-final nil)
         (i 0))
     (when in
       (loop for byte = (read-byte in nil)
          while byte do
            (push byte byte-list)
            (if (< i 4)
                (format t "byte: ~a ~%" byte))
            (incf i)
           )
       (close in))
     (loop for byte in byte-list
          do(push byte byte-final))
     byte-final)
)
(defun expand-byte(byte)
  (bit-vector->integer
   (expand-bit-array
    (integer->bit-vector byte))))
(defun int-int-xor (a b)
  ;(format t "a:~a b:~a~%" a b)
  (bit-vector->integer
   (bit-xor ( adjust-array (integer->bit-vector a) 8 :element-type 'bit)
            ( adjust-array (integer->bit-vector b) 8 :element-type 'bit) )))

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
  (+ (* (truncate (mod (* xi 100) 10)) 100) ;2
     (* (truncate (mod (* xi 1000) 10)) 10) ;3
     (* (truncate (mod (* xi 10000) 10)) 1) ;4
     ))
    
(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  )
