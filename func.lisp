;; non-nagative only
(defun integer->bit-vector (integer divds)
  "Create a bit-vector from a positive integer."
  ;; up overflow
  (if  (/= 0 divds)
       (if (>= integer (expt 2 divds))
          (progn
            (format t "OVERFLOW <integer->bit-vector> ~A~%" divds )
            (return-from integer->bit-vector))))
  (if (/= 0 divds)
      (if (>= integer (expt 2 divds))
          (progn
            (format t "non-nagative only <integer->bit-vector>~%")
            (return-from integer->bit-vector))))
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (let ((bit-vector
           (coerce (integer->bit-list integer) 'bit-vector))
          (vector (make-array 32 :adjustable T
                              :element-type 'bit
                              :fill-pointer 0)))
      (if (/= 0 divds)
          (loop for i below (- divds (length bit-vector))
             do(vector-push-extend 0 vector)))
      (loop for bit across bit-vector
           do(vector-push-extend bit vector))
      vector)
    ))
;; integer->bit-vector
;; bit-vector->integer
(defun test-integer->bit-vector()
  (loop for n in (list 0 11 (expt 2 32) -1)
       do(format t "~A  ~A " (integer->bit-vector n 32)
               (length  (integer->bit-vector n 32)
                        ))
       (format t "~A ~%" (bit-vector->integer (integer->bit-vector n 32)))
       ))

(defun bit-vector->integer (bits)
  (reduce #'(lambda (a b) (+ (ash a 1) b)) bits))

(defun test-bignum(n)
  (if (= 1 n)
      1
      (* n (test-bignum (- n 1)))
      )
  )

;reverse list: l
(defun rev(l)
  (let ((newl nil))
    (loop for elm in l
         do
         (push elm newl))
    newl))

;; to get debug info
(defun start-debug()
  (setf *debug* T)
  (setf *decrypt* T))
(defun stop-debug()
  (setf *debug* nil)
  (setf *decrypt* nil))

(defun vector->list(v)
  (loop for e across v
       collect e))
(defun gen-file-name (pre params tail)
  (let ((new-name (make-array 0 :adjustable T
                              :fill-pointer 0
                              :element-type 'base-char)))
    (with-output-to-string (s new-name)
      (format s "~a" pre)
      (loop for n in params 
         do(format s "_~a" n))
      (format s "~a" tail))
    ;;(format t "~A~%" new-name)
    new-name))
