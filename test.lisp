(defun get-best()
  (let ((min 1000000)
        (plain-size 0)
        (plainimage "test.jpg")
        (pre "c_test"))
    (with-open-file (plain plainimage
                           :direction :input)
      (setf plain-size (file-length plain)))
    (loop for topM from 5 to 100
       do(let ((*topM* topM))
           (format t "  topM: ~A~%" *topM*)
           (loop for max-itrs from 16 to 17
              do (let ((*MAX-itrs* max-itrs)
                       (ciper-size nil)
                       (ciperimage
                        (gen-file-name pre
                                       (list topM
                                             MAX-itrs) ".jpg")))
                   (encrypt plainimage ciperimage)
                   (with-open-file (ciper ciperimage
                                          :direction :input)
                     (setf ciper-size (file-length ciper)))
                   (if (< ciper-size plain-size)
                       (format t "++++")
                       (format t "    "))
                   (if (< ciper-size min)
                       (setf min ciper-size))
                   (format t "~A  ~A:  ~A --> ~A~%" min
                           *MAX-itrs*
                           plain-size ciper-size)))))))

(defun test-all()
  (loop for plainiamge in (list "p_girl.jpg"
                                "p_cow.jpg"
                                "p_hub.jpg")
     for pre in (list "p_girl" 
                      "p_cow"
                      "p_hub")
     do (get-best plainimage pre)))

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

;; get-pix-list  : not sure
;; make-ciper-iamge: not pass
;; note:  use clpython
(defun test-get-pix-list()
  (make-ciper-image "p_girl.jpg" "test_girl.jpg" (get-pix-list "p_girl.jpg")))

;; scan-sort-desc : pass
;; map-pixvalue-on-phase-space : not sure
(defun test-scan-sort-desc()
  (let ((v-p-array nil)
        (field-pixvalue-map nil))
    (setf *plain-list* (get-pix-list "p_girl.jpg"))
    (setf v-p-array
          (scan-sort-desc *plain-list*))
    (setf field-pixvalue-map (map-pixvalue-on-phase-space v-p-array))
    (print "old v-p-array~%")
    ;;(print (subseq *plain-list* 0 20))
    ;;(print (subseq v-p-array 0 20))
    (print field-pixvalue-map)
    (if (/= 78 (elt *plain-list* 0))
        (progn
          (print "/=")
          (setf (elt *plain-list* 0) 78)))
    (setf v-p-array
          (scan-sort-desc *plain-list*))
    (setf field-pixvalue-map (map-pixvalue-on-phase-space v-p-array))
    (print "new v-p-array~%")
    ;;(print (subseq *plain-list* 0 20))
    ;;(print (subseq v-p-array 0 20))
    (print field-pixvalue-map)
    nil))

;; : all encoded is in topM or 0
(defun test-huf-encode(int-seq-cleared v-p-array)
  (format t "test-huf-encode~%")
  (loop for e across int-seq-cleared
     do(if (not (if-in-topM v-p-array *topM* e))
           (progn
             nil)
           (progn
             (if (/= e 0)
                 (format t "    ERROr huf-encoded~%"))))))
;; decode int-seqencoded with huf-tree 
(defun test-huf-encoded-by-decode (huf-tree encoded-int-seq int-seq)
  (let ((de-int-seq (make-array 0 :adjustable T
                                :element-type '(unsigned-byte)
                                :fill-pointer 0)))
    (loop for code across encoded-int-seq
       do(let ((elem (get-element-by-code huf-tree code)))
           (if elem
               (vector-push-extend elem de-int-seq)
               (progn
                 (vector-push-extend (bit-vector->integer code)
                                     de-int-seq)
                 (format t "~A ~A ~%" elem code)
                 ;; check if all not in topM
                 ;;(print-huffman-code-table  huf-tree)
                 ;;(return-from test-huf-encoded-by-decode)
                 ))))
    (print (subseq int-seq 0 100))
    (print (subseq encoded-int-seq 0 100))
    (print (subseq de-int-seq 0 100))
    (if (not (seq-equal int-seq de-int-seq))
        (format t "~%Decode Failed~%")
        (print "Decode SUCCESSFULLY~%"))))

;; get element by code
(defun get-element-by-code(huf-tree code)
  (loop for node being each hash-value of huf-tree
     do(if (seq-equal code
                      (huffman-node-encoding node))
           (return-from get-element-by-code
             (huffman-node-element node))))
  nil)

;; encrypt-plain-text
(defun test-encrypt-plain-text()
  (let ((v-p-array nil)
        (int-seq nil)
        (mask-seq nil)
        (counter 1)
        (field-pixvalue-map nil))
    (setf *plain-list* (get-pix-list "p_girl.jpg"))
    (setf v-p-array
          (scan-sort-desc *plain-list*))
    (setf field-pixvalue-map (map-pixvalue-on-phase-space v-p-array))
    (multiple-value-bind (int-seq mask-seq)
        (encrypt-plain-text *plain-list* v-p-array field-pixvalue-map)
      (loop for i below (length int-seq)
           do(if (= 0 (elt int-seq i))
                 (incf i)
                 (progn
                   (incf counter)
                   (print (elt int-seq i))))))
    ;;(print v-p-array)
    (print counter)
    (print (sumvector v-p-array)))
    nil)

;; get cum of *topM* of v-p-array
(defun sumvector(v)
  (let ((sum 0))
    (loop for n across v
         for i below *topM*
       do (incf sum (elt n 0)))
    sum))

;; mask-int
(defun test-mask-int-with-unmask(ciper-bit-seq int-bit-seq
                                 mask-seq c--1 ciper-block-count)
  (let ((de-int-bit-seq nil))
    (setf de-int-bit-seq
          (ciper-bit->int-bit ciper-bit-seq c--1 ciper-block-count))
    ;;(print (subseq int-bit-seq 0 400))
    ;;(print (subseq ciper-bit-seq 0 400))
    (multiple-value-bind (begin max-length)
        (find-max-match int-bit-seq 
                        (subseq de-int-bit-seq 100 20000))
      (format t "begin ~A  max-length ~A~%" begin max-length))
    (format t "~%")))
