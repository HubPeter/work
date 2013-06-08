(defvar *debug-int-seq* nil)
(defvar *debug-mask-seq* nil)
(defvar *debug-ciper-bit-seq* nil)
(defvar *debug-c--1* nil)
(defvar *debug-encoded-int-seq* nil)
(defvar *debug-int-bit-seq* nil)

(defun get-best()
  (format t "~%")
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
;; get-pix-list  : not sure
;; make-ciper-iamge: not pass
;; note:  use clpython
(defun test-get-pix-list()
  (format t "~%")
  (make-ciper-image "p_girl.jpg" "test_girl.jpg" (get-pix-list "p_girl.jpg")))

;; scan-sort-desc : pass
;; map-pixvalue-on-phase-space : not sure
(defun test-scan-sort-desc()
  (format t "~%")
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
  (format t "~%")
  (format t "test-huf-encode~%")
  (loop for e across int-seq-cleared
     do(if (not (if-in-topM v-p-array *topM* e))
           (progn
             nil)
           (progn
             (if (/= e 0)
                 (format t "    ERROr huf-encoded~%"))))))
;; pass
;; decode int-seqencoded with huf-tree 
(defun test-huf-encoded-by-decode (huf-tree encoded-int-seq int-seq)
  (format t "~%")
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

;; store in dynamtic to debug
(defun store-mask-seq( mask-seq )
  (setf *debug-mask-seq* mask-seq))

(defun store-ciper-bit-seq( ciper-bit-seq )
  (setf *debug-ciper-bit-seq* ciper-bit-seq))

(defun store-c--1( c--1 )
  (setf *debug-c--1* c--1))

(defun store-int-seq( int-seq )
  (setf *debug-int-seq* int-seq))

(defun store-int-bit-seq(int-bit-seq)
  (setf *debug-int-bit-seq* int-bit-seq))

(defun store-encoded-int-seq(encoded-int-seq)
  (setf *debug-encoded-int-seq* encoded-int-seq))
;; encoded-int-seq --> int-bit-seq
(defun test-encoded-int-seq->int-bit-seq(encoded-int-seq int-bit-seq)
  (format t "~%")
  (let ((new-int-bit-seq (make-array 0 :adjustable T
                                     :element-type 'bit
                                     :fill-pointer 0)))
    ;; encoded-int-seq -> int-bit-seq
    (format t "encoded-int-seq -> int-bit-seq~%")
    (loop for i below 32
       do(vector-push-extend 0 new-int-bit-seq))
    (loop for code across encoded-int-seq
         do(loop for bit across code
              do(vector-push-extend bit new-int-bit-seq)))
    (if (not (seq-equal new-int-bit-seq int-bit-seq))
        (format t "  FAILED:~%")
        (format t "  SUCCESS: ~%"))
    ))

;; int-bit-seq --> ciper-bit-seq
(defun test-int-bit-seq<->ciper-bit-seq(int-bit-seq ciper-bit-seq
                                        c--1)
  (format t "~%")
  (let ((de-int-bit-seq nil)
        (ciper-block-count (ceiling (/ (length ciper-bit-seq) 32))))
    (format t "int-bit-seq -> ciper-bit-seq~%")
    ;;  --> length
    (format t "   int-bit-seq-length: ~A~%" (length int-bit-seq))
    (format t "   ciper-bit-seq-length: ~A~%" (length ciper-bit-seq))
    (format t "  length-test~%")
    (if (/= (length int-bit-seq) (length ciper-bit-seq))
        (format t "    FAILED:~%")
        (format t "    SUCCESS:~%"))
    ;;  <--
    (format t "int-bit-seq <- ciper-bit-seq~%")
    (setf de-int-bit-seq
          (ciper-bit->int-bit ciper-bit-seq c--1))
    ;;    length
    (format t "  length~%")
    (format t "   int-bit-seq-length: ~A~%" (length int-bit-seq))
    (format t "   de-int-bit-seq-length: ~A~%" (length de-int-bit-seq))
    (if (/= (length int-bit-seq) (length de-int-bit-seq))
        (format t "  FAILED~%")
        (format t "  SUCCESS~%"))
    ;;    contend
    (format t "  contend equal~%")
    (let* ((subseq-length 2000)
           (lcs
            (lcs-length
             (subseq int-bit-seq 32 (+ 32 subseq-length))
             (subseq de-int-bit-seq 0 (+ 32 subseq-length)))))
      (format t "    lcs: ~A of ~A~%" lcs subseq-length)
      (if (= lcs subseq-length)
          (format t "  SUCCESS~%")
          (progn 
            (format t "  FAILED~%")
            (format t "    my-lcs-length: ~A / ~A ~A ~%"
                    (my-lcs-length int-bit-seq de-int-bit-seq)
                    (length int-bit-seq)
                    (length de-int-bit-seq)))))))
;; p: 32 0000000 lasted in decrypt
(defun test-decode-with-huffman(int-bit-seq int-seq)
  (format t "~%")
  (format t "int-bit-seq -> int-seq~%")
  (format t "  prepare test: ~%")
  (let* ((lcs-test 2000)
         (begin 0)
         (int-bit-seq *debug-int-bit-seq*)
         (result-lcs (lcs-length (subseq int-bit-seq begin (+ begin lcs-test))
                                 (subseq *debug-int-bit-seq*
                                         (+ begin) (+ begin lcs-test)))))
    ;;(format t "~A~%" (subseq *debug-int-bit-seq* 0 32))
    ;;(format t "~A~%" (subseq int-bit-seq 0 32))
    (format t "   lcs-test: ~A / ~A~%" result-lcs lcs-test)
    (if (= lcs-test result-lcs)
        (format t "     SUCCESS ~%")
        (format t "     FAIL ~%"))
    (format t "   my-lcs-length: ~A / ~A ~A~%"
            (my-lcs-length (subseq int-bit-seq 0)
                           (subseq *debug-int-bit-seq* 0))
            (length (subseq int-bit-seq 0))
            (length (subseq *debug-int-bit-seq* 0))))
  (if (not (seq-equal int-bit-seq *debug-int-bit-seq*))
      (format t "   FAIL~%")
      (format t "   SUCCESS~%"))
  (format t "start real test ~%")
  (if (not (seq-equal int-seq *debug-int-seq*))
      (progn
        (format t " my-lcs-length: ~A / ~A ~A~%"
                (my-lcs-length int-seq *debug-int-seq*)
                (length int-seq)
                (length *debug-int-seq*))
        (format t "pre: ~A~%" (subseq *debug-int-seq* 1200 1232))
        (format t "now: ~A~%" (subseq int-seq 1200 1232))
        (let* ((lcs-test 500)
               (begin 1200)
               (result-lcs
                (lcs-length (subseq int-seq begin (+ begin lcs-test))
                            (subseq *debug-int-seq* begin (+ begin lcs-test)))))
          (format t " lcs-test: ~A / ~A ~%" result-lcs lcs-test)
          (if (= lcs-test result-lcs)
              (format t "     SUCCESS ~%")
              (format t "     FAIL~%")))
        (format t "   FAIL~%"))
      (format t "   SUCCESS~%")))

(defun test-bit-seq->byte-seq(bit-seq byte-seq)
  (format t "~%")
  (let ((de-bit-seq nil)
        (de-byte-seq nil))
    (format t "bit-seq --> byte-seq~%")
    (setf de-bit-seq (pix-list->ciper-bit byte-seq))
    (format t "    lcs: ~A~%" (lcs-length
                               (subseq bit-seq 0 1000)
                               (subseq de-bit-seq 0 1000)))
    (format t "  bit-seq: ~A ~%" (length bit-seq))
    (format t "  de-bit-seq: ~A ~%" (length de-bit-seq))
    (if (seq-equal de-bit-seq bit-seq)
        (format t "  SUCCESS~%")
        (format t "  FAIL~%"))
    (format t "byte-seq --> bit-seq~%")
    (setf de-byte-seq (bit-vector->byte-vector bit-seq))
    (format t "    lcs: ~A~%" (lcs-length 
                               (subseq byte-seq 0 500)
                               (subseq de-byte-seq 0 500)))
    (if (seq-equal de-byte-seq byte-seq)
        (format t "  SUCCESS~%")
        (format t "  FAIL~%"))))

(defun my-lcs-length(seq1 seq2)
  (let ((counter 0))
    (loop for e1 across seq1
       for e2 across seq2
       do(if (= e1 e2)
             (incf counter)
             (return-from my-lcs-length counter)))
    counter))

(defun test-mask-seq (mask-seq)
  (format t "~%")
  (format t "test-mask-seq ~%")
  (if (not (seq-equal mask-seq *debug-mask-seq*))
      (progn
        (format t " my-lcs-length: ~A / ~A ~A~%"
                (my-lcs-length mask-seq *debug-mask-seq*)
                (length mask-seq)
                (length *debug-mask-seq*))
        (let* ((lcs-test 1000)
               (begin 0)
               (result-lcs
                (lcs-length (subseq mask-seq begin (+ begin lcs-test))
                            (subseq *debug-mask-seq* begin (+ begin lcs-test)))))
          (format t " lcs-test: ~A / ~A ~%" result-lcs lcs-test)
          (if (= lcs-test result-lcs)
              (format t "     SUCCESS ~%")
              (format t "     FAIL~%")))
        (format t "   FAIL~%"))
      (format t "   SUCCESS~%")))






















#|
(defun test-huf-tree(huf-tree)
  (format t "value -> huf-code -> value~%")
  (loop for value below 256
     do(let* ((encoding (get-encoding-by-value huf-tree value))
              (de-value (get-element-by-encoding huf-tree encoding)))
         (if value
             (if (/= value de-value)
                 (progn
                   (format t "~A -> ~A -> ~A~%" value encoding de-value)))
             (format t "no value with: ~A~%" encoding)))))

(defun get-encoding-by-value (huf-tree value)
  (format t "get-encoding~%")
  (loop for node being each hash-value of huf-tree
     do(if (= value (huffman-node-element node))
           (return-from get-encoding-by-value 
             (huffman-node-encoding node))))
  nil)
(defun get-element-by-encoding (huf-tree encoding)
  (format t "get-value~%")
  (loop for node being each hash-value of huf-tree
     do(if (seq-equal encoding (huffman-node-encoding node))
           (return-from get-element-by-encoding
             (huffman-node-element node))))
  nil)
|#