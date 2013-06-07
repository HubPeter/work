;; bignum
;; decrypt
(ql:quickload "opticl")
;;(in-package :opticl)
(use-package :opticl)
;;(use-package :common-lisp-user)
(load "huffman.lisp")
(load "func.lisp")
(load "test.lisp")
(load "compute.lisp")
(load "lcs.lisp")
(defvar *x0* 0.3388)                ; xn: (0, 1)
(defvar *alpha* 3.9999991)       ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *N* 1024)
(defvar *xn* *x0*)
(defvar *topM* 100)                  ; top M in [0, 255]
(defvar *MAX-itrs* 50)              ; max length of search mode
;; plain-list 
(defvar *plain-list* nil)
;; ciper-list
(defvar *ciper-list* nil)
;; de-list
(defvar *de-list* nil)

(defvar *debug* nil)
(defvar *decrypt* nil)

(defun main()
  (FORMAT T "ENCRYPTION....~%")
  (multiple-value-bind (huf-tree field-pixvalue-map)
      (ENCRYPT "p_wei2.jpg" "c_wei2.jpg")
    (FORMAT T "DE CRYPTION......~%")
    (DECRYPT huf-tree field-pixvalue-map "c_wei2.jpg" "de_wei2.jpg")
    #|
    (print "plain-list:")
    (print (subseq *plain-list* 0 100))
    (print "ciper-list:")
    (print (subseq *ciper-list* 0 100))
    (print "de-list:")
    (print (subseq *de-list* 0 100))
    |#
    nil))

(DEFUN INITVAR()
  (format t "initvar...~%")
  (SETF *xn* *X0*))

(DEFUN DECRYPT (huf-tree field-pixvalue-map ciperimage deimage)
  (INITVAR)
  ;; unmask the ciper text sequence with mask-seq
  (let ((pix-list nil)
        (ciper-bit-seq nil)
        (plain-list nil)
        (int-bit-seq nil)
        (int-seq nil)
        (c--1 (gen-init-block))
        (length-of-ciper-bit-seq nil)
        (ciper-block-count nil))
    (if *debug*
        (progn
          (setf pix-list (get-pix-list ciperimage)))
        (progn
          (setf pix-list *ciper-list*)))
    ;; get ciper-bit-seq from pix-list
    (setf ciper-bit-seq (pix-list->ciper-bit pix-list))
    (setf length-of-ciper-bit-seq (length ciper-bit-seq))
    (setf ciper-block-count (ceiling (/ length-of-ciper-bit-seq 32)))
    ;; get int-bit-seq from ciper-bit-seq
    ;;(format t "ciper-bit->int-bit...~%")
    (format t "")
    (setf int-bit-seq
          (ciper-bit->int-bit ciper-bit-seq c--1 ciper-block-count))
    ;; decode with hufman tree
    ;;(format t "decode-with-huffman...~%")
    ;;   A problem
    (setf int-seq (decode-with-huffman int-bit-seq huf-tree))
                                        ; test point
    (test-decode-with-huffman int-bit-seq int-seq) ;; pass until here
    ;; scan the int-seq
    ;;(format t "int-seq->plain-list...~%")
    (setf plain-list (int-seq->plain-list int-seq field-pixvalue-map))
    ;; make ciper image
    ;;(format t "make-ciper-image...~%")
    (make-ciper-image ciperimage deimage plain-list)
    (setf *de-list* plain-list)))

(defun encrypt-plain-text (pix-list v-p-array field-pixvalue-map)
  (let ((mask-seq (make-array 0 :adjustable T
                              :element-type 'bit
                              :fill-pointer 0))
        (int-seq (make-array 0 :adjustable T
                             :element-type 'INTEGER
                             :fill-pointer 0)))
    (loop for pix-value across pix-list
       do(if (if-in-topM v-p-array *topM* pix-value)
             (progn
               ;;---find length of iteration to target field
               (multiple-value-bind (itr-length temp-mask-bits)
                   (loop-to-target field-pixvalue-map pix-value)
                 (let ((pre-xn *xn*))
                   (if itr-length
                       (progn
                         (loop for bit across temp-mask-bits
                            do(vector-push-extend bit mask-seq))
                         ;;------search mode
                         (vector-push-extend itr-length int-seq))
                       (progn
                         (setf *xn* pre-xn)
                         ;;------mask mode
                         (loop for bit in (loop-next);iterate once
                            do(vector-push-extend bit mask-seq))
                         (vector-push-extend 0 int-seq)
                         (vector-push-extend pix-value int-seq))))))
             (progn
               ;;------mask mode
               (loop for bit in (loop-next);iterate logistic map once
                  do (vector-push-extend bit mask-seq))
               (vector-push-extend 0 int-seq)
               (vector-push-extend pix-value int-seq))))
    (values int-seq mask-seq)
    ))

;; int-seq gen here
(defun pix-list-encrypt(pix-list)
  (let ((v-p-array nil)
        (field-pixvalue-map (make-array *N*))
        (int-seq nil);intermediate swquence
        (encoded-int-seq nil)
        (mask-seq (make-array 0 :adjustable T
                              :element-type 'bit
                              :fill-pointer 0)))
    ;;step 1: scan and sort desc
    (setf v-p-array (scan-sort-desc pix-list))
    ;;step 2: map pixvalue on phase space
    (setf field-pixvalue-map (map-pixvalue-on-phase-space v-p-array))
    ;;step 3: encrypt each plain text
    (multiple-value-bind (int-seq mask-seq)
        (encrypt-plain-text pix-list v-p-array field-pixvalue-map)
      ;;step 4: Huffman tree from int-seq
      (multiple-value-bind (huf-tree encoded-int-seq)
          (huf-encode int-seq)
        (store-int-seq int-seq);; debug
        ;;step 5: mask int with mask-seq
        (values field-pixvalue-map huf-tree
                (mask-int encoded-int-seq mask-seq))))))

(DEFUN ENCRYPT (plainimage ciperimage)
  (INITVAR)
  ;;SYMBOL NUMBER <SORT DESC>  M SYMBOLS
  ;;DIVID PHASE SPACE INTO N AND GET CODEBOOK
  ;; order R G B
  (let ((all-pix-list (get-pix-list plainimage))
        (field-pixvalue-map nil)
        (jpeg-array (make-array 0 :fill-pointer 0
                                :element-type '(unsigned-byte)
                                :adjustable T)))
    ;; store plain-lsit
    (setf *plain-list* all-pix-list)
    (with-open-file (out ciperimage :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (multiple-value-bind (field-pixvalue-map
                            huf-tree ciper-list)
          (pix-list-encrypt all-pix-list)
        ;; code-list --> jpeg-array
        (loop for int across ciper-list
           do(vector-push-extend int jpeg-array))
        (make-ciper-image plainimage ciperimage jpeg-array)
        (setf *ciper-list* jpeg-array)
        (values huf-tree field-pixvalue-map)))))

(defun int-seq->plain-list (int-seq field-pixvalue-map)
  (let ((plain-list (make-array 0 :adjustable T
                                :element-type '(unsigned-byte)
                                :fill-pointer 0))
        (length-of-int-seq (length int-seq)))
    (loop for i below length-of-int-seq
       do (let ((value (elt int-seq i)))
            (if (= value 0 )
                ;; mask mode
                (progn
                  ;; loop once
                  (incf i)
                  (if (< i length-of-int-seq)
                      (vector-push-extend (elt int-seq i) plain-list)))
                ;; search mode
                (progn
                  ;; loop value times
                  (loop for i below value
                     do(loop-next))
                  ;; loop up the field-pixvalue-map
                  (vector-push-extend
                   (elt field-pixvalue-map
                        (truncate (* *xn* *N*)))
                   plain-list)))))
    plain-list))

(defun decode-with-huffman (int-bit-seq huf-tree)
  (let ((int-seq (make-array 0 :adjustable T
                             :element-type '(unsigned-byte)
                             :fill-pointer 0))
        (max-length 100)
        (begin 0) ;; >= begin
        (pre-0 nil)
        (value nil)
        (length-of-int-bit-seq (length int-bit-seq))
        (next-code nil)
        (progress 0))
    (loop for i from 1
         ;; begin is safe
       always begin
       never (>= begin (- length-of-int-bit-seq 1))
       do
         #|(let ((new-progress (truncate
                              (* 100 (/ begin length-of-int-bit-seq)))))
           (if (> new-progress progress)
               (progn (setf progress new-progress)
                 ;;(format t " ~A % ~C~%" progress #\return) ;;debug
                 )))|#
         (multiple-value-bind (new-value new-begin new-pre-0)
             (find-next huf-tree int-bit-seq begin pre-0)
           (setf value new-value)
           (setf begin new-begin)
           (setf pre-0 new-pre-0)
           ;; check if subseq is in huf-tree
           ;; get pix-value and jump pix-value
           (if value
               (progn
                 (vector-push-extend value int-seq))
               (progn
                 (format t "Not found ~%" )))
           ;; update begin
           (setf begin new-begin)))
    int-seq))

(defun find-next(huf-tree int-bit-seq begin pre-0)
  (let ((value nil)
        (new-begin nil)
        (max-length 100)
        (new-pre-0 nil)
        (length-of-int-bit-seq (length int-bit-seq)))
    (loop for length from 1 to (min max-length
                                    (- length-of-int-bit-seq begin 1))
       do(let ((temp-vector
                (subseq int-bit-seq begin (+ begin length))))
           (if pre-0
               (progn
                 (setf value
                       (bit-vector->integer
                        (subseq int-bit-seq
                                begin (+ begin 8))))
                 (setf new-begin (+ begin 8))
                 (return-from find-next
                   (values value new-begin new-pre-0)))
               (progn
                 (loop for node being each hash-value of huf-tree
                    do(if (seq-equal temp-vector
                                     (huffman-node-encoding node))
                          (progn
                            (setf value (huffman-node-element node))
                            (setf new-begin (+ begin length))
                            (if (= 0 value)
                                (setf new-pre-0 T))
                            (return-from find-next
                              (values value new-begin new-pre-0)))))))))))
;; loop-next 4 times
(defun get-32-mask()
  (let ((mask-seq (make-array 0 :adjustable T
                              :element-type 'bit
                              :fill-pointer 0)))
    (loop for i below 4
         do(loop for bit in (loop-next)
              do(vector-push-extend bit mask-seq)))
    mask-seq))
(defun ciper-bit->int-bit (ciper-bit-seq c--1 ciper-block-count)
  (let ((int-bit-seq (make-array 0 :adjustable T
                                 :element-type 'bit
                                 :fill-pointer 0))
        (c-i-1 c--1)
        (c-i nil)
        (r-i+1 nil)
        (m-i nil)
        (length-of-ciper-bit-seq (length ciper-bit-seq)))
    (loop for c-index below ciper-block-count
       do;; get c-i
         (let* ((begin (* 32 c-index))
                (end (min (+ 32 begin)
                          length-of-ciper-bit-seq)))
           (setf c-i (subseq ciper-bit-seq begin end)))
         ;; get m-i
         (setf m-i (get-32-mask))
         ;; compute r-i+1
         (setf r-i+1
               (integer->bit-vector
                (mod (- (bit-vector->integer c-i)
                        (bit-vector->integer c-i-1))
                     (expt 2 32))
                32))
         ;; push r-i+1 to int-seq
         (loop for bit across r-i+1
            do (vector-push-extend bit int-bit-seq))
         ;; update c-i-1
         (setf c-i-1 c-i))
    (loop for bit across c--1
         do(vector-push-extend bit int-bit-seq))
    int-bit-seq))

(defun test-find-max-match()
  (find-max-match #*111100110011001101110011 #*1101)
  )

(defun pix-list->ciper-bit (pix-list)
  (let ((ciper-bit-seq (make-array 0 :adjustable T
                                   :element-type 'bit
                                   :fill-pointer 0)))
    (loop for byte across pix-list
       do(loop for bit across (integer->bit-vector byte 8)
            do(vector-push-extend bit ciper-bit-seq)))
    ciper-bit-seq))

(defun seq-equal(s1 s2)
    (if (/= (length s1) (length s2))
        (return-from seq-equal nil))
    (loop for bit across s1
         for bit2 across s2
         do(if (/= bit bit2)
               (return-from seq-equal nil)))
    T)

(defun mask-int (encoded-int-seq mask-seq)
  (let* ((ciper-byte-seq nil)
         (ciper-bit-seq (make-array 0 :adjustable T
                                    :fill-pointer 0
                                   :element-type 'bit))
         (encoded-int-bit-seq (make-array 0 :adjustable T
                                  :fill-pointer 0
                                :element-type 'bit))
         (c--1 (gen-init-block))
         (mask-bit-seq mask-seq)
         (c-i-1 c--1)
         (mask-block-count nil)
         (int-block-count nil)
         (length-of-mask-bit-seq (length mask-bit-seq))
         (length-of-int-bit-seq nil))
    ;; sequence encoded-int-seq into bit array
    ;;    to use first block
    (loop for i below 32
       do(vector-push-extend 0 encoded-int-bit-seq))
    (loop for code-elm across encoded-int-seq
       do(loop for bit across code-elm
            do(vector-push-extend bit encoded-int-bit-seq)))
                                        ;test point
    (test-encoded-int-seq->int-bit-seq encoded-int-seq encoded-int-bit-seq)
    (store-int-bit-seq encoded-int-bit-seq) ;; debug
    ;; length-of-int-bit-seq
    (setf length-of-int-bit-seq (length encoded-int-bit-seq))
    (setf int-block-count (ceiling (/ (length encoded-int-bit-seq) 32)))
    ;;    0000000....000  and c-index + 1 will hit the last block
    (decf int-block-count)
    ;; gen new ciper-bit-seq
    (loop for c-index below int-block-count
       do(let ((result-block nil)
               (block1 nil) (block2 nil) (block3 nil))
           ;; block1: r[i+1]
           (let* ((begin (* (+ c-index 1) 32))
                  (end (min
                        (+ begin 32)
                        length-of-int-bit-seq)))  ;; [begin, end)
             (setf block1 (subseq encoded-int-bit-seq begin end)))
           ;; block2: m[m-index]
           (let* ((begin (mod
                          (* 32 (mod (+ (bit-vector->integer c-i-1)
                                        (bit-vector->integer block1))
                                     int-block-count))
                          length-of-mask-bit-seq))
                  (end (min (+ begin 32) length-of-mask-bit-seq)))
             (setf block2 (subseq mask-bit-seq begin end))
             ;; compute result-block
             (setf result-block
                   (integer->bit-vector
                    (mod (+ (bit-vector->integer block1) ;; <= 32 bits
                            ;;(bit-vector->integer block2)
                            (bit-vector->integer c-i-1)) ;; 32 bits
                         (expt 2 32))
                    32))
             (loop for bit across result-block
                do (vector-push-extend bit ciper-bit-seq))
             ;; update c-i-1 before incf c-index and for incfed c-index
             (let* ((begin (* 32 c-index))
                    (end (+ 32 begin)))
               (setf c-i-1 (subseq ciper-bit-seq begin end))))))
                                        ; test point
    (test-int-bit-seq<->ciper-bit-seq encoded-int-bit-seq ciper-bit-seq
                                      c--1)
    ;; gen ciper-byte-seq from ciper-bit-seq
    (setf ciper-byte-seq (bit-vector->byte-vector ciper-bit-seq))
                                        ; test point
    (test-bit-seq->byte-seq ciper-bit-seq ciper-byte-seq)
  ciper-byte-seq))

(defun encode (plainimage encoded)
  (let ((pix-list (get-pix-list plainimage)))
    (make-ciper-image plainimage encoded pix-list)))

(defun make-ciper-image(plainimage ciperimage jpeg-array)
  (let ((p-image (read-jpeg-file plainimage)))
    (with-image-bounds (p-height p-width) p-image
      (let* ((width p-width)
             (height (ceiling (/ (length jpeg-array) 3 width)))
             (c-image nil)
             (pix-count (length jpeg-array)))
        ;; should typecase
        (setf c-image (make-8-bit-rgb-image height width))
        (loop for r from 0 below pix-count by 3
           for g from 1 below pix-count by 3
           for b from 2 below pix-count by 3
           do(setf (pixel c-image
                          (truncate (/ r 3 width))
                          (mod (/ r 3) width))
                   (values (elt jpeg-array r)
                           (elt jpeg-array g)
                           (elt jpeg-array b))))
        (if (and *debug* *decrypt*)
            (write-jpeg-file ciperimage p-image)
            (write-jpeg-file ciperimage c-image))
        ))))

(defun field-map-to-byte-array (field-pix-map)
  (let ((map-array (make-array 0 :adjustable T
                               :element-type 'integer
                               :fill-pointer 0)))
      (loop for index below (length field-pix-map)
         with begin = 0
         with length = 0 ;; first loop is diff : collectin start from 0
           ;; other collections start from -1
         with cur-value = (elt field-pix-map 0)
         do(if (= cur-value
                  (elt field-pix-map index))
               ;; still is current value
               (progn
                 (incf length))
               ;; new value
               (progn
                 (if (> length 255)
                     (format  t "~A length :field-map-to-byre-array~%" 
                              length))
                 (vector-push-extend cur-value map-array)
                 (vector-push-extend length map-array)
                 (setf begin index)
                 (setf length 1)
                 (setf cur-value (elt field-pix-map begin))))
           ;; to save last value-length
           (if (= 1023 index)
               (progn(vector-push-extend cur-value map-array)
                     (vector-push-extend length map-array))))
    map-array))


(defun huf-tree-to-array (huf-tree)
  (let ((huf-tree-array (make-array 0 :adjustable T
                                    :element-type '(unsigned-byte)
                                    :initial-element 0))
        (len-array (len-in-byte (huf-tree-length huf-tree) 2)))
    ;; len-array
    (loop for byte across len-array
       do (vector-push-extend byte huf-tree-array))
    ;; huf-tree
    (loop for node being each hash-value of huf-tree
       do (vector-push-extend
          (huffman-node-element node) huf-tree-array)
         (loop for byte across (bit-array-to-byte-array
                                (huffman-node-encoding node))
            do (vector-push-extend byte huf-tree-array)))
    huf-tree-array))

;; function  v
(defun len-in-byte(len-in-int max)
  (if (> len-in-int (* 250 255))
      (progn
        (format t "big huf-man : ~A~%" len-in-int)
        (return-from len-in-byte nil)))
  (let* ((byte-array (make-array  0 :adjustable T
                                 :fill-pointer 0
                                 :element-type '(unsigned-byte)
                                 :initial-element 0))
         (v (truncate (/ len-in-int 250)))
         (r (- len-in-int (* 250 v))))
    (format t "len-in-int :~A~%" len-in-int)
    (loop for i from 1 to max
         until (= v 0)
       do
         (format t "v r ~a ~A~%" v r)
         (vector-push-extend v byte-array)
         (setf v (truncate (/ r 250)))
         (setf r (- r (* 250 v))))
    (vector-push-extend r byte-array)
    byte-array))
;; function v
(defun len-in-int(len-bytes)
  (let ((len 0)
        (mask 1))
    (loop for byte across len-bytes
         do(setf len (+ (* len 250) byte)))
    len))
(defun test-len-in-byte()
  (loop for x in '(0 1 250 251 1000 1024 199999)
       do(format t "~A-->~A-->~A~%" x (len-in-byte x 2)
                 (len-in-int (len-in-byte x 2))
                 )))

(defun bit-array-to-byte-array(bit-array)
  (let ((byte-array (make-array 0 :adjustable T
                                :element-type '(unsigned-byte)
                                :initial-element 0)))
    (loop for bit across bit-array
         do(vector-push-extend bit byte-array))
    byte-array))
;; pass
(defun bit-vector->byte-vector(ciper-bit-seq)
  (let* ((length-of-ciper-bit-seq (length ciper-bit-seq))
        (block-count (ceiling (/ length-of-ciper-bit-seq 8)))
        (ciper-byte-seq (make-array 0 :adjustable T
                                    :fill-pointer 0
                                    :element-type '(unsigned-byte))))
    (loop for i below block-count
       do(let* ((begin (* i 8))
                (end (min (+ (* i 8) 8)
                          length-of-ciper-bit-seq))
                (divds (- end begin))
                (sub (subseq ciper-bit-seq begin end))
                (int (bit-vector->integer sub))
                (de-sub (integer->bit-vector int divds)))
           (if (not (seq-equal sub de-sub))
               (format t "~A --> ~A -> ~A~%" sub int de-sub))
           (vector-push-extend int ciper-byte-seq)))
    ciper-byte-seq))

(defun gen-init-block()
  (let ((init-block (make-array 0 :adjustable T
                                :fill-pointer 0
                                :element-type 'bit)))
    (loop for i below 4
         do(loop for bit in (least-8-bits *x0*)
              do(vector-push-extend bit init-block)))
    (subseq init-block 0 (length init-block))))
;; tested : pass
(defun huf-encode(int-seq)
  (let ((int-seq-cleared
         (make-array 0 :element-type 'INTEGER
                     :fill-pointer 0
                     :adjustable t
                     :initial-element 0))
        (huf-tree (make-hash-table))
        (encoded-int-seq
         (make-array 0 :element-type 'INTEGER
                     :fill-pointer 0
                     :adjustable t
                     :initial-element 0)))
    ;; get cleared sequence to get huf-tree
    (loop for i below (length int-seq)
         do(let ((value (elt int-seq i)))
             ;; push
             ;; jump the real value
             (vector-push-extend value int-seq-cleared)
             (if (= value 0)
                 (progn
                   (incf i)))))
    (setf huf-tree (huffman-codes int-seq-cleared)) ;build huf-tree
    ;;code int-seq with huf-tree
    (loop for i below (length int-seq)
       do (let* ((value (elt int-seq i))
                (code (get-code-by-elem huf-tree value)))
            (if code
                (vector-push-extend code encoded-int-seq)
                (progn 
                  (vector-push-extend 
                   (integer->bit-vector value 0)
                   encoded-int-seq))
                )))
    (values huf-tree encoded-int-seq)))

;; get element by code
(defun get-code-by-elem(huf-tree elem)
  (loop for node being each hash-value of huf-tree
     do(if (= elem
              (huffman-node-element node))
           (return-from get-code-by-elem
             (huffman-node-encoding node))))
  nil)

(defun loop-to-target(field-pixvalue-map pix-value)
   ;;;return length or nil if bigger than *MAX-len*
  (let ((temp-mask-bits (make-array 0 :adjustable T
                                    :element-type 'bit
                                    :fill-pointer 0)))
    (loop for itr-length from 1 to *MAX-itrs*
       do(multiple-value-bind (result temp-bits)
             (if-hit-sphase *xn* pix-value field-pixvalue-map)
           (loop for bit in temp-bits
                do(vector-push-extend bit temp-mask-bits))
           (if result
               (progn
                 (return-from loop-to-target
                   (values itr-length temp-mask-bits)))))
       finally (return (values nil temp-mask-bits)))))

(defun if-hit-sphase (xn pix-value field-pixvalue-map)
  (let ((index-*xn* (truncate (* xn 1024)))
        (result nil)
        (temp-bits (loop-next)));;logistic map for once
    (if (= pix-value (elt field-pixvalue-map index-*xn*))
        (progn
          T)
        nil)))

(defun huffman-codes (sequence &key (test 'eql))
  (multiple-value-bind
        (nodes tree)
      (huffman-tree sequence :test test)
    (labels ((hc (node length bits)
               (let ((left (huffman-node-left node))
                     (right (huffman-node-right node)))
                 (cond
                   ((and (null left) (null right))
                    (setf (huffman-node-encoding node)
                          (make-array length :element-type 'bit
                                      :initial-contents
                                      (reverse bits))))
                   (t (hc left (1+ length) (list* 0 bits))
                      (hc right (1+ length) (list* 1 bits)))))))
      (hc tree 0 '())
      nodes)))

(defun least-8-bits(x)
  ;;;return least 8 bits of x in list form
  (let ((bit-array (make-array 8 :element-type 'bit 
                               :fill-pointer 0 :initial-element 0))
        (bit 0))
    ;;; make integer
    (loop for i below 20
         do
         (if (= x (truncate x))
             (return))
         (setf x (* x 10)))
    ;;;(format t "x: ~a~%" x)
    ;;; get least 8 bits
    (loop for i below 8
       do(setf bit (mod x 2))
         ;;;(format t "x: ~a bit: ~a~%" x bit)
         (if (= 1 bit)
             (vector-push 1 bit-array)
             (vector-push 0 bit-array))
         (setf x (truncate (/ x 2))))
    (loop for bit across bit-array
       collect bit)))

(defun if-in-topM( v-p-array top-n pix-value)
  (loop for i below top-n
     do(if (= (elt (elt v-p-array i) 1) pix-value)
           (return-from if-in-topM T)))
  nil)
(defun map-pixvalue-on-phase-space(v-p-array)
    (let (
          (sum-topM (cal-topm-sum v-p-array *topM*))        
          (field-pixvalue-map (make-array *N* :element-type 'INTEGER
                                          :initial-element 0))
          (partion-begin 0))
      (loop for j below *topM* ;map
           do
           (let* ((partion-number
                   (+ 1 (truncate
                         (/ (* *N* (elt (elt v-p-array j) 0))
                            sum-topM) )))
                  (pix-value (elt (elt v-p-array j) 1)))
             (loop for partion from partion-begin to
                  (min (- *N* 1) (+ -1  partion-begin partion-number))
                do (setf (aref field-pixvalue-map partion) pix-value))
             (setf partion-begin (+ partion-begin partion-number))
             (setf partion-number
                   (+ 1 (truncate (/
                                   (* *N* (elt (elt v-p-array j) 0))
                                   sum-topM))))))
      field-pixvalue-map))

(defun cal-topm-sum(v-p-array topM)
  (let ((sum 0))
    (loop for i below topM
       summing (elt (elt v-p-array i) 0) into sum
       finally (return sum))))

(defun scan-sort-desc(pix-list)
  (let* ((v-p-array (make-array 0 :element-type 'list
                                :adjustable T
                                :fill-pointer 0)))
    (loop for i below 256
       do(vector-push-extend `(0, i) v-p-array))
    (loop for value across pix-list
       do(incf (elt (elt v-p-array value) 0)))
    (setf v-p-array (sort v-p-array #'> :key 
                          #'(lambda(elm) (elt elm 0))))
    ;;(setf v-p-array (sort v-p-array #'> :key #'car))
    v-p-array))

(defun get-rgb-pix-list(imagefile)
  (let ((img (read-jpeg-file imagefile))
        (r-pix-list (make-array 0 :element-type 'INTEGER
                                :initial-element 0
                                :adjustable T
                                :fill-pointer 0))
        (g-pix-list (make-array 0 :element-type 'INTEGER
                                :initial-element 0
                                :adjustable T
                                :fill-pointer 0))
        (b-pix-list (make-array 0 :element-type 'INTEGER
                                :initial-element 0
                                :adjustable T
                                :fill-pointer 0)))
    (with-image-bounds ( height width) img
      (loop for i below height
         do (loop for j below width
               do (multiple-value-bind (r g b)
                     (pixel img i j)
                   (declare (type (unsigned-byte 8) r g b))
                   (vector-push-extend r r-pix-list)
                   (vector-push-extend g g-pix-list)
                   (vector-push-extend b b-pix-list)))))
    (list r-pix-list g-pix-list b-pix-list)))
;; tested
(defun get-pix-list(imagefile)
  (let ((img (read-jpeg-file imagefile))
        (pix-list (make-array 0 :element-type '(unsigned-byte 8)
                              :initial-element 0
                              :adjustable T
                              :fill-pointer 0)))
    (with-image-bounds ( height width) img
      (loop for i below height
         do (loop for j below width
               do (multiple-value-bind (r g b)
                      (pixel img i j)
                    (declare (type (unsigned-byte 8) r g b))
                    (vector-push-extend r pix-list)
                    (vector-push-extend g pix-list)
                    (vector-push-extend b pix-list)))))
    pix-list))

(defun loop-next()
  ;logstic next value
  (setf *xn* (* *alpha* *xn* (- 1 *xn*)))
  (least-8-bits *xn*))
