(ql:quickload "opticl")
(ql:quickload "flexi-streams")
(use-package :opticl)
(load "test.lisp")
(load "decrypt.lisp")
(load "huffman.lisp")
(load "func.lisp")
(defvar *x0* 0.3388)                ; xn: (0, 1)
(defvar *alpha* 0.3999999991)       ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *N* 1024)
(defvar *xn* nil)
(defvar *topM* 10)                  ; top M in [0, 255]
(defvar *MAX-itrs* 50)              ; max length of search mode

(defun main()
  (FORMAT T "ENCRYPTION....~%")
  (ENCRYPT "p_girl.jpg" "c_girl.jpg")
  (FORMAT T "DE CRYPTION......~%")
  (DECRYPT "C_GIRL.JPG" "DE_GIRL.JPG"))
(DEFUN INITVAR()
  (SETF *XN* *X0*))

(DEFUN ENCRYPT (plainimage ciperimage)
  (INITVAR)
  ;;SYMBOL NUMBER <SORT DESC>  M SYMBOLS
  ;;DIVID PHASE SPACE INTO N AND GET CODEBOOK
  ;; order R G B
  (let ((all-pix-list (get-RGB-pix-list plainimage))
        (field-pixvalue-map nil)
        (jpeg-array (make-array 0 :fill-pointer 0
                                :adjustable T)))
    (with-open-file (out ciperimage :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (loop for pix-list in all-pix-list
         do (multiple-value-bind (field-pixvalue-map 
                                  huf-tree ciper-list)
                (pix-list-encrypt pix-list)
              ;; field-pixvalue-map
              (let ((value-length-array 
                     (field-map-to-byte-array field-pixvalue-map)))
                ;;   count of value-length
                (vector-push-extend (/ (length value-length-array) 2) jpeg-array)
                ;; real value-legnth
                (loop for byte across value-length-array
                   do(vector-push-extend byte jpeg-array)))
              ;;(return-from encrypt)
              ;; huf-tree --> jpeg-array
              (loop for node being each hash-value of huf-tree
                 do (let ((node-element (huffman-node-element node))
                          (node-encoding 
                           (huffman-node-encoding node)))
                      (vector-push-extend node-element jpeg-array)
                      ;; EOF:-
                      (vector-push-extend (char-int #\-) jpeg-array)
                      (loop for bit across node-encoding
                         do (vector-push-extend bit jpeg-array))
                      ;; EOF:-
                      (vector-push-extend (char-int #\-) jpeg-array)))
              ;; code-list --> jpeg-array
              (loop for int across ciper-list
                 do (vector-push-extend int jpeg-array))))
      (make-ciper-image plainimage ciperimage jpeg-array))))
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
                     (format  t "~A length :field-map-to-byre-array~%" length))
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
;; function v
(defun make-ciper-image(plainimage ciperimage jpeg-array)
  (let ((p-image (read-jpeg-file plainimage)))
    (with-image-bounds (p-height p-width) p-image
      (let* ((width p-width)
             (height (ceiling (/ (length jpeg-array) width)))
             (c-image (make-8-bit-rgb-image height width))
             (pix-count (length jpeg-array)))
        (loop for r from 0 below pix-count by 3
           for g from 1 below pix-count by 3
           for b from 2 below pix-count by 3
           do
             (setf (pixel c-image 
                          (truncate (/ r width))
                          (mod r width))
                   (values (elt jpeg-array r)
                           (elt jpeg-array g)
                           (elt jpeg-array b))))
        (write-jpeg-file ciperimage c-image)))))

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
       do
         (vector-push-extend
          (huffman-node-element node) huf-tree-array)
         (loop for byte across (bit-array-to-byte-array
                                (huffman-node-encoding node))
            do (vector-push-extend byte huf-tree-array)))
    huf-tree-array
    ))

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
         do(vector-push-extend bit byte-array)
         )
    byte-array)
  )

;; int-seq gen here
(defun pix-list-encrypt(pix-list)
  (let ((v-p-array nil)
        (field-pixvalue-map (make-array *N*))
        (int-seq nil);intermediate swquence
        (encoded-int-seq-cleared nil)
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
      ;;step 5: mask int with mask-seq
      (multiple-value-bind (huf-tree encoded-int-seq-cleared)
          (huf-encode int-seq)
        (values field-pixvalue-map huf-tree 
                (mask-int encoded-int-seq-cleared mask-seq))
        ))))
(defun mask-int(int-seq mask-seq)
  ;; int-seq is huf-code array, you should sequence it into bit array
  ;; mask-seq is bit-array
  (let* ((ciper-byte-seq (make-array 0 :adjustable T
                                     :fill-pointer 0
                                :element-type '(unsigned-byte)))
         (ciper-bit-seq (make-array 0 :adjustable T
                                   :fill-pointer 0
                                   :element-type 'bit))
         (int-bit-seq (make-array 0 :adjustable T
                                 :fill-pointer 0
                                :element-type 'bit))
         (c--1 (gen-init-block))
         (c-i-1 c--1)
         ;; plaintext length in blocko
         (block-count nil))
    ;; sequence int-seq into bit array
    (loop for code-elm across int-seq
       do(loop for bit across code-elm
            do(vector-push-extend bit int-bit-seq)))
    ;; set block-count
    (setf block-count (ceiling (/ (length int-bit-seq) 32)))
    ;; gen new ciper-bit-seq
    (loop for c-index below block-count
       do
         (let ((result-block nil)
               (block1 nil) (block2 nil) (block3 nil))
           ;; block1: r[i+1]
           (let* ((begin (* (+ c-index 1) 32))
                  (end (+ begin 32)))
             (setf block1 (subseq mask-seq begin end)))
           ;; -- last r[i+1] is c--1
           (if (= c-index block-count)
               (setf block1 c--1))
           ;; block2: m[m-index]
           (let* ((begin 
                   (* 32 (mod (+ (bit-vector->integer c-i-1)
                                 (bit-vector->integer block1)) block-count)))
                  (end (+ begin 32)))
             (setf block2 (subseq int-bit-seq begin end))
             (format t "begin ~%")
             ;; compute result-block
             (setf result-block
                   (integer->bit-vector
                    (mod (+ (bit-vector->integer block1)
                            (bit-vector->integer block2)
                            (bit-vector->integer c-i-1)) 
                         (expt 2 32)) 32))
             (loop for bit across result-block
                do(vector-push-extend bit ciper-bit-seq))
             ;; update c-i-1
             (setf c-i-1 (subseq ciper-bit-seq begin end))
             )))
    ;; gen ciper-byte-seq from ciper-bit-seq
    (loop for i below (- (length ciper-bit-seq) 8) by 8
         do(vector-push-extend
            (reduce #'(lambda(a b)(+ (ash a 1) b))
                    (subseq ciper-bit-seq i (+ i 8)))
            ciper-byte-seq))
    ;; check if there are lease 8 bits
    (if (/= 0 (mod (length ciper-bit-seq) 8))
        (let ((bit-length (length ciper-bit-seq)))
          (vector-push-extend 
           ciper-byte-seq
           (reduce #'(lambda(a b)(+ (ash a 1) b))
                   (subseq ciper-bit-seq
                           (- bit-length 1)
                           (- bit-length 1 (mod bit-length 8)))))))
  ciper-byte-seq))

(defun bit-array-to-int (bits)
  (reduce #'(lambda(a b)(+ (ash a 1) b)) bits))

(defun gen-init-block()
  (let ((init-block (make-array 0 :adjustable T
                                :fill-pointer 0
                                :element-type 'bit)))
    (loop for i below 4
         do(loop for bit in (least-8-bits *x0*)
              do(vector-push-extend bit init-block)))
    (subseq init-block 0 (length init-block))))

(defun huf-encode(int-seq)
  (let ((int-seq-cleared 
         (make-array 0 :element-type 'INTEGER
                     :fill-pointer 0
                     :adjustable t
                     :initial-element 0))
        (huf-tree (make-hash-table))
        (encoded-int-seq-cleared 
         (make-array 0 :element-type 'INTEGER
                     :fill-pointer 0
                     :adjustable t
                     :initial-element 0)))
    ;;remove symbols from int-seq
    (loop for i below (- (length int-seq) 1)
       do
         (let ((item (elt int-seq i)))
           (vector-push-extend item int-seq-cleared)
           (if (= item 0)
               (incf i))))
    (setf huf-tree (huffman-codes int-seq-cleared));build huf-tree
    ;;code int-seq with huf-tree
    (loop for i below (length int-seq-cleared)
       do
         (vector-push-extend
          (huffman-node-encoding
           (gethash (elt int-seq-cleared i) huf-tree))
          encoded-int-seq-cleared))
    (values huf-tree encoded-int-seq-cleared)))
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
      nodes)
    )
  )
(defun encrypt-plain-text(pix-list v-p-array field-pixvalue-map)
  (let ((int-seq (make-array 0 :element-type 'INTEGER
                             :initial-element 0
                             :fill-pointer 0
                             :adjustable t))
        (mask-seq (make-array 0 :adjustable T
                             :element-type 'bit
                             :fill-pointer 0)))
    (loop for pix-value across pix-list
       do
         (if (if-in-topM v-p-array *topM* pix-value)
             (progn
               ;;---find length of iteration to target field
               (multiple-value-bind (itr-length temp-mask-bits)
                   (loop-to-target field-pixvalue-map pix-value)
                 (loop for bit across temp-mask-bits
                    do (vector-push-extend bit mask-seq))
                 (if itr-length
                     (progn
                       ;;------search mode
                       ;;append itr-length to int-seq
                       (vector-push-extend itr-length int-seq)))))
             (progn
               ;;------mask mode
               (loop for bit in (loop-next);iterate logistic map once
                  do (vector-push-extend bit mask-seq))
               ;; the plaintext is directly outputed
               ;;append h0:0 and pix-value to int-seq
               (vector-push-extend (values 0 pix-value) int-seq))))
    (values int-seq mask-seq)))

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
                 (return-from loop-to-target (values itr-length temp-mask-bits)))))
       finally (return (values (+ 1 *max-itrs*) temp-mask-bits)))))

(defun if-hit-sphase (xn pix-value field-pixvalue-map)
  (let ((index-*xn* (truncate (* xn 1024)))
        (result nil)
        (temp-bits (loop-next)));;logistic map for once
        #|
        (format t "index-*xn*:~a pix-value:~a in-map: ~a~%" 
        index-*xn* pix-value 
        (elt field-pixvalue-map index-*xn*))|#
    (if (= pix-value (elt field-pixvalue-map index-*xn*))
        (progn
          T)
        nil)))

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
       do
         (setf bit (mod x 2))
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
                            sum-topM) ))))
             ;(format t "sum-topM ~a ~%" sum-topM)
             (loop for partion from partion-begin to
                  (min 1023 (+ -1  partion-begin partion-number))
                do
                  (setf (aref field-pixvalue-map partion) j))
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
  (let* (;;;(v-p-array 
         ;;;    (make-sequence 'list 256 :initial-element (list 0 0)))
         (v-p-array (make-array 256 :element-type 'list 
                                :fill-pointer 0
                                :initial-element '(0 0))))
    (loop for i below 256
       do(vector-push `(0, i) v-p-array))
    (loop for value across pix-list
       do
         (incf (elt (elt v-p-array value) 0)))
    (setf v-p-array (sort v-p-array #'> :key #'(lambda(elm) (elt elm 0))))
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
               do
                 (multiple-value-bind (r g b)
                     (pixel img i j)
                   (declare (type (unsigned-byte 8) r g b))
                   #|(if (= i 0)
                       (format t "(~a ~a ~a) ~%" r g b))|#
                   (vector-push-extend r r-pix-list)
                   (vector-push-extend g g-pix-list)
                   (vector-push-extend b b-pix-list)))))
    (list r-pix-list g-pix-list b-pix-list)))
;reverse list: l
(defun rev(l)
  (let ((newl nil))
    (loop for elm in l
         do
         (push elm newl))
    newl))
(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  (least-8-bits *xn*))