(ql:quickload "opticl")
(ql:quickload "flexi-streams")
(use-package :opticl)
(defvar *x0* 0.3010198)             ; xn: (0, 1)
(defvar *alpha* 3.5946)             ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *topM* 10)                  ; top M in [0, 255]
(defvar *N* 1024)
(defvar *xn* *x0*)
(defvar *MAX-itrs* 128)                 ; max length of search mode

(defun main()
  (initvar)
  (FORMAT T "ENCRYPTION....~%")
  (ENCRYPT "p_girl.jpg" "c_girl.jpg")
  ;;(FORMAT T "DE CRYPTION......~%")
  ;;(DECRYPT "C_GIRL.JPG" "DE_GIRL.JPG")
  )

(defun gen-file-name (pre params tail)
  (let ((new-name (make-array 0 :adjustable T
                              :fill-pointer 0
                              :element-type 'base-char)))
    (with-output-to-string (s new-name)
      (format s "~a" pre)
      (loop for n in params 
         do(format s "_~a" n))
      (format s "~a" tail))
    new-name))
(defun get-best()
  (loop for topM below 255
       do(let ((*topM* topM)
               (plainimage "p_girl.jpg")
               (ciperimage (gen-file-name
                            "c_girl" (list *topM*) ".jpg"))
               (plain-size nil) (ciper-size nil))
           (ENCRYPT plainimage ciperimage)
           (with-open-file (plain plainimage :direction :input)
             (setf plain-size (file-length plain)))
           (with-open-file (ciper ciperimage :direction :input)
             (setf ciper-size (file-length ciper)))
           (if (< ciper-size plain-size)
               (progn
                 (format t "topM: ~A  ~A --> ~A~%"
                         *topM* plain-size ciper-size)
                 (loop for max-itrs below 128
                    do(let ((*MAX-itrs* max-itrs)
                            (plain-size nil)
                            (ciper-size nil)
                            (ciperimage 
                             (gen-file-name "c_girl" 
                                            (list *topM*) ".jpg")))
                        (ENCRYPT plainimage ciperimage)
                        (with-open-file (plain plainimage 
                                               :direction :input)
                          (setf plain-size (file-length plain)))
                        (with-open-file (ciper ciperimage 
                                               :direction :input)
                          (setf ciper-size (file-length ciper)))
                        (if (< ciper-size plain-size)
                            (format t "     max-itrs: ~A  ~A --> ~A~%"
                                    *MAX-itrs* plain-size ciper-size)))))
               (format t "------topM: ~A  ~A --> ~A~%"
                       *topM* plain-size ciper-size)
               ))))

(DEFUN INITVAR()
  (SETF *XN* *X0*)
  (setf *topM* 10))

(DEFUN ENCRYPT (plainimage ciperimage)
  ;;SYMBOL NUMBER <SORT DESC>  M SYMBOLS
  ;;DIVID PHASE SPACE INTO N AND GET CODEBOOK
  ;; order R G B
  (let ((all-pix-list (get-RGB-pix-list plainimage))
        (jpeg-array (make-array 0 :fill-pointer 0
                                :adjustable T)))
    (with-open-file (out ciperimage :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (loop for pix-list in all-pix-list
         do
           (multiple-value-bind (huf-tree ciper-list)
               (pix-list-encrypt pix-list)
             ;; huf-tree --> jpeg-array
             (loop for node being each hash-value of huf-tree
                do (let ((node-element (huffman-node-element node))
                         (node-encoding (huffman-node-encoding node)))
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

;; function v
(defun make-ciper-image(plainimage ciperimage jpeg-array)
  (let ((p-image (read-jpeg-file plainimage)))
    (with-image-bounds (p-height p-width) p-image
      (let* (
             (width p-width)
             (height (ceiling (/ (length jpeg-array) width)))
             (c-image (make-8-bit-rgb-image height width))
             (pix-count (length jpeg-array))
             )
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
        (write-jpeg-file ciperimage c-image)
        ))))

(defun huf-tree-save(filename huf-tree)
  (format t "~%saving huf-tree ...~%")
  (with-open-file (out filename :direction :output
                       :if-exists :supersede)
      (with-standard-io-syntax
        (print huf-tree out))))
(defun huf-tree-load(filename)
  (format t "reading huf-tree ... ~%")
  (let ((huf-tree nil))
    (with-open-file (in filename)
      (with-standard-io-syntax 
        (setf huf-tree (read in))))
    huf-tree
    ))
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
        (mask-seq nil);masking swquence used in step-5
        (int-seq nil);intermediate swquence
        (encoded-int-seq-cleared nil))
    ;;step 1: scan and sort desc
    (setf v-p-array (scan-sort-desc pix-list))
    ;;step 2: map pixvalue on phase space
    (setf field-pixvalue-map (map-pixvalue-on-phase-space v-p-array))
    ;;step 3: encrypt each plain text
    (multiple-value-bind (mask-seq int-seq)
        (encrypt-plain-text pix-list v-p-array field-pixvalue-map)
      ;;step 4: Huffman tree from int-seq
      ;;step 5: mask int with mask-seq
      (multiple-value-bind (huf-tree encoded-int-seq-cleared)
          (huf-encode int-seq)
        ;;(print-huffman-code-table huf-tree)
        (values huf-tree (mask-int encoded-int-seq-cleared mask-seq))
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
        ;; plaintext length in block
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
           (let* ((begin )
                  (end (+ begin 32)))
             (setf block2 (subseq int-bit-seq begin end))
             ;; compute result-block
             (setf result-block
                   (bit-ior block1 (bit-ior block2 c-i-1)))
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

;;;Huffman tree code
(defstruct huffman-node
  (weight 0 :type number)
  (element nil :type t)
  (encoding nil :type (or null bit-vector))
  (left nil :type (or null huffman-node))
  (right nil :type (or null huffman-node)))
 
(defun initial-huffman-nodes (sequence &key (test 'eql))
  (let* ((length (length sequence))
         (increment (/ 1 length))
         (nodes (make-hash-table :size length :test test))
         (queue '()))
    (map nil #'(lambda (element)
                 (multiple-value-bind 
                       (node presentp) (gethash element nodes)
                   (if presentp
                     (incf (huffman-node-weight node) increment)
                     (let ((node (make-huffman-node :weight increment
                                                    :element element)))
                       (setf (gethash element nodes) node
                             queue (list* node queue))))))
         sequence)
    (values nodes (sort queue '< :key 'huffman-node-weight))))
 
(defun huffman-tree (sequence &key (test 'eql))
  (multiple-value-bind (nodes queue)
      (initial-huffman-nodes sequence :test test)
    (do () ((endp (rest queue)) (values nodes (first queue)))
      (destructuring-bind (n1 n2 &rest queue-rest) queue
        (let ((n3 (make-huffman-node
                   :left n1
                   :right n2
                   :weight (+ (huffman-node-weight n1)
                              (huffman-node-weight n2)))))
          (setf queue (merge 'list (list n3) queue-rest '<
                             :key 'huffman-node-weight)))))))1
 
(defun print-huffman-code-table (nodes &optional (out *standard-output*))
  (format out "~&Element~10tWeight~20tCode")
  (loop for node being each hash-value of nodes
        do (format out "~&~s~10t~s~20t~s"
                   (huffman-node-element node)
                   (huffman-node-weight node)
                   (huffman-node-encoding node))))

(defun encrypt-plain-text(pix-list v-p-array field-pixvalue-map)
  (let ((mask-seq (make-array 0 :element-type 'bit
                             :initial-element 0
                             :fill-pointer 0
                             :adjustable t))
        (int-seq (make-array 0 :element-type 'INTEGER
                             :initial-element 0
                             :fill-pointer 0
                             :adjustable t)))
    (loop for pix-value across pix-list
       do
         (if (if-in-topM v-p-array *topM* pix-value)
             (progn
               ;;---find length of iteration to target field
               (let ((itr-length 
                      (loop-to-target field-pixvalue-map pix-value)))
                 (if itr-length
                     (progn
                       ;;------search mode
                       ;;append itr-length to int-seq
                       (vector-push-extend itr-length int-seq)))))
             (progn
               ;;------mask mode
               (logistic-map);iterate logistic map once
               ;;append 8 bit mask bits gen from *xn* to mask-seq
               (loop for bit in (least-8-bits *xn*)
                    do (vector-push-extend bit mask-seq))
               ;; the plaintext is directly outputed
               ;;append h0:0 and pix-value to int-seq
               (vector-push-extend (values 0 pix-value) int-seq))))
    (values mask-seq int-seq)))

(defun loop-to-target(field-pixvalue-map pix-value)
   ;;;return length or nil if bigger than *MAX-len*
  (loop for itr-length from 1 to *MAX-itrs*
     do (if (if-hit-sphase *xn* pix-value field-pixvalue-map)
           (progn
             (return-from loop-to-target itr-length)))
     finally (return (+ 1 *max-itrs*))))

(defun if-hit-sphase (xn pix-value field-pixvalue-map)
  (let ((index-*xn* (truncate (* xn 1024))))
    (logistic-map);;logistic map for once
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
          (field-pixvalue-map (make-array *N*))
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
       do
         (vector-push `(0, i) v-p-array))
    (loop for value across pix-list
       do
         (incf (elt (elt v-p-array value) 0)))
    (setf v-p-array (sort v-p-array #'> :key #'(lambda(elm) (elt elm 0))))
    ;;(setf v-p-array (sort v-p-array #'> :key #'car))
    v-p-array))
(defun logistic-map()
      ;;;8 masking bits from the least significant byte of the
  ;;chaotic map output x
  (loop-next))
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
                   (vector-push-extend b b-pix-list)
                   )
                 )
           )
      )
    (list r-pix-list g-pix-list b-pix-list)
    )
  )
;reverse list: l
(defun rev(l)
  (let ((newl nil))
    (loop for elm in l
         do
         (push elm newl))
    newl
    )
  )
(defun decrypt(ciperimage plainimage)
  
   )
(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  )
(defun test-huffman-codes()
  (print-huffman-code-table 
   (huffman-codes '( 1 2 3 4 5)))
  )