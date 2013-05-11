(ql:quickload "opticl")
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
  (ENCRYPT "P_HUB.JPG" "C_HUB.JPG")
  (FORMAT T "DE CRYPTION......~%")
  (DECRYPT "C_GIRL.JPG" "DE_GIRL.JPG")
  )
(DEFUN INITVAR()
  (SETF *XN* *X0*)
  ;;(FORMAT T "INIT..~%*XN* ~A~%" *XN*)
  )

(DEFUN ENCRYPT (PLAINIMAGE CIPERIMAGE)
  ;;SYMBOL NUMBER <SORT DESC>  M SYMBOLS
  ;;DIVID PHASE SPACE INTO N AND GET CODEBOOK
  (SETF *XN* *X0*)
  (let* ((RGB-pix-list nil))
    (setf RGB-pix-list (get-RGB-pix-list plainimage))
    (gen-ciper-image plainimage ciperimage
                     (combine-pix-list 
                      (pix-list-encrypt (nth 0 rgb-pix-list))
                      (pix-list-encrypt (nth 1 rgb-pix-list))
                      (pix-list-encrypt (nth 2 rgb-pix-list))
                      )
                     )
    )
  )
;; int-seq gen here
(defun pix-list-encrypt(pix-list)
  (let (
        (v-p-array nil)
        (field-pixvalue-map (make-array *N*))
        (mask-seq nil);masking swquence used in step-5
        (int-seq nil);intermediate swquence
        (temp-list nil)
        (encoded-int-seq-cleared nil)
        )
    ;;step 1: scan and sort desc
    (setf v-p-array (scan-sort-desc pix-list))
    ;;step 2: map pixvalue on phase space
    (setf field-pixvalue-map (map-pixvalue-on-phase-space v-p-array))
    ;;step 3: encrypt each plain text
    (multiple-value-bind (mask-seq int-seq)
        (encrypt-plain-text pix-list v-p-array field-pixvalue-map))
    ;;step 4: Huffman tree from int-seq
    (return-from pix-list-encrypt)
    (setf encoded-int-seq-cleared (huf-encode int-seq))
    )
  )

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
                       (vector-push-extend itr-length int-seq)
                       )
                     )
                 )
               )
             (progn
               ;;------mask mode
               (logistic-map);iterate logistic map once
               ;;append 8 bit mask bits gen from *xn* to mask-seq
               (loop for bit in (least-8-bits *xn*)
                    do (vector-push-extend bit mask-seq))
               ;;append h0:0 and pix-value to int-seq
               (vector-push-extend (values 0 pix-value) int-seq)
               )
             )
         )
    (values mask-seq int-seq)
    )
  )

(defun loop-to-target(field-pixvalue-map pix-value)
   ;;;return length or nil if bigger than *MAX-len*
  (loop for itr-length from 1 to *MAX-itrs*
     do
       (if (if-hit-sphase *xn* pix-value field-pixvalue-map)
           (progn
             (return-from loop-to-target itr-length))
           )
     finally (return (+ 1 *max-itrs*)))
  )

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
        (bit 0)
        )
    ;;; make integer
    (loop for i below 20
         do
         (if (= x (truncate x))
             (return))
         (setf x (* x 10))
         )
    ;;;(format t "x: ~a~%" x)
    ;;; get least 8 bits
    (loop for i below 8
       do
         (setf bit (mod x 2))
         ;;;(format t "x: ~a bit: ~a~%" x bit)
         (if (= 1 bit)
             (vector-push 1 bit-array)
             (vector-push 0 bit-array)
             )
         (setf x (truncate (/ x 2)))
         )
    (loop for bit across bit-array
       collect bit
         )
    )
  )
(defun if-in-topM( v-p-array top-n pix-value)
  (loop for i below top-n
     do
       (if (= (elt (elt v-p-array i) 1) pix-value)
           (return-from if-in-topM T)
           )
       )
  nil
  )

(defun huf-encode(int-seq)
  (let ((int-seq-cleared nil)
        (huf-tree nil)
        (encoded-int-seq-cleared nil)
        )
                                        ;remove symbols from int-seq
    (loop for i below (- (length int-seq) 1)
       do
         (let ((item (nth i int-seq))
               (next-item (nth (+ i 1) int-seq))
               )
           (append item int-seq-cleared)
           (if (= item 0)
               (incf i))
           )
         )

    (setf huf-tree (huffman-codes int-seq-cleared));build huf-tree
    (format t "Code int-seq with huffman tree ...~%")
    (loop for i below (length int-seq-cleared) ;code int-seq with huf-tree
       do
         (append encoded-int-seq-cleared
                 (huffman-node-encoding (nth int-seq-cleared i)))
         )
    encoded-int-seq-cleared
    )
  )

(defun huffman-codes (sequence &key (test 'eql))
  (multiple-value-bind (nodes tree)
      (huffman-tree sequence :test test)
    (labels ((hc (node length bits)
               (let ((left (huffman-node-left node))
                     (right (huffman-node-right node)))
                 (cond
                  ((and (null left) (null right))
                   (setf (huffman-node-encoding node)
                         (make-array length :element-type 'bit
                                     :initial-contents (reverse bits))))
                  (t (hc left (1+ length) (list* 0 bits))
                     (hc right (1+ length) (list* 1 bits)))))))
      (hc tree 0 '())
      nodes)))

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

(defun map-pixvalue-on-phase-space(v-p-array)
    (let (
          (sum-topM (cal-topm-sum v-p-array *topM*))        
          (field-pixvalue-map (make-array *N*))
          (partion-begin 0)
          )
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
                  (setf (aref field-pixvalue-map partion) j)
                  )
             (setf partion-begin (+ partion-begin partion-number))
             (setf partion-number 
                   (+ 1 (truncate (/ 
                                   (* *N* (elt (elt v-p-array j) 0))
                                   sum-topM))))
             )
           )
      field-pixvalue-map
      )
  )

(defun cal-topm-sum(v-p-array topM)
  (let ((sum 0))
    (loop for i below topM
       summing (elt (elt v-p-array i) 0) into sum
       finally (return sum)
         )
    )
  )

(defun scan-sort-desc(pix-list)
  (let* (;;;(v-p-array 
         ;;;    (make-sequence 'list 256 :initial-element (list 0 0)))
         (v-p-array (make-array 256 :element-type 'list 
                                :fill-pointer 0
                                :initial-element '(0 0)))
        )
    (loop for i below 256
       do
         (vector-push `(0, i) v-p-array)
         )
    (loop for value across pix-list
       do
         (incf (elt (elt v-p-array value) 0))
         )
    (setf v-p-array (sort v-p-array #'> :key #'(lambda(elm) (elt elm 0))))
    ;;(setf v-p-array (sort v-p-array #'> :key #'car))
    v-p-array
    )
  )

(defun logistic-map()
      ;;;8 masking bits from the least significant byte of the
  ;;chaotic map output x
  (loop-next)
  )

(defun test()
  (initvar)
  (let (
        (rgb-list (get-RGB-pix-list "/home/w/wk/work/p_girl.jpg"))
        )
    (pix-list-encrypt (nth 0 rgb-list))
    
    (return-from test)
    (combine-pix-list (pix-list-encrypt (nth 0 rgb-list))
                      (pix-list-encrypt (nth 1 rgb-list))
                      (pix-list-encrypt (nth 2 rgb-list)))
    )
  )

(defun combine-pix-list(r-pix-list g-pix-list b-pix-list)
        ;;; combine channels as RGB pixels, with append
  (format t "~%length of R ~a~%" (length r-pix-list))
  (format t "length of R ~a~%" (length r-pix-list))
  (format t "length of R ~a~%" (length r-pix-list))
  )

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

(defun gen-ciper-image (plainimage ciperimage rgb-pix-list)
                                        ;generate ciperimage with header of plainimage
                )


(defun decrypt(ciperimage plainimage)
  
   )

(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  )

(defun test-huf()
  (print-huffman-code-table 
   (huffman-codes '( 1 2 3 4 5)))
  )

