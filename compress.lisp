;(ql:quickload "opticl")
;(use-package :opticl)
(format t "Hello Opticl")
(defvar *x0* 0.3010198)             ; xn: (0, 1)
(defvar *alpha* 3.5946)             ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *topM* 10)                  ; top M in [0, 255]
(defvar *N* 1024)
(defvar *xn*)
(defvar *MAX-itrs*)                 ; max length of search mode

(defun main()
  (format t "Encryption....~%")
  (encrypt "p_girl.jpg" "c_girl.jpg")
  (format t "De cryption......~%")
  (decrypt "c_girl.jpg" "de_girl.jpg")
  )
(defun encrypt (plainimage ciperimage)
                                        ;symbol number <sort desc>  M symbols
                                        ;divid phase space into N and get codebook
  (setf *xn* *x0*)
  (let* ((RGB-pix-list nil))
    (setf RGB-pix-list (get-RGB-pix-list plainimage))
    (gen-ciper-image plainimage ciperimage
                     (combine-pix-list
                      (list (pix-list-encrypt (nth 0 rgb-pix-list))
                            (pix-list-encrypt (nth 1 rgb-pix-list))
                            (pix-list-encrypt (nth 2 rgb-pix-list))
                            )
                      )
                     )
    )
  )


(defun pix-list-encrypt(pix-list)
  (let (
        (v-p-array (make-sequence 'list 256 :initial-element '0))
        (field-pixvalue-map (make-array *N*))
        (mask-seq nil);masking swquence used in step-5
        (int-seq nil);intermediate swquence
        )
                                        ;step 1: scan and sort desc
    (loop for value in pix-list         
       do
         (setf (elt v-p-array value) (+ 1 (elt v-p-array value)))
         )
    (setf v-p-array (sort v-p-array '> ))
                                        ;step 2: map pixvalue on phase space
    (let (
          (sum-topM (cal-topm-sum v-p-array *topM*))
          (partion-begin 0)
          )
      (loop for j below *topM* ;map
           do
           (let* ((partion-number
                   (+ 1 (truncate (/ (* *N* (elt v-p-array j)) sum-topM) ))))
             (loop for partion from partion-begin to
                  (min 1023 (+ -1  partion-begin partion-number))
                do
                  ;(format t "number ~a partion ~a~%" partion-number partion)
                  (setf (aref field-pixvalue-map partion) j)
                  )
             (setf partion-begin (+ partion-begin partion-number))
             (setf partion-number (+ 1 (truncate (/ (* *N* (elt v-p-array j)) sum-topM))))
             )
           )
                                        ;(format t "v-p-array:~%~a~% field-pixvalue-map:~%~a~%" v-p-array field-pixvalue-map)
                                        ;step 3: encrypt each plain text
      (loop for pix-value in pix-list 
           do
           (if (if-in-topM v-p-array *topM* pix-value)
               (progn
                                        ;---find length of iteration to target field
                 (let ((itr-length (loop-to-target field-pixvalue-map pix-value)))
                   (if itr-length
                                       
                       (progn           
                                        ;------search mode
                         (append (list itr-length) int-seq);append itr-length to int-seq
                         )
                       )
                   )
                 )
               (progn
                                        ;------mask mode
                 (loop-next);iterate logistic map once
                 (append (least-8-bits *xn*) mask-seq);append 8 bit mask bits gen from *xn* to mask-seq
                 (append (list 0 pix-value) int-seq);append h0:0 and pix-value to int-seq
                 )
               )
           )
                                        ;step 4: Huffman tree from int-seq
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
        (loop for i below (length int-seq-cleared) ;code int-seq with huf-tree
             do
             (append encoded-int-seq-cleared
                   (huffman-node-encoding (nth int-seq-cleared i)))
             )
        )
      )
    )
  )

(defun least-8-bits(x)
  ;return least 8 bits of x in list form
  )

(defun loop-to-target(field-pixvalue-map pix-value);return length or nil if bigger than *MAX-len*
                                        ;if nil recover the *xn*
  nil)
(defun logistic-map()
                                        ;8 masking bits from the least significant byte of the
                                        ;chaotic map output x
  
  )
(defun if-in-topM( v-p-array top-n pix-value)
  (loop for i below top-n
     do
       (if (= (elt v-p-array i) pix-value)
           (return T))))

(defun cal-topm-sum(v-p-array topM)
  (let ((sum 0))
    (loop for i below topM
         summing (elt v-p-array i) into sum
         finally (return sum)
         )
    )
  )

(defun test()
  (let (
        (rgb-list (get-RGB-pix-list "/home/w/wk/work/p_girl.jpg"))
        )
    (combine-pix-list (pix-list-encrypt (nth 0 rgb-list))
                      (pix-list-encrypt (nth 1 rgb-list))
                      (pix-list-encrypt (nth 2 rgb-list)))
    )
  )

(defun combine-pix-list(r-pix-list g-pix-list b-pix-list)
                                        ; combine channels as RGB pixels, with append
  (format t "length of R ~a~%" (length r-pix-list))
  (format t "length of R ~a~%" (length r-pix-list))
  (format t "length of R ~a~%" (length r-pix-list))
  )

(defun get-rgb-pix-list(imagefile)
  (let ((img (read-jpeg-file imagefile))
        (r-pix-list nil)
        (g-pix-list nil)
        (b-pix-list nil))
    (with-image-bounds ( height width) img 
      (loop for i below height
         do (loop for j below width
               do
                 (multiple-value-bind (r g b)
                     (pixel img i j)
                   (declare (type (unsigned-byte 8) r g b))
                   #|(if (= i 0)
                       (format t "(~a ~a ~a) ~%" r g b))|#
                   (push r r-pix-list)
                   (push g g-pix-list)
                   (push b b-pix-list)
                   )
                 )
           )
      )
    ;(format t "iiii ~%~a ~%" r-pix-list)
    (setf r-pix-list (rev r-pix-list))
    ;(format t "iiii ~% ~a ~%" r-pix-list)
    (setf g-pix-list (rev g-pix-list))
    (setf b-pix-list (rev b-pix-list))
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
                 (multiple-value-bind (node presentp) (gethash element nodes)
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
 
(defun print-huffman-code-table (nodes &optional (out *standard-output*))
  (format out "~&Element~10tWeight~20tCode")
  (loop for node being each hash-value of nodes
        do (format out "~&~s~10t~s~20t~s"
                   (huffman-node-element node)
                   (huffman-node-weight node)
                   (huffman-node-encoding node))))