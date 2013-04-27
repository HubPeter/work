;(ql:quickload "opticl")
;(use-package :opticl)
(format t "Hello Opticl")
(defvar *x0* 0.3010198)  ; xn: (0, 1)
(defvar *alpha* 3.5946) ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar *topM* 10) ; top M in [0, 255]
(defvar *N* 1024)

(defun main()
  (format t "Encryption....~%")
  (encrypt "p_girl.jpg" "c_girl.jpg")
  (format t "De cryption......~%")
  (decrypt "c_girl.jpg" "de_girl.jpg")
  )
(defun encrypt (plainimage ciperimage)
                                        ;symbol number <sort desc>  M symbols
                                        ;divid phase space into N and get codebook
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
        )
                                        ;scan and sort desc
    (loop for value in pix-list         
       do
         (setf (elt v-p-array value) (+ 1 (elt v-p-array value)))
         )
    (setf v-p-array (sort v-p-array '> ))
                                        ;map pixvalue on phase space
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
      )
    )
  )

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
    (pix-list-encrypt (nth 0 rgb-list))
    (pix-list-encrypt (nth 1 rgb-list))
    (pix-list-encrypt (nth 2 rgb-list))
    )
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

(defun combine-pix-list(channels)
                                        ; combine channels as RGB pixels, with append
                                        ;
  )

(defun decrypt(ciperimage plainimage)
  
   )

(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  )
