(defpackage #:compress (:use #:cl #:opticl))
;(in-package #:compress)

(defvar *x0* 0.3010198)  ; xn: (0, 1) 
(defvar *alpha* 3.5946) ; alpha: 3.5699456<u<=4,0<Xi<1
(defvar topM 10) ; top M in [0, 255]

(defun main()
  (encrypt "p_girl.jpg" "c_girl.jpg")
  (decrypt "c_girl.jpg" "de_girl.jpg")
  )
(defun encrypt (plainimage ciperimage)
                                        ;symbol number <sort desc>  M symbols
                                        ;divid phase space into N and get codebook
  (let ((RGB-pix-list nil))
    (setf RGB-pix-list (get-RBG-pix-list plainimage))
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

(gen-ciper-image (plainimage ciperimage rgb-pix-list)
                                        ;generate ciperimage with header of plainimage
                )

(defun combine-pix-list(channels)
                                        ; combine channels as RGB pixels, with append
                                        ;
  )

(defun pix-list-encrypt(channel)
                                        ;encrypt channel with enbedd compression
  )

(defun decrypt()
  
   )

(defun loop-next()
  ;logstic next value
  (setf *xn* (* *xn* *alpha* (- 1 *xn*)))
  )