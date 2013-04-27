(ql:quickload 'opticl)
(use-package 'opticl)

(format t "hello go")

(let ((img (read-jpeg-file "p_girl.jpg")))
  (typecase img
    (8-bit-rgb-image
     (locally (declare (type 8-bit-rgb-image img))
       (with-image-bounds (height width)
           img
         (time
          (loop for i below height
             do (loop for j below width
                   do
                     (multiple-value-bind   (r g b)
                       (pixel img i j)
                     (declare (type (unsigned-byte 8) r g b))
                     (setf (pixel img i j)
                           (values (- 255 r) g b))))))))))
  (write-jpeg-file "re_girl.jpg" img))