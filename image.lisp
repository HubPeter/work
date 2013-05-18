;;(defpackage #:opticl-examples
  ;;(:use #:cl #:opticl))

(defun example-image (filename)
  (merge-pathnames filename
                   (asdf:component-pathname
                    (reduce #'asdf:find-component
                            '("opticl-examples" "images")))))

(defparameter *gray-image* (make-8-bit-gray-image 4 4))