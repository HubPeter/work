(defun example-image (filename)
  (merge-pathnames filename
                   (asdf:component-pathname
                    (reduce #'asdf:find-component
                            '("opticl-examples" "images")))))

(let ((output-directory
       (merge-pathnames "output/"
                        (asdf:component-pathname
                         (asdf:find-system "opticl-examples")))))
  (ensure-directories-exist output-directory)
  (defun output-image (filename) (merge-pathnames filename output-directory)))
