;;
(defun scan-single-list(plainimage)
  (let ((pix-list (get-pix-list plainimage))
        (v-p-array nil))
    (setf v-p-array (scan-sort-desc pix-list))
    (format t "~A~%" v-p-array)))