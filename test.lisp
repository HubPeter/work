(defun get-best()
  (let ((min 1000000)
        (plain-size 0))
    (with-open-file (plain plainimage
                           :direction :input)
      (setf plain-size (file-length plain)))
    (loop for topM from 10 to 30
       do(let ((*topM* topM)
               (plainimage "p_girl.jpg"))
           (format t "topM: ~A~%"
                   *topM*)
           (loop for max-itrs from 1 to 30
              do (let ((*MAX-itrs* max-itrs)
                       (ciper-size nil)
                       (ciperimage
                        (gen-file-name "c_girl"
                                       (list *topM*
                                             *MAX-itrs*) ".jpg")))
                   (encrypt plainimage ciperimage)
                   (with-open-file (ciper ciperimage
                                          :direction :input)
                     (setf ciper-size (file-length ciper)))
                   (if (< ciper-size plain-size)
                       (progn
                         (format t "++++")
                         (setf min ciper-size))
                       (format t "    "))
                   (format t "~A  ~A:  ~A --> ~A~%" min
                           *MAX-itrs*
                           plain-size ciper-size)
                   ;;(return-from get-best)
                   ))))))
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