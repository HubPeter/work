(defun get-best()
  (let ((min 1000000)
        (plain-size 0)
        (plainimage "p_girl.jpg")
        (pre "c_girl"))
    (with-open-file (plain plainimage
                           :direction :input)
      (setf plain-size (file-length plain)))
    (loop for topM from 10 to 30
       do(let ((*topM* topM))
           (format t "  topM: ~A~%" *topM*)
           (loop for max-itrs from 1 to 30
              do (let ((*MAX-itrs* max-itrs)
                       (ciper-size nil)
                       (ciperimage
                        (gen-file-name pre
                                       (list topM
                                             MAX-itrs) ".jpg")))
                   (encrypt plainimage ciperimage)
                   (with-open-file (ciper ciperimage
                                          :direction :input)
                     (setf ciper-size (file-length ciper)))
                   (if (< ciper-size plain-size)
                       (format t "++++")
                       (format t "    "))
                   (if (< ciper-size min)
                       (setf min ciper-size))
                   (format t "~A  ~A:  ~A --> ~A~%" min
                           *MAX-itrs*
                           plain-size ciper-size)))))))

(defun test-all()
  (loop for plainiamge in (list "p_girl.jpg"
                                "p_cow.jpg"
                                "p_hub.jpg")
     for pre in (list "p_girl" 
                      "p_cow"
                      "p_hub")
     do (get-best plainimage pre)))

(defun gen-file-name (pre params tail)
  (let ((new-name (make-array 0 :adjustable T
                              :fill-pointer 0
                              :element-type 'base-char)))
    (with-output-to-string (s new-name)
      (format s "~a" pre)
      (loop for n in params 
         do(format s "_~a" n))
      (format s "~a" tail))
    (format t "~A~%" new-name)
    new-name))