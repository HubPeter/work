(defun create-lcs-table (i j)
  (make-array `(,(1+ i) ,(1+ j))))

(defun last1 (sequence)
  (typecase sequence
    (string (aref sequence (1- (length sequence))))
    (list   (nth (1- (length sequence)) sequence))
    (vector (aref sequence (1- (length sequence))))))

(defun lcs-table (seq1 seq2)
  (let* ((i (length seq1))
	 (j (length seq2))
	 (lcs-table (create-lcs-table i j)))
    (loop for ci from 0 upto i
       do
	 (loop for cj from 0 upto j
	      do
	      (cond ((or (zerop ci) (zerop cj))
		     (setf (aref lcs-table ci cj) 0))
		    ((and (> ci 0) (> cj 0)
			  (equal (last1 (subseq seq1 0 ci))
				 (last1 (subseq seq2 0 cj))))
		     (setf (aref lcs-table ci cj)
			   (1+ (aref lcs-table (1- ci) (1- cj)))))
		    ((and (> ci 0) (> cj 0)
			  (not (equal (last1 (subseq seq1 0 ci))
				      (last1 (subseq seq2 0 cj)))))
		     (setf (aref lcs-table ci cj)
			   (max (aref lcs-table (1- ci) cj)
				(aref lcs-table ci (1- cj))))))))
    lcs-table))

(defun lcs-length (seq1 seq2)
  (let ((table (lcs-table seq1 seq2)))
    (aref table (length seq1) (length seq2))))