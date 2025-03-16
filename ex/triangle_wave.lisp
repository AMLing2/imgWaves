(defconstant +amplitude+ 20)
(defconstant +raise+ 15)
(defconstant +period+ 22)

(defun triangle (g n x)
  (declare (ignorable n))
  (* g (+ +raise+ 
          (* (/ +amplitude+ (/ +period+ 2))
             (- (/ +period+ 2) (abs (- (mod x +period+) (/ +period+ 2))))))))
