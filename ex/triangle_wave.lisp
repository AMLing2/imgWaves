(defconstant +amplitude+ 40)
(defconstant +raise+ 20)
(defconstant +period+ 40)

(defun triangle (g n x)
  (declare (ignorable n))
  (* g (+ +raise+ 
          (* (/ +amplitude+ (/ +period+ 2))
             (- (/ +period+ 2) (abs (- (mod x +period+) (/ +period+ 2))))))))
