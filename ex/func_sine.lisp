(defconstant +amplitude+ 10)
(defconstant +raise+ 0)
(defconstant +wavelength+ 3.3)

(defun sine-wave (g n x)
  (declare (ignore n))
  (+ (* g +raise+) 
     (* g +amplitude+ (sin (* x (/ 1 +wavelength+))))))
