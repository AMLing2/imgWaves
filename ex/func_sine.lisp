(defconstant +amplitude+ 10)
(defconstant +raise+ 20)
(defconstant +wavelength+ 4)

(defun sine-wave (g n x)
  (declare (ignore n))
  (+ (* g +raise+) 
     (* g +amplitude+ (sin (* x (/ 1 +wavelength+))))))
