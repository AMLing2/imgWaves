;;; this works best with vector graphics
(defconstant +amplitude+ 10)
(defconstant +raise+ 20)
(defconstant +wavelength+ 10)

(defun square-wave (g n x)
  (declare (ignore n))
  (+ (* g +raise+) 
     (* g +amplitude+ (signum (sin (* x (/ 1 +wavelength+)))))))
