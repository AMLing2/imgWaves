(defconstant +amplitude+ 8)
(defconstant +raise+ 2)
(defconstant +wavelength+ 10)
(defconstant +wavelength-gain+ 0)

(defun sine-wave (g n x)
  (+ (* g +raise+) 
     (* g +amplitude+ (abs (sin (* (+ x (* (* +wavelength+ 1.5) n))
                                   (/ 1 (+ (* g +wavelength-gain+)
                                             +wavelength+))))))))

;; main file was created using:
;; imgwaves ex/header_img.png -f ex/func_bounce.lisp -L 1 -b 000000 -c ffffff -n 26 -Tt 8 -rR 50 -I
