;;;params object:
;; (defstruct params
;;   (g-up 10.0 :type float)
;;   (g-down 10.0 :type float)
;;   (n-lines 30 :type integer)
;;   (angle 0.0 :type float) ;degrees 
;;   (offset 0 :type integer)
;;   (l-thickness 0 :type integer) ;0 = 1px
;;   (l-color imago:+black+ :type imago:rgb-pixel)
;;   (bg-color imago:+white+ :type imago:rgb-pixel)
;;   (img-invert nil :type boolean)
;;   (fix-gap nil :type boolean)
;;   (in-func #'base-sine-wave :type function)
;;   (filename #p"./out.png" :type pathname))

(defconstant +scan-speed+ 1)
(defconstant +image-height+ 32)

(defun anim-scan (params-obj run-count)
  (incf (slot-value params-obj 'offset) +scan-speed+)
  (<= run-count (/ +image-height+ +scan-speed+)))

;;; imgwaves image.png -n 1
