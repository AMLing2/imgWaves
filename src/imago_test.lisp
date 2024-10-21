(ql:quickload "imago")

(defvar myimage (imago:make-rgb-image 100 100 
  (imago:make-color 255 0 0 255)))

(imago:do-region-pixels (myimage color x y 25 25 50 50)
  (setf color (imago:make-color 0 0 255 127)))

(imago:write-png myimage "hello.png")

