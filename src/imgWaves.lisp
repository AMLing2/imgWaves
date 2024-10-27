;argparser:
(ql:quickload "IMAGO")

(defstruct params
  (m 1.0 :type float)
  (g-up 0.2 :type float)
  (g-down 0.2 :type float)
  (n-lines 10 :type integer)
  (angle 0.0 :type float) ;;need to convert angle to rad, mod by 2 pi then -pi if >pi then *-1 so it is opposite
  )

(defvar Inparams (make-params))

;;(print (params-m Inparams)) ;;access slots

(defun parseArgs () (print *posix-argv*)
  (let ((arg-list *posix-argv*))
    (dotimes (i (- (list-length *posix-argv*) 1))
      (setq arg-list (rest arg-list)) (print arg-list)
        (cond ((string-equal (first arg-list) "-i") (print "hello i"))
              ((string-equal (first arg-list) "-b") (print "hello b"))
              ))))

;;; TODO: what i want options to do, delete from list as they get added
;;; -m = mass
;;; -x = function in x direction (relative)
;;; -y = function in y dir
;;; -Rr = ramp gain up/down
;;; -f = follow preset line or follow curve
;;; -a = angle of lines in deg (slightly more human readable)
;;; -n = number of lines
;;; -i = input image
;;; -o = output image
;;; -t = offset of lines in Y direction
;;; -b = background colour, hex
;;; -l = line colour, hex
;;; -c = image colour, hex, this option will append the image behind the lines, will be black if no color passed
;;; -p = user function to run before loop
;;; -s = size of output image WWWWxHHHH, may be difficult to implement, low priority 
;;; -d = [top,bottom,middle,outer] pattern to draw lines in
;;; -C = collision of lines, not sure on inputs yet, either spring or dampener coeff or both

(defvar in-Image (imago::read-image "~/Documents/projects/imgWaves/src/test.png"))
(defvar img-size (list (imago::image-width in-Image) (imago::image-height in-Image)))
(print img-size)

;;;;i dont think this is the most optimal way for this, should probably try rewriting soon
(defun linePoints (cx cy a imgsize);;;get image endpoints of a line based on rad angle and center x y 
  (let ((cutoff (atan (/ (nth 1 imgsize) (nth 0 imgsize)))))
    (cond ((and (< a cutoff) (> a (- cutoff)))
           (list 0 (+ cy (* (- cx (nth 0 imgsize)) (tan a))))) ;;left 
          ((and (> a cutoff) (< a (+ pi cutoff)))
           (list (+ cx (* (- cy (nth 1 imgsize)) (- (tan (- a pi))))) 0)) ;;top
          ((and (< a (- cutoff)) (> a (- (+ pi cutoff)))) 
           (list (+ cx (* (- cy (nth 1 imgsize)) (tan (- a pi)))) (nth 1 imgsize))) ;;bottom
          ((or (> a (+ pi cutoff)) (< a (- (+ pi cutoff)))) 
           (list (nth 0 imgsize) (+ cy (* (- cx (nth 0 imgsize)) (tan a))))) ;;right
          (t (print "helo")) ;;same angle as cutoff
    )))
;;;get item by index: (nth [index] [list])

(linePoints (/ (nth 0 img-size) 2) (/ (nth 1 img-size) 2) 0.8726646 img-size)

(defvar myimage (imago:make-rgb-image 100 100 
  (imago:make-color 255 0 0 255)))

(imago:do-region-pixels (myimage color x y 25 25 50 50)
  (setf color (imago:make-color 0 0 255 127)))

(imago:write-png myimage "hello.png")

;(defvar myimage (imago:read-image "~/Pictures/finn/ascent2.jpg"))

;(defvar embossedimage (imago:emboss myimage :angle (* pi 0.75) :depth 1.0))

;(imago:write-image embossed "~/Pictures/embossed.jpg")


