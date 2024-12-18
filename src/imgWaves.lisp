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
;;; -I = invert


(defvar in-Image (imago::read-image "~/Documents/projects/imgWaves/src/test.png"))
(defvar img-size (list (imago::image-width in-Image) (imago::image-height in-Image)))
(print img-size)

;;get item by index: (nth [index] [list])


(defun flipPoints (x1 y1 x2 y2)
  (list x2 y2 x1 y1))
(defun degtorad (deg) 
  (* deg (/ pi 180)))
(degtorad 80)

;;;; this function should return ONLY start points (list x1 y1) as well as no jump going from a = 89 to 90

(defun line-x (cx cy a y)
  (+ cx (* (tan (+ a (/ pi 2))) 
           (- y cy))))

(defun line-y (cx cy a x) ;y = tan(a)*x + b, b = cy, x = cx - x
  (+ cy (* (tan a) 
           (- cx x))))

(defun on-right-side? (a) (or 
                            (and (<= a (/ pi 2)) ;;find if on right side
                                 (>= a (- (/ pi 2))))
                            (> a (/ (* pi 3) 2))
                            (< a (- (/ (* pi 3) 2))))) ;;wont work outside of [-2*pi to 2*pi]

(defun linepoints (cx cy a imgsize) ;;no error thrown on (tan 90), so use htis then check if OOB
  (let* ((xval) (y (if (on-right-side? a)
               (prog1 ;prog1 returns first and runs both expr
                   (line-y cx cy a (first imgsize)) ;right
                 (setq xval (first imgsize)))
              (prog1 
                 (line-y cx cy a 0.0) ;left
               (setq xval 0.0)))))
    (cond
      ((and (<= y (nth 1 imgsize))
            (>= y 0.0)) 
       (list xval y)) ;left / right
      ((> y (nth 1 imgsize)) 
        (list (line-x cx cy a (nth 1 imgsize)) (nth 1 imgsize))) ; bottom
      ((< y 0.0) 
       (list (line-x cx cy a 0.0) 0.0))))) ;top

(defun linepoints-r (cx cy a imgsize) ;;a rounded version of linepoints, mavbe remove prev
  (let* ((xval) (y (if (on-right-side? a)
               (prog1 ;prog1 returns first and runs both expr
                   (line-y cx cy a (first imgsize)) ;right
                 (setq xval (first imgsize)))
              (prog1 
                 (line-y cx cy a 0.0) ;left
               (setq xval 0.0)))))
    (cond
      ((and (<= y (nth 1 imgsize))
            (>= y 0.0)) 
       (list (round xval) (round y))) ;left / right
      ((> y (nth 1 imgsize)) 
        (list (round (line-x cx cy a (nth 1 imgsize))) (round (nth 1 imgsize)))) ; bottom
      ((< y 0.0) 
       (list (round (line-x cx cy a 0.0)) 0))))) ;top

(defun linepoints-test (ang-add) ;;TODO: remove 
  (let ((ang 0))
  (loop while (and (<= ang (* 2 pi)) (>= ang (- (* 2 pi))))
        do (prog1
               (print ang)
             (print (linepoints 200 200 ang (list 600 600)))
             (setf ang (+ ang ang-add))))))
(linepoints-test 0.05)

(defun start-end-points (a imgsize) ;no cx cy for now, add if needed
  (list (linepoints (/ (first imgsize) 2) (/ (second imgsize) 2) a imgsize)
        (linepoints (/ (first imgsize) 2) (/ (second imgsize) 2) (+ a pi) imgsize)))

(defun start-end-points2 (cx cy a imgsize) ;with cx cy
  (list (linepoints-r cx cy a imgsize)
        (linepoints-r cx cy (+ a pi) imgsize)))

(defun dist-between-points (p)
  (sqrt (+ (expt (- (first (second p))
                    (first (first p))) 2)
           (expt (- (second (second p))
                    (second (first p))) 2))))

(defun gen-start-points (line-num a offset imgsize)  ;;TODO: add offset
  (let* ((ang (+ a (/ pi 2)))
         (p (start-end-points ang imgsize))
         (dist-x (/ (- (first (second p))
                   (first (first p))) (+ line-num 1)))
         (dist-y (/ (- (second (second p))
                   (second (first p))) (+ line-num 1))))
    (loop for l from 1 to line-num by 1
          collect (start-end-points2 (+ (* dist-x l)
                           (first (first p))) 
                         (+ (* dist-y l)
                            (second (first p)))
                         a
                         imgsize))))

;; Image processing:

(defun add-gain (g up? index line) ;; destructve modifiicaation of line list
  (let ((ind-move (if up? 1 -1))
        (cnt 0))
    (loop for g-add from 1 to 0 by (/ 1 g)
          do (progn (setq cnt (+ cnt ind-move))
              (if (or (> (+ index cnt) (list-length line))
                     (< (+ index cnt) 0))
                 (break)
                 (if (> (nth (+ index cnt) line) g-add)
                     (break)
                     (setf (nth (+ index cnt) line) g-add)))))))

(defun apply-gain-list (g up? index-list line)
  (dolist (i index-list)
    (add-gain g up? i line)))

(defun find-color-jumps (grayscale-line) ;TODO:continue, err test it now i guess
  (let ((prev (first grayscale-line))
        (current)
        (index-list nil))
    (loop for i from 1 to (- (length grayscale-line) 1) by 1
          do (prog1 (setq current (nth i grayscale-line))
               (cond ((= prev current)
                    (setq prev (nth i grayscale-line))) ; goes up
                     ((> (- prev current) 0)
                      (push '(i 1) index-list)) ;adds item to front but does not matter in this context
                     (T
                      (push '(i 0) index-list))) ; goes down
               (setq prev current)))))

;find-color-jumps test, temp
(defun find-color-jumps-test ()
  (let ((test-list (make-list 200 :initial-element 0)))
    (loop for i from 120 to 160 by 1
          do (setq (nth i test-list) 255))
    (print (find-color-jumps test-list))))

(defvar cummy (make-list 200 :initial-element 0))
; end

(defun make-gain-line (g image line-point) 
  (let ((g-list nil))
    (imago:do-line-pixels (image color x y (first (first line-point))
                                 (second (first line-point))
                                 (first (second line-point))
                                 (second (second line-point)))
      (push color g-list)) ;should be ANDed with #x00FF?
    (setq g-list (reverse g-list))
    ())) ; continue here with find-color-jumps and apply-gain-list
;; image:

(defun gray-pixel (image x y)
  (logand (imago:image-pixel image x y) #x00FF))

(defun drawpoints (point-list image) ;is this a temporary test?
  (dolist (p point-list)
    (prog1 (imago:draw-circle image (first (first p)) (second (first p)) 10 imago:+red+)
      (imago:draw-circle image (first (second p)) (second (second p)) 10 imago:+blue+))))


;;image tests:

(defun import-binary-image (location) ;; not using 
    (defvar (imago:convert-to-binary ((imago:read-image "./test.png") 10))))

(defvar gray-image (imago:invert (imago:convert-to-grayscale ;use something like this
                     (imago:read-image "~/Documents/projects/imgWaves/src/test.png"))))

(print (gray-pixel gray-image 150 150))

(imago:write-png gray-image "~/Documents/projects/imgWaves/bin.png")
(import-binary-image 1)


(setq myimage2 (imago:make-rgb-image 600 600 imago:+white+))
(drawpoints (gen-start-points 15 (degtorad 45) 0 (list (imago:image-width myimage2)
                                          (imago:image-height myimage2))) myimage2)

(imago:write-png myimage2 "hello.png"); should write to current path...

;(imago:do-region-pixels (myimage color x y 25 25 50 50)
;  (setf color (imago:make-color 0 0 255 127)))

;(defvar myimage (imago:read-image "~/Pictures/finn/ascent2.jpg"))

;(defvar embossedimage (imago:emboss myimage :angle (* pi 0.75) :depth 1.0))

;(imago:write-image embossed "~/Pictures/embossed.jpg")


