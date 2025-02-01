(ql:quickload "IMAGO")

(defstruct params
  (g-up 10.0 :type float)
  (g-down 10.0 :type float) ;remove?
  (n-lines 30 :type integer)
  (angle 0.0 :type float) ;degrees 
  (offset 0 :type integer) ;needs to be added
  (l-thickness 0 :type integer) ;0 = 1px
  (l-color imago:+black+ :type imago:rgb-pixel)
  (bg-color imago:+white+ :type imago:rgb-pixel)
  (filename "out.png" :type string)) ;converted to pathname type later

(defun print-help ()
  (format t "Usage: imgwaves [image file] [options]~%~%")
  (format t "Options:~%")
  (format t "  -R, --ramp-up <float>        Set the ramp gain up.~%")
  (format t "  -r, --ramp-down <float>      Set the ramp gain down.~%")
  (format t "  -a, --angle <float>          Set the angle of lines in degrees.~%")
  (format t "  -n, --num-lines <integer>    Set the number of lines.~%")
  (format t "  -o, --output-file <string>   Set the output image filename and filetype (default: png).~%")
  (format t "  -t, --line-offset <integer>  Set the offset of lines in the relative Y direction.~%")
  (format t "  -b, --background-color <hex> Set the background color in hexadecimal format.~%")
  (format t "  -l, --line-color <hex>       Set the line color in hexadecimal format.~%")
  (format t "  -L, --line-thickness <integer> Set the line thickness in pixels.~%")
;  (format t "  -I                           Invert the image.~%")
  (format t "  -h, --help                   Display this help message.~%")
  (format t "Example:~%")
  (format t "  imgwaves base.png -R 5.0 -a 45 -n 100 -o out.png~%"))

;TODO: need to convert val to int/float where neeeded, is probably string always
(defmethod fill-from-args ((obj params) d-arg val)
  (cond
    ((or (string= d-arg "-h")
         (string= d-arg "--help"))
     (print-help)
     (uiop:quit 0))

    ;; Set the ramp gain up/down ('R'/'r' short, 'ramp-up'/'ramp-down' long)
    ((or (string= d-arg "R")
         (string= d-arg "ramp-up")) (setf (slot-value obj 'g-up) val))
    ((or (string= d-arg "r")
         (string= d-arg "ramp-down")) (setf (slot-value obj 'g-down) val))

    ;; Set the angle of lines ('a' short, 'angle' long)
    ((or (string= d-arg "a")
         (string= d-arg "angle")) (setf (slot-value obj 'angle) val))

    ;; Set the number of lines ('n' short, 'num-lines' long)
    ((or (string= d-arg "n")
         (string= d-arg "num-lines")) (setf (slot-value obj 'n-lines) val))

    ;; Set the output filename ('o' short, 'output-file' long)
    ((or (string= d-arg "o")
         (string= d-arg "output-file")) (setf (slot-value obj 'filename) val))

    ;; Set the offset of lines ('t' short, 'line-offset' long)
    ((or (string= d-arg "t")
         (string= d-arg "line-offset")) (setf (slot-value obj 'offset) val))

    ;; Set the background color ('b' short, 'background-color' long)
    ((or (string= d-arg "b")
         (string= d-arg "background-color")) (setf (slot-value obj 'bg-color) val))

    ;; Set the line color ('l' short, 'line-color' long)
    ((or (string= d-arg "l")
         (string= d-arg "line-color")) (setf (slot-value obj 'l-color) val))

    ;; Set the line thickness ('L' short, 'line-thickness' long)
    ((or (string= d-arg "L")
         (string= d-arg "line-thickness")) (setf (slot-value obj 'l-thickness) val))

    ;; Handle unknown arguments
    (t (format t "Unknown argument provided: ~a~%" d-arg)
       (print-help)
       (uiop:quit 1))))

;;; NOTE: what i want options to do, note on list as they get added
;;; -f = y = f(t) function (relative)
;;; -Rr = ramp gain up/down
;;; -a = angle of lines in deg (slightly more human readable)
;;; -n = number of lines
;;; -i / base = input image
;;; -o = output image and filetype (png default)
;;; -t = offset of lines in relative Y direction
;;; -b = background colour, hex
;;; -l = line colour, hex
;;; -L = line thickness, int
;;; -c = image colour, hex, this option will append the image behind the lines, black by default
;;; -s = size of output image WWWWxHHHH 
;;; -I = invert
;;;; future: dynamic function:
;;; -f = follow preset line or follow curve
;;; -m = mass
;;; -x = function in x dir
;;; -y = function in y dir
;;; -C = collision of lines, not sure on inputs yet, either spring or dampener coeff or both
;;;; unsure if implementing:
;;; -p = initial user function to run before loop
;;; -d = [top,bottom,middle,outer] pattern to draw lines in

;;;NOTE: how the user will interract with the program:
;  1 - write a file with the desired wave function then run:
;    imgwaves /path/image.png -f /path/file.lisp -arg1 -arg2... in the terminal
;  2 - write a file with the desired wave func then in the file run:
;    (img-waves *FUNC* *image* *param-object*)
;  3 - run in terminal with no other args to open REPL with choosable options:
;    imgwaves /path/image

(defvar in-Image (imago::read-image "~/Documents/projects/imgWaves/src/test.png"))
(defvar img-size (list (imago::image-width in-Image) (imago::image-height in-Image)))
(print img-size)

;;get item by index: (nth [index] [list])


(defun flipPoints (x1 y1 x2 y2) ; remove?
  (list x2 y2 x1 y1))
(defun degtorad (deg) 
  (* deg (/ pi 180)))

(defun clamp-ang-rad (ang)
  "run mod 2 pi and preserve sign"
  (let ((ang-sign (/ ang (abs ang))))
    (* ang-sign 
       (mod (abs ang) (* 2 pi)))))

(defun get-quadrant (a) ;-2 pi to 2 pi
  (let ((n (if (> a 0)
              (ceiling (* 2 (/ (clamp-ang-rad a) pi)))
              (ceiling (* 2 (/ (+ (clamp-ang-rad a) (* 2 pi)) pi))))))
    (if (= n 0) 1 n))) ;for when a = -2pi
;;;; this function should return ONLY start points (list x1 y1) as well as no jump going from a = 89 to 90 to 91
(defun line-x (cx cy a y)
  (+ cx (* (tan (+ a (/ pi 2))) 
           (- y cy))))

(defun line-y (cx cy a x) ;y = tan(a)*x + b, b = cy, x = cx - x
  (+ cy (* (tan a) 
           (- cx x))))

(defun on-right-side? (a) 
  "check if a point following the line +x will hit the right or left side of image"
  (or 
    (and (<= a (/ pi 2)) ;;find if on right side
         (>= a (- (/ pi 2))))
    (> a (/ (* pi 3) 2))
    (< a (- (/ (* pi 3) 2))))) ;;wont work outside of [-2*pi to 2*pi]

(defun linepoints-init (ang imgsize) ;surprisingly ang = 0 works, might add to cond
  "get points of line of angle (A + pi/2) which intersects lines of angle A from corners"
  ;three lines y = ax + c, y = bx + d1 (top), y = bx + d2 (bottom)
  (cond ((= ang (degtorad (- 90))) (list (first imgsize) ; floating point comparison hurts
                                         (/ (second imgsize) 2)))
      ((= ang (degtorad (- 180 90))) (list 0 ;works without this but might not on all machines
                                          (/ (second imgsize) 2)))
      (T (let* ((ang-init (+ ang (/ pi 2)))
                (a (tan ang-init))
                (b (tan ang))
                (cx (/ (first imgsize) 2))
                (cy (- (/ (second imgsize) 2)))
                (lq (get-quadrant ang-init))
                (w (if (or (= lq 1) (= lq 4)) 
                       (first imgsize) 
                       0))
                (h (if (or (= lq 1) (= lq 2))
                       0
                       (- (second imgsize)))))
           (list (/ (- (+ h (* a cx)) cy (* b w)) ; checked
                    (- a b))
                 (- (/ (- (+ (* b cx) h) (* b w) (/ (* cy b) a))
                       (- 1 (/ b a)))))))))

(defun linepoints-r (cx cy a imgsize) ;;a rounded version of linepoints, mavbe remove prev
  (let* ((xval)
         (y (if (on-right-side? a) ;WARN: use this or prev?
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

(defun init-line-points (a imgsize)
  (list (linepoints-init a imgsize)
        (linepoints-init (+ a pi) imgsize)))

(defun start-end-points2 (cx cy a imgsize) ;with cx cy
  (list (linepoints-r cx cy a imgsize)
        (linepoints-r cx cy (+ a pi) imgsize)))

(defun dist-between-points (p)
  (sqrt (+ (expt (- (first (second p))
                    (first (first p))) 2)
           (expt (- (second (second p))
                    (second (first p))) 2))))

(defun mapAdd (num apply-list)
  (map 'list (lambda (x) (+ x num)) apply-list))

(gen-start-points-test 30 (degtorad 135) 0 (imago:make-rgb-image 500 300 imago:+white+)) ;PERF:

(defun gen-start-points (line-num a offset imgsize)  ;;TODO: add offset, use for main func
  (let* ((imgsizefixed (mapAdd -1 imgsize))
         (p (init-line-points a imgsizefixed)) ;TODO: continue, test with -init and negatives
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
                         imgsizefixed))))

;;;;; Image processing - adding gain:
(gainline-test 5 gray-image 45 1) ;PERF: test here

;TODO: fix artifact in going up here
(defun add-gain (g up? index line) ; destructve modification of line list
  (let ((ind-move (if up? 1 -1)) ;finished?
        (cnt 0))
    (loop for g-add downfrom 255 to 0 by g ;FIX: replace 255 -> (nth index line)?
          do (progn 
               (setq cnt (+ cnt ind-move))
              (if (or (> (+ index cnt) (list-length line));length -1 ?
                     (< (+ index cnt) 0))
                 (return-from add-gain nil) ;use instead of (break)
                 (if (> (nth (+ index cnt) line) g-add); can be moved to first or?
                     (return-from add-gain nil)
                     (setf (nth (+ index cnt) line) g-add)))))))

(defun apply-gain-list (g index-list grayscale-line)
  (dolist (i index-list)
    (add-gain g (second i) (first i) grayscale-line)))

(defun find-color-jumps (grayscale-line)
  (let ((prev (first grayscale-line))
        (current)
        (index-list nil))
    (loop for i from 1 to (- (length grayscale-line) 1) by 1
          do (prog1 (setq current (nth i grayscale-line))
               (cond ((> (- prev current) 0); goes up
                      (push (list i T) index-list)) ;adds item to front but does not matter in this context
                     ((< (- prev current) 0); goes down
                      (push (list i nil) index-list))) 
               (setq prev current)))
    (values index-list)))

;find-color-jumps test, temp
(defun find-color-jumps-test () ;finished i think
  (let ((test-list (fill (make-list 200 :initial-element 0)
                         255 :start 120 :end 160)))
; if &KEY, this means you need to use :name to fill in the variables as they are
    (print (find-color-jumps test-list))))
(find-color-jumps-test)

(defun make-gain-line-linear (g image line-point) ;finished? rename in the future maybe
  (let ((g-list nil))
    (imago:do-line-pixels (image color x y ;if 300x300 image then 0->299
                                 (first (first line-point))   ;x1
                                 (second (first line-point))  ;y1
                                 (first (second line-point))  ;x2
                                 (second (second line-point)));y2
      (push (logand color #x00FF) g-list)) ;should be ANDed with #x00FF?
    ;(print g-list) ;remove
    (apply-gain-list g
                     (find-color-jumps g-list) ; this line works
                     g-list); dont reverse, dosent seem to be destructive..
    (values (reverse g-list))))                 ;a double reverse that might work well?

;;;;; creating and reading shape for relative line:

(defun create-relative-line-static (line-func gain-line line-num) ;no idea what to call this function
    (let ((out-line nil)) ;input line-func with #'[func-name]
      (loop for i from 0 to (- (length gain-line) 1) by 1
            do (push (funcall line-func ;g n x
                              (/ (nth i gain-line) 255);normalize g to 0-1
                              line-num
                              i)
                     out-line))
      (values out-line)))

(defun create-relative-line-static2 (line-func gain-line line-num)
      (loop for i from 0 to (- (length gain-line) 1) by 1
            collect (funcall line-func ;g n x
                              (/ (nth i gain-line) 255);normalize g to 0-1
                              line-num
                              i)))

;;;;; image:
;;HACK: redo this function, good idea but leaves a lot of artifacts
(defun draw-filled-circle (image x y r color)
  (prog1 (imago:draw-point image x y color) ;fill in center point
    (loop for ri downfrom r to 1 by 1 
          do (imago:draw-circle image x y ri color))))

(defun gray-pixel (image x y)
  (logand (imago:image-pixel image x y) #x00FF))

;NOTE: way to do this!!!! create a relative x,y line with the wave pattern, then with imago drawline, use the x,y of current pixel + relative x,y after some angle conversion to draw the pixel or filled circle

(defun x-func-add (old-x func-val angle) ;func-val acts as len for a new vector
  (round (+ old-x (* func-val (- (sin angle))))))
(defun y-func-add (old-y func-val angle)
  (round (+ old-y (* func-val (- (cos angle))))))

;tests:
; (defun x-func-add (old-x func-val angle) ;FIX: remove? func-val acts as len for a new vector
;   (prog1 (round (+ old-x (* func-val (cos (+ angle (/ pi 2))))))
;     (when (/= func-val 0) (print (list "old-x:" old-x "func-val:" func-val "angle:" angle
;                                        "out:" (round (+ old-x (* func-val (cos (+ angle (/ pi 2)))))))))))
; old-x
; (defun y-func-add (old-y func-val angle)
;   (prog1 (round (+ old-y (* func-val (sin (+ angle (/ pi 2))))))
;     (when (/= func-val 0) (print (list "old-y:" old-y "func-val:" func-val "angle:" angle
;                                        "out:" (round (+ old-y (* func-val (sin (+ angle (/ pi 2)))))))))))
; (x-func-add 127 -8.487476 0.7853981633974483)

(defun draw-func-line (image line-points relative-shape-line angle
                             line-thickness line-color)
  (let ((i 0))
  (imago:do-line-pixels (image color x y 
                               (first (first line-points));x1
                               (second (first line-points));y1
                               (first (second line-points))
                               (second (second line-points)))
    (prog1
    (draw-filled-circle image
                        (x-func-add x (nth i relative-shape-line) angle)
                        (y-func-add y (nth i relative-shape-line) angle)
                        line-thickness
                        line-color)
    (incf i)))))

;putting it all together
(defun main-loop (base-img new-img num-lines angle offset gain
                           line-func line-thickness line-color)
  (let* ((imgsize (list (imago:image-width base-img)
                        (imago:image-height base-img)))
         (line-points (gen-start-points num-lines
                                        (degtorad angle)
                                        offset 
                                        imgsize))
         (line-index 0))
    (dolist (p line-points)
      (prog1 
      (draw-func-line new-img p
                      (create-relative-line-static line-func 
                                                    (make-gain-line-linear gain
                                                                           base-img
                                                                           p)
                                                    line-index)
                      (degtorad angle) line-thickness line-color)
      (incf line-index)))))

;taken from https://github.com/sbcl/sbcl/blob/master/src/code/filesys.lisp#L270
;HACK: and like dont use this for obvious reasons, dont put on git....
(defun path-kind (path)
  (multiple-value-bind (exists error ino mode)
      (sb-unix:unix-lstat path)
    (declare (ignore error ino))
    (when exists
      (case (logand mode sb-unix:s-ifmt)
    (#.sb-unix:s-ifreg :file)
    (#.sb-unix:s-ifdir :directory)))))

(defun get-filename (filename) ;wip
  "Get a usable path for writing the image to, default to ./out.png if bad"
  (if (or (t)
          (t)) (t)
      ((if (char= #\/ (char filename 0));check if name starts with "/"
         (pathname filename) ;use user input filename
         (merge-pathnames (truename ".") ;get current dir
                          (pathname filename))))))

(defun sanitize-ang (ang)
  "run mod 360 and convert to rad"
  (let ((ang-sign (/ ang (abs ang))))
    (degtorad (* ang-sign 
                 (mod (abs ang) 360)))))

;FIX: image is flipped in the x direction, can just flip before saving
(defun img-waves (wave-func param-obj base-img) ;;main
  "Create a new image by run the function WAVE-FUNC over BASE-IMG with parameters"
  (let* ((new-img (imago:make-rgb-image
                    (imago:image-width base-img)
                    (imago:image-height base-img)
                    (slot-value param-obj 'bg-color)))
         (out-file (get-filename (slot-value param-obj 'filename))))
    (main-loop base-img new-img
               (slot-value param-obj 'n-lines)
               (slot-value param-obj 'angle)
               (slot-value param-obj 'offset)
               (slot-value param-obj 'g-up);add g-down later or remove?
               wave-func
               (slot-value param-obj 'l-thickness)
               (slot-value param-obj 'l-color))
    (imago:write-image new-img out-file)))

;;image tests:
;NOTE: all of this will be moved later

(defun drawpoints-filled (point-list image)
  (dolist (p point-list)
    (prog1 (draw-filled-circle image (first (first p)) (second (first p)) 5 imago:+red+)
      (draw-filled-circle image (first (second p)) (second (second p)) 5 imago:+blue+))))

(defun drawpoints (point-list image)
  (dolist (p point-list)
    (prog1 (imago:draw-circle image (first (first p)) (second (first p)) 10 imago:+red+)
      (imago:draw-circle image (first (second p)) (second (second p)) 10 imago:+blue+))))

(defun draw-straight-lines (point-list image)
  (dolist (p point-list)
    (imago:draw-line image (first (first p)) (second (first p))
                     (first (second p)) (second (second p))
                     imago:+black+)))
;drawing lines and points test:
(setq myimage2 (imago:make-rgb-image 300 300 imago:+white+))
(drawpoints-filled (gen-start-points 100
                                     (degtorad 10)
                                     0
                                     (list (imago:image-width myimage2)
                                           (imago:image-height myimage2)))
                   myimage2)
(draw-straight-lines (gen-start-points 100
                                     (degtorad 10)
                                     0
                                     (list (imago:image-width myimage2)
                                           (imago:image-height myimage2)))
                   myimage2)
(imago:write-png myimage2 "~/Documents/projects/imgWaves/out.png")

(defun gen-start-points-test (line-num a offset blankimg)
  (let ((p-l (gen-start-points line-num
                               a
                               offset
                               (list (imago:image-width blankimg)
                                     (imago:image-height blankimg)))))
    (print p-l)
    (drawpoints-filled p-l blankimg)
    (draw-straight-lines p-l blankimg)
    (imago:write-png blankimg "~/Documents/projects/imgWaves/out.png")))

(defun gainline-test (g image angle line-num) ;TODO: continue?
  (let* ((imgsize (list (imago:image-width image)
                        (imago:image-height image)))
         (line-points (gen-start-points line-num (degtorad angle) 0 imgsize)))
  (print line-points)
  (print "a")
  (print (make-gain-line-linear g image (first line-points)))))

(defun import-binary-image (location) ;; not using, remove
    (defvar (imago:convert-to-binary ((imago:read-image "./test.png") 10))))

;imago:invert not in quicklisp but in the imago git repo
;probably easy to implement self where needed
;(defvar gray-image (imago:invert (imago:convert-to-grayscale ;use something like this
;                     (imago:read-image "~/Documents/projects/imgWaves/src/test.png"))))
(setq gray-image (imago:convert-to-grayscale ;use something like this
                     (imago:read-image "~/Documents/projects/imgWaves/pingus.png")))

(print (gray-pixel gray-image 150 150))

(imago:write-png gray-image "~/Documents/projects/imgWaves/bin.png")
(import-binary-image 1)

(imago:write-png myimage2 "hello.png"); should write to current path...

;(imago:do-region-pixels (myimage color x y 25 25 50 50)
;  (setf color (imago:make-color 0 0 255 127)))

;(defvar myimage (imago:read-image "~/Pictures/finn/ascent2.jpg"))

;(defvar embossedimage (imago:emboss myimage :angle (* pi 0.75) :depth 1.0))

;(imago:write-image embossed "~/Pictures/embossed.jpg")

;PERF: TEST OF MAIN LOOP:
(defun temp-sine-wave (g n x)
  (+ (* g 20) (* g (+ 1 (* n 0)) 10 (sin (* x 0.3)))))
(defun no-wave (g n x)
  (* g 20))
(defun sawtooth-wave (g n x) ;normalize g
  (* g (mod x 20) 1))

(defvar params-in (make-instance 'params))
(img-waves #'temp-sine-wave params-in base-bg-image)

(main-loop base-bg-image myimage2 80 80 0 10 #'temp-sine-wave 4 imago:+blue+)
(setq myimage2 (imago:make-rgb-image 1920 1080 imago:+black+))
(imago:write-png myimage2 "~/Documents/projects/imgWaves/bgout.png")
(defvar base-bg-image (imago:convert-to-grayscale ;use something like this
                     (imago:read-image "~/Documents/projects/imgWaves/bg.png")))


;silly gif test:
(defun speeeen ()
  (let ((outImg (imago:make-rgb-image 600 600 imago:+white+))
        (dirname "~/Documents/projects/imgWaves/fungif/")
        (outnum 1))
   (loop for i from -180 to 180 by 1
        do (prog1 (main-loop gray-image outImg 30 i 0 20 #'temp-sine-wave 1 imago:+black+)
             (imago:write-png outImg
                              (concatenate 'string dirname (write-to-string outnum) ".png"))
             (setq outImg (imago:make-rgb-image 600 600 imago:+white+))
             (incf outnum)))))
(speeeen)
