(ql:quickload "IMAGO")
;(load #P"~/Documents/projects/imgWaves/src/arg.lisp")
(load #P"src/arg.lisp")
(load #P"src/svgout.lisp")

(defconstant +arg-count+ 3) ;number of arguments for the input function (ex below)
(defun base-sine-wave (g n x)
  (+ (* g (+ 1 (* n 0)) 10 (sin (* x 0.3)))))

(defstruct params
  (g-up 10.0 :type float)
  (g-down 10.0 :type float)
  (n-lines 30 :type integer)
  (angle 0.0 :type float) ;degrees 
  (offset 0 :type integer)
  (l-thickness 0 :type integer) ;0 = 1px
  (l-color imago:+black+ :type imago:rgb-pixel)
  (bg-color imago:+white+ :type imago:rgb-pixel)
  (img-invert nil :type boolean)
  (in-func #'base-sine-wave :type function)
  (filename #p"./out.png" :type pathname))

(defun print-help ()
  (format t "Usage: imgwaves [image file] [options]~%~%")
  (format t "Options:~%")
  (format t "  -R, --ramp-up <float>        Set the ramp gain up.~%")
  (format t "  -r, --ramp-down <float>      Set the ramp gain down.~%")
  (format t "  -a, --angle <float>          Set the angle of lines in degrees.~%")
  (format t "  -n, --num-lines <integer>    Set the number of lines.~%")
  (format t "  -o, --output-file <filename> Set the output image filename and filetype (default: png).~%")
  (format t "  -t, --line-offset <integer>  Set the offset of lines in the relative Y direction.~%")
  (format t "  -b, --background-color <hex> Set the background color in the format \"RRGGBB\"~%")
  (format t "  -l, --line-color <hex>       Set the line color in the format \"RRGGBB\"~%")
  (format t "  -L, --line-thickness <integer> Set the line thickness in pixels.~%")
  (format t "  -I                           Invert the image.~%")
  (format t "  -f, --function <filename>    Set a custom line function from a lisp program.~%")
  (format t "  -h, --help                   Display this help message.~%")
  (format t "Example:~%")
  (format t "  imgwaves base.png -R 5.0 -a 45 -n 100 -o out.png~%"))

; conversions
(defun stoi (str)
  "string to integer"
  (parse-integer str))
(defun stof (str)
  "string to float"
  (float (read-from-string str)))
(defun degtorad (deg) 
  (* deg (/ pi 180)))
(defun sanitize-ang (ang)
  "mod 360 and convert to rad"
    (degtorad (* (signum ang) 
                 (mod (abs ang) 360))))
(defun hex-to-color (hex-str)
  "convert a string of a hex value in the format \"RRGGBB\" or \"#xRRGGBB\" to imago:rgb-pixel type"
  (if (or (equal (subseq hex-str 0 2) "#x") ;might remove this
          (= (length hex-str) 8)
          (= (length hex-str) 6))
      (let ((hex (if (= (length hex-str) 8)
                     (subseq hex-str 2)
                     hex-str)))
        (imago:make-color
          (parse-integer (subseq hex 0 2) :radix 16)
          (parse-integer (subseq hex 2 4) :radix 16)
          (parse-integer (subseq hex 4 6) :radix 16)))
      (progn
        (format t "Expected hex color in format: RRGGBB, got: ~a~%" hex-str)
        (uiop:quit 1))))

(defun left-pad-2 (str)
  (if (= (length str) 1)
    (concatenate 'string "0" str)
      str))
(defun imago-color-to-hex (col)
  "convert imago color format to hex of '#RRGGBB' for svg"
  (let ((r (left-pad-2 (write-to-string (imago:color-red   col) :base 16)))
        (g (left-pad-2 (write-to-string (imago:color-green col) :base 16)))
        (b (left-pad-2 (write-to-string (imago:color-blue  col) :base 16))))
    (concatenate 'string "#" r g b)))

;maybe in the future make it so the argument name is mentioned in the error message
(defgeneric fill-from-args (obj d-arg val)
  (:documentation "Set parameters from command line arguments."))

(defmethod fill-from-args ((obj params) d-arg val)
  (cond
    ((or (string= d-arg "-h")
         (string= d-arg "--help"))
     (print-help)
     (uiop:quit 0))

    ;; Set the ramp gain up/down ('R'/'r' short, 'ramp-up'/'ramp-down' long)
    ((or (string= d-arg "R")
         (string= d-arg "ramp-up")) (setf (slot-value obj 'g-up) (stof val)))
    ((or (string= d-arg "r")
         (string= d-arg "ramp-down")) (setf (slot-value obj 'g-down) (stof val)))

    ;; Set the angle of lines ('a' short, 'angle' long)
    ((or (string= d-arg "a")
         (string= d-arg "angle")) (setf (slot-value obj 'angle) (stof val)))

    ;; Set the number of lines ('n' short, 'num-lines' long)
    ((or (string= d-arg "n")
         (string= d-arg "num-lines")) (setf (slot-value obj 'n-lines) (stoi val)))

    ;; Set the output filename ('o' short, 'output-file' long)
    ((or (string= d-arg "o")
         (string= d-arg "output-file")) (setf (slot-value obj 'filename) 
         (get-out-filename val '("png" "jpg" "pnm" "tga" "svg"))))

    ;; Set the output filename ('f' short, 'function' long)
    ((or (string= d-arg "f")
         (string= d-arg "function")) 
     (setf (slot-value obj 'in-func) 
           (get-wave-func val +arg-count+)))

    ;; Set the offset of lines ('t' short, 'line-offset' long)
    ((or (string= d-arg "t")
         (string= d-arg "line-offset")) (setf (slot-value obj 'offset) (stoi val)))

    ;; Set the background color ('b' short, 'background-color' long)
    ((or (string= d-arg "b")
         (string= d-arg "background-color")) (setf (slot-value obj 'bg-color) (hex-to-color val)))

    ;; Set the line color ('l' short, 'line-color' long)
    ((or (string= d-arg "l")
         (string= d-arg "line-color")) (setf (slot-value obj 'l-color) (hex-to-color val)))

    ;; Set the line thickness ('L' short, 'line-thickness' long)
    ((or (string= d-arg "L")
         (string= d-arg "line-thickness")) (setf (slot-value obj 'l-thickness) (stoi val)))

    ;; set value to invert the image to true
    ((string= d-arg "I") (setf (slot-value obj 'img-invert) T))

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

(defmacro p-pos (xy-str n p-list) ;TODO: replace all [first [second p-list]] junk in file with this
  (cond ((equalp xy-str "x") (list 'first  (list 'nth n p-list)))
        ((equalp xy-str "y") (list 'second (list 'nth n p-list)))
        (t (list 'first (list 'first p-list)))))

(defun flipPoints (p-l)
  (list (second p-l) (first p-l)))

(defun clamp-ang-rad (ang)
  "run mod 2 pi and preserve sign"
  (let ((ang-sign (/ ang (abs ang))))
    (* ang-sign 
       (mod (abs ang) (* 2 pi)))))

(defun float-eq (a b)
  "compare if two floating point numbers are within a tolerence to eachother"
  (< (abs (- a b)) 1.0e-4))

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
  (cond ((float-eq ang (degtorad (- 90))) (list (first imgsize)
                                                (/ (second imgsize) 2)))
        ((float-eq ang (degtorad (- 180 90))) (list 0
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

(defun linepoints-r (cx cy a imgsize) ;;a rounded version of original function
  (let* ((xval)
         (y (if (on-right-side? a)
               (prog1
                   (line-y cx cy a (first imgsize)) ;right
                 (setq xval (first imgsize)))
              (prog1 
                 (line-y cx cy a 0.0) ;left
               (setq xval 0.0)))))
    (cond
      ((and (<= y (nth 1 imgsize))
            (>= y 0.0)) 
       (print (list (round xval) (round y)))) ;left / right
      ((> y (nth 1 imgsize)) 
        (print (list (round (line-x cx cy a (nth 1 imgsize))) (round (nth 1 imgsize))))) ; bottom
      ((< y 0.0) 
       (print (list (round (line-x cx cy a 0.0)) 0)))))) ;top

(defun init-line-points (a imgsize)
  (list (linepoints-init a imgsize)
        (linepoints-init (+ a pi) imgsize)))

(defun start-end-points2 (img l-cxcy a imgsize) ;with cx cy ;FIX: remove img
  (print (list "start-end-points2" l-cxcy))
  (drawpoints-filled l-cxcy img) ;FIX: REMOVE
  (list (linepoints-r (first l-cxcy) (second l-cxcy) a imgsize)
        (linepoints-r (first l-cxcy) (second l-cxcy) (+ a pi) imgsize)))

(defun dist-between-points (p)
  (sqrt (+ (expt (- (first (second p))
                    (first (first p))) 2)
           (expt (- (second (second p))
                    (second (first p))) 2))))

(defun mapAdd (num apply-list)
  (map 'list (lambda (x) (+ x num)) apply-list))

; taken from https://stackoverflow.com/a/14416133
(defun rmod (n min max)
  (+ (mod (+ (mod (- n min) (- max min))
             (- max min))
          (- max min))
     min))


(defun add-offset (offset a limits1 imgsize l-cxcy) ;TODO: continue
  "Add and return offset to an x or y value c and wrap if outside of image"
  (print (list "limits:" limits1))
  (let ((x-min (min 
                 (p-pos "x" 0 limits1) (p-pos "x" 1 limits1))) ;replace with loop or move to gen-start-points? same calcs are done for every line its very inefficient
        (x-max (max 
                    (p-pos "x" 0 limits1) (p-pos "x" 1 limits1)))
        (y-min (min 
                 (p-pos "y" 0 limits1) (p-pos "y" 1 limits1)))
        (y-max (max 
                    (p-pos "y" 0 limits1) (p-pos "y" 1 limits1)))
        (new-x (+ (first l-cxcy)  (* offset (sin a))))
        (new-y (+ (second l-cxcy) (* offset (cos a)))))
    (print (list "x-min" x-min "x-max" x-max "y-min" y-min "y-max" y-max "old-x" (first l-cxcy) "new-x" new-x "old-y" (second l-cxcy) "new-y" new-y))
    (cond ((float-eq 0.0 (mod a (/ pi 2))) 
           (list (+ x-min (mod new-x (- x-max x-min)))
                 (+ y-min (mod new-y (- y-max y-min)))))
           ((and 
             (or (>= new-x x-max) ;both in an annoying place
                 (<= new-x x-min))
             (or (>= new-y y-max)
                 (<= new-y y-min)))
           (progn (format t "~%~c[33m---> case 1~c[0m" #\esc #\esc) ; still broken, see a = 30 t = 100
                  (print (list (+ x-min (mod new-x x-max))
                               (+ y-min (mod new-y y-max))))))
          ((or (> new-x x-max) ;only x never seems to hit this?
               (< new-x x-min))
           (progn (format t "~%~c[32m---> case 2~c[0m" #\esc #\esc) ; still broken, see a = 30 t = 100
                  (print (list (+ x-min (/ (tan a) (mod new-y (- y-max y-min)))) 
                               (+ y-min (mod new-y (- y-max y-min))))))) ;FIX: here
          ((or (> new-y y-max) ; only y 
               (< new-y y-min))
           (progn (format t "~%~c[31m---> case 3~c[0m" #\esc #\esc) ; still broken, see a = 30 t = 100
                  (print (list (+ x-min (mod new-x (- x-max x-min))) ;FIX: here? is this correct and case 1 is wrong?
                               (+ y-min (/ (mod new-x (- x-max x-min)) (tan a)))))))
          (t (list new-x new-y)))))

(defun add-offset2 (offset a limits1 imgsize l-cxcy) ;FIX: remove, old
  "Add and return offset to an x or y value c and wrap if outside of image"
  (let* ((limits2 (init-line-points (- (/ pi 2) a) imgsize))) ; get max/min of reflection of A
        (let ((x-min (min 0
                          (p-pos "x" 0 limits1) (p-pos "x" 1 limits1) ;replace with loop or move to gen-start-points? same calcs are done for every line its very inefficient
                          (p-pos "x" 0 limits2) (p-pos "x" 1 limits2)))
              (x-max (max (first imgsize)
                          (p-pos "x" 0 limits1) (p-pos "x" 1 limits1)
                          (p-pos "x" 0 limits2) (p-pos "x" 1 limits2)))
              (y-min (min 0
                          (p-pos "y" 0 limits1) (p-pos "y" 1 limits1)
                          (p-pos "y" 0 limits2) (p-pos "y" 1 limits2)))
              (y-max (max (second imgsize)
                          (p-pos "y" 0 limits1) (p-pos "y" 1 limits1)
                          (p-pos "y" 0 limits2) (p-pos "y" 1 limits2)))
              (new-x (+ (first l-cxcy)  (* offset (sin a))))
              (new-y (+ (second l-cxcy) (* offset (cos a)))))
          (print (list "x-min" x-min "x-max" x-max "y-min" y-min "y-max" y-max "old-x" (first l-cxcy) "new-x" new-x "old-y" (second l-cxcy) "new-y" new-y))
          (cond ((and 
                   (or (> new-x x-max) ;check if both need to be wrapped
                       (< new-x x-min))
                   (or (> new-y y-max)
                       (< new-y y-min))) 
                 (progn (print "---> case 1") ;FIX: broken
                 (print (list (+ (rmod new-x x-min x-max) x-min)
                              (+ (rmod new-y y-min y-max) x-min))
                        )))
                ((or (> new-x x-max) ;only x
                     (< new-x x-min))
                 (progn (print "---> case 2")
                 (print (list (rmod new-x x-min x-max) ; seems to work fine?
                       (+ (- new-y (* (cos a) (second imgsize))) y-min)))))
                ((or (> new-y y-max) ; only y
                     (< new-y y-min))
                 (progn (print "--->case 3")
                 (print (list (+ (- new-x (* (sin a) (first imgsize))) 
                                 (* (sin a) 2 x-min)) ;FIX: this kind of works but not perfect
                       (rmod new-y y-min y-max)))))
                (t (list new-x new-y))))))
(cos (degtorad 30))

;;           FIX: REMOVE vvv
(defun gen-start-points (img line-num a offset imgsize)
  (let* ((imgsizefixed (mapAdd -1 imgsize))
         (p (init-line-points a imgsizefixed))
         (dist-x (/ (- (first (second p))
                   (first (first p))) (+ line-num 1)))
         (dist-y (/ (- (second (second p))
                   (second (first p))) (+ line-num 1))))
    (print p)
    (if (= offset 0)
        (loop for l from 1 to line-num by 1
              collect (start-end-points2 img ;FIX: REMOVE
                        (list (+ (* dist-x l) ;cx
                                 (first (first p)))
                              (+ (* dist-y l) ;cy
                                 (second (first p))))
                        a
                        imgsizefixed))
        (loop for l from 1 to line-num by 1
              collect (start-end-points2 img ;FIX: REMOVE
                        (add-offset offset a p imgsizefixed
                                    (list (+ (* dist-x l) ;cx
                                             (first (first p)))
                                          (+ (* dist-y l) ;cy
                                             (second (first p)))))
                                    a
                                    imgsizefixed)))))

;;;;; Image processing - adding gain:

(defun add-gain (g up? index line) ; destructve modification of line list
  (unless (<= g 0.0)
    (let ((ind-move (if up? 1 -1))
          (cnt 0))
      (loop for g-add downfrom (nth index line) to 0 by g
            do (progn 
                 (setq cnt (+ cnt ind-move))
                 (if (or (> (+ index cnt) (- (list-length line) 1))
                         (< (+ index cnt) 0)) 
                     (return-from add-gain nil) ;use instead of (break)
                   (if (> (nth (+ index cnt) line) g-add)
                       (return-from add-gain nil)
                       (setf (nth (+ index cnt) line) g-add)))))))) ;(+ index cnt) is done 4 times here, move to let?

(defun apply-gain-list (g-up g-down index-list grayscale-line)
  (let ((g nil))
    (dolist (i index-list)
      (setq g (if (second i) g-up g-down))
      (add-gain g (second i) (first i) grayscale-line))))

(defun find-color-jumps (grayscale-line) ;FIX: this function will sometimes place indexes which are right next to eacother, leading to the gain function being run across the same area twice
  (let ((prev (first grayscale-line)) ;   a fix would be to check if previous would overwrite it by checking nth *prev* grayscale-line - g > nth *current* grayscale-line
        (current)
        (index-list nil))
    (loop for i from 1 to (- (length grayscale-line) 1) by 1
          do (prog1 (setq current (nth i grayscale-line))
               (cond ((> (- prev current) 0); goes up
                      (push (list (- i 1) T) index-list)) ;adds item to front but does not matter in this context
                     ((< (- prev current) 0); goes down
                      (push (list i nil) index-list))) 
               (setq prev current)))
    (values index-list)))

;find-color-jumps test, temp
(defun find-color-jumps-test () ;finished i think FIX: remove
  (let ((test-list (fill (make-list 200 :initial-element 0)
                         255 :start 120 :end 160)))
; if &KEY, this means you need to use :name to fill in the variables as they are
    (print (find-color-jumps test-list))))
;(find-color-jumps-test)

(defun make-gain-line-linear (g-up g-down image line-point) ; rename in the future
  (let ((g-list nil))                                       ; for both linear and smooth
    (imago:do-line-pixels (image color x y ;if 300x300 image then 0->299
                                 (first (first line-point))   ;x1
                                 (second (first line-point))  ;y1
                                 (first (second line-point))  ;x2
                                 (second (second line-point)));y2
      (push (logand color #x00FF) g-list)) ;should be ANDed with #x00FF?
    (when (or (> g-up 0.0)
              (> g-down 0.0))
      (apply-gain-list g-up
                       g-down
                       (find-color-jumps g-list) ; this line works
                       g-list)); dont reverse, dosent seem to be destructive..
    (values (reverse g-list))))                ;a double reverse that might work well?

;;;;; creating and reading shape for relative line:

(defun create-relative-line-static (line-func gain-line line-num) ;no idea what to call this function
  "Use user input function to create lines with gain line"
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

; based on https://stackoverflow.com/a/1201227
(defun draw-filled-circle (image x y r color)
  (let ((h 0))
    (loop for nx from (- r) to r by 1
           do (setq h (truncate (sqrt (- (* r r ) (* nx nx)))))
           (loop for ny from (- h) to h by 1
                 do 
                 (imago:draw-point image
                                   (+ x nx)
                                   (+ y ny)
                                   color)))
    (imago:draw-circle image x y r color))) ;smoothen edges

(defun invert-image (image) ; imago:invert dosen't exist?
  (imago:do-image-pixels (image gray-col x y)
    (setf gray-col (imago:invert-gray gray-col))))

(defun gray-pixel (image x y)
  (logand (imago:image-pixel image x y) #x00FF))

(defun x-func-add (old-x func-val angle) ;func-val acts as len for a new vector
  (round (+ old-x (* func-val (- (sin angle))))))
(defun y-func-add (old-y func-val angle)
  (round (+ old-y (* func-val (- (cos angle))))))

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

(defun points-func-line (image line-points relative-shape-line angle)
  "Return list of points of function going over image"
  (let ((i 0)
        (p-list nil))
    (imago:do-line-pixels (image color x y 
                                 (first (first line-points));x1
                                 (second (first line-points));y1
                                 (first (second line-points))
                                 (second (second line-points)))
      (prog1
          (push (list
                  (x-func-add x (nth i relative-shape-line) angle)
                  (y-func-add y (nth i relative-shape-line) angle))
                p-list)
        (incf i)))
    p-list))

(defun make-vector-loop (out-file line-points line-func g-up g-down base-img angle
                          line-thickness line-color bg-color)
  (with-open-file (filestream out-file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
    (write-svg-header filestream (list (imago:image-width base-img)
                                       (imago:image-height base-img))
                      (imago-color-to-hex bg-color)
                      (imago-color-to-hex line-color)
                      line-thickness)
    (let ((line-index 0))
      (dolist (p line-points)
        (prog1
            (add-line-to-svg
              filestream
              (points-func-line 
                base-img
                (flipPoints p)
                (create-relative-line-static 
                  line-func 
                  (make-gain-line-linear g-up
                                         g-down
                                         base-img
                                         p)
                  line-index)
                (sanitize-ang angle)))
              (incf line-index)))
      (write-svg-end filestream))))

;putting it all together
(defun main-loop (base-img out-file num-lines angle offset g-up g-down
                           line-func line-thickness line-color bg-color vectorize)
  (let* ((imgsize (list (imago:image-width base-img)
                        (imago:image-height base-img)))
         (new-img (imago:make-rgb-image ;FIX: MOVE TO SECOND LET
                    (imago:image-width base-img)
                    (imago:image-height base-img)
                    bg-color))
         (line-points (gen-start-points new-img num-lines ;FIX: REMOVE new-img
                                        (sanitize-ang angle)
                                        offset 
                                        imgsize)))
    (if vectorize
        (make-vector-loop out-file line-points line-func g-up g-down base-img angle
                          line-thickness line-color bg-color)
      (let ((line-index 0))
        (dolist (p line-points)
          (prog1 
              (draw-func-line new-img (flipPoints p)
                              (create-relative-line-static line-func 
                                                           (make-gain-line-linear g-up
                                                                                  g-down
                                                                                  base-img
                                                                                  p)
                                                           line-index)
                              (sanitize-ang angle) line-thickness line-color)
            (incf line-index)))
        (imago:write-image new-img out-file)))
    (format t "Image saved as: ~a~%" (namestring out-file))))

(defun make-vector-p (filename)
  (equalp "svg" 
          (nth-value 1 (uiop:split-name-type (namestring filename)))))

(defun img-waves (wave-func param-obj base-img) ;;main
  "Create a new image by using the function WAVE-FUNC over BASE-IMG with parameters"
  (let ((out-file (get-out-filename (slot-value param-obj 'filename))))
    (when (slot-value param-obj 'img-invert)
      (invert-image base-img))
    (main-loop base-img
               out-file
               (slot-value param-obj 'n-lines)
               (slot-value param-obj 'angle)
               (slot-value param-obj 'offset)
               (slot-value param-obj 'g-up)
               (slot-value param-obj 'g-down)
               wave-func
               (slot-value param-obj 'l-thickness)
               (slot-value param-obj 'l-color)
               (slot-value param-obj 'bg-color)
               (make-vector-p (slot-value param-obj 'filename)))))

;TODO: continue
(defun start-program () ;start function for sbcl, read args then begin
  (let ((prog-params (make-params))
        (input-img (parse-main-file *posix-argv* #'print-help)))
    (parse-args  
      (lambda (arg val) 
        (fill-from-args prog-params arg val)) ; error handling done here
      *POSIX-ARGV*)
    (img-waves (slot-value prog-params 'in-func)
               prog-params
               (imago:convert-to-grayscale
                 (imago:read-image input-img)))))

;(setq *posix-argv* (list "sbcl" "-rR" "5" "-a" "5"))
;*POSIX-ARGV*
;(start-program)




(defun drawpoints-filled (p image) ;FIX: TEST, REMOVE ALL REFERENCES TO
      (draw-filled-circle image (round (first p)) (round (second p)) 30 imago:+green+))

;;image tests:
;NOTE: all of this will be moved later
(when (eq t nil) ;block comment for repl loading

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

(defun gainline-test (g image angle line-num)
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
  (+ (* g (+ 1 (* n 0)) 10 (sin (* x 0.3)))))
  ;(+ (* g 20) (* g (+ 1 (* n 0)) 10 (sin (* x 0.3)))))
(defun no-wave (g n x)
  (* g 20))
(defun sawtooth-wave (g n x) ;normalize g
  (* g (mod x 20) 1))

(defvar params-in (make-instance 'params))
(img-waves #'temp-sine-wave params-in base-bg-image)

(defvar myimage2 (imago:make-rgb-image 1920 1080 imago:+black+))
(defvar base-bg-image (imago:convert-to-grayscale ;use something like this
                     (imago:read-image "~/Documents/projects/imgWaves/polkadots.jpg")))
(setq base-bg-image (imago:convert-to-grayscale ;use something like this
                     (imago:read-image "~/Documents/projects/imgWaves/src/test_i2.png")))
(setq base-bg-image (imago:invert-gray (imago:image-pixels base-bg-image)))

(imago:do-image-pixels (base-bg-image gray-col x y)
                  (setf gray-col (imago:invert-gray gray-col)))

(full-test 20 10 0 1 10 #'temp-sine-wave 0 T)
(full-test 10 0 0 20 20 #'temp-sine-wave 2 nil)

(defun full-test (num-lines a offset g-up g-down l-func stroke blur) ;do the two defvar above first in REPL
  (let ((newimg (imago:make-rgb-image (imago:image-width base-bg-image)
                                       (imago:image-height base-bg-image)
                                       imago:+black+)))
  (main-loop base-bg-image newimg num-lines a offset g-up g-down l-func stroke imago:+white+)
  (when blur  
    (imago:blur newimg)) ;not work ;|
  (imago:write-png newimg "~/Documents/projects/imgWaves/bgout.png")))

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

)
