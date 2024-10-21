;argparser:

(defun parseArgs () (print *posix-argv*)
  (let ((arg-list *posix-argv*))
    (dotimes (i (- (list-length *posix-argv*) 1))
      (setq arg-list (rest arg-list)) (print arg-list)
        (cond ((string-equal (first arg-list) "-i") (print "hello i"))
              ((string-equal (first arg-list) "-b") (print "hello b"))))))



;(ql:quickload "IMAGO")

;(defvar myimage (imago:read-image "~/Pictures/finn/ascent2.jpg"))

;(defvar embossedimage (imago:emboss myimage :angle (* pi 0.75) :depth 1.0))

;(imago:write-image embossedimage "~/Pictures/embossed.jpg")
