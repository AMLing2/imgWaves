;argparser:

(setq *posix-argv* (list *posix-argv* "-rR" "-i"))

(defun splitArg (inString) ;;allows options such as -Rr
  (let ((splitString (list (subseq inString 1 2))))
    (dotimes (n (- (length inString) 2) splitString)
      (nconc splitString (list (subseq inString (+ n 2) (+ n 3)))))))
    ;;;(values splitString)))

(defun parseArgs () ;;(print *posix-argv*)
  (let ((arg-list *posix-argv*))
    (dotimes (i (- (list-length *posix-argv*) 1))
      (setq arg-list (rest arg-list)) ;;(print arg-list)
      (if (equalp (char (first arg-list) 0) (char "-" 0))
          (dolist (arg (splitArg (first arg-list)) arg) 
            (cond ((string= arg "i") (print "hello i"))
                  ((string= arg "R") (print "hello R"))
                  )))))) ;;it veeerks
;;TODO: would be cool to make parseArgs a macro instead, where i can simply drop in (cond ...)
;;TODO: add --text options, should be a really easy implementation :D
(parseArgs)
