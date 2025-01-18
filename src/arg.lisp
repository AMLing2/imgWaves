;argparser:

;(setq *posix-argv* (list "sbcl" "-rR" "5" "-a" "--long_arg" "153000.0"))

(defun first-is-dash? (arg); dont need to check if string type, *posix-argv* uses strings only
  (equalp (char arg 0)
          (char "-" 0)))
(defun get-arg-val (arg-list)
  (unless (<= (list-length arg-list) 1)
    (unless (first-is-dash? (second arg-list))
      (second arg-list))))

(defun split-arg (inString) ;;allows options such as -Rr and 
  "return list of split argument for combinations such as -ab, passes --long_arg as unsplit string"
  (if (and (first-is-dash? inString)
           (equalp (char inString 1)
                   (char "-" 0)))
      (list (remove #\- inString)) ;if --long_arg, will remove --long-arg names, change?
      (loop for char across (remove #\- inString)
            collect (string char))))

(defun parse-args (arg-func arg-list)
  "iterate argv over a function ARG-FUNC with arguments (DASH-ARG VALUE), VALUE is NIL if no input and a string of the value otherwise, DASH-ARG is passed without the - , '-o' becomes 'o' and '--long_arg' becomes 'long_arg'"
  (unless (null arg-list)
    (when (first-is-dash? (first arg-list))
      (dolist (arg (split-arg (first arg-list)) arg)
        (funcall arg-func arg (get-arg-val arg-list))))
    (parse-args arg-func (rest arg-list))))

(parse-args #'arg-example *posix-argv*)

(defun arg-example (dash-arg val)
  (print (list "dash:" (string dash-arg) "val:" (string val)))
  (cond 
    ((string= dash-arg "a") (print "hello a"))
    ((string= dash-arg "long_arg") (print "long arg detected"))
    ((string= dash-arg "r") (print "split r1"))
    ((string= dash-arg "R") (print "split R2"))))

