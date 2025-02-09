;argparser:

;(setq *posix-argv* (list "sbcl" "-rR" "5" "-a" "5"))

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

(defun file-exists-p (path) ;rename? existing function with different result: uiop:file-exists-p
  "Check if PATH refers to an existing file and not a directory."
  ;; Convert string to pathname if necessary
  (let ((pathconv (pathname path)))
    (and (not (equal (probe-file pathconv) nil))
         (not (uiop:directory-pathname-p pathconv)))))

(defun parse-last-file (arg-list help-func); ex: [PROGRAM] [OPTIONS] [FILE]
  (let ((last-val (first (reverse arg-list))))
        (cond ((or (= (length arg-list) 1) ;checked twice but keeping in case only this func is used
                   (equalp (char last-val 0)
                           (char "-" 0)))
               (progn
                 (format t "No file input ~%")
                 (funcall help-func)
                 (uiop:quit 1)))
              ((not (file-exists-p last-val))
               (progn 
                 (format t "Invalid file input ~%")
                 (uiop:quit 1)))
              (t last-val))))

(defun parse-main-file (arg-list help-func); ex: [PROGRAM] [FILE] [OPTIONS]
  "get input file located before or after options"
  (cond ((= (length arg-list) 1)
         (progn
           (format  t "No file input ~%")
           (funcall help-func)
           (uiop:quit 1)))
        ((equalp (char (second arg-list) 0)
                 (char "-" 0)) 
         (parse-last-file arg-list help-func)) ;check if file is at the end instead
        ((not (file-exists-p (second arg-list)))
              (progn 
                (format t "Invalid file input ~%")
                (uiop:quit 1)))
        (t (second arg-list))))
  
;;;; file reading and function input

(defun get-out-filename (filename &optional usable-filetypes)
  "Get a usable path and filename for writing a file to, returns a filename and if the file overwrites another."
  (unless (null usable-filetypes)
    (when (equal nil
                 (find (nth-value 1 (uiop:split-name-type (namestring filename)))
                  usable-filetypes
                  :test #'equal))
      (format t "Unexpected output filetype, got: ~a, expected: ~{~a~^, ~}"
              (nth-value 1 (uiop:split-name-type (namestring filename)))
              usable-filetypes)
      (uiop:quit 1)))
  (let ((newfile (if (equal nil (find #\/ (namestring filename)))
                     (merge-pathnames #P"./" filename) ; current dir
                   (pathname filename))))
    (cond ((file-exists-p newfile) ;check if file exists
           (values newfile T))
          ((uiop:directory-pathname-p newfile) ;check if directory
           (progn
             (format t "Chosen output filename: ~a is a directory and not a file"
                     filename)
             (uiop:quit 1)))
          (t (values newfile nil))))) ; is new file

(defun read-forms-from-file (func-path)
  (with-open-file (stream func-path)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun get-wave-func (func-path func-arg-count) ;FIX: dosent output function type
  "get last defined function, return nil if no function defined"
  (unless (file-exists-p func-path)
    (format t "Invalid function file input ~%")
    (uiop:quit 1))
  (load func-path)
  (let ((forms-list (read-forms-from-file func-path))
        (last-func nil)
        (last-args-count func-arg-count))
    (dolist (form forms-list)
      (when (and (consp form)
                 (eq (first form) 'DEFUN)
                 (symbolp (second form)))
        (setf last-func (second form))
        (setf last-args-count (length (third form))))) 
    (if (= last-args-count func-arg-count)
        (coerce last-func 'function)
        (error "Input function has an invalid number of arguments, expected: ~D, got: ~D" 
               func-arg-count 
               last-args-count))))

;(defun arg-example (dash-arg val)
;  (format t (list "dash:" (string dash-arg) "val:" (string val)))
;  (cond 
;    ((string= dash-arg "a") (format t "hello a"))
;    ((string= dash-arg "long_arg") (format t "long arg detected"))
;    ((string= dash-arg "r") (format t "split r1"))
;    ((string= dash-arg "R") (format t "split R2"))))

