(defun linepoints-test (ang-add)
  (let ((ang 0))
  (loop while (and (<= ang (* 2 pi)) (>= ang (- (* 2 pi))))
        do (prog1
               (print (linepoints 300 300 ang (list 600 600))) 
             (setf ang (+ ang ang-add))))))

(linepoints-test (- 0.01))
(linepoints-test 0.01)

;; one with comments:
(defun linepoints (cx cy a imgsize) ;;no error thrown on (tan 90), so use htis then check if OOB
  (let* ((xval) (y (if (on-right-side? a)
               (prog1 ;prog1 returns first and runs both expr
                   (line-y cx cy a (first imgsize)) ;right
                 (setq xval (first imgsize))
                 (print "right aa")) ; <- this line causes fucky ups on 4th quadrant
              (prog1 
                 (line-y cx cy a 0.0)
               (setq xval 0.0)
                (print "left aa"))))); left
    (cond
      ((and (<= y (nth 1 imgsize))
            (>= y 0.0)) 
       (prog1 (list xval y)
         (print "left / right"))) ;;left or right, works
      ((> y (nth 1 imgsize)) 
       (prog1 (list (line-x cx cy a (nth 1 imgsize)) (nth 1 imgsize))
         (print y))) ;;; bot, wrong in 4rd quadrant
      ((< y 0.0) 
       (prog1 (list (line-x cx cy a 0.0) 0.0) 
         (print y)))))) ;;; top, works
