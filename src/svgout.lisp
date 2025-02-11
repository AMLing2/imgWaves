(defun write-svg-header (filestream image-size bg-color line-color stroke)
  (format filestream "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~d\" height=\"~d\" fill=\"~a\">~%" 
          (first image-size)
          (second image-size)
          bg-color)
  (format filestream 
          "<style>~%
          .l-style{~%
          fill: ~a~%
          stroke: ~a~%
          stroke-width: ~d~%
          }~%
          </style>~%~%"
          bg-color
          line-color
          stroke))

(defun write-svg-end (filestream)
  "Write svg end tag and close filestream"
  (format filestream "~%</svg>~%")
  (close filestream))

(defun svg-start (filename image-size bg-color line-color stroke)
  (let ((filestream (open filename :DIRECTION :output)))
    (write-svg-header filestream image-size bg-color line-color stroke)
    filestream))

(defun add-line-to-svg (filestream line-list)
  (format filestream "<polyline class=\"l-style\" points=\"~{~{~a~^,~}~^ ~}\"/>~%"
          line-list))

(svg-start #p"~/Pictures/test.svg" (list 300 300))

