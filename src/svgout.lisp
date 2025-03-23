(defun write-svg-header (filestream image-size bg-color line-color stroke)
  (format filestream 
"<svg
width=\"~d\"
height=\"~d\"
fill=\"~a\"
xmlns=\"http://www.w3.org/2000/svg\">

<rect width=\"100%\" height=\"100%\" fill=\"~a\" />~%" 
    (first image-size)
    (second image-size)
    bg-color
    bg-color)
  (format filestream "
  <style>
    <![CDATA[
      polyline.l-style {
      fill: ~a;
      stroke: ~a;
      stroke-width: ~d;
      }
    ]]>
  </style>~%~%"
  bg-color
  line-color
  (+ 1 stroke)))

(defun write-svg-end (filestream)
  "Write svg end tag"
  (format filestream "~%</svg>~%"))

(defun add-line-to-svg (filestream line-list)
  (format filestream "<polyline class=\"l-style\" points=\"~{~{~3$~^,~}~^ ~}\"/>~%"
          line-list))
