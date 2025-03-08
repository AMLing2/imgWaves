# imgWaves

### Building
Dependencies:
- SBCL
- Quicklisp set up
- (optional for animations) imagemagick

In the project's root directory, build with make:
```bash
make
```
### Usage
See the -h argument for the options.
```bash
imgwaves -h
```

Input a b/w or grayscale image where a specified function (a sine wave by default) will be drawn in a line over the image, being flat in black regions and following the function in white or brighter regions.\
The ramp options -R and -r specify how fast to ramp up the gain for the function when moving from dark to brigher pixels, set -Rr to 0 to disable ramping.\
Set the output file as a .svg to create a vector graphics image. The supported output filetypes are: .png, .jpg, .pnm, .tga, .svg.\
Other functions such as the ones in the **ex/** folder can be input with the -f option.\
Example:
```bash
imgwaves my_image.png -Rr 10 -n 30 -a 20 -L 3 -c ABCC2C -b 6A779C  -f ex/triangle_wave.lisp
```
The units in the options are generally in pixels or 1/pixel for the ramping.

##### Creating custom line functions
A custom function for the shape of the lines to output the local pixel Y value of the line can be written in Common Lisp in the form:
```lisp
(defun my-func (gain line-number x)
 (* g 50)) ;example: increase the height of the line on white pixels
```
Where:
- *gain* is a floating number from 0 to 1
- *line-number* represents which of the lines is currently being drawn
- *x* is the current value across the line's local x axis\

See the examples in the **ex/** folder.

##### Creating animations
Animations can be created with a lisp function to modify the parameters struct during runtime, passed with the -A or --animation argument \
The function should return nil when it is finished, otherwise it will loop infinitely. \
The parameters struct can be found at the top of */src/imgWaves.lisp*
```lisp
(defun my-animation (params-obj run-count)
    (incf (slot-value params-obj 'angle)) ;;example: increase the angle by 1 degree each frame
    (<= run-count 100)) ; return nil and finish animation once run-count is over 100 frames
```
Where:
 - *params-obj* is the parameters struct to modify, which can be seen at the top of src/imgWaves.lisp
 - *run-count* is an integer referring to which frame the animation is on.

The initial parameters are based on the command line inputs, but this can be changed by setting the initial parmeters when run-count is 0. \
Each frame will be saved as a .png image in a new folder *img_anim/* which can be made to a .gif with imagemagick or other similar software:
```bash
magick -delay 2 img_anim/*.png my-animation.gif
```
See the examples in the *ex/* folder.

### Known issues
 - High offset values which causes lines to wrap twice will have a wrong placement
 - The --help argument is overtaken by SBCL's --help option...

### TODO:
 - interpolation or increased density of points
 - drawing on top of original image
 - no overlapping (works somewhat with vector graphics)
 - testing on other distributions of CL
 - possibly a GUI port in the future
