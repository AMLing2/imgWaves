# imgWaves

### Building
Dependencies:
- SBCL
- Quicklisp set up
- (optional for animations) FFMPEG\

In the project's root directory, build with make:
```
make
```
### Usage
See the -h argument for the options.
```
imgwaves -h
```

Input a b/w or grayscale image where a specified function (a sine wave by default) will be drawn in a line over the image, being flat in black regions and following the function in white or brighter regions, the image can be inverted with the -I option.\
The ramp options -R and -r specify how fast to ramp up the gain for the function when moving from dark to brigher pixels, set -Rr to 0 to disable ramping.\
Colors can be changed with the --line-color and --background-color options, and the line thickness can be set with the -L option.\
The output file can be specified with the -o option where it will overwrite the previous file. Set the output file as a .svg to create a vector graphics image. The supported output filetypes are: .png, .jpg, .pnm, .tga, .svg.\
Other functions such as the ones in the **ex/** folder can be input with the -f option.\
Example:
```
imgwaves my_image.png -Rr 10 -n 30 -a 20 -L 3 -c ABCC2C -b 6A779C  -f ex/triangle_wave.lisp
```
The units in the options are generally in pixels or 1/pixel for the ramping.

##### Creating custom line functions
A custom function for the shape of the lines to output the local pixel Y value of the line can be written in Common Lisp in the form:
```
(defun my-func (gain line-number x)
 ...)
```
Where:
- *gain* is a floating number from 0 to 1
- *line-number* represents which of the lines is currently being drawn
- *x* is the current value across the line's local x axis\
See the examples in the **ex/** folder.

### Known issues
 - High offset values which causes lines to wrap twice will have a wrong placement

### TODO:
 - interpolate, if distance between 2 points is > [input] pixels, draw a line between them
 - drawing ontop of original image
 - no overlapping (works somewhat with vector graphics)
