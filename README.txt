A program which generates images of the Mandelbrot set. 
It has three modes:

"manual": Manually navigate through The Mandelbrot set. An image is generated, and by using the arrow keys, you can move around in the complex plane and zoom in or out. Every time you move, the image file is modified.

"zoom": Creates a series of images, zooming in on a point in the complex plane. These can later be combined into a gif using the gifgen.py program. The user passes the x coord, y coord, max amount of iterations, and amount of frames to render.

"histzoom": Similar to the above, but where the colors of every frame are based off a histogram of the total amount of pixels that hold the same amount of iterations. This creates more vivid images without needing a high amount of iterations.

All images are placed in the "zoom/" directory, which is automatically created. To combine the images in a gif, simply enter "python3 gifgen.py".


When compiling the code, you need the ImgUtil library (https://github.com/diku-dk/img-util-fs)
The code is then compiled by entering 
fsharpc -r img_util.dll fractalimages.fsx

To run the code, enter
mono fractalimages.exe [args]

Examples:
mono fractalimages.exe manual
mono fractalimages.exe zoom zoom -0.207107867 1.122757067 100 200
mono fractalimages.exe zoom histzoom -0.207107867 1.122757067 100 200
python3 gifgen.py