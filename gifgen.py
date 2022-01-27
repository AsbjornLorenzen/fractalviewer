import imageio
import os

def buildgif(dir):
    filenames = []
    images = []
    for _,_, files in os.walk(dir):
        filenames = (dir+str(file) for file in files if file != "Manual_image.png" and file.endswith(".png"))
    filenames = sorted(filenames,key = lambda x: (len (x), x))
    for filename in filenames:
        images.append(imageio.imread(filename))
    imageio.mimsave('zoom/fractalzoom.gif', images)

if __name__ == '__main__':
    buildgif('zoom/')