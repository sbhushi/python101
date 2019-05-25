#Libraries
from matplotlib import pyplot as plt
import scipy
from skimage import data
from skimage.feature import blob_dog, blob_log, blob_doh
from math import sqrt
from skimage.color import rgb2gray
import glob
from skimage.io import imread


#importing the image
example_file=glob.glob(r"C:\Users\Sharath\Downloads\night_sky.png" )[0]
im=imread(example_file,as_gray=True)

cm_gray = plt.get_cmap('gray')
plt.imshow(im,cmap=cm_gray)
plt.show()

#Counting the number of stars (dots in this case)
blobs_log = blob_log(im, max_sigma=30, num_sigma=10, threshold=.1)
# Compute radii in the 3rd column.
blobs_log[:, 2] = blobs_log[:, 2] * sqrt(2)
numrows = len(blobs_log)
print("Number of stars counted : " ,numrows)

#for validation and marking
fig, ax = plt.subplots(1, 1)
plt.imshow(im, cmap=cm_gray)
for blob in blobs_log:
    y, x, r = blob
    c = plt.Circle((x, y), r+5, color='lime', linewidth=2, fill=False)
    ax.add_patch(c)

