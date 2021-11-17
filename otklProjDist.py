import cv2
import numpy as np
import os

klikovi = []
def getCoordinates(event,x,y,flags,params):
    if event == cv2.EVENT_LBUTTONDOWN:
        klikovi.append([x,y,1])

#slika = cv2.imread("/home/pap1rana/Desktop/avav.jpg")
slika = cv2.imread("test.jpg")
try:
    cv2.namedWindow("slika")
    cv2.setMouseCallback("slika", getCoordinates)
    while True:
        cv2.imshow("slika", slika)
        key = cv2.waitKey(1) & 0xFF
        if key == ord("c") or len(klikovi) == 8:
            break        
except:
    print("greska pri ucitavanju slike")

try:
    with open("tacke.txt", "w") as f:
        for tacka in klikovi:
            f.write("%i %i %i" % (tacka[0], tacka[1], tacka[2]))
            f.write("\n")
        print(klikovi)
except:
    print("neuspelo parsiranje koordinata")


os.system('stack run')

try:
    cv2.waitKey(2000)
    m = np.genfromtxt("matPreslikavanja.txt")
    print(m)
    sredjena = cv2.warpPerspective(slika, m, (1920, 980))
    cv2.namedWindow("sredjena")
    while True:
        cv2.imshow("sredjena", sredjena)
        key = cv2.waitKey(1) & 0xFF
        if key == ord("c"):
            break 
except:
    print("greska pri ucitavanju matrice")
