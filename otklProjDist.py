import cv2
import numpy as np
import os

klikovi = []
def getCoordinates(event,x,y,flags,params):
    if event == cv2.EVENT_LBUTTONDOWN:
        klikovi.append([x,y,1])

path = input("unesite putanju do fotografije / (t)est\n")
if (path == 't'):
    path = "test.jpg"
    
try:
    opcija = int(input("koliko koordinata u sta se slika birate:\n\t(1)gore levo / (4)free\n"))
    if(opcija==1):
        print("tacke zadajete u smeru kazaljke, pocevsi od gornje leve")
    foto = cv2.imread(path)
    h, w, _ = foto.shape
    cv2.namedWindow("slika")
    cv2.setMouseCallback("slika", getCoordinates)
    while True:
        cv2.imshow("slika", foto)
        key = cv2.waitKey(1) & 0xFF
        if key == ord("q") or len(klikovi) == 4+opcija:
            break
    if(opcija == 1):
        x, y, _ = klikovi[len(klikovi)-1]
        if(x>w/2 or y>h/2):
            exit("")
        klikovi.append([w-x,y,1])
        klikovi.append([w-x,h-y,1])
        klikovi.append([x,h-y,1])
except:
    exit("greska pri ucitavanju slike")

try:
    with open("tacke.txt", "w") as f:
        for tacka in klikovi:
            f.write("%i %i %i" % (tacka[0], tacka[1], tacka[2]))
            f.write("\n")
except:
    exit("neuspelo parsiranje koordinata")

os.system('stack run')

try:
    cv2.waitKey(2000)
    m = np.genfromtxt("matPreslikavanja.txt")
    print("matrica preslikavanja:", sep="\n\t")
    print(m)
except:
    exit("greska pri ucitavanju matrice")

try:
    sredjena = cv2.warpPerspective(foto, m, (w, h))
    cv2.namedWindow("sredjena")
    while True:
        cv2.imshow("sredjena", sredjena)
        key = cv2.waitKey(1) & 0xFF
        if key == ord("q"):
            break
except:
    exit("warpPerspective fail")