# ---------------------------------------------------------------------------
# test_qgis_alea.py
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script must be executed with the python console within Qgis (copy/paste). 
# It was tested with Qgis 2.8.1 
#
# Create random points based on a table values (one point per line) and within one given shape spatial extent.
# Used in order to spatially randomized variables.
# ---------------------------------------------------------------------------

## import qgis modules
import processing
from PyQt4.QtCore import *
from PyQt4.QtGui import *

# sys time
import time
import os

# START : execution time
time0 = time.time()

# Local variables:
## working directory (depend on which computer this script is executed)
wd = "C:\\Users\\Romain Louvet\\Desktop\\test_comparatif_outils"
## polygone zone pour alea
map = wd+"\\SHP\\commune_base.shp"
map = iface.addVectorLayer(map, "map", "ogr")
## sortie
out = wd+"\\results\\"

## table with values to randomized (here, wildfire data)
field = "SURFACES"
file = wd+"\\promethee1997_2013.csv"
table = open(file,"r")
table = table.read()
table = str.split(table,"\n")
del table[0]
del table[-1]
surfaces = []
nbpoints = len(table)
data = wd+"\\results\\intermed\\data.csv"
fichier = open(data,"w")
fichier.write("id;SURFACES")

for i in range(len(table)):
	ligne = str.split(table[i],";")
	surface = float(ligne[10])
	surfaces.append(surface)
	fichier.write("\n"+str(i)+";"+str(surface))

fichier.close()
fichier = open(wd+"\\results\\intermed\\data.csvt","w")
fichier.write('"Integer","Real"')
fichier.close()
# number of random points shapes
nalea = 2

for i in range(nalea):
# fonction alea
	outname = out+"intermed\\DELalea"+str(i)+".shp"
	output0 = processing.runalg("qgis:randompointsinlayerbounds",map,nbpoints,0,outname)



for i in range(nalea):
	outname = out+"alea"+str(i)+".shp"
	processing.runalg("qgis:joinattributestable",out+"intermed\\DELalea"+str(i)+".shp",data,"id","id",outname)
# display layers
	iface.addVectorLayer(outname, "alea"+str(i), "ogr")

# STOP : execution time
print(str(time.time()-time0)+" sec")

