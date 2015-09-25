# ---------------------------------------------------------------------------
# test_arcgis_alea.py
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script can be executed independently with python or with the python console in ArcMAP (open console, right click, load). 
# It needs python 2.7 and ArcGIS 10 (it was tested with Python 2.7.5 and ArcGIS 10.2.1). It needs Spatial Analyst or advanced ArcGIS licence.
#
# Create random points based on a table values (one point per line) and within one given shape spatial extent.
# Used in order to spatially randomized variables.
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy
# sys time
import time
# check licence for spatial analyst
arcpy.CheckOutExtension('Spatial')

# START : execution time
time0 = time.time()

# parameters
## field name
field = "SURFACES"
## number of random points shapes to create
nalea = 2

# Local variables:
## working directory
wd = "C:\\Users\\Romain Louvet\\Desktop\\test_comparatif_outils"
## output folder
out_path = wd+"\\results"
## output name
out_name = "pointsalea"
## spatial extent (works only with a dissolved polygon)
spatextent = wd+"\\SHP\\commune_base_diss.shp"
## table with values to randomized (here, wildfire data)
tabl = wd+"\\promethee1997_2013.csv"
## open and read this csv file
tabl = open(tabl,"r")
table = tabl.read()
tabl.close()
###split lines
table = str.split(table,"\n")
###del column names
del table[0]
###del last row ("" in csv)
del table[-1]
### variable
var = []
### column number corresponding to variable
varcolumn = 10
### number of points to create (number of lines in table)
nbpoints = len(table)
### fill the list var with values from wanted variable 
for i in range(len(table)):
###split column
    ligne = str.split(table[i],";")
###get value from wanted variable column
    v = float(ligne[varcolumn])
    var.append(v)

# Process: create random points
for i in range(nalea):
    arcpy.CreateRandomPoints_management(out_path,out_name+str(i), spatextent, "",nbpoints,"","POINT","")
# add field
    arcpy.AddField_management(out_path+"\\"+out_name+str(i)+".shp", field, "DOUBLE")

# update field
    fc = out_path+"\\"+out_name+str(i)+".shp"
    fields = [field]
    cursor = arcpy.da.UpdateCursor(fc,fields)
    j = 0
    for row in cursor:
        row[0] = var[j]
        cursor.updateRow(row)
        j = j + 1

# STOP : execution time
print(str(time.time()-time0)+" sec")
