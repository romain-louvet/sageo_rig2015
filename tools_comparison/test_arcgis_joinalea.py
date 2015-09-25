# ---------------------------------------------------------------------------
# test_arcgis_joinalea.py
# Created on: 2015-06-11 
# Author : Romain Louvet, PhD student
# Contact : romain.louvet@alumni.univ-avignon.fr
# Description: 
# This script can be executed independently with python or with the python console in ArcMAP (open console, right click, load).
# It needs python 2.7 and ArcGIS 10 (it was tested with Python 2.7.5 and ArcGIS 10.2.1).
#
# Join random points created by test_arcgis_alea.py to polygons created by test_arcgis_aggre.py
# and sum random points values by polygon
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy
# sys time
import time

# START : execution time
time0 = time.time()

# Local variables:
## working directory
wd = "C:\\Users\\Romain Louvet\\Desktop\\test_comparatif_outils"
## output directory
path = wd+"\\results\\"
## shp names based on aggregation fields
groupfield = ["CODE_CO","CODE_CA","EPCI","CODE_A","CODE_D","CODE_R"]

# parameters
## field name
fname = "SURFACES"
## number of random points shapes created
nalea = 2

# Process:
for i in range(nalea):
# join origin (test_arcgis_alea.py output)
        join = path+"pointsalea"+str(i)+".shp"
        for f in groupfield:
# join target (test_arcgis_aggre.py)
                target= path+f+".shp"
# sortie
                out_feature = path+"JOIN_"+f+str(i)+".shp"
                
## map fields: rules in order to sum random points values to polygon
# Create a new fieldmappings and add the two input feature classes.
                fieldmappings = arcpy.FieldMappings()
                fieldmappings.addTable(target)
                fieldmappings.addTable(join)
# First get the wanted field fieldmap.
                aleaFieldIndex = fieldmappings.findFieldMapIndex(fname)
                fieldmap = fieldmappings.getFieldMap(aleaFieldIndex)
# Get the output field's properties as a field object
                field = fieldmap.outputField
# Rename the field and pass the updated field object back into the field map
                field.name = "sum_"+fname[0:4]
                field.aliasName = "sum_"+fname[0:4]
                fieldmap.outputField = field
# Set the merge rule to mean and then replace the old fieldmap in the mappings object
# with the updated one
                fieldmap.mergeRule = "sum"
                fieldmappings.replaceFieldMap(aleaFieldIndex,fieldmap)
#jointure par localisation
                arcpy.SpatialJoin_analysis(target,join, out_feature,"JOIN_ONE_TO_ONE","KEEP_ALL",fieldmappings,"CONTAINS", "", "")

# STOP : execution time
print(str(time.time()-time0)+" sec")
