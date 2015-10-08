# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# densite_lineaire.py
# Created on: 2015-06-26 11:13:53.00000
# Author: Romain Louvet
# Description: 
# calculate linear density from lines shape file and polygons shape file
# 
# for example, road density by countries is :
# sum by country of km of roads / country area
#
# Tested with ArcGIS 10.2.1
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy

#### #### #### #####
# Tool parameters  #
#### #### #### #####
# 0 : lines shape file
# Type : Feature class, Required
# Direction : Input
Input0 = arcpy.GetParameterAsText(0)

# 1 : polygons shape file
# Type : Feature class, Required
# Direction : Input
Input1 = arcpy.GetParameterAsText(1)

Input_Features = [Input0,Input1]

# 2 : polygons OID field
# Type : field, Required
# Direction : Input
# Obtained from : parameter 1
id = arcpy.GetParameterAsText(2)

# 3 : polygons areas field
# Type : field, Required
# Direction : Input
# Obtained from : parameter 1
field0 = arcpy.GetParameterAsText(3)

# 4 : lines length field
# Type : field, Required
# Direction : Input
# Obtained from : parameter 0
field1 = arcpy.GetParameterAsText(4)

# 5 : output from intersect tool
# Type : feature class, required
# Direction : Ouput
Output0 = arcpy.GetParameterAsText(5)

# 6 : output table results
# Type : table (.dbf), required
# Direction : Ouput
Output1 = arcpy.GetParameterAsText(6)

### ### ####
# Process: #
### ### ####

# Intersect
arcpy.Intersect_analysis(Input_Features,Output0, "ALL", "", "INPUT")

# Calc length
arcpy.CalculateField_management(Output0, field1, "!Shape.length@KILOMETERS!", "PYTHON_9.3")

# summary statistics analysis
arcpy.Statistics_analysis(Output0, Output1, [[field0,"FIRST"],[field1,"SUM"]], id)
arcpy.AddField_management (Output1, "DENS_kmkm2", "DOUBLE")


# calc length/surface
name0 = "FIRST_"+field0
name0 = name0[0:10]
name1 = "SUM_"+field1
name1 = name1[0:10]
arcpy.CalculateField_management(Output1, "DENS_kmkm2", "!"+name1+"!/!"+name0+"!", "PYTHON_9.3")