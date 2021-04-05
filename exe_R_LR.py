# Name : exe_R_LR.py
# Objective : executer code fit_distrib_LR_py.R sur un path avec plusieurs sous dossiers et fichiers pour calculer LR
# Created by : mjacquet
# Last version : 20.12.2020


"""
DATA :

path1 = intra du poi
    /INTRA_ANCH_POI/
    ou
    /INTRA_IND/
path2 = inter trace vs bdd
    /INTER_TRACE/
    ou
    /INTER_SUSP/
    ou
    /INTER_INDE/
path3 = Comparaisons poi vs trace
    /COMP_H1/
path4 = csv LR
    /LRs/anch_anch/

pathpoi = path avec images de tous les POI (si autres paths incomplets)
"""

import os
import subprocess
import shutil

######## A CHANGER SELON APPROCHE ###########

path1 = "C:/Users/mjacquet/Scores/INTRA_ANCH_POI/"
# ou
# path1 = "C:/Users/mjacquet/Scores/INTRA_INDE/all_intra_poi.csv"

# path2 = "C:/Users/mjacquet/Scores/INTER_SUSP/"
# ou
path2 = "C:/Users/mjacquet/Scores/INTER_TRACE/"
# ou
# path2 = "C:/Users/mjacquet/Scores/INTER_INDE/inter_inde_sample.csv"

path4 = "C:/Users/mjacquet/LRs/anch_Tanch/H1/"
path5 = "C:/Users/mjacquet/LRs/anch_Tanch/H2/"
#### ATTENTION : CHANGER path4 et path5 DANS R SCRIPT selon approche ##########################

#############################################

pathpoi = "C:/Users/mjacquet/Scores/INTRA_ANCH_MUGS/"
path3 = "C:/Users/mjacquet/Scores/COMP_H1/"

pathtemp = "C:/Users/mjacquet/Scores/Poubelle_temp/temp_exe_R_py/"

if not os.path.exists( pathtemp + "intra/" ) :
    os.makedirs( pathtemp + "intra/" )
if not os.path.exists( pathtemp + "inter/" ) :
    os.makedirs( pathtemp + "inter/" )
if not os.path.exists( pathtemp + "comp/" ) :
    os.makedirs( pathtemp + "comp/" )
if not os.path.exists( path4 ) :
    os.makedirs( path4 )
if not os.path.exists( path5 ) :
    os.makedirs( path5 )

listpoi = os.listdir( pathpoi )
list_to_comp = []


def getLR(path) :
    scenario = os.path.basename( path )

    for comp in os.listdir( path ) :
        poi = comp[:3]
        list_to_comp.append( poi )

    for element in listpoi :
        namepoi = element[:3]
        if namepoi in list_to_comp :
            shutil.copy2( path3 + scenario + '/' + namepoi + "_comp_H1.csv",
                          pathtemp + "comp/" + namepoi + "_comp_H1_" + scenario + ".csv" )

            if "INTRA_ANCH" in path1:
                shutil.copy2( path1 + element, pathtemp + "intra/" + namepoi + "_intra.csv" )
            elif "INTRA_INDE" in path1:
                shutil.copy2( path1, pathtemp + "intra/intra_inde.csv" )

            if "INTER_INDE" in path2:
                shutil.copy2( path2, pathtemp + "inter/inter_inde.csv" )
            elif "INTER_SUSP" in path2:
                if os.path.exists( path2 + namepoi + '_inter_susp.csv' ) :
                    shutil.copy2( path2 + namepoi + '_inter_susp.csv', pathtemp + "inter/" + element.replace( ".csv", "_" ) + scenario + ".csv" )
                else:
                    continue
            elif "INTER_TRACE" in path2:
                shutil.copy2( path2 + scenario + "/" + namepoi + '_inter_trace.csv',
                              pathtemp + "inter/" + element.replace( ".csv", "_" ) + scenario + ".csv" )


            process = subprocess.Popen( ['C:/Program Files/R/R-4.0.2/bin/Rscript.exe',
                                         'C:/Users/mjacquet.AD/Desktop/THESE/Codes/Scripts/R_scripts/codes_these/fit_distr_LR_python_KDE-Norm.R'] )
            stdout = process.communicate()[0]

            for d in os.listdir( pathtemp ) :
                for f in os.listdir( pathtemp + d ) :
                    os.remove( pathtemp + d + '/' + f )

        else :
            print( "Il manque le POI : ", namepoi )


getLR("C:/Users/mjacquet.AD/Desktop/THESE/MANIP_DEV_MODEL/FN/Results2/COMP_H1/atm")

#################
