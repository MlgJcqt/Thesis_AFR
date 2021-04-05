# Name : investig_rank.py
# Objective : get the rank of searched POI in candidates lists from scores lists or SLR lists
# Created by : mjacquet
# Last version : 03.01.2021

import os


def Sort(sub_li) :
    sub_li.sort( key=lambda x : x[5],
                 reverse=True )  # reverse = True pour ordre décroissant (MFE/MFI/SLR) // x[5] pour files SLR, x[2] pour file scores
    return sub_li


def investig_from_scores(path1, path2, path3) :
    """
    path1 = scores de comparaisons traces vs ref du POI recherché
    path2 = scores d'inter : comparaisons T vs ref autres que POI
    path3 = path output csv
    """

    for scenarios in os.listdir( path1 ) :
        scenario, _ = os.path.splitext( os.path.basename( scenarios ) )

        out = open( path3 + "/rang_recherche_POI_" + scenario + ".csv", 'w' )
        out.write( "%s;%s;%s;%s\n" % ("Rang", "Trace", "Ref", "Score") )

        for file in os.listdir( path1 + scenario ) :
            poi = file[:3]

            with open( path2 + scenario + "/" + poi + "_inter_trace.csv", "r+" ) as d :
                backgr = []
                for row in d :
                    row = row.strip().split( ";" )
                    row[2] = float( row[2] )
                    backgr.append( row )

                for line in open( path1 + scenario + "/" + file ) :
                    query = []
                    line = line.strip().split( ";" )
                    line[2] = float( line[2] )
                    query.append( line )

                    img1 = line[0]
                    img2 = line[1]
                    if not "_f" in img1 :
                        if "_f" in img2 :
                            trace = img1
                        else :
                            print( "   a wild ID appears in background   " )
                            continue
                    elif "_f" in img1 :
                        trace = img2

                    for a in backgr :
                        if trace in a :
                            query.append( a )

                    Sort( query )
                    i = 0
                    for el in query :
                        if el == line :
                            out.write( "%s;%s;%s;%s\n" % (i + 1, el[0], el[1], el[2]) )
                        else :
                            i = i + 1



def investig_from_SLR(path1, path2, path3) :
    """
    path1 = SLR du POI recherché (H1)
    path2 = SLR de comp des candidats potentiels (non correspondant) vs la trace (H2)
    path3 = path output csv
    """
    scenario = os.path.basename( os.path.normpath( path1 ) )

    out = open( path3 + "/rangs_SLR_" + scenario + ".csv", 'w' )
    out.write( "%s;%s;%s;%s\n" % ("img1", "img2", "Rang", "SLR") )

    for file in os.listdir( path1 ) :
        if os.path.exists( path2 + file ) :
            with open( path2 + file, "r+" ) as d :
                backgr = []
                for row in d :
                    row = row.strip().split( ";" )
                    if row[5] != '"LR"' :
                        row[5] = row[5].replace( ",", "." )
                        row[5] = float( row[5] )
                        backgr.append( row )

                for line in open( path1 + file ) :
                    query = []
                    line = line.strip().split( ";" )
                    if line[5] != '"LR"' :
                        line[5] = line[5].replace( ",", "." )
                        line[5] = float( line[5] )
                        query.append( line )
                        img1 = line[0]
                        img2 = line[1]

                        if not "_f" in img1 :
                            if "_f" in img2 :
                                trace = img1.replace( '"', '' )
                            else :
                                print( "   a wild ID appears in background   " )
                                continue
                        elif "_f" in img1 :
                            trace = img2.replace( '"', '' )

                        for a in backgr :
                            if trace in a[0] or trace in a[1] :
                                query.append( a )

                        Sort( query )
                        i = 0
                        for el in query :
                            if el == line :
                                out.write( "%s;%s;%s;%s\n" % (el[0], el[1], i + 1, el[5]) )
                            else :
                                i = i + 1
        else :
            print( file )


investig_from_scores( 'C:/Users/mjacquet/COMP_H1/',
                      'C:/Users/mjacquet/INTER_TRACE/',
                      'C:/Users/mjacquet/Investigatif/' )

investig_from_SLR( 'C:/Users/mjacquet/LRs/H1/cctv/',
                   'C:/Users/mjacquet/LRs/H2/cctv/',
                   'C:/Users/mjacquet/Investigatif/' )
