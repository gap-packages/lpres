############################################################################
##
#W gap/jql/extqs.gd			LPRES				René Hartung
##

############################################################################
##
#O  ExtendRationalQuotientSystem ( <quo> )
##
## Extends the quotient system for G/gamma_i(G) to a consistent quotient
## system for G/gamma_{i+1}(G).
##
DeclareGlobalFunction( "ExtendRationalQuotientSystem" );

############################################################################
##
#F  LPRES_RationalCoveringGroupByQSystem 
## 
DeclareGlobalFunction( "LPRES_RationalCoveringGroupByQSystem" );

############################################################################
##
#F  LPRES_JenningsSeries( <QS> )
##
## computes the p-Jennings central series of the p-quotient represented
## by the weighted nilpotent quotient system <QS>.
##
DeclareGlobalFunction( "LPRES_RationalLowerCentralSeries" );

############################################################################
##
#A  JenningsClass( <PcpGroup> )
## 
# DeclareAttribute( "JenningsClass", IsPcpGroup );

############################################################################
##
#A  RationalLowerCentralSeries( <PcpGroup> )
## 
DeclareAttribute( "RationalLowerCentralSeries", IsPcpGroup );

############################################################################
##
#F  LPRES_CreateNewRationalQuotientSystem
## 
DeclareGlobalFunction( "LPRES_CreateNewRationalQuotientSystem" );
