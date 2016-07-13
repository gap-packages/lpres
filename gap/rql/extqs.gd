############################################################################
##
#W gap/jql/extqs.gd			LPRES				René Hartung
##

############################################################################
##
#O  ExtendRationalQuotientSystem ( <quo> )
##
## Extends the quotient system for G/G_i to a consistent quotient
## system for G/G_{i+1}.
##
DeclareGlobalFunction( "ExtendRationalQuotientSystem" );

############################################################################
##
#F  LPRES_RationalCoveringGroupByQSystem 
## 
DeclareGlobalFunction( "LPRES_RationalCoveringGroupByQSystem" );

############################################################################
##
#F  LPRES_RationalLowerCentralSeries( <QS> )
##
## computes the rational lower central series of the torsion-free nilpotent
## quotient represented by the quotient system <QS>.
##
DeclareGlobalFunction( "LPRES_RationalLowerCentralSeries" );

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
