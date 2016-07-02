############################################################################
##
#W gap/jql/extqs.gd			LPRES				René Hartung
##

############################################################################
##
#O  ExtendJenningsQuotientSystem ( <quo> )
##
## Extends the quotient system for G/gamma_i(G) to a consistent quotient
## system for G/gamma_{i+1}(G).
##
DeclareGlobalFunction( "ExtendJenningsQuotientSystem" );

############################################################################
##
#F  LPRES_JenningsCoveringGroupByQSystem 
## 
DeclareGlobalFunction( "LPRES_JenningsCoveringGroupByQSystem" );

############################################################################
##
#F  LPRES_ConsistencyChecks
## 
# DeclareGlobalFunction( "LPRES_ConsistencyChecks" );

############################################################################
##
#F  LPRES_InduceSpinning
## 
# DeclareGlobalFunction( "LPRES_InduceSpinning" );

############################################################################
##
#F  LPRES_CreateNewQuotientSystem
## 
# DeclareGlobalFunction( "LPRES_CreateNewQuotientSystem" );

############################################################################
##
#F  LPRES_JenningsSeries( <QS> )
##
## computes the p-Jennings central series of the p-quotient represented
## by the weighted nilpotent quotient system <QS>.
##
DeclareGlobalFunction( "LPRES_JenningsSeries" );
