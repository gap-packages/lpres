############################################################################
##
#W quotsys.gd			LPRES				René Hartung
##

############################################################################
##
#F  SmallerQuotientSystem ( <Q>, <int> )
## 
## Computes a nilpotent quotient system for G/gamma_i(G) if a nilpotent 
## quotient system for G/gamma_j(G) is known, i<j.
##
DeclareGlobalFunction( "SmallerQuotientSystem" );

############################################################################
##
#F  LPRES_SaveQuotientSystem( <Q>, <String> )
##
## stores the quotient system <Q> in the file <String>.
##
DeclareGlobalFunction( "LPRES_SaveQuotientSystem" );

############################################################################
##
#F  LPRES_SaveQuotientSystemCover( <Q>, <String> )
##
DeclareGlobalFunction( "LPRES_SaveQuotientSystemCover" );
