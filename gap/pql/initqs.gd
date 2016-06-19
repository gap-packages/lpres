############################################################################
##
#W initqs.gd			LPRES				René Hartung
##

############################################################################
##
#O  InitQuotientSystem ( <LpGroup>, <PosInt> )
##
DeclareOperation( "InitPQuotientSystem", [ IsLpGroup, IsPosInt ] );

############################################################################
##
#F  LPRES_AddPRow ( <mat> , <evec> )
##
DeclareGlobalFunction( "LPRES_AddPRow" );
