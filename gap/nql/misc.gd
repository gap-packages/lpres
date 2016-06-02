############################################################################
##
#W misc.gd			LPRES				René Hartung
##

############################################################################
##
#F  LPRES_WordsOfLengthAtMostN( <list>, <n> )
##   
## returns a list of all words of <list> of length at most <n>
##
DeclareGlobalFunction( "LPRES_WordsOfLengthAtMostN" );

############################################################################
##
#F  LPRES_LowerCentralSeriesSections( <PcpGroup> )
##   
## returns either the p-ranks of the lower central series sections of
## <PcpGroup> or its abelian invariants.
##
DeclareGlobalFunction( "LPRES_LowerCentralSeriesSections" );

############################################################################
##
#F  LPRES_LCSofGuptaSidki( <PcpGroup>, <prime> )
##
## computes the lower central series sections of the Gupta-Sidki group
## from an index-3 subgroup which is invariantly L-presented.
##
DeclareGlobalFunction( "LPRES_LCSofGuptaSidki" );
