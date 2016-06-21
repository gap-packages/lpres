############################################################################
##
#W pq.gd			LPRES				René Hartung
##

############################################################################
##
#O  NqPQuotient( <LpGroup>, <c> )
##
DeclareOperation( "NqPQuotient", [ IsLpGroup, IsPosInt, IsPosInt ] );

############################################################################
##
#O  NqEpimorphismPQuotient( <LpGroup>, <prime>, <c> )
##
DeclareOperation( "NqEpimorphismPQuotient", [ IsLpGroup, IsPosInt, IsPosInt ] );

############################################################################
##
#A  PQuotientSystem ( <LpGroup> )
##
## A set of the largest p-quotient systems of an invariant LpGroup that has 
## been computed by InitPQuotientSystem or ExtendPQuotientSystem.
##
DeclareAttribute( "PQuotientSystems", IsLpGroup and
                                      HasIsInvariantLPresentation and
                                      IsInvariantLPresentation);

############################################################################
##
#A  LargestPQuotients( <LpGroup> )
## 
## stores the largest p-quotients as a list
DeclareAttribute( "LargestPQuotients", IsLpGroup );

############################################################################
##
#F  SmallerPQuotientSystem ( <Q>, <int> )
## 
## Computes a nilpotent quotient system for G/gamma_i(G) if a nilpotent 
## quotient system for G/gamma_j(G) is known, i<j.
##
DeclareGlobalFunction( "SmallerPQuotientSystem" );

############################################################################
##
#A  ExponentPCentralSeries( <PcpGroup> )
## 
## stores the largest p-quotients as a list
DeclareAttribute( "ExponentPCentralSeries", IsPcpGroup );
