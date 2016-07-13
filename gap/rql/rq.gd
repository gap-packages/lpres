############################################################################
##
#W pq.gd			LPRES				René Hartung
##

############################################################################
##
#O  TorsionFreeNilpotentQuotient( <LpGroup>, <c> )
##
DeclareOperation( "TorsionFreeNilpotentQuotient", [ IsLpGroup, IsPosInt ] );

############################################################################
##
#O  EpimorphismTorsionFreeNilpotentQuotient( <LpGroup>, <c> )
##
DeclareOperation( "EpimorphismTorsionFreeNilpotentQuotient", [ IsLpGroup, IsPosInt ] );

############################################################################
##
#A  TorsionFreeNilpotentQuotientSystem ( <LpGroup> )
##
## A set of the largest p-quotient systems of an invariant LpGroup that has 
## been computed by InitTorsionFreeNilpotentQuotientSystem or ExtendTorsionFreeNilpotentQuotientSystem.
##
DeclareAttribute( "TorsionFreeNilpotentQuotientSystem", IsLpGroup and
                                      HasIsInvariantLPresentation and
                                      IsInvariantLPresentation);

############################################################################
##
#A  LargestTorsionFreeNilpotentQuotient( <LpGroup> )
## 
## stores the largest p-quotients as a list
DeclareAttribute( "LargestTorsionFreeNilpotentQuotient", IsLpGroup );

############################################################################
##
#F  SmallerTorsionFreeNilpotentQuotientSystem ( <Q>, <int> )
## 
## Computes a nilpotent quotient system for G/gamma_i(G) if a nilpotent 
## quotient system for G/gamma_j(G) is known, i<j.
##
DeclareGlobalFunction( "SmallerTorsionFreeNilpotentQuotientSystem" );

############################################################################
##
#A  ExponentPCentralSeries( <PcpGroup> )
## 
## stores the largest p-quotients as a list
DeclareAttribute( "RationalLowerCentralSeries", IsPcpGroup );
