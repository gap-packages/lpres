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
## Stores the largest torsion free nilpotent quotient systems of an invariant
## LpGroup that has been computed by either InitRationalQuotientSystem
## or ExtendRationalQuotientSystem.
##
DeclareAttribute( "TorsionFreeNilpotentQuotientSystem", IsLpGroup and
                                      HasIsInvariantLPresentation and
                                      IsInvariantLPresentation);

############################################################################
##
#A  LargestTorsionFreeNilpotentQuotient( <LpGroup> )
## 
## stores the largest torsion free nilpotent quotient.
DeclareAttribute( "LargestTorsionFreeNilpotentQuotient", IsLpGroup );

############################################################################
##
#F  SmallerTorsionFreeNilpotentQuotientSystem ( <Q>, <int> )
## 
## Computes a nilpotent quotient system for G/G_i if a nilpotent 
## quotient system for G/G_j is known, i<j.
##
DeclareGlobalFunction( "SmallerTorsionFreeNilpotentQuotientSystem" );

############################################################################
##
#A  RationalLowerCentralSeries( <PcpGroup> )
## 
## stores the rational lower central series of the quotient.
DeclareAttribute( "RationalLowerCentralSeries", IsPcpGroup );
