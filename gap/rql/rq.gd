############################################################################
##
#W gap/rql/rq.gd			LPRES				René Hartung
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
## The largest known (torsion-free) quotient systems of an invariant LpGroup.
##
DeclareAttribute( "TorsionFreeNilpotentQuotientSystem", IsLpGroup and
                                      HasIsInvariantLPresentation and
                                      IsInvariantLPresentation);

############################################################################
##
#A  LargestTorsionFreeNilpotentQuotient( <LpGroup> )
## 
## stores the largest torsion-free nilpotent quotient if this has been found
## already.
## 
DeclareAttribute( "LargestTorsionFreeNilpotentQuotient", IsLpGroup );

############################################################################
##
#F  LPRES_SmallerTorsionFreeNilpotentQuotientSystem ( <Q>, <int> )
## 
## Computes a nilpotent quotient system for G/G_i if a nilpotent 
## quotient system for G/G_j is known, i<j.
##
DeclareGlobalFunction( "LPRES_SmallerTorsionFreeNilpotentQuotientSystem" );

############################################################################
##
#A  RationalLowerCentralSeries( <PcpGroup> )
## 
## stores the rational lower central series of the quotient
## 
DeclareAttribute( "RationalLowerCentralSeries", IsPcpGroup );
############################################################################
##
#F  internal function
##
## stores the largest torsion-free nilpotent quotient as an attribute of
## the <LpGroup>
##
DeclareGlobalFunction( "LPRES_StoreLargestTorsionFreeNilpotentQuotient" );

############################################################################
##
#F  internal function
##
## stores the quotient system as an attribute of the <LpGroup>
##
DeclareGlobalFunction( "LPRES_StoreTorsionFreeNilpotentQuotientSystem" );
