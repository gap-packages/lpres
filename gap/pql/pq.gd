############################################################################
##
#W nq.gd			LPRES				René Hartung
##

############################################################################
##
#O  NilpotentQuotient( <LpGroup>, <c> )
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
#A  NilpotentQuotients( <LpGroup>)
##
## stores the nilpotent quotients known from the NilpotentQuotient- or from
## the NqEpimorphismNilpotentQuotient-method. The quotients are stored as
## epimorphisms from <LpGroup> onto the corresponding quotient.
##
# DeclareAttribute( "NilpotentQuotients", IsLpGroup );

############################################################################
##
#F  LPRES_PCS( <QS> )
##
## computes the exponent-p central series of the p-quotient represented
## by the weighted nilpotent quotient system <QS>.
##
DeclareGlobalFunction( "LPRES_ExponentPCentralSeries" );

############################################################################
##
#O  LargestPQuotient( <LpGroup>, <prime> )
##
## computes the largest nilpotent quotient of the group 
## <LpGroup>. Note that this method will only terminate if <LpGroup> 
## has a largest p-quotient.
##
DeclareOperation( "LargestPQuotient", [ IsLpGroup, IsPosInt ] );

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
