############################################################################
##
#W gap/jql/jq.gd			LPRES				René Hartung
##

############################################################################
##
#O  JenningsQuotient( <LpGroup>, <c> )
##
DeclareOperation( "JenningsQuotient", [ IsLpGroup, IsPosInt, IsPosInt ] );

############################################################################
##
#O  EpimorphismJenningsQuotient( <LpGroup>, <prime>, <c> )
##
DeclareOperation( "EpimorphismJenningsQuotient", [ IsLpGroup, IsPosInt, IsPosInt ] );

############################################################################
##
#A  JenningsQuotientSystem ( <LpGroup> )
##
## A set of the largest p-Jennings quotient systems of an invariant LpGroup
## that has been computed by InitPQuotientSystem or ExtendJenningsQuotientSystem.
##
DeclareAttribute( "JenningsQuotientSystems", IsLpGroup and
                                             HasIsInvariantLPresentation and
                                             IsInvariantLPresentation);

############################################################################
##
#A  LargestJenningsQuotients( <LpGroup> )
## 
## stores the largest p-Jennings quotients as a list
DeclareAttribute( "LargestJenningsQuotients", IsLpGroup );

############################################################################
##
#F  SmallerJenningsQuotientSystem ( <Q>, <int> )
## 
## Computes a nilpotent quotient system for G/phi_i(G) if a nilpotent 
## quotient system for G/phi_j(G) is known, i<j.
##
DeclareGlobalFunction( "SmallerJenningsQuotientSystem" );
