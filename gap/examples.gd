############################################################################
##
#W examples.gd		The LPRES-package			René Hartung
##

############################################################################
##
#F  ExamplesOfLPresentations (n)
##
## Returns some important examples of L-presented groups (e.g. Grigorchuk).
## (from "Endomorphic presentations of Branch Groups", Laurent Bartholdi")
##
DeclareGlobalFunction( "ExamplesOfLPresentations" );

############################################################################
##
#O  FreeEngelGroup ( <num>, <n> )
##
## returns an L-presentation for the free <n>-Engel Group on <num>
## generators.
##
DeclareOperation( "FreeEngelGroup", [ IsPosInt, IsPosInt ]);

############################################################################
##
#O  FreeBurnsideGroup( <num>, <exp> )
##
##  returns an $L$-presentation for the free Burnside group B(m,n) on 
##  <num> generators with exponent <exp>.
##
############################################################################
DeclareOperation( "FreeBurnsideGroup", [ IsPosInt, IsPosInt ]);

############################################################################
##
#O  FreeNilpotentGroup( <num>, <c> )
##
## returns an L-presentation for the free nilpotent group of class <c>
## on <num> generators.
##
DeclareOperation( "FreeNilpotentGroup", [ IsPosInt, IsPosInt ]);

############################################################################
##
#O  GeneralizedFabrykowskiGuptaLpGroup ( <n> )
##
## returns an L-presentation for the generalized Fabrykowski-Gupta group for
## a positive integer <n>; for details on the L-presentation see [BEH].
##
DeclareOperation( "GeneralizedFabrykowskiGuptaLpGroup", [ IsPosInt ] );

############################################################################
##
#C  LamplighterGroup( <filter>, <PcGroup> )
#C  LamplighterGroup( <filter>, <PosInt> )
##
## returns an L-presentation for the Lamplighter Group <PcGroup> \wr \Z,
## where <PcGroup> is cyclic.
##
############################################################################
DeclareConstructor( "LamplighterGroup", [ IsLpGroup, IsGroup ]);
DeclareConstructor( "LamplighterGroup", [ IsLpGroup, IsPosInt ]);

############################################################################
##
#C  IASubgroup( <automorphism group of free group> )
##
## returns an L-presentation for the IA subgroup of the automorphism group
## of a free group, i.e. the group of automorphism that act trivially on
## the abelianization.
##
############################################################################
DeclareOperation("EmbeddingOfIASubgroup", [IsAutomorphismGroup] );
