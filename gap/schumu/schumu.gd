############################################################################
##
#W  schumu.gd			LPRES				René Hartung
##

############################################################################
##
#A  GeneratingSetOfMultiplier( <LpGroup> )
##
DeclareAttribute( "GeneratingSetOfMultiplier", IsLpGroup and 
                   HasIsInvariantLPresentation and IsInvariantLPresentation );

############################################################################
##
#O  FiniteRankSchurMultiplier( <LpGroup>, <int> )
##
DeclareOperation( "FiniteRankSchurMultiplier", [ IsLpGroup and 
                   HasIsInvariantLPresentation and IsInvariantLPresentation, 
                   IsPosInt ] );

############################################################################
##
#O  EndomorphismsOfFRSchurMultiplier( <LpGroup>, <int> )
##
DeclareOperation( "EndomorphismsOfFRSchurMultiplier", [ IsLpGroup and 
                   HasIsInvariantLPresentation and IsInvariantLPresentation, 
                   IsPosInt ] );

############################################################################
##
#F  LPRES_BuildCoveringGroup
##
DeclareGlobalFunction( "LPRES_BuildCoveringGroup" );

############################################################################
##
#F  LPRES_InduceEndosToCover
##
DeclareGlobalFunction( "LPRES_InduceEndosToCover" );

############################################################################
##
#F  LPRES_QSystemOfCoveringGroup
##
DeclareGlobalFunction( "LPRES_QSystemOfCoveringGroup" );

############################################################################
##
#O  EpimorphismFiniteRankSchurMultipliers( <LpGroup>, <int>, <int> )
##
DeclareOperation( "EpimorphismFiniteRankSchurMultipliers", [ IsLpGroup and 
                   HasIsInvariantLPresentation and IsInvariantLPresentation, 
                   IsPosInt, IsPosInt ] );

############################################################################
##
#F  ImageInFiniteRankSchurMultiplier( <LpGroup>, <int>, <elm> )
##
DeclareGlobalFunction( "ImageInFiniteRankSchurMultiplier" );

############################################################################
##
#F  LPRES_SchuMuFromCover( <QS>, <gens>, <Endos> )
##
DeclareGlobalFunction( "LPRES_SchuMuFromCover" );


############################################################################
##
#O DwyerQuotient( <LpGroup>, <int> )
##
DeclareOperation( "DwyerQuotient", [ IsGroup, IsPosInt ] );
