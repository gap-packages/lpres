############################################################################
##
#W  reidschr.gi			The LPRES-package		René Hartung
##

############################################################################
##
#A SchreierData
##
############################################################################
DeclareAttribute( "SchreierData", IsSubgroupLpGroup );

############################################################################
##
#A  PeriodicityOfSubgroupPres
##
############################################################################
DeclareAttribute( "PeriodicityOfSubgroupPres", IsSubgroupLpGroup );

############################################################################
##
#O ReidemeisterRewriting 
##
############################################################################
DeclareOperation( "ReidemeisterRewriting", [ IsSubgroupLpGroup,
                                             IsElementOfFreeGroup ] );

############################################################################
##
#A ReidemeisterMap
##
############################################################################
DeclareAttribute( "ReidemeisterMap", IsSubgroupLpGroup );

############################################################################
##
#O FreeProductOp
##
############################################################################
DeclareOperation( "FreeProductOp", [ IsLpGroup, IsLpGroup ] );
DeclareOperation( "FreeProductOp", [ IsLpGroup, IsFpGroup ] );
DeclareOperation( "FreeProductOp", [ IsFpGroup, IsLpGroup ] );

############################################################################
##
#A EmbeddingIntoFreeProduct
##
############################################################################
DeclareAttribute( "EmbeddingIntoFreeProduct", IsLpGroup );

############################################################################
##
#M FreeFactors
##
############################################################################
DeclareAttribute( "FreeFactors", IsLpGroup );

############################################################################
##
#A FreeGeneratorsOfFullPreimage
##
############################################################################
DeclareAttribute( "FreeGeneratorsOfFullPreimage", IsSubgroupLpGroup );


############################################################################
##
#F LPRES_OrbStab
##
############################################################################
DeclareGlobalFunction( "LPRES_OrbStab" );
DeclareGlobalFunction( "LPRES_Orbits" );
