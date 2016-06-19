############################################################################
##
#W  subgrps.gd			The LPRES-package		René Hartung
##

############################################################################
##
#O TraceCosetTableLpGroup
##
DeclareOperation( "TraceCosetTableLpGroup", [ IsList, IsObject, IsPosInt ] );

############################################################################
##
#O SubgroupLpGroupByCosetTable
##
DeclareOperation( "SubgroupLpGroupByCosetTable", [ IsObject, IsList ] );

############################################################################
##
#O IsCosetTableLpGroup
##
DeclareOperation( "IsCosetTableLpGroup", [ IsSubgroupLpGroup, IsList ] );

############################################################################
##
#F LPRES_EnforceCoincidences
##
DeclareGlobalFunction( "LPRES_EnforceCoincidences" );

############################################################################
##
#O LowIndexSubgroupsLpGroupByFpGroup
##
DeclareOperation( "LowIndexSubgroupsLpGroupByFpGroup", [ IsLpGroup, IsPosInt, IsPosInt ] );

############################################################################
##
#O LowIndexSubgroupsLpGroupIterator
##
DeclareOperation( "LowIndexSubgroupsLpGroupIterator", [ IsLpGroup, IsPosInt, IsPosInt ] );
