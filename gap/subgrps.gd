############################################################################
##
#W  subgrps.gd			The LPRES-package		René Hartung
##

############################################################################
##
#O TraceCosetTableLpGroup
##
DeclareOperation( "TraceCosetTableLpGroup", [ IsList, IsObject, IsPosInt ] );

DeclareOperation( "SubgroupLpGroupByCosetTable", [ IsObject, IsList ] );

DeclareOperation( "IsCosetTableLpGroup", [ IsSubgroupLpGroup, IsList ] );

DeclareGlobalFunction( "LPRES_EnforceCoincidences" );

DeclareOperation( "LowIndexSubgroupsLpGroupByFpGroup", [ IsLpGroup, IsPosInt, IsPosInt ] );
DeclareOperation( "LowIndexSubgroupsLpGroupIterator", [ IsLpGroup, IsPosInt, IsPosInt ] );

DeclareOperation( "NilpotentQuotientIterator", [ IsLpGroup ] );

DeclareOperation( "NqEpimorphismNilpotentQuotientIterator", [ IsLpGroup ] );

DeclareOperation( "LowerCentralSeriesIterator", [ IsLpGroup ] );
