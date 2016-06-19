############################################################################
##
#W  gap/nql/subgrps.gi			LPRES				René Hartung
##

############################################################################
##
#M DerivedSubgroup
##
InstallMethod( DerivedSubgroup,
  "for an LpGroup", true, 
  [ IsLpGroup ], 0,
  G -> Kernel( NqEpimorphismNilpotentQuotient( G, 1 ) ) );

############################################################################
##
#M NilpotentQuotientIterator
##
InstallMethod( NilpotentQuotientIterator, 
  "for an LpGroup", true, 
  [ IsLpGroup ], 0, 
  function( G )
  local filter, it, NextIterator, IsDoneIterator, ShallowCopyIT;

  NextIterator := function( iter )
    local H;
      iter!.class := iter!.class + 1;;
      H := NilpotentQuotient( iter!.group, iter!.class );
      if NilpotencyClassOfGroup( H ) < iter!.class then 
        iter!.max := iter!.class;
        Error( "exhausted the iterator <iter>" );
      fi;
      return( H );
    end;
 
  IsDoneIterator := function( iter )
      return( iter!.max > 0 );   
    end;

  ShallowCopyIT := function( iter )
    return( rec( group := iter!.group,
                 class := iter!.class, 
                 max   := iter!.max ) );
    end;

  filter := IsIteratorByFunctions and IsAttributeStoringRep and IsMutable;

  it := rec( NextIterator := NextIterator, IsDoneIterator := IsDoneIterator,
             ShallowCopy := ShallowCopyIT );

  it!.group := G;
  it!.class := 0;
  it!.max   := 0;

  return( Objectify( NewType( IteratorsFamily, filter ), it ) );
  end);

############################################################################
##
#M LowerCentralSeriesIterator
##
InstallMethod( LowerCentralSeriesIterator, 
  "for an LpGroup", true, 
  [ IsLpGroup ], 0, 
  function( G )
  local filter, it, NextIterator, IsDoneIterator, ShallowCopyIT;

  NextIterator := function( iter )
    local epi;
      iter!.class := iter!.class + 1;;
      epi := NqEpimorphismNilpotentQuotient( iter!.group, iter!.class );
      if NilpotencyClassOfGroup( Range( epi ) ) < iter!.class then 
        iter!.max := iter!.class;
        Error( "exhausted the iterator <iter>" );
      fi;
      return( Kernel( epi ) );
    end;
 
  IsDoneIterator := function( iter )
      return( iter!.max > 0 );   
    end;

  ShallowCopyIT := function( iter )
    return( rec( group := iter!.group,
                 class := iter!.class, 
                 max   := iter!.max ) );
    end;

  filter := IsIteratorByFunctions and IsAttributeStoringRep and IsMutable;

  it := rec( NextIterator := NextIterator, IsDoneIterator := IsDoneIterator,
             ShallowCopy := ShallowCopyIT );

  it!.group := G;
  it!.class := 0;
  it!.max   := 0;

  return( Objectify( NewType( IteratorsFamily, filter ), it ) );
  end);
