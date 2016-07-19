############################################################################
##
#W quots.gi			LPRES				René Hartung
##

############################################################################
##
#M  LPRES_QuotientAlgorithmEpimorphism( <LpGroup>, <prime>, <class> ) . . .
## 
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an invariant LpGroup, a positive integer and a record",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt,
    IsRecord ], 0,
  function( G, c, myRecord )
  local Q,           # current quotient system
	       weights,    	# old weights quotient system
	       i,	          # loop variable
	       time;	       # runtime

  # Compute a weighted nilpotent presentation for the Frattini quotient G/G'G^p
  time := Runtime();
  Q := myRecord.initQS( G );
  Info( InfoLPRES, 2, "Runtime for this step ",  StringTime(Runtime()-time));

  # store the largest quotient and its quotient sysstem if it's trivial
  if Length( Q.Weights ) = 0 then 
    myRecord.storeLargest( G, Q );
    myRecord.storeQS( G, Q );

    return( Q.Epimorphism );
  fi;
  
  for i in [2..c] do 
    # copy the old quotient system to compare with the extended qs.
    weights := ShallowCopy(Q.Weights);

    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time:=Runtime();
    Q := myRecord.extendQS( Q );
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
   
    # if we couldn't extend the quotient system any more, we're finished
    if myRecord.hasLargest( G ) then 
      break;
    fi;
  od;
  
  # store the largest known nilpotent quotient of <G>
  myRecord.storeQS( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  LPRES_QuotientAlgorithmEpimorphism ( <LpGroup>, <prime>, <class> ) . . . . . . 
## 
## computes the natural homomorphism on the <class> p-quotient of the 
## invariant <LpGroup>, if the latter has already some quotient system
## stored as attribute.
##
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an invariant LpGroup with quotient system a positive integer, and a record",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt, 
    IsRecord ], 1,
  function( G, c, myRecord )
  local Q,    # current quotient system
        H,    # the nilpotent quotient of <G>
        i,    # loop variable
        weights, 
        class,
        time,	# runtime
        j;    # nilpotency class

  # check if there's already a quotient system w.r.t. this prime number
  if not myRecord.hasQS( G ) then 
    TryNextMethod();
  fi;

  # known quotient system of <G>
  Q := myRecord.getQS( G );
 
  # class of the quotient system
  j := Q.Class;
 
  if c = j then  
    # requested this quotient system
    return( Q.Epimorphism );
  elif c<j then 
    # the quotient system is already there
    return( myRecord.smallerQS( Q, c ).Epimorphism );
  fi;

  # check if there's already a largest p-quotient
  if myRecord.hasLargest( G ) then
    return( myRecord.getLargest( G ).Epimorphism );
  fi;

  # extend the largest known quotient system
  for i in [j+1..c] do
    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time := Runtime();
    Q := myRecord.extendQS( Q );
    Info( InfoLPRES, 2, "Runtime for this step ", StringTime(Runtime()-time) );

    # if we couldn't extend the quotient system any more, we're finished
    if myRecord.hasLargest( G ) then 
      break;
    fi;
  od;

  # store this quotient system as an attribute of the group G
  myRecord.storeQS( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismJenningsQuotient( <LpGroup>, <prime> ) . . .
## 
## computes the natural homomorphism on the largest Jennings-quotient of the 
## invariant <LpGroup>.
##
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an invariant LpGroup and a record",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsRecord ], 0,
  function( G, myRecord )
  local Q,           # current quotient system
	       time;	       # runtime

  # Compute a weighted nilpotent presentation for the first quotient
  time := Runtime();
  Q :=  myRecord.initQS( G );
  Info( InfoLPRES, 2, "Runtime for this step ",  StringTime(Runtime()-time));

  # store the largest quotient and its quotient sysstem if it's trivial
  if Length( Q.Weights ) = 0 then 
    myRecord.storeLargest( G, Q );
    myRecord.storeQS( G, Q );

    return( Q.Epimorphism );
  fi;
  
  repeat
    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time:=Runtime();
    Q := myRecord.extendQS( Q );
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
  until myRecord.hasLargest( G );
  
  # store the largest known nilpotent quotient of <G>
  myRecord.storeQS( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismJenningsQuotient ( <LpGroup>, <prime>, <class> ) . . . . . . 
## 
## computes the natural homomorphism on the largest Jennings quotient of the 
## invariant <LpGroup>, if the latter has already some quotient system
## stored as attribute.
##
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an invariant LpGroup with quotient system and a record",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsRecord ], 1,
  function( G, myRecord )
  local Q,    # current quotient system
        time,	# runtime
        j;    # nilpotency class

  # check if there's already a quotient system
  if not myRecord.hasQS( G ) then 
    TryNextMethod();
  fi;

  # known quotient system of <G>
  Q := myRecord.getQS( G );
 
  # class of the quotient system
  j := Q.Class;
 
  # check if there's already a largest p-quotient
  if myRecord.hasLargest( G ) then
    return( myRecord.getLargest( G ).Epimorphism );
  fi;

  # extend the largest known quotient system
  repeat
    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time := Runtime();
    Q := myRecord.extendQS( Q );
    Info( InfoLPRES, 2, "Runtime for this step ", StringTime(Runtime()-time) );
    # if we couldn't extend the quotient system any more, we're finished
  until myRecord.hasLargest( G );

  # store this quotient system as an attribute of the group G
  myRecord.storeQS( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  LPRES_QuotientAlgorithmEpimorphism( <LpGroup>, <class>, <record> ) . . . . . .
## 
## computes the natural homomorphism on the <class> quotient of an 
## arbitrary <LpGroup>.
##
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an (arbitrary) LpGroup, a positive integer, and a record",
  true,
  [ IsLpGroup,
    IsPosInt,
    IsRecord ], 0,
  function( G, c, myRecord )
  local Grp, epi, pi, H, N, hom;

  # work with the underlying invariant LpGroup which maps onto G
  Grp := UnderlyingInvariantLPresentation( G );

  if not HasIsInvariantLPresentation( Grp ) then
    SetIsInvariantLPresentation( Grp, true );
  fi;

  # natural homomorphism onto the p-quotient
  epi := LPRES_QuotientAlgorithmEpimorphism( Grp, c, myRecord );

  # the quotient
  H := Range( epi );

  # natural homomorphisms from the free group onto the LpGroup
  pi := GroupHomomorphismByImages( FreeGroupOfLpGroup( Grp ), Grp,
                                   GeneratorsOfGroup( FreeGroupOfLpGroup( Grp ) ),
                                   GeneratorsOfGroup( Grp ) );
  
  # normal subgroup generated by the images of the fixed relations
  N := NormalClosure( H, Subgroup( H, List( FixedRelatorsOfLpGroup( G ), x -> ( x^pi ) ^ epi ) ) );

  # natural homomorphism onto H/N
  hom := NaturalHomomorphismByNormalSubgroup( H, N );

  # return the factor group
  return( GroupHomomorphismByImagesNC( G, Range( hom ),
                                       GeneratorsOfGroup( G ), 
                                       List( GeneratorsOfGroup( G ), x -> ( ( UnderlyingElement( x )^pi ) ^ epi ) ^ hom ) ) );
  end);

############################################################################
##
#M  LPRES_QuotientAlgorithmEpimorphism( <FpGroup>, <class>, <record> )
## 
## computes the natural homomorphism on the <class> quotient of  
## <FpGroup>.
##
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an FpGroup, a prime number, and a positive integer (using the LPRES-package)",
  true,
  [ IsGroup,
    IsPosInt,
    IsRecord ], -1,                       # give priority to other methods (e.g. NQ, ANUPQ,..)
  function( G, c, myRecord )
  local iso, 	# isomorphism from FpGroup to LpGroup
        mapi,	# MappingGeneratorsImages of <iso>
        epi;	# epimorphism from LpGroup onto its nilpotent quotient

  # check if IsomorphismLpGroup could use IsomorphismFpGroup
  if ApplicableMethod( IsomorphismFpGroup, [ G, "F" ] ) = fail then
    Error( "the input group should allow IsomorphismFpGroup to compute an L-presentation" );
  fi;

  # compute an isomorphism to an LpGroup
  iso  := IsomorphismLpGroup( G );
  mapi := MappingGeneratorsImages( iso );

  # run the quotient algorithm on the LpGroup
  epi  := LPRES_QuotientAlgorithmEpimorphism( Range( iso ), c, myRecord );

 
  # and return the composition map
  return( GroupHomomorphismByImagesNC( G,
                                       Range( epi ),
                                       mapi[1],
                                       List( mapi[1], x -> Image( epi, Image( iso, x ) ) ) ) );
  end);

############################################################################
##
#M  LPRES_QuotientAlgorithmEpimorphism( <FpGroup>, <class>, <record> )
## 
## computes the natural homomorphism on the <class> quotient of  
## <FpGroup>.
##
InstallOtherMethod( LPRES_QuotientAlgorithmEpimorphism,
  "For an FpGroup, a prime number, and a positive integer (using the LPRES-package)",
  true,
  [ IsGroup,
    IsRecord ], -1,                       # give priority to other methods (e.g. NQ, ANUPQ,..)
  function( G, myRecord )
  local iso, 	# isomorphism from FpGroup to LpGroup
        mapi,	# MappingGeneratorsImages of <iso>
        epi;	# epimorphism from LpGroup onto its nilpotent quotient

  # check if IsomorphismLpGroup could use IsomorphismFpGroup
  if ApplicableMethod( IsomorphismFpGroup, [ G, "F" ] ) = fail then
    Error( "the input group should allow IsomorphismFpGroup to compute an L-presentation" );
  fi;

  # compute an isomorphism to an LpGroup
  iso  := IsomorphismLpGroup( G );
  mapi := MappingGeneratorsImages( iso );

  # run the quotient algorithm on the LpGroup
  epi  := LPRES_QuotientAlgorithmEpimorphism( Range( iso ), myRecord );

 
  # and return the composition map
  return( GroupHomomorphismByImagesNC( G,
                                       Range( epi ),
                                       mapi[1],
                                       List( mapi[1], x -> Image( epi, Image( iso, x ) ) ) ) );
  end);
