############################################################################
##
#W nq.gi			LPRES				René Hartung
##

############################################################################
##
#M  NqPQuotient ( <LpGroup>, <int> ) . . for invariant LpGroups
##
## computes a weighted nilpotent presentation for the class-<int> quotient
## of the invariant <LpGroup> if it already has a nilpotent quotient system.
##
InstallOtherMethod( NqPQuotient,
  "for invariantly L-presented groups (with qs) and positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasPQuotientSystems,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  local Q,    # current quotient system
        QS,   # old quotient system
        H,    # the nilpotent quotient of <G>
        i,    # loop variable
        weights, 
        largestQuot,
        time,	# runtime
        j;    # nilpotency class

  Print( "Running method with a qsystem 1... \n" );
  i := Position( PQuotientSystems( G )[1], prime );
  if i = fail then
    Print( "Try next method...\n" );
    TryNextMethod();
  fi;

  # known quotient system of <G>
  Q := PQuotientSystems( G )[2][i];
 
  # p-class
  j := Maximum( Q.Weights );
 
  if c = j then 
    # the given nilpotency class <j> is already known
    H := PcpGroupByCollectorNC( Q.Pccol );
    SetExponentPCentralSeries( H, LPRES_ExponentPCentralSeries( Q ) );

    return( H );
  elif c<j then
    # the given nilpotency class <c> is already computed 
    QS := SmallerPQuotientSystem( Q, c );

    # build the nilpotent quotient with lcs
    H := PcpGroupByCollectorNC( QS.Pccol );
    SetExponentPCentralSeries( H, LPRES_ExponentPCentralSeries(QS) );

    return(H);
  else
    # check if there's already a largest p-quotient
    if HasLargestPQuotients(G) then
      j := Position( LargestPQuotients(G)[1], prime );
      if j <> fail then 
        return( LargestPQuotients(G)[2][j] );
      fi;
    fi;

    # extend the largest known quotient system
    for i in [j+1..c] do
      weights := ShallowCopy( Q.Weights );

      time := Runtime();

      # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
      Q := ExtendPQuotientSystem( Q );

      if Length( Q.Weights ) - Length( weights ) > InfoLPRES_MAX_GENS then 
        Info( InfoLPRES, 1, "Class ", Maximum(Q.Weights), ": ", Length(Q.Weights)-Length(weights), " generators");
      else
        Info( InfoLPRES, 1, "Class ", Maximum(Q.Weights), ": ", Length(Q.Weights)-Length(weights),
      	       " generators with relative orders: ", RelativeOrders(Q.Pccol){[Length(weights)+1..Length(Q.Weights)]});
      fi;
      Info( InfoLPRES, 2, "Runtime for this step ", StringTime(Runtime()-time) );

      # if we couldn't extend the quotient system any more, we're finished
      if weights = Q.Weights then 
        largestQuot := [ [], [] ];
        if HasLargestPQuotients( G ) then 
          Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
          Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
        fi;
    
        i := Position( largestQuot[1], prime ); 
        if i <> fail then 
          largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
        else 
          Add( largestQuot[1], prime );
          Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
        fi;
        ResetFilterObj( G, LargestPQuotients );
        SetLargestPQuotients( G, largestQuot );

        break;
      fi;
    od;

    # store the quotient systems
    QS := [ [], [] ];
    if HasPQuotientSystems( G ) then 
      Append( QS[1], ShallowCopy( PQuotientSystems(G)[1] ) );
      Append( QS[2], ShallowCopy( PQuotientSystems(G)[2] ) );
    fi;

    i := Position( QS[1], prime );
    if i = fail then
      Add( QS[1], prime );
      Add( QS[2], Q );
    else
      QS[2][i] := Q;
    fi;
    ResetFilterObj( G, PQuotientSystems );
    SetPQuotientSystems( G, QS );

    # build the nilpotent quotient with its lower central series attribute
    H := PcpGroupByCollectorNC( Q.Pccol );
    SetExponentPCentralSeries(H,LPRES_ExponentPCentralSeries(Q));

    return( H );
  fi;
  end );

############################################################################
##
#M  NqPQuotient ( <LpGroup>, <int>, <int> ) . . for invariant LpGroups 
##
## computes a weighted nilpotent presentation for the p-class-<int> quotient
## of the invariant <LpGroup>.
##
InstallOtherMethod( NqPQuotient,
  "for an invariantly L-presented group and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, class )
  local Q,           # current quotient system
	       H,           # the nilpotent quotient of <G>
	       OldWeights, 	# old weights quotient system
	       i,	          # loop variable
        QS,          # quotient systems
        largestQuot, # largest quotient 
	       time;	       # runtime

  Print( "Simple version...\n" );
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;

  time := Runtime();

  # Compute a confluent nilpotent presentation for G/G'
  Q := InitPQuotientSystem( G, prime );

  # store the largest quotient and its quotient sysstem if it's trivial
  if Length( Q.Weights ) = 0 then 
    largestQuot := [ [], [] ];
    if HasLargestPQuotients( G ) then 
      Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
      Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
    fi;

    i := Position( largestQuot[1], prime ); 
    if i <> fail then 
      largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
    else 
      Add( largestQuot[1], prime );
      Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
    fi;
    ResetFilterObj( G, LargestPQuotients );
    SetLargestPQuotients( G, largestQuot );

    # store the quotient systems
    QS := [ [], [] ];
    if HasPQuotientSystems( G ) then 
      Append( QS[1], ShallowCopy( PQuotientSystems(G)[1] ) );
      Append( QS[2], ShallowCopy( PQuotientSystems(G)[2] ) );
    fi;

    i := Position( QS[1], prime );
    if i = fail then
      Add( QS[1], prime );
      Add( QS[2], Q );
    else
      QS[2][i] := Q;
    fi;
    ResetFilterObj( G, PQuotientSystems );
    SetPQuotientSystems( G, QS );
    
    return( PcpGroupByCollectorNC( Q.Pccol ) );
  fi;

  if Length( Q.Weights ) > InfoLPRES_MAX_GENS then
    Info( InfoLPRES, 1, "Class ", 1, ": ", Length( Q.Weights ), " generators" );
  else
    Info( InfoLPRES, 1, "Class ", 1, ": ", Length(Q.Weights),
          " generators with relative orders: ", RelativeOrders( Q.Pccol ) );
  fi;
  Info( InfoLPRES, 2, "Runtime for this step ",  StringTime(Runtime()-time));
  
  for i in [1..class-1] do 
    # copy the old quotient system to compare with the extended qs.
    OldWeights := ShallowCopy(Q.Weights);

    # runtime
    time:=Runtime();

    # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
    Q := ExtendPQuotientSystem( Q );
    if Q = fail then 
      return(fail);
    fi;
   
    # if we couldn't extend the quotient system any more, we're finished
    if OldWeights = Q.Weights then 
      Info( InfoLPRES, 1, "Found a maximal ", prime, "-quotient of the group" );
      Info( InfoLPRES, 2, "Runtime for this step ", StringTime(Runtime()-time) );
      break;
    fi;

    if Length(Q.Weights)-Length(OldWeights) > InfoLPRES_MAX_GENS then 
      Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(OldWeights), " generators");
    else
      Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(OldWeights),
           " generators with relative orders: ", RelativeOrders(Q.Pccol){[Length(OldWeights)+1..Length(Q.Weights)]});
    fi;
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
  od;
  
  # store the largest known nilpotent quotient of <G>
  QS := [ [], [] ];
  if HasPQuotientSystems( G ) then 
    Append( QS[1], ShallowCopy( PQuotientSystems( G )[1] ) );
    Append( QS[2], ShallowCopy( PQuotientSystems( G )[2] ) );
  fi;
  ResetFilterObj( G, PQuotientSystems );

  i := Position( QS[1], prime ); 
  if i <> fail then 
    QS[2][i] := Q;
  else 
    Add( QS[1], prime );
    Add( QS[2], Q );
  fi;
  SetPQuotientSystems( G, QS );

  # build the nilpotent quotient with lcs
  H := PcpGroupByCollectorNC( Q.Pccol );
  SetExponentPCentralSeries(H,LPRES_ExponentPCentralSeries(Q));

  return( H );
  end);

############################################################################
##
#M  NqPQuotient ( <LpGroup> ) . . . . . . for invariant LpGroups
##
## attempts to compute the largest nilpotent quotient of <LpGroup>.
## Note that this method only terminates if <LpGroup> has a largest 
## nilpotent quotient.
##
InstallOtherMethod( NqPQuotient,
  "for an invariantly L-presented group with a quotient system",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasPQuotientSystems,
    IsPosInt ], 0,
  function( G, prime )
  local Q,    # current quotient system
        H,    # largest nilpotent quotient of <G>
        QS,   # old quotient system
        time,	# runtime
        weights,
        largestQuot,
        i;	   # loop variable

  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;

  # check if there's already a largest p-quotient known
  if HasLargestPQuotients(G) then 
    i := Position( LargestPQuotients(G)[1], prime );
    if i <> fail then 
      return( LargestPQuotients(G)[2][i] );
    fi;
  fi;

  Print( "Running method with a qsystem 2... \n" );
  i := Position( PQuotientSystems( G )[1], prime );
  if i = fail then
    Print( "Try next method...\n" );
    TryNextMethod();
  fi;

  # Compute a confluent nilpotent presentation for G/G'
  Q := PQuotientSystems( G )[2][i];
  
  repeat
    weights:=ShallowCopy(Q.Weights);
    
    time := Runtime();

    # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
    Q := ExtendPQuotientSystem(Q);
  
    if weights <> Q.Weights then 
      if Length( Q.Weights ) - Length( weights ) > InfoLPRES_MAX_GENS then
        Info( InfoLPRES, 1, "Class ", Maximum(Q.Weights), ": ", Length(Q.Weights)-Length(weights), " generators");
      else
        Info( InfoLPRES, 1, "Class ", Maximum(Q.Weights), ": ", Length(Q.Weights)-Length(weights),
             " generators with relative orders: ", RelativeOrders(Q.Pccol){[Length(weights)+1..Length(Q.Weights)]});
      fi; 
    else 
      Info(InfoLPRES,1,"The group has a maximal nilpotent quotient of class ", Maximum(Q.Weights) );
    fi;
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));

  until weights = Q.Weights;

  largestQuot := [ [], [] ];
  if HasLargestPQuotients( G ) then 
    Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
    Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
  fi;

  i := Position( largestQuot[1], prime ); 
  if i <> fail then 
    largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
  else 
    Add( largestQuot[1], prime );
    Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
  fi;
  ResetFilterObj( G, LargestPQuotients );
  SetLargestPQuotients( G, largestQuot );

  # store the quotient systems
  QS := [ [], [] ];
  if HasPQuotientSystems( G ) then 
    Append( QS[1], ShallowCopy( PQuotientSystems(G)[1] ) );
    Append( QS[2], ShallowCopy( PQuotientSystems(G)[2] ) );
  fi;

  i := Position( QS[1], prime );
  if i = fail then
    Add( QS[1], prime );
    Add( QS[2], Q );
  else
    QS[2][i] := Q;
  fi;
  ResetFilterObj( G, PQuotientSystems );
  SetPQuotientSystems( G, QS );

  # build the nilpotent quotient with lcs
  H:=PcpGroupByCollectorNC(Q.Pccol);
  SetExponentPCentralSeries(H,LPRES_ExponentPCentralSeries(Q));

  return(H);
  end);

############################################################################
##
#M  PQuotient( <LpGroup> ) . . . . . . for invariant LpGroups
##
## determines the largest nilpotent quotient of <LpGroup> if it 
## has a nilpotent quotient system as an attribute.
## Note that this method only terminates if <LpGroup> has a largest 
## nilpotent quotient.
##
InstallOtherMethod( NqPQuotient,
  "for an invariantly L-presented group",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt ], 0,
  function( G, prime )
  local Q,	   # current quotient system
        H,    # largest nilpotent quotient of <G>
        QS,   # old quotient system
        i,	   # loop variable
        weights,
        largestQuot,
        time;	# runtime

  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;

  time:=Runtime();

  # Compute a confluent nilpotent presentation for G/G'
  Q := InitPQuotientSystem( G, prime );

  # store the largest quotient if this one is trivial
  if Length( Q.Weights ) = 0 then 
    largestQuot := [ [], [] ];
    if HasLargestPQuotients( G ) then 
      Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
      Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
    fi;
    ResetFilterObj( G, LargestPQuotients );

    i := Position( largestQuot[1], prime ); 
    if i <> fail then 
      largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
    else 
      Add( largestQuot[1], prime );
      Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
    fi;
    SetLargestPQuotients( G, largestQuot );

    # reset the quotient system
    QS := [ [], [] ];
    if HasPQuotientSystems( G ) then 
      Append( QS[1], ShallowCopy( PQuotientSystems( G )[1] ) );
      Append( QS[2], ShallowCopy( PQuotientSystems( G )[2] ) );
    fi;
    ResetFilterObj( G, PQuotientSystems );

    i := Position( QS[1], prime ); 
    if i <> fail then 
      QS[2][i] := Q;
    else 
      Add( QS[1], prime );
      Add( QS[2], Q );
    fi;
    SetPQuotientSystems( G, QS );
   
    return( PcpGroupByCollectorNC( Q.Pccol ) );
  fi;

  if Length(Q.Weights) > InfoLPRES_MAX_GENS then 
    Info(InfoLPRES,1,"Class ",1,": ", Length(Q.Weights), " generators");
  else
    Info(InfoLPRES,1,"Class ",1,": ", Length(Q.Weights),
               " generators with relative orders: ", RelativeOrders(Q.Pccol));
  fi;
  Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
  
  repeat
    weights:=ShallowCopy(Q.Weights);
    
    time := Runtime();
    # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
    Q:=ExtendPQuotientSystem(Q);
  
    if weights <> Q.Weights then 
      if Length(Q.Weights)-Length(weights) > InfoLPRES_MAX_GENS then
        Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(weights), " generators");
      else
        Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(weights),
             " generators with relative orders: ", RelativeOrders(Q.Pccol){[Length(weights)+1..Length(Q.Weights)]});
      fi;
    fi;
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));

  until weights = Q.Weights;

  # store the largest quotient if this one is trivial
  largestQuot := [ [], [] ];
  if HasLargestPQuotients( G ) then 
    Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
    Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
  fi;
  ResetFilterObj( G, LargestPQuotients );

  i := Position( largestQuot[1], prime ); 
  if i <> fail then 
    largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
  else 
    Add( largestQuot[1], prime );
    Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
  fi;
  SetLargestPQuotients( G, largestQuot );

  # store the quotient systems
  QS := [ [], [] ];
  if HasPQuotientSystems( G ) then 
    Append( QS[1], ShallowCopy( PQuotientSystems(G)[1] ) );
    Append( QS[2], ShallowCopy( PQuotientSystems(G)[2] ) );
  fi;

  i := Position( QS[1], prime );
  if i = fail then
    Add( QS[1], prime );
    Add( QS[2], Q );
  else
    QS[2][i] := Q;
  fi;
  ResetFilterObj( G, PQuotientSystems );
  SetPQuotientSystems( G, QS );

  # build the nilpotent quotient with lcs
  H:=PcpGroupByCollectorNC(Q.Pccol);
  SetExponentPCentralSeries(H,LPRES_ExponentPCentralSeries(Q));

  return(H);
  end);

############################################################################
##
#M  NqEpimorphismPQuotient( <LpGroup>, <int>, <int> ) . . . . . . for invariant LpGroups
##
##
InstallOtherMethod( NqEpimorphismPQuotient,
  "for an L-presented group",
  true,
  [ IsLpGroup,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  local Grp, epi, pi, H, N, hom;

  # work with the underlying invariant LpGroup which maps onto G
  Grp := UnderlyingInvariantLPresentation( G );

  if not HasIsInvariantLPresentation( Grp ) then
    SetIsInvariantLPresentation( Grp, true );
  fi;

  # natural homomorphism onto the p-quotient
  epi := NqEpimorphismPQuotient( Grp, prime, c );

  # the p-quotient
  H := Range( epi );

  # natural homomorphisms from the free group onto the LpGroup
  pi := GroupHomomorphismByImages( FreeGroupOfLpGroup( Grp ),
                                   Grp,
                                   GeneratorsOfGroup( FreeGroupOfLpGroup( Grp ) ),
                                   GeneratorsOfGroup( Grp ) );
  
  # normal subgroup generated by the images of the fixed relations
  N := NormalClosure( H, Subgroup( H, List( FixedRelatorsOfLpGroup( G ), x -> ( x^pi ) ^ epi ) ) );

  hom := NaturalHomomorphismByNormalSubgroup( H, N );

  # return the factor group
  return( GroupHomomorphismByImagesNC( G, 
                                       Range( hom ),
                                       GeneratorsOfGroup( G ), 
                                       List( GeneratorsOfGroup( G ), x -> ( ( UnderlyingElement( x )^pi ) ^ epi ) ^ hom ) ) );
  end);

############################################################################
##
#M  NqEpimorphismPQuotient( <LpGroup>, <int>, <int> ) . . . . . . for invariant LpGroups
##
InstallOtherMethod( NqPQuotient,
  "for an L-presented group",
  true,
  [ IsLpGroup,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  return( Range( NqEpimorphismPQuotient( G, prime, c ) ) );
  end );

############################################################################
##
#M  NqEpimorphismPQuotient ( <LpGroup>, <int>, <int> )
##
## computes an epimorphism from <LpGroup> onto its p-class-<int> quotient 
## if a nilpotent quotient system of <LpGroup> is already known.
##
InstallOtherMethod( NqEpimorphismPQuotient,
  "for an invariantly L-presented group with a quotient system and an integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasPQuotientSystems,
    IsPosInt,
    IsPosInt], 0,
  function( G, prime, c )
  local Q,    # current quotient system
        QS,   # old quotient system
      	 i,    # loop variable
        weights,
        largestQuot,
        time,	# runtime
        n;    # nilpotency class

  i := Position( PQuotientSystems( G )[1], prime );
  if i = fail then 
    Print( "Try Next Method...\n" );
    TryNextMethod();
  fi;

  # known quotient system of <G>
  Q := PQuotientSystems( G )[2][i];
 
  # if the Frattini quotient is trivial
  if IsEmpty( Q.Weights ) then 
    return( Q.Epimorphism );
  fi;

  # p class of <Q>
  n := Maximum( Q.Weights );
 
  if c = n then 
    # the given nilpotency class <n> is already known
    return( Q.Epimorphism );
  elif c<n then
    # the given nilpotency class <c> is already computed 
    QS := SmallerPQuotientSystem(Q,c);
    return( QS.Epimorphism );
  else
    if HasLargestPQuotients(G) then 
      i := Position( LargestPQuotient( G ), prime );
      if i <> fail then
        Info( InfoLPRES, 1, "Largest nilpotent quotient of class ", NilpotencyClassOfGroup(LargestPQuotient(G)[2][i]));
        return(Q.Epimorphism);
      fi;
    fi;

    # extend the largest known quotient system
    for i in [n+1..c] do
      weights:=ShallowCopy(Q.Weights);

      time := Runtime();
      # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
      Q:=ExtendPQuotientSystem(Q);
  
      # if we couldn't extend the quotient system any more, we're finished
      if weights = Q.Weights then 
        # store the largest quotient if this one is trivial
        largestQuot := [ [], [] ];
        if HasLargestPQuotients( G ) then 
          Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
          Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
        fi;
        ResetFilterObj( G, LargestPQuotients );
      
        i := Position( largestQuot[1], prime ); 
        if i <> fail then 
          largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
        else 
          Add( largestQuot[1], prime );
          Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
        fi;
        SetLargestPQuotients( G, largestQuot );

        Info(InfoLPRES,1,"Largest nilpotent quotient of class ", Maximum(Q.Weights)); 
        break;
      else 
        if Length(Q.Weights)-Length(weights) > InfoLPRES_MAX_GENS then 
          Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(weights), " generators");
        else
          Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(weights),
               " generators with relative orders: ", RelativeOrders(Q.Pccol){[Length(weights)+1..Length(Q.Weights)]});
        fi;
      fi;
      Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
    od;

    # store the quotient systems
    QS := [ [], [] ];
    if HasPQuotientSystems( G ) then 
      Append( QS[1], ShallowCopy( PQuotientSystems(G)[1] ) );
      Append( QS[2], ShallowCopy( PQuotientSystems(G)[2] ) );
    fi;
  
    i := Position( QS[1], prime );
    if i = fail then
      Add( QS[1], prime );
      Add( QS[2], Q );
    else
      QS[2][i] := Q;
    fi;
    ResetFilterObj( G, PQuotientSystems );
    SetPQuotientSystems( G, QS );

    return(Q.Epimorphism); 
  fi;
  end);

############################################################################
##
#M  NqEpimorphismPQuotient ( <LpGroup>, <int> )
##
## computes an epimorphism from <LpGroup> onto its class-<int> quotient.
##
InstallOtherMethod( NqEpimorphismPQuotient,
  "for an invariantly L-presented group",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt ], 0,
  function( G, prime )
  local i,  # loop variable
        H; 	# nilpotent quotient <G>/gamma_<c>(<G>)
  
  Print( "NqEpimorphismsPQuotient - version\n" );
  if not IsPrime( prime ) then 
    Error( "<prime> should be a prime number\n" );
  fi;

  # compute the nilpotent quotient of <G>
  H := NqPQuotient( G, prime );
   
  i := Position( PQuotientSystems( G )[1], prime );
  if i = fail then 
    Error( "Should not happen?" );
  fi;

  return( PQuotientSystems( G )[2][i].Epimorphism );
  end);

############################################################################
##
#M  NqEpimorphismPQuotient ( <LpGroup>, <int>, <int> )
##
## computes an epimorphism from <LpGroup> onto its p-class-<int> quotient 
## if a nilpotent quotient system of <LpGroup> is already known.
##
InstallOtherMethod( NqEpimorphismPQuotient,
  "for an invariantly L-presented group with a quotient system and an integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasPQuotientSystems,
    IsPosInt ], 0,
  function( G, prime )
  local Q,    # current quotient system
        QS,   # old quotient system
      	 i,    # loop variable
        weights,
        largestQuot,
        time,	# runtime
        n;    # nilpotency class

  i := Position( PQuotientSystems( G )[1], prime );
  if i = fail then 
    Print( "Try Next Method...\n" );
    TryNextMethod();
  fi;

  # known quotient system of <G>
  Q := PQuotientSystems( G )[2][i];
 
  # if the Frattini quotient is trivial
  if IsEmpty( Q.Weights ) then 
    return( Q.Epimorphism );
  fi;

  # p class of <Q>
  n := Maximum( Q.Weights );
 
  if HasLargestPQuotients(G) then 
    i := Position( LargestPQuotient( G ), prime );
    if i <> fail then
      Info( InfoLPRES, 1, "Largest nilpotent quotient of class ", NilpotencyClassOfGroup(LargestPQuotient(G)[2][i]));
      return(Q.Epimorphism);
    fi;
  fi;

  # extend the largest known quotient system
  repeat
    weights:=ShallowCopy(Q.Weights);

    time := Runtime();
    # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
    Q:=ExtendPQuotientSystem(Q);

    if Length(Q.Weights)-Length(weights) > InfoLPRES_MAX_GENS then 
      Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(weights), " generators");
    else
      Info(InfoLPRES,1,"Class ",Maximum(Q.Weights),": ", Length(Q.Weights)-Length(weights),
         " generators with relative orders: ", RelativeOrders(Q.Pccol){[Length(weights)+1..Length(Q.Weights)]});
    fi;
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
  until weights = Q.Weights;

  # store the largest quotient if this one is trivial
  largestQuot := [ [], [] ];
  if HasLargestPQuotients( G ) then 
    Append( largestQuot[1], ShallowCopy( LargestPQuotients( G )[1] ) );
    Append( largestQuot[2], ShallowCopy( LargestPQuotients( G )[2] ) );
  fi;
  ResetFilterObj( G, LargestPQuotients );

  i := Position( largestQuot[1], prime ); 
  if i <> fail then 
    largestQuot[2][i] := PcpGroupByCollectorNC( Q.Pccol );
  else 
    Add( largestQuot[1], prime );
    Add( largestQuot[2], PcpGroupByCollectorNC( Q.Pccol ) );
  fi;
  SetLargestPQuotients( G, largestQuot );

  Info(InfoLPRES,1,"Largest nilpotent quotient of class ", Maximum(Q.Weights)); 

  # store the quotient systems
  QS := [ [], [] ];
  if HasPQuotientSystems( G ) then 
    Append( QS[1], ShallowCopy( PQuotientSystems(G)[1] ) );
    Append( QS[2], ShallowCopy( PQuotientSystems(G)[2] ) );
  fi;

  i := Position( QS[1], prime );
  if i = fail then
    Add( QS[1], prime );
    Add( QS[2], Q );
  else
    QS[2][i] := Q;
  fi;
  ResetFilterObj( G, PQuotientSystems );
  SetPQuotientSystems( G, QS );

  return(Q.Epimorphism); 

  end);

############################################################################
##
#M  NqEpimorphismPQuotient ( <LpGroup>, <int> )
##
## computes an epimorphism from <LpGroup> onto its class-<int> quotient.
##
InstallOtherMethod( NqEpimorphismPQuotient,
  "for an invariantly L-presented group",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt,
    IsPosInt], 0,
  function( G, prime, c )
  local i,  # loop variable
        H; 	# nilpotent quotient <G>/gamma_<c>(<G>)
  
  Print( "NqEpimorphismsPQuotient - simple version\n" );
  if not IsPrime( prime ) then 
    Error( "<prime> should be a prime number\n" );
  fi;

  # compute the nilpotent quotient of <G>
  H := NqPQuotient( G, prime, c );
   
  i := Position( PQuotientSystems( G )[1], prime );
  if i = fail then 
    Error( "Should not happen?" );
  fi;

  # note that we've computed precisely the (ATMOST) class c quotient 
  return( PQuotientSystems( G )[2][i].Epimorphism );
  end);


############################################################################
##
#F  LPRES_ExponentPCentralSeries( <QS> )
##
## computes the lower central series of a nilpotent quotient given by a 
## quotient system <QS>.
##
InstallGlobalFunction( LPRES_ExponentPCentralSeries,
  function(Q)
  local weights,	# weights-function of <Q>
       	i,		     # loop variable
       	c, 		    # nilpotency class of <Q>
       	H,		     # nilpotent presentation corr. to <Q>
       	gens,	   # generators of <H>
        pcs;		   # lower central series of <H>

  # nilpotent presentation group corr. to <Q>
  H := PcpGroupByCollectorNC( Q.Pccol );

  # generators of <H>
  gens := GeneratorsOfGroup( H );

  # weights function of the given quotient system
  weights := Q.Weights;

  # nilpotency class of <Q>
  c := Maximum(weights);

  # build the exponent-p central series
  pcs:=[];
  pcs[c+1] := SubgroupByIgs(H,[]);
  pcs[1]   := H;
 
  for i in [2..c] do
    # the exponent-p central series as subgroups by an induced generating system
    # with weights at least <i>
    pcs[i] := SubgroupByIgs( H, gens{Filtered([1..Length(gens)],x->weights[x]>=i)} );
  od;

  return( pcs );
  end);

############################################################################
##
#A  PQuotients( <LpGroup> ) .  .  .  .  . for invariant LpGroups
##
# InstallMethod( PQuotients,
#   "for an invariantly L-presented group with a quotient system",
#   true,
#   [ IsLpGroup and HasPQuotientSystems ], 0,
#   function ( G )
#   local c;	# nilpotency class of the known quotient system
#    
#   c:=Maximum(PQuotientSystems(G).Weights); 
#   return( List([1..c], i -> NqEpimorphismPQuotient(G,i) ) );
#   end);

############################################################################
##
#M  PQuotient( <FpGroup> )
##
# InstallOtherMethod( PQuotient,
#   "for an FpGroup using the LPRES-package", true,
#   [ IsFpGroup, IsPosInt ], -1, # give priority to NQ package
#   function( G, c )
#   return( PQuotient( Range( IsomorphismLpGroup( G ) ), c ) );
#   end);
# 
# InstallOtherMethod( PQuotient,
#   "for an FpGroup using the LPRES-package", true,
#   [ IsFpGroup ], -1, # give priority to NQ package
#   G -> PQuotient( Range( IsomorphismLpGroup( G ) ) ) );

############################################################################
##
#M  NqEpimorphismPQuotient( <FpGroup> )
##
# InstallOtherMethod( NqEpimorphismPQuotient,
#   "for an FpGroup using the LPRES-package", true,
#   [ IsFpGroup, IsPosInt ], 0,
#   function( G, c )
#   local iso, 	# isomorphism from FpGroup to LpGroup
# 	mapi,	# MappingGeneratorsImages of <iso>
# 	epi;	# epimorphism from LpGroup onto its nilpotent quotient
# 
#   iso  := IsomorphismLpGroup( G );
#   mapi := MappingGeneratorsImages( iso );
#   epi  := NqEpimorphismPQuotient( Range( iso ), c );
#   return( GroupHomomorphismByImages( G, Range( epi ), mapi[1], 
#                         List( mapi[1], x -> Image( epi, Image( iso, x ) ) ) ) );
#   end);

############################################################################
##
#M  LargestPQuotient ( <LpGroup>, <int> ) . . for invariant LpGroups
##
## computes a weighted nilpotent presentation for the class-<int> quotient
## of the invariant <LpGroup> if it already has a nilpotent quotient system.
##
InstallOtherMethod( LargestPQuotient,
  "for invariantly L-presented groups (with qs) and positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt ], 0,
  function( G, prime ) 
  local i;

  if HasLargestPQuotients( G ) and prime in LargestPQuotients( G )[1] then 
    i := Position( LargestPQuotients(G)[1], prime );
    return( LargestPQuotients(G)[2][i] );
  fi;

  return( NqPQuotient( G, prime ) );
  end );

############################################################################
##
#F  SmallerPQuotientSystem ( <Q>, <int> )
## 
## computes a nilpotent quotient system for G/gamma_i(G) if a nilpotent 
## quotient system for G/gamma_j(G) is known, i<j.
##
InstallGlobalFunction( SmallerPQuotientSystem,
  function( Q, c )
  local QS,		            # new quotient system
	       i,j,k,		         # loop variables
        n,		             # number of gens of <QS>
        orders,		        # relative orders of the new qs.
       	imgs,		          # new images of the epimorphism
        rhs_old,rhs_new; # right hand side of a relation

  # set up the new quotient system
  QS := rec( Lpres := Q.Lpres, 
             Weights := Filtered( Q.Weights, x -> x<=c ) );
  
  # number of gens of <QS>
  n := Length( QS.Weights );
 
  QS.Definitions := Q.Definitions{[1..n]};
  
  # build new collector using <Q.Pccol>
  QS.Pccol := FromTheLeftCollector(n);

  # the conjugate relations
  for i in [1..n] do
    for j in [i+1..n] do
      rhs_old := GetConjugate( Q.Pccol, j, i );
      rhs_new := [];
      for k in [1,3..Length(rhs_old)-1] do
        if Q.Weights[rhs_old[k]]<=c then 
          Append(rhs_new,rhs_old{[k,k+1]});
        else 
          # the weights-function is increasing
          break;
        fi;
      od; 
      SetConjugate( QS.Pccol, j, i, rhs_new );
    od;
  od;

  # find the gens with power relations
  orders := RelativeOrders( Q.Pccol ){[1..n]};

  # new power relations
  for i in Filtered([1..Length(orders)],x->orders[x]<>0) do
    rhs_old:=GetPower(Q.Pccol,i);
    rhs_new:=[];
    for k in [1,3..Length(rhs_old)-1] do
      if Q.Weights[rhs_old[k]]<=c then 
        Append(rhs_new,rhs_old{[k,k+1]});
      else 
        # the weights-function is increasing
        break;
      fi;
    od; 
    SetRelativeOrder(QS.Pccol,i,orders[i]);
    SetPower(QS.Pccol,i,rhs_new);
  od;
  UpdatePolycyclicCollector(QS.Pccol);

  # the new images of the epimorphism
  QS.Imgs:=[];
  for i in [1..Length(Q.Imgs)] do 
    if IsInt(Q.Imgs[i]) then
      QS.Imgs[i]:=Q.Imgs[i];
    else
      rhs_old:=Q.Imgs[i];
      rhs_new:=[];
      for k in [1,3..Length(rhs_old)-1] do
        if Q.Weights[rhs_old[k]]<=c then 
          Append(rhs_new,rhs_old{[k,k+1]});
        else 
          # the weights-function is increasing
          break;
        fi;
      od;
      QS.Imgs[i]:=rhs_new;
    fi;
  od; 
  
  # build the new epimorphism
  imgs:=[];
  for i in [1..Length(QS.Imgs)] do
    if IsInt(QS.Imgs[i]) then 
      imgs[i]:=[QS.Imgs[i],1];
    else 
      imgs[i]:=QS.Imgs[i];
    fi;
  od;
  imgs:=List(imgs,x->PcpElementByGenExpList(QS.Pccol,x));

  QS.Epimorphism:=GroupHomomorphismByImagesNC( QS.Lpres,
                                            			PcpGroupByCollectorNC(QS.Pccol),
                                               GeneratorsOfGroup(QS.Lpres),
                                               imgs);
 
  return( QS );
  end);

############################################################################
##
#F  PCentralSeries ( <PcpGroup> )
## 
##
InstallOtherMethod( PCentralSeries,
  "",
  true,
  [ IsPcpGroup and HasExponentPCentralSeries ], 0,
  ExponentPCentralSeries );
