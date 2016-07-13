############################################################################
##
#W gap/pql/pq.gi			LPRES				René Hartung
##

############################################################################
##
#F  internal function
##
## stores the largest torsion-free nilpotent quotient as an attribute of
## the <LpGroup>
##
LPRES_StoreLargestTorsionFreeNilpotentQuotient := function( G, Q ) 
  if HasLargestTorsionFreeNilpotentQuotient( G ) then 
    ResetFilterObj( G, LargestTorsionFreeNilpotentQuotient );
  fi;
  SetLargestTorsionFreeNilpotentQuotient( G, PcpGroupByCollectorNC( Q.Pccol ) );
end;

############################################################################
##
#F  internal function
##
## stores the quotient system as an attribute of the <LpGroup>
##
LPRES_StoreTorsionFreeNilpotentQuotientSystem := function( G, Q ) 

  # store the quotient systems
  if HasTorsionFreeNilpotentQuotientSystem( G ) then
    if Maximum( TorsionFreeNilpotentQuotientSystem( G ).Weights ) < Maximum( Q.Weights ) then 
      ResetFilterObj( G, TorsionFreeNilpotentQuotientSystem );
    else
      return;
    fi;
  fi;
  SetTorsionFreeNilpotentQuotientSystem( G, Q );
end;

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient( <LpGroup>, <class> ) . . .
## 
## computes the natural homomorphism on the <class> torsion free nilpotent
## quotient of the invariant <LpGroup>.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an invariant LpGroup and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt ], 0,
  function( G, c )
  local Q,           # current quotient system
	       H,           # the nilpotent quotient of <G>
	       weights, 	# old weights quotient system
	       i,	          # loop variable
	       time;	       # runtime

  # Compute a weighted nilpotent presentation for the Frattini quotient G/G'G^p
  time := Runtime();
  Q := InitRationalQuotientSystem( G );
  Info( InfoLPRES, 2, "Runtime for this step ",  StringTime(Runtime()-time));

  # store the largest quotient and its quotient sysstem if it's trivial
  if Length( Q.Weights ) = 0 then 
    LPRES_StoreLargestTorsionFreeNilpotentQuotient( G, Q );
    LPRES_StoreTorsionFreeNilpotentQuotientSystem( G, Q );
    return( Q.Epimorphism );
  fi;
  
  for i in [2..c] do 
    # copy the old quotient system to compare with the extended qs.
    weights := ShallowCopy(Q.Weights);

    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time:=Runtime();
    Q := ExtendRationalQuotientSystem( Q );
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
   
    # if we couldn't extend the quotient system any more, we're finished
    if weights = Q.Weights then 
      LPRES_StoreLargestTorsionFreeNilpotentQuotient( G, Q );
      Info(InfoLPRES,1,"The group has a maximal tosion free nilpotent quotient of class ", Maximum(Q.Weights) );
      break;
    fi;
  od;
  
  # store the largest known nilpotent quotient of <G>
  LPRES_StoreTorsionFreeNilpotentQuotientSystem( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient ( <LpGroup>, <class> ) . . . . .
## 
## computes the natural homomorphism on the <class> torsion free nilpotent 
## quotient of the invariant <LpGroup>, if the latter has already some
## quotient system stored as attribute.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an invariant LpGroup with quotient system and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasTorsionFreeNilpotentQuotientSystem,
    IsPosInt ], 0,
  function( G, c )
  local Q,    # current quotient system
        H,    # the nilpotent quotient of <G>
        i,    # loop variable
        weights, 
        time,	# runtime
        j;    # nilpotency class

  # known quotient system of <G>
  Q := TorsionFreeNilpotentQuotientSystem( G );
 
  # p-class of the quotient system
  j := Maximum( Q.Weights );
 
  if c = j then  
    # requested this quotient system
    return( Q.Epimorphism );
  elif c<j then 
    # the quotient system is already there
    return( SmallerTorsionFreeNilpotentQuotientSystem( Q, c ).Epimorphism );
  fi;

  # check if there's already a largest p-quotient
  if HasLargestTorsionFreeNilpotentQuotient(G) then
    return( LargestTorsionFreeNilpotentQuotient(G).Epimorphism );
  fi;

  # extend the largest known quotient system
  for i in [j+1..c] do
    weights := ShallowCopy( Q.Weights );

    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time := Runtime();
    Q := ExtendRationalQuotientSystem( Q );
    Info( InfoLPRES, 2, "Runtime for this step ", StringTime(Runtime()-time) );

    # if we couldn't extend the quotient system any more, we're finished
    if weights = Q.Weights then 
      LPRES_StoreLargestTorsionFreeNilpotentQuotient( G, Q );
      Info(InfoLPRES,1,"The group has a maximal p-quotient of p-class ", Maximum(Q.Weights) );
      break;
    fi;
  od;

  # store this quotient system as an attribute of the group G
  LPRES_StoreTorsionFreeNilpotentQuotientSystem( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient ( <LpGroup> ) . . . . . . . . . .
## 
## attempts to compute the natural homomorphism onto the largest torsion-free 
## nilpotent quotient of the invariant <LpGroup>.
## This method only terminates if <LpGroup> has such a largest quotient.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an invariant LpGroup",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation ], 0,
  function( G )
  local Q,	   # current quotient system
        H,    # largest nilpotent quotient of <G>
        QS,   # old quotient system
        i,	   # loop variable
        weights,
        time;	# runtime

  # Compute a confluent nilpotent presentation for G/G'
  time:=Runtime();
  Q := InitRationalQuotientSystem( G );
  Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));

  # store the largest quotient if this one is trivial
  if Length( Q.Weights ) = 0 then 
    LPRES_StoreLargestTorsionFreeNilpotentQuotient( G, Q );
    LPRES_StoreTorsionFreeNilpotentQuotientSystem( G, Q );
    return( Q.Epimorphism );
  fi;
  
  repeat
    weights := ShallowCopy(Q.Weights);
    
    # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
    time := Runtime();
    Q := ExtendRationalQuotientSystem( Q );
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));

  until weights = Q.Weights;
  Info(InfoLPRES,1,"The group has a maximal p-quotient of p-class ", Maximum(Q.Weights) );

  LPRES_StoreLargestTorsionFreeNilpotentQuotient( G, Q );
  LPRES_StoreTorsionFreeNilpotentQuotientSystem( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient ( <LpGroup>, <int c> ) . . for invariant LpGroups
## 
## attempts to compute the natural homomorphism onto the largest torsion-free
## nilpotent quotient of the invariant <LpGroup> if the latter has a quotient
## system as attribute.
## This method only terminates if <LpGroup> has such a largest quotient. 
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an invariant Lpgroup with a quotient system",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasTorsionFreeNilpotentQuotientSystem ], 0,
  function( G )
  local Q,    # current quotient system
        H,    # largest nilpotent quotient of <G>
        QS,   # old quotient system
        time,	# runtime
        weights,
        i;	   # loop variable

  # check if there's already a largest p-quotient known
  if HasLargestTorsionFreeNilpotentQuotient(G) then 
    return( LargestTorsionFreeNilpotentQuotient(G).Epimorphism );
  fi;

  # Compute a confluent nilpotent presentation for G/G'
  Q := TorsionFreeNilpotentQuotientSystem( G );
  
  repeat
    weights := ShallowCopy(Q.Weights);
    
    # extend the quotient system of G/\gamma_i to G/\gamma_{i+1}
    time := Runtime();
    Q := ExtendRationalQuotientSystem(Q);
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
  
  until weights = Q.Weights;
  Info(InfoLPRES,1,"The group has a maximal p-quotient of p-class ", Maximum(Q.Weights) );

  LPRES_StoreLargestTorsionFreeNilpotentQuotient( G, Q );
  LPRES_StoreTorsionFreeNilpotentQuotientSystem( G, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient( <LpGroup>, <class> ) . . . . . .
## 
## computes the natural homomorphism on the <class> torsion free nilpotent 
## quotient of <LpGroup>.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an (arbitrary) LpGroup and a positive integer",
  true,
  [ IsLpGroup,
    IsPosInt ], 0,
  function( G, c )
  local Grp, epi, pi, H, N, hom;

  # work with the underlying invariant LpGroup which maps onto G
  Grp := UnderlyingInvariantLPresentation( G );

  if not HasIsInvariantLPresentation( Grp ) then
    SetIsInvariantLPresentation( Grp, true );
  fi;

  # natural homomorphism onto the p-quotient
  epi := EpimorphismTorsionFreeNilpotentQuotient( Grp, c );

  # the p-quotient
  H := Range( epi );

  # natural homomorphisms from the free group onto the LpGroup
  pi := GroupHomomorphismByImages( FreeGroupOfLpGroup( Grp ), Grp,
                                   GeneratorsOfGroup( FreeGroupOfLpGroup( Grp ) ),
                                   GeneratorsOfGroup( Grp ) );
  
  # normal subgroup generated by the images of the fixed relations
  N := NormalClosure( H, Subgroup( H, List( FixedRelatorsOfLpGroup( G ), x -> ImageElm( epi, ImageElm( pi, x ) ) ) ) );

  # natural homomorphism onto the quotietn H/N
  hom := NaturalHomomorphismByNormalSubgroup( H, N );

  # return the factor group
  return( GroupHomomorphismByImagesNC( G, Range( hom ),
                                       GeneratorsOfGroup( G ), 
                                       List( GeneratorsOfGroup( G ), x -> ( ( UnderlyingElement( x )^pi ) ^ epi ) ^ hom ) ) );
  end);

############################################################################
##
#M  TorsionFreeNilpotentQuotient ( <LpGroup>, <class> ) . . . . . . . . . . .
##
## computes the <class> torsion-free nilpotent quotient of <LpGroup>.
##
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For an LpGroup and a positive integer",
  true,
  [ IsLpGroup,
    IsPosInt ], 0,
  function( G, c )
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G, c ) ) );
  end);

############################################################################
##
#M  TorsionFreeNilpotentQuotient( <LpGroup> ) . . . . . . 
## 
## attempts to compute the largest torsion free nilpotent quotient of <LpGroup>.
## This method terminates if and only if <LpGroup> admits such a largest 
## quotient.
##
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For an LpGroup",
  true,
  [ IsLpGroup ], 0,
  function( G )
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G ) ) );
  end);

############################################################################
##
#M  TorsionFreeNilpotentQuotient( <FpGroup>, <class> )
## 
## computes the <class> torsion free nilpotent quotient of <FpGroup>.
##
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For an FpGroup and a positive integer (using the LPRES-package)", true,
  [ IsFpGroup, IsPosInt ], -1, # give priority to ANUPQ package
  function( G, c )
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G, c ) ) );
  end);

############################################################################
##
#M  TorsionFreeNilpotentQuotient( <PcpGroup>, <class> )
## 
## computes the <class> torsion free nilpotent quotient of <PcpGroup>.
##
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For a PcpGroup and a positive integer (using the LPRES-package)", true,
  [ IsPcpGroup, IsPosInt ], -1, # give priority to ANUPQ package
  function( G, c )
  local iso;
  iso := IsomorphismFpGroup( G );
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( Range( iso ), c ) ) );
  end);

############################################################################
##
#M  TorsionFreeNilpotentQuotient( <FpGroup> )
##
## attempts to compute the largest torsion free nilpotent quotient of <FpGroup>.
## This method terminates if and only if <FpGroup> admits such a largest 
## quotient.
##
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For an FpGroup (using the LPRES-package)", true,
  [ IsFpGroup ], -1,           # give priority to ANUPQ package
  function( G )
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G ) ) );
  end );

############################################################################
##
#M  TorsionFreeNilpotentQuotient( <PcpGroup> )
##
## attempts to compute the largest torsion free nilpotent quotient of <PcpGroup>.
## This method terminates if and only if <FpGroup> admits such a largest 
## quotient.
##
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For a PcpGroup (using the LPRES-package)", true,
  [ IsPcpGroup ], -1,           # give priority to ANUPQ package
  function( G )
  local iso;
  iso := IsomorphismFpGroup( G );
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( Range(iso) ) ) );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient( <FpGroup>, <class> )
## 
## computes the natural homomorphism on the <class> torsion free nilpotent 
## quotient of <FpGroup>.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an FpGroup and a positive integer (using the LPRES-package)",
  true,
  [ IsFpGroup,
    IsPosInt ], -1,                       # give priority to ANUPQ package
  function( G, c )
  local iso, 	# isomorphism from FpGroup to LpGroup
        mapi,	# MappingGeneratorsImages of <iso>
        epi;	# epimorphism from LpGroup onto its nilpotent quotient

  iso  := IsomorphismLpGroup( G );
  mapi := MappingGeneratorsImages( iso );
  epi  := EpimorphismTorsionFreeNilpotentQuotient( Range( iso ), c );

  return( GroupHomomorphismByImagesNC( G,
                                       Range( epi ),
                                       mapi[1],
                                       List( mapi[1], x -> Image( epi, Image( iso, x ) ) ) ) );
  end);

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient( <FpGroup> )
## 
## attempts to compute the natural homomorphism onto the largest torsion 
## free nilpotent quotient of <FpGroup>.
## This method only terminates if <FpGroup> has such a largest quotient. 
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an FpGroup (using the LPRES-package)",
  true,
  [ IsFpGroup ], -1,                        # give priority to ANUPQ-package
  function( G )
  local iso, 	# isomorphism from FpGroup to LpGroup
        mapi,	# MappingGeneratorsImages of <iso>
        epi;	# epimorphism from LpGroup onto its nilpotent quotient

  iso  := IsomorphismLpGroup( G );
  mapi := MappingGeneratorsImages( iso );
  epi  := EpimorphismTorsionFreeNilpotentQuotient( Range( iso ) );
  return( GroupHomomorphismByImagesNC( G,
                                       Range( epi ),
                                       mapi[1],
                                       List( mapi[1], x -> Image( epi, Image( iso, x ) ) ) ) );
  end);

############################################################################
##
#F  SmallerTorsionFreeNilpotentQuotientSystem ( <Q>, <int> )
## 
## computes a quotient system for G/G_i if a nilpotent 
## quotient system for G/G_j, i<j, is known.
##
InstallGlobalFunction( SmallerTorsionFreeNilpotentQuotientSystem,
  function( Q, c )
  local QS,		            # new quotient system
	       i,j,k,		         # loop variables
        n,		             # number of gens of <QS>
        orders,		        # relative orders of the new qs.
       	imgs,		          # new images of the epimorphism
        H,
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

  H := PcpGroupByCollectorNC(QS.Pccol);
  SetRationalLowerCentralSeries( H, LPRES_RationalLowerCentralSeries( QS ) );

  QS.Epimorphism:=GroupHomomorphismByImagesNC( QS.Lpres, H, GeneratorsOfGroup(QS.Lpres), imgs);
 
  return( QS );
  end);
