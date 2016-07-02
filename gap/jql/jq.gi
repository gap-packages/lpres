############################################################################
##
#W gap/jql/jq.gi			LPRES				René Hartung
##

############################################################################
##
#F  internal function
##
## stores the largest p-quotient as an attribute of the <LpGroup>
##
LPRES_StoreLargestJenningsQuotient := function( G, prime, Q ) 
  local largestQuot, i;

  largestQuot := [ [], [] ];
  if HasLargestJenningsQuotients( G ) then 
    Append( largestQuot[1], ShallowCopy( LargestJenningsQuotients( G )[1] ) );
    Append( largestQuot[2], ShallowCopy( LargestJenningsQuotients( G )[2] ) );
  fi;
    
  i := Position( largestQuot[1], prime ); 
  if i <> fail then 
    largestQuot[2][i] := Q;
  else 
    Add( largestQuot[1], prime );
    Add( largestQuot[2], Q );
  fi;
  ResetFilterObj( G, LargestJenningsQuotients );
  SetLargestJenningsQuotients( G, largestQuot );
end;

############################################################################
##
#F  internal function
##
## stores the quotient system as an attribute of the <LpGroup>
##
LPRES_StoreJenningsQuotientSystems := function( G, prime, Q ) 
  local QS, i;

  # store the quotient systems
  QS := [ [], [] ];
  if HasJenningsQuotientSystems( G ) then 
    Append( QS[1], ShallowCopy( JenningsQuotientSystems(G)[1] ) );
    Append( QS[2], ShallowCopy( JenningsQuotientSystems(G)[2] ) );
  fi;

  i := Position( QS[1], prime );
  if i = fail then
    Add( QS[1], prime );
    Add( QS[2], Q );
  else 
    # replace
    QS[2][i] := Q;
  fi;
  ResetFilterObj( G, JenningsQuotientSystems );
  SetJenningsQuotientSystems( G, QS );
end;

############################################################################
##
## No need for a method of the form?? REALLY - may still see something 
## like this in the p-covering group?
##
## EpimorphismJenningsQuotient( <LpGroup>, <prime> ), or
## JenningsQuotient( <LpGroup>, <prime> )
## 
## due to the following remark:
## Remark 3. Let now G be a group but not a pgroup.
## There is another limitation which applies
## to the Jennings series, and not to the lower central
## series and to the lower p-series: when Di(G) =
## Di+1(G) 6= {1}, with p dividing the order of Di+1(G),
## we do not know whether we have reached the end
## of the calculation or not.
## 
## www.math.rwth-aachen.de/~Frank.Luebeck/Nikolaus/2003/CostantiniTalk.pdf
##

############################################################################
##
#M  EpimorphismJenningsQuotient( <LpGroup>, <prime>, <class> ) . . .
## 
## computes the natural homomorphism on the <class> p-quotient of the 
## invariant <LpGroup>.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an invariant LpGroup, a prime number, and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  local Q,           # current quotient system
	       H,           # the nilpotent quotient of <G>
	       weights, 	# old weights quotient system
	       i,	          # loop variable
	       time;	       # runtime

  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;

  # Compute a weighted nilpotent presentation for the Frattini quotient G/G'G^p
  time := Runtime();
  Q := InitPQuotientSystem( G, prime );
  Q.Class := 1;
  Info( InfoLPRES, 2, "Runtime for this step ",  StringTime(Runtime()-time));

  # store the largest quotient and its quotient sysstem if it's trivial
  if Length( Q.Weights ) = 0 then 
    LPRES_StoreLargestJenningsQuotient( G, prime, Q );
    LPRES_StoreJenningsQuotientSystems( G, prime, Q );

    return( Q.Epimorphism );
  fi;
  
  for i in [2..c] do 
    # copy the old quotient system to compare with the extended qs.
#   weights := ShallowCopy(Q.Weights);

    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time:=Runtime();
    Q := ExtendJenningsQuotientSystem( Q );
    Info(InfoLPRES,2,"Runtime for this step ", StringTime(Runtime()-time));
   
    # if we couldn't extend the quotient system any more, we're finished
#   if weights = Q.Weights then 
#     LPRES_StoreLargestJenningsQuotient( G, prime, Q );
#     Info(InfoLPRES,1,"The group has a maximal p-quotient of p-class ", Maximum(Q.Weights) );
#     break;
#   fi;
  od;
  
  # store the largest known nilpotent quotient of <G>
  LPRES_StoreJenningsQuotientSystems( G, prime, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismJenningsQuotient ( <LpGroup>, <prime>, <class> ) . . . . . . 
## 
## computes the natural homomorphism on the <class> p-quotient of the 
## invariant <LpGroup>, if the latter has already some quotient system
## stored as attribute.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an invariant LpGroup with quotient system, a prime number, and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasJenningsQuotientSystems,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  local Q,    # current quotient system
        H,    # the nilpotent quotient of <G>
        i,    # loop variable
        weights, 
        time,	# runtime
        j;    # nilpotency class

  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;

  # check if there's already a quotient system w.r.t. this prime number
  i := Position( JenningsQuotientSystems( G )[1], prime );
  if i = fail then
    TryNextMethod();
  fi;

  # known quotient system of <G>
  Q := JenningsQuotientSystems( G )[2][i];
 
  # p-class of the quotient system
  j := Q.Class;
 
  if c = j then  
    # requested this quotient system
    return( Q.Epimorphism );
  elif c<j then 
    # the quotient system is already there
    return( SmallerJenningsQuotientSystem( Q, c ).Epimorphism );
  fi;

  # check if there's already a largest p-quotient
  if HasLargestJenningsQuotients(G) then
    i := Position( LargestJenningsQuotients(G)[1], prime );
    if i <> fail then 
      return( LargestJenningsQuotients(G)[2][i].Epimorphism );
    fi;
  fi;

  # extend the largest known quotient system
  for i in [j+1..c] do
#   weights := ShallowCopy( Q.Weights );

    # extend the quotient system of G/\phi_i to G/\phi_{i+1}
    time := Runtime();
    Q := ExtendJenningsQuotientSystem( Q );
    Info( InfoLPRES, 2, "Runtime for this step ", StringTime(Runtime()-time) );

    # if we couldn't extend the quotient system any more, we're finished
#   if weights = Q.Weights then 
#     LPRES_StoreLargestJenningsQuotient( G, prime, Q );
#     Info(InfoLPRES,1,"The group has a maximal p-quotient of p-class ", Maximum(Q.Weights) );
#     break;
#   fi;
  od;

  # store this quotient system as an attribute of the group G
  LPRES_StoreJenningsQuotientSystems( G, prime, Q );
  return( Q.Epimorphism );
  end );

############################################################################
##
#M  EpimorphismJenningsQuotient( <LpGroup>, <prime>, <class> ) . . . . . .
## 
## computes the natural homomorphism on the <class> p-quotient of  
## <LpGroup>.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an (arbitrary) LpGroup, a prime number, and a positive integer",
  true,
  [ IsLpGroup,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  local Grp, epi, pi, H, N, hom;

  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;

  # work with the underlying invariant LpGroup which maps onto G
  Grp := UnderlyingInvariantLPresentation( G );

  if not HasIsInvariantLPresentation( Grp ) then
    SetIsInvariantLPresentation( Grp, true );
  fi;

  # natural homomorphism onto the p-quotient
  epi := EpimorphismJenningsQuotient( Grp, prime, c );

  # the p-quotient
  H := Range( epi );

  # natural homomorphisms from the free group onto the LpGroup
  pi := GroupHomomorphismByImages( FreeGroupOfLpGroup( Grp ), Grp,
                                   GeneratorsOfGroup( FreeGroupOfLpGroup( Grp ) ),
                                   GeneratorsOfGroup( Grp ) );
  
  # normal subgroup generated by the images of the fixed relations
  N := NormalClosure( H, Subgroup( H, List( FixedRelatorsOfLpGroup( G ), x -> ( x^pi ) ^ epi ) ) );

  hom := NaturalHomomorphismByNormalSubgroup( H, N );

  # return the factor group
  return( GroupHomomorphismByImagesNC( G, Range( hom ),
                                       GeneratorsOfGroup( G ), 
                                       List( GeneratorsOfGroup( G ), x -> ( ( UnderlyingElement( x )^pi ) ^ epi ) ^ hom ) ) );
  end);

############################################################################
##
#M  JenningsQuotient ( <LpGroup>, <prime>, <class> ) . . . . . . . . . . .
##
## computes the <class> p-quotient of <LpGroup>.
##
InstallOtherMethod( JenningsQuotient,
  "For an LpGroup, a prime number, and a positive integer",
  true,
  [ IsLpGroup,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  return( Range( EpimorphismJenningsQuotient( G, prime, c ) ) );
  end);

############################################################################
##
#M  JenningsQuotient( <FpGroup>, <prime>, <class> )
## 
## computes the <class> p-quotient of <FpGroup>.
##
InstallOtherMethod( JenningsQuotient,
  "For an FpGroup, a prime number, and a positive integer (using the LPRES-package)", true,
  [ IsFpGroup, IsPosInt, IsPosInt ], -1, # give priority to ANUPQ package
  function( G, prime, c )

  if not IsPrime( prime ) then 
    Error( "<prime> must be a prime number" );
  fi;

  return( Range( EpimorphismJenningsQuotient( G, prime, c ) ) );
  end);

############################################################################
##
#M  JenningsQuotient( <PcpGroup>, <prime>, <class> )
## 
## computes the <class> p-quotient of <PcpGroup>.
##
InstallOtherMethod( JenningsQuotient,
  "For a PcpGroup, a prime number, and a positive integer (using the LPRES-package)", true,
  [ IsPcpGroup, IsPosInt, IsPosInt ], -1, # give priority to ANUPQ package
  function( G, prime, c )
  local iso;

  if not IsPrime( prime ) then 
    Error( "<prime> must be a prime number" );
  fi;

  iso := IsomorphismFpGroup( G );
  return( Range( EpimorphismJenningsQuotient( Range( iso ), prime, c ) ) );
  end);

############################################################################
##
#M  EpimorphismJenningsQuotient( <FpGroup>, <prime>, <class> )
## 
## computes the natural homomorphism on the <class> p-quotient of  
## <FpGroup>.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an FpGroup, a prime number, and a positive integer (using the LPRES-package)",
  true,
  [ IsFpGroup,
    IsPosInt,
    IsPosInt ], -1,                       # give priority to ANUPQ package
  function( G, prime, c )
  local iso, 	# isomorphism from FpGroup to LpGroup
        mapi,	# MappingGeneratorsImages of <iso>
        epi;	# epimorphism from LpGroup onto its nilpotent quotient

  if not IsPrime( prime ) then 
    Error( "<prime> must be a prime number" );
  fi;

  iso  := IsomorphismLpGroup( G );
  mapi := MappingGeneratorsImages( iso );
  epi  := EpimorphismJenningsQuotient( Range( iso ), prime, c );

  return( GroupHomomorphismByImagesNC( G,
                                       Range( epi ),
                                       mapi[1],
                                       List( mapi[1], x -> Image( epi, Image( iso, x ) ) ) ) );
  end);

############################################################################
##
#F  SmallerJenningsQuotientSystem ( <Q>, <int> )
## 
## computes a quotient system for G/phi_i(G) if a nilpotent 
## quotient system for G/phi_j(G), i<j, is known.
##
InstallGlobalFunction( SmallerJenningsQuotientSystem,
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
             Weights := Filtered( Q.Weights, x -> x<=c ),
             Class := c );
  
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
  SetJenningsClass( H, QS.Class );
  SetJenningsSeries( H, LPRES_JenningsSeries( QS ) );

  QS.Epimorphism:=GroupHomomorphismByImagesNC( QS.Lpres, H, GeneratorsOfGroup(QS.Lpres), imgs);
 
  return( QS );
  end);
