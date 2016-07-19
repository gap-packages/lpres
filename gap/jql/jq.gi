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
InstallGlobalFunction( LPRES_StoreLargestJenningsQuotient,
  function( G, prime, Q ) 
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
  end );

############################################################################
##
#F  internal function
##
## stores the quotient system as an attribute of the <LpGroup>
##
InstallGlobalFunction( LPRES_StoreJenningsQuotientSystems,
  function( G, prime, Q ) 
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
  end );

############################################################################
##
#F internal function
## 
## to create an object for LPRES_QuotientAlgorithmEpimorphism
## 
LPRES_GetJenningsObject := function( prime ) 
  return( rec( initQS := g -> InitPQuotientSystem( g, prime ),
               storeLargest := function( g, q ) LPRES_StoreLargestJenningsQuotient( g, prime, q ); end,
               storeQS := function( g, q ) LPRES_StoreJenningsQuotientSystems( g, prime, q ); end,
               extendQS := ExtendJenningsQuotientSystem,
               hasQS := function( g )
                        return( HasJenningsQuotientSystems( g ) and prime in JenningsQuotientSystems( g )[1] );
                        end,
               getQS := function( g )
                        return( JenningsQuotientSystems( g )[2][ Position( JenningsQuotientSystems( g )[1], prime ) ] );
                        end,
               smallerQS := LPRES_SmallerJenningsQuotientSystem,
               hasLargest := function( g )
                        return( HasLargestJenningsQuotients( g ) and prime in LargestJenningsQuotients( g )[1] );
                        end,
               getLargest := function( g )
                        return( LargestJenningsQuotients( g )[2][ Position( LargestJenningsQuotients( g )[1], prime ) ] );
                        end,
               ) );
  end;

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
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetJenningsObject( prime ) ) );
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
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetJenningsObject( prime ) ) );
  end );

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
#M  EpimorphismJenningsQuotient( <LpGroup>, <prime> ) . . .
## 
## computes the natural homomorphism on the largest Jennings-quotient of the 
## invariant <LpGroup>.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an invariant LpGroup, a prime number, and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation,
    IsPosInt ], 0,
  function( G, prime )
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( LPRES_QuotientAlgorithmEpimorphism( G, LPRES_GetJenningsObject( prime ) ) );
  end );

############################################################################
##
#M  EpimorphismJenningsQuotient ( <LpGroup>, <prime>, <class> ) . . . . . . 
## 
## computes the natural homomorphism on the largest Jennings quotient of the 
## invariant <LpGroup>, if the latter has already some quotient system
## stored as attribute.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an invariant LpGroup with quotient system, a prime number, and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasJenningsQuotientSystems,
    IsPosInt ], 0,
  function( G, prime )
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( LPRES_QuotientAlgorithmEpimorphism( G, LPRES_GetJenningsObject( prime ) ) );
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
  [ IsGroup,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetJenningsObject( prime ) ) );
  end);

############################################################################
##
#M  EpimorphismJenningsQuotient( <LpGroup>, <prime>, <class> ) . . . . . .
## 
## computes the natural homomorphism on the <class> p-quotient of  
## <LpGroup>.
##
InstallOtherMethod( EpimorphismJenningsQuotient,
  "For an (arbitrary) LpGroup and a prime number",
  true,
  [ IsGroup,
    IsPosInt ], 0,
  function( G, prime )
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( LPRES_QuotientAlgorithmEpimorphism( G, LPRES_GetJenningsObject( prime ) ) );
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
  [ IsGroup,
    IsPosInt,
    IsPosInt ], 0,
  function( G, prime, c )
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( Range( EpimorphismJenningsQuotient( G, prime, c ) ) );
  end);

############################################################################
##
#M  JenningsQuotient ( <LpGroup>, <prime> ) . . . . . . . . . . .
##
## computes the largest Jennings-quotient of <LpGroup>.
##
InstallOtherMethod( JenningsQuotient,
  "For an LpGroup and a prime number",
  true,
  [ IsGroup,
    IsPosInt ], 0,
  function( G, prime )
  if not IsPrime( prime ) then
    Error( "<prime> should be a prime number" );
  fi;
  return( Range( EpimorphismJenningsQuotient( G, prime ) ) );
  end);

# ############################################################################
# ##
# #M  JenningsQuotient( <FpGroup>, <prime>, <class> )
# ## 
# ## computes the <class> p-quotient of <FpGroup>.
# ##
# InstallOtherMethod( JenningsQuotient,
#   "For an FpGroup, a prime number, and a positive integer (using the LPRES-package)", true,
#   [ IsFpGroup, IsPosInt, IsPosInt ], -1, # give priority to ANUPQ package
#   function( G, prime, c )
# 
#   if not IsPrime( prime ) then 
#     Error( "<prime> must be a prime number" );
#   fi;
# 
#   return( Range( EpimorphismJenningsQuotient( G, prime, c ) ) );
#   end);
# 
# ############################################################################
# ##
# #M  JenningsQuotient( <PcpGroup>, <prime>, <class> )
# ## 
# ## computes the <class> p-quotient of <PcpGroup>.
# ##
# InstallOtherMethod( JenningsQuotient,
#   "For a PcpGroup, a prime number, and a positive integer (using the LPRES-package)", true,
#   [ IsPcpGroup, IsPosInt, IsPosInt ], -1, # give priority to ANUPQ package
#   function( G, prime, c )
#   local iso;
# 
#   if not IsPrime( prime ) then 
#     Error( "<prime> must be a prime number" );
#   fi;
# 
#   iso := IsomorphismFpGroup( G );
#   return( Range( EpimorphismJenningsQuotient( Range( iso ), prime, c ) ) );
#   end);
# 
# ############################################################################
# ##
# #M  EpimorphismJenningsQuotient( <FpGroup>, <prime>, <class> )
# ## 
# ## computes the natural homomorphism on the <class> p-quotient of  
# ## <FpGroup>.
# ##
# InstallOtherMethod( EpimorphismJenningsQuotient,
#   "For an FpGroup, a prime number, and a positive integer (using the LPRES-package)",
#   true,
#   [ IsFpGroup,
#     IsPosInt,
#     IsPosInt ], -1,                       # give priority to ANUPQ package
#   function( G, prime, c )
#   if not IsPrime( prime ) then 
#     Error( "<prime> must be a prime number" );
#   fi;
#   return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetJenningsObject( prime ) ) );
#   end);

############################################################################
##
#F  LPRES_SmallerJenningsQuotientSystem ( <Q>, <int> )
## 
## computes a quotient system for G/phi_i(G) if a nilpotent 
## quotient system for G/phi_j(G), i<j, is known.
##
InstallGlobalFunction( LPRES_SmallerJenningsQuotientSystem,
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
