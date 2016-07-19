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
InstallGlobalFunction( LPRES_StoreLargestTorsionFreeNilpotentQuotient,
  function( G, Q ) 
  if HasLargestTorsionFreeNilpotentQuotient( G ) then 
    ResetFilterObj( G, LargestTorsionFreeNilpotentQuotient );
  fi;
  SetLargestTorsionFreeNilpotentQuotient( G, PcpGroupByCollectorNC( Q.Pccol ) );
  end );

############################################################################
##
#F  internal function
##
## stores the quotient system as an attribute of the <LpGroup>
##
InstallGlobalFunction( LPRES_StoreTorsionFreeNilpotentQuotientSystem,
  function( G, Q ) 
  # store the quotient systems
  if HasTorsionFreeNilpotentQuotientSystem( G ) then
    if Maximum( TorsionFreeNilpotentQuotientSystem( G ).Weights ) >= Maximum( Q.Weights ) then 
      return;
    fi;

    ResetFilterObj( G, TorsionFreeNilpotentQuotientSystem );
  fi;
  SetTorsionFreeNilpotentQuotientSystem( G, Q );
  end );

############################################################################
##
#F internal function
## 
## to create an object for LPRES_QuotientAlgorithmEpimorphism
## 
LPRES_GetRationalObject := function( ) 
  return( rec( initQS := InitRationalQuotientSystem,
               storeLargest := function( g, q ) LPRES_StoreLargestTorsionFreeNilpotentQuotient( g, q ); end,
               storeQS := function( g, q ) LPRES_StoreTorsionFreeNilpotentQuotientSystem( g, q ); end,
               extendQS := ExtendRationalQuotientSystem,
               hasQS := HasTorsionFreeNilpotentQuotientSystem,
               getQS := TorsionFreeNilpotentQuotientSystem,
               smallerQS := LPRES_SmallerTorsionFreeNilpotentQuotientSystem,
               hasLargest := HasLargestTorsionFreeNilpotentQuotient,
               getLargest := LargestTorsionFreeNilpotentQuotient
               ) );
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
  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetRationalObject( ) ) );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient ( <LpGroup>, <prime>, <class> ) . . . . . . 
## 
## computes the natural homomorphism on the <class> torsion-free nilpotent 
## quotient of the invariant <LpGroup>, if the latter has already some
## quotient system stored as attribute.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an invariant LpGroup with quotient system and a positive integer",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation and HasTorsionFreeNilpotentQuotientSystem,
    IsPosInt ], 0,
  function( G, c )
  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetRationalObject( ) ) );
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
  "For an invariant LpGroup and a prime number",
  true,
  [ IsLpGroup and HasIsInvariantLPresentation and IsInvariantLPresentation ], 0,
  function( G )
  return( LPRES_QuotientAlgorithmEpimorphism( G, LPRES_GetRationalObject( ) ) );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient ( <LpGroup> ) . . for invariant LpGroups
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
  return( LPRES_QuotientAlgorithmEpimorphism( G, LPRES_GetRationalObject( ) ) );
  end );

############################################################################
##
#M  EpimorphismTorsionFreeNilpotentQuotient( <LpGroup>, <class> ) . . . . . .
## 
## computes the natural homomorphism onto the <class> torsion-free nilpotent
## quotient of <LpGroup>.
##
InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
  "For an (arbitrary) LpGroup and a positive integer",
  true,
  [ IsGroup,
    IsPosInt ], 0,
  function( G, c )
  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetRationalObject( ) ) );
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
  [ IsGroup,
    IsPosInt ], 0,
  function( G, c )
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G, c ) ) );
  end);

#############################################################################
###
##M  TorsionFreeNilpotentQuotient( <LpGroup> ) . . . . . . 
### 
### attempts to compute the largest torsion-free nilpotent quotient of <LpGroup>.
### This method terminates if and only if <LpGroup> admits such a largest 
### quotient.
###
InstallOtherMethod( TorsionFreeNilpotentQuotient,
  "For an LpGroup",
  true,
  [ IsGroup ], 0,
  function( G )
  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G ) ) );
  end);

#############################################################################
###
##M  TorsionFreeNilpotentQuotient( <FpGroup>, <class> )
### 
### computes the <class> torsion-free nilpotent quotient of <FpGroup>.
###
#InstallOtherMethod( TorsionFreeNilpotentQuotient,
#  "For an FpGroup and a positive integer (using the LPRES-package)", true,
#  [ IsFpGroup, IsPosInt ], -1, # give priority to ANUPQ package
#  function( G, c )
#  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G, c ) ) );
#  end);
#
#############################################################################
###
##M  TorsionFreeNilpotentQuotient( <PcpGroup>, <class> )
### 
### computes the <class> torsion-free nilpotent quotient of <PcpGroup>.
###
#InstallOtherMethod( TorsionFreeNilpotentQuotient,
#  "For a PcpGroup and a positive integer (using the LPRES-package)", true,
#  [ IsPcpGroup, IsPosInt ], -1, # give priority to ANUPQ package
#  function( G, c )
#  local iso;
#  iso := IsomorphismFpGroup( G );
#  return( Range( EpimorphismTorsionFreeNilpotentQuotient( Range( iso ), c ) ) );
#  end);
#
#############################################################################
###
##M  TorsionFreeNilpotentQuotient( <FpGroup> )
###
### attempts to compute the largest torsion-free nilpotent quotient of <FpGroup>.
### This method terminates if and only if <FpGroup> admits such a largest 
### quotient.
###
#InstallOtherMethod( TorsionFreeNilpotentQuotient,
#  "For an FpGroup and a prime number (using the LPRES-package)", true,
#  [ IsFpGroup ], -1,           # give priority to ANUPQ package
#  function( G )
#  return( Range( EpimorphismTorsionFreeNilpotentQuotient( G ) ) );
#  end );
#
#############################################################################
###
##M  TorsionFreeNilpotentQuotient( <PcpGroup> )
###
### attempts to compute the largest torsion-free nilpotent quotient of <PcPGroup>.
### This method terminates if and only if <FpGroup> admits such largest 
### quotient.
###
#InstallOtherMethod( TorsionFreeNilpotentQuotient,
#  "For a PcpGroup (using the LPRES-package)", true,
#  [ IsPcpGroup ], -1,           # give priority to ANUPQ package
#  function( G )
#  local iso;
#  iso := IsomorphismFpGroup( G );
#  return( Range( EpimorphismTorsionFreeNilpotentQuotient( Range(iso) ) ) );
#  end );
#
#############################################################################
###
##M  EpimorphismTorsionFreeNilpotentQuotient( <FpGroup>, <class> )
### 
### computes the natural homomorphism onto the <class> torsion-free nilpotent
### quotient of <FpGroup>.
###
#InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
#  "For an FpGroup and a positive integer (using the LPRES-package)",
#  true,
#  [ IsFpGroup,
#    IsPosInt ], -1,                       # give priority to ANUPQ package
#  function( G, c )
#  return( LPRES_QuotientAlgorithmEpimorphism( G, c, LPRES_GetRationalObject( ) ) );
#  end);
#
#############################################################################
###
##M  EpimorphismTorsionFreeNilpotentQuotient( <FpGroup> )
### 
### attempts to compute the natural homomorphism onto the largest torsion-free
### nilpotent quotient of <FpGroup>.
### This method only terminates if <FpGroup> has such a largest quotient. 
###
#InstallOtherMethod( EpimorphismTorsionFreeNilpotentQuotient,
#  "For an FpGroup (using the LPRES-package)",
#  true,
#  [ IsFpGroup ], -1,                        # give priority to ANUPQ-package
#  function( G )
#  return( LPRES_QuotientAlgorithmEpimorphism( G, LPRES_GetRationalObject( ) ) );
#  end);

############################################################################
##
#F  LPRES_SmallerTorsionFreeNilpotentQuotientSystem ( <Q>, <int> )
## 
## computes a quotient system for G/G_i if a nilpotent 
## quotient system for G/G_j, i<j, is known.
##
InstallGlobalFunction( LPRES_SmallerTorsionFreeNilpotentQuotientSystem,
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
