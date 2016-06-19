############################################################################
##
#W initqs.gi			LPRES				René Hartung
##

############################################################################
##
#M  InitPQuotientSystem ( <LpGroup> )
##
## computes a weighted nilpotent quotient system for the abelian quotient 
## of <LpGroup>.
##
InstallMethod( InitPQuotientSystem,
  "for an L-presented group and a prime number", 
  true,
  [ IsLpGroup, IsPosInt ], 0,
  function( G, prime )
  local ftl,		# FromTheLeftCollector for G/G'G^p
          A,		# power relations from Hermite normal form
          Q,		# new quotient system
          n,		# number of generators of L
          ev,evn,	# exponent vectors for spinning algorithm
          rel,		# loop variable for (iterated) relators 
          map,		# loop variable for endomorphisms 
          i,j,k,	# loop variables
          Basis,		# Hermite normal form of the relations
          stack,	# stack for the spinning algorithm
          endos,	# endomorphisms as matrices 
          obj, mat,	# loop variables to determine the matrices
	         Gens,		# position of new gens in the HNF
	         Imgs,		# loop variable to build the endomorphism
          F;     # finite field

  if not IsPrime( prime ) then
    Error( "<prime> needs to be a prime number" );
  fi;

  F := GF( prime );

  # number of generators
  n := Length( GeneratorsOfGroup(G) );
  
  # determine the iterated relators
  stack:=[];
  Basis := rec( mat := [], Heads := [] );
  for rel in IteratedRelatorsOfLpGroup( G ) do 
    ev := ListWithIdenticalEntries( n, Zero( F ) );
    obj := ExtRepOfObj( rel );
    for i in [1,3..Length(obj)-1] do 
      ev[obj[i]] := ev[obj[i]] + obj[i+1] * One( F );
    od;

#   Print( List( ev, Int ), "\n" );
    if not IsZero( ev ) then 
      Add( stack, ShallowCopy(ev) );
      Add( Basis.mat, ev );
    fi;
  od;

  # set up the basis
  TriangulizeMat( Basis.mat );
  Basis.mat := Filtered( Basis.mat, x -> not IsZero( x ) );
  Basis.Heads := List( Basis.mat, PositionNonZero );

  # determine the endomorphisms
  endos:=[];
  for map in EndomorphismsOfLpGroup(G) do
    mat := NullMat( n, n, F );
    obj := List( GeneratorsOfGroup(G),
                 x->ExtRepOfObj(UnderlyingElement(x)^map) );

    for j in [1..n] do 
      for k in [1,3..Length(obj[j])-1] do 
        mat[j][obj[j][k]] := mat[j][obj[j][k]] + obj[j][k+1] * One( F );
      od;
    od;
    Add(endos,mat);
#   Print( List( mat, x->List( x, Int) ), "\n" );
  od;
  
  # spinning algorithm
  while not Length(stack)=0 do
    ev:=stack[1];
    Remove( stack, 1);
    if not IsZero(ev) then 
      for i in [1..Length(endos)] do 
        evn := ev * endos[i];
        if LPRES_AddPRow( Basis, evn ) then 
          Add( stack, evn );
          Print( List( evn, Int ), "\n" );
        fi;
      od;
    fi;
  od;
  
  # add the (fixed) relators
  for rel in FixedRelatorsOfLpGroup(G) do
    ev := ListWithIdenticalEntries( n, Zero( F ) );
    obj := ExtRepOfObj( rel );
    for i in [1,3..Length(obj)-1] do 
      ev[obj[i]] := ev[obj[i]] + obj[i+1] * One( F );
    od;
#   Print( List( ev, Int ), "\n" );
    LPRES_AddPRow(Basis,ev);
  od;
  
  # surviving pseudo generators
  Gens := Filtered( [1..n], x -> not x in Basis.Heads );
  
  # if the p-Frattini quotient is trivial
  if Length( Gens ) = 0 then 
    Q := rec( Lpres       := G, 
              Pccol       := FromTheLeftCollector( 0 ),
              Imgs        := ListWithIdenticalEntries( n, [] ),
              Weights     := [],
              Definitions := [] );

    UpdatePolycyclicCollector( Q.Pccol );

    Imgs := List( Q.Imgs, x -> PcpElementByGenExpList( Q.Pccol, x ) );

    Q.Epimorphism := GroupHomomorphismByImagesNC( Q.Lpres, 
                                                  PcpGroupByCollectorNC( Q.Pccol), 
                                                  GeneratorsOfGroup( Q.Lpres),
                                                  Imgs );
                                        
    return( Q );
  fi;
  
  # build quotient system
  Q := rec( Lpres := G,
            Pccol := FromTheLeftCollector( Length(Gens) ),
            Prime := prime,
            Imgs  := [],
            Weights := ListWithIdenticalEntries( Length(Gens), 1 )
          );
  
  # the new collector
  for i in [1..Length(Gens)] do 
    SetRelativeOrder( Q.Pccol, i, prime );
  od;
  UpdatePolycyclicCollector(Q.Pccol);

  # the natural homomorphism onto the new presentation
  for i in [1..n] do 
    if i in Gens then 
      k := i - Length( Filtered( Basis.Heads, x -> x <i ) );
      Add( Q.Imgs, k );
    else
      ev := -Basis.mat[ Position( Basis.Heads, i ) ]{Gens};
      ev := List( ev, Int );
      Add( Q.Imgs, ObjByExponents( Q.Pccol, ev ) );
    fi;
  od;
  
  # the epimorphism into the new presentation
  Imgs:=[];
  for i in [1..Length(Q.Imgs)] do
    if IsInt(Q.Imgs[i]) then 
      Add( Imgs, PcpElementByGenExpList( Q.Pccol, [ Q.Imgs[i], 1 ] ) );
    else
      Add( Imgs, PcpElementByGenExpList( Q.Pccol, Q.Imgs[i] ) );
    fi;
  od;
  Q.Epimorphism := GroupHomomorphismByImagesNC( Q.Lpres,
                                                PcpGroupByCollectorNC(Q.Pccol),
                                                GeneratorsOfGroup(Q.Lpres),
                                                Imgs);
  
  Q.Definitions := Filtered( [1..Length(Q.Imgs)],
                             x->not IsList(Q.Imgs[x]));
  
  return(Q);
  end);

############################################################################
##
#F  LPRES_AddPRow ( <mat> , <evec> )
##
## adds the row <evec> to the Hermite normal form <mat> and returns
## whether <mat> has changed.
##
InstallGlobalFunction( LPRES_AddPRow,
  function( Basis, ev )
  local i, val, l;

  if Size( Basis.mat ) = 0 and not IsZero( ev ) then 
    Add( Basis.mat, ev );
    Add( Basis.Heads, PositionNonZero( ev ) );
    return( true );
  fi;

  # reduce a vector with the Basis
  for i in [1..Length(ev)] do
   if not IsZero( ev[i] ) then 
     l := Position( Basis.Heads, i );
     if l <> fail then 
       ev := ev - ev[i]/Basis.mat[l][i]*Basis.mat[l];
     else
       break;
     fi;
   fi;
  od;
   
  if IsZero( ev ) then 
    return( false );
  fi;
  Add( Basis.mat, ev );

  # triangulize matrix (destructive)
  TriangulizeMat( Basis.mat );
  Basis.mat := Filtered( Basis.mat, x -> not IsZero( x ) );
  Basis.Heads := List( Basis.mat, PositionNonZero );
   
  return( true );
  end);
