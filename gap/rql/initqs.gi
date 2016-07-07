############################################################################
##
#W gap/rql/initqs.gi			LPRES				René Hartung
##

############################################################################
##
#M  InitRationalQuotientSystem ( <LpGroup> )
##
## computes a weighted nilpotent quotient system for the torsion free abelian
## quotient of <LpGroup>.
##
InstallMethod( InitRationalQuotientSystem,
  "For an L-presented group", true,
  [ IsLpGroup ], 0,
  function( G )
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
        SNF,
        Torsion,
        H,
        imgs; # TMP

  # number of generators
  n := Length( GeneratorsOfGroup(G) );
  
  Basis := rec( mat := [], Heads := [] );

  # exponent vectors of the iterated relators 
  stack:=[];
  for rel in IteratedRelatorsOfLpGroup( G ) do 
    ev := ListWithIdenticalEntries( n, 0 );
    obj := ExtRepOfObj( rel );
    for i in [1,3..Length(obj)-1] do 
      ev[obj[i]] := ev[obj[i]] + obj[i+1];
    od;

    if not IsZero( ev ) then 
      Add( stack, ShallowCopy( ev ) );
      Add( Basis.mat, ev );
    fi;
  od;

  # set up the basis
  Basis.mat := HermiteNormalFormIntegerMat( Basis.mat );
  Basis.mat := Filtered( Basis.mat, x -> not IsZero( x ) );
  Basis.Heads := List( Basis.mat, PositionNonZero );

  # map the endomorphisms to endomorphisms of the elementary abelian group
  endos:=[];
  for map in EndomorphismsOfLpGroup(G) do
    mat := NullMat( n, n );
    obj := List( GeneratorsOfGroup(G),
                 x->ExtRepOfObj(UnderlyingElement(x)^map) );

    for j in [1..n] do 
      for k in [1,3..Length(obj[j])-1] do 
        mat[j][obj[j][k]] := mat[j][obj[j][k]] + obj[j][k+1];
      od;
    od;
    Add(endos,mat);
  od;
  
  # spinning algorithm
  while not IsEmpty(stack) do
    ev := Remove( stack, 1);
    if not IsZero(ev) then 
      for i in [1..Length(endos)] do 
        evn := ev * endos[i];
        if LPRES_AddRow( Basis, evn ) then 
          Add( stack, evn );
        fi;
      od;
    fi;
  od;
  
  # add the (fixed) relators
  for rel in FixedRelatorsOfLpGroup(G) do
    ev := ListWithIdenticalEntries( n );
    obj := ExtRepOfObj( rel );
    for i in [1,3..Length(obj)-1] do 
      ev[obj[i]] := ev[obj[i]] + obj[i+1];
    od;
    LPRES_AddRow(Basis,ev);
  od;

  # smith normal form to extract the torsion of G/G'
  # ( SNF.rowtrans x Basis.mat x SNF.coltrans = SNF.normal )
  SNF := SmithNormalFormIntegerMatTransforms( Basis.mat );

  # generators w/ finite order
  Torsion := Filtered( [1..Length(SNF.normal)], x-> SNF.normal[x][x] > 0 );

  # determine the inverse of the coltrans matrix 
  # the first |Torsion| rows are the generators of the torsion subgroup
  mat := SNF.coltrans ^ -1;
 
  # add the torsion generators to the Basis:
  for i in [1..Length(Torsion)] do
    LPRES_AddRow( Basis, mat[i] );
  od;

Display( Basis );

  # we have now killed the torsion of G/G' and continue as in NQL:
  
  # generators with power relations:
  Gens := Basis.Heads{ Filtered( [1..Length(Basis.Heads)], x->Basis.mat[x][Basis.Heads[x]]<>1) };
  
  # infinite generators;
  Append( Gens, Filtered( [1..n], x -> not x in Basis.Heads ) );
  Sort( Gens );

  # adjust the HNF 
  A := LPRES_PowerRelationsOfHNF( Basis );

  # set up the quotient system
  Q := rec( Lpres := G,
            Pccol := FromTheLeftCollector( Length( Gens ) ),
            Imgs := [],
            Weights := ListWithIdenticalEntries( n - Length( Torsion ), 1 ),
            Definitions := [ ]
            );
  # set up the collector
  for i in [ 1 .. Length( Gens ) ] do
    k := Position( Basis.Heads, Gens[i] );
    if k <> fail then
      SetRelativeOrder( Q.Pccol, i, A[k][Gens[i]] );
      ev := ShallowCopy( A[k]{ Gens } );
      ev[i] := 0;
      if not IsZero( ev ) then
        SetPower( Q.Pccol, i, ObjByExponents( Q.Pccol, -ev ) );
      fi;
    fi;
  od;
  UpdatePolycyclicCollector( Q.Pccol );

# imgs := [];
# for i in [1..n] do
#   Q.Imgs[i] := ObjByExponents( Q.Pccol, SNF.coltrans[i]{[Length(Torsion)+1..n]} );
#   imgs[i] := PcpElementByGenExpList( Q.Pccol, Q.Imgs[i] );
# od;
  H := PcpGroupByCollector( Q.Pccol );

  # the natural homomorphism onto the new presentation
  for i in [ 1 .. n ] do
    if i in Gens then
      k := i - Length( Filtered( [1..Length(Basis.Heads)], x-> Basis.mat[x][Basis.Heads[x]] = 1 and Basis.Heads[x]<i ) );
      Add( Q.Imgs, k );
    else
      Add( Q.Imgs, ObjByExponents( Q.Pccol, -A[Position(Basis.Heads,i)]{Gens} ) );
    fi;
  od;
  
  # the epimorphism into the new presentation
  Imgs := [];
  for i in [ 1 .. Length(Q.Imgs) ] do
    if IsInt( Q.Imgs[i] ) then
      Add( Imgs, PcpElementByGenExpList( Q.Pccol, [Q.Imgs[i],1] ) );
    else
      Add( Imgs, PcpElementByGenExpList( Q.Pccol, Q.Imgs[i] ) );
    fi;
  od;
  Q.Definitions := Filtered( [1..Length(Q.Imgs)], x -> not IsList(Q.Imgs[x]) );

# TODO amend later on to a hom from the LpGroup not from the free group
# Q.Epimorphism := GroupHomomorphismByImagesNC( Q.Lpres, PcpGroupByCollectorNC(Q.Pccol), GeneratorsOfGroup(Q.Lpres),Imgs);
  Q.Epimorphism := GroupHomomorphismByImagesNC( FreeGroupOfLpGroup( Q.Lpres ), H, FreeGeneratorsOfLpGroup( Q.Lpres ), Imgs );
                                         
  return( Q );
  end);

############################################################################
##
#M  InitRationalQuotientSystem ( <FpGroup> )
##
InstallOtherMethod( InitRationalQuotientSystem,
  "For an FpGroup", true,
  [ IsFpGroup ], 0, 
  function( G )
  return( InitRationalQuotientSystem( Range( IsomorphismLpGroup( G ) ) ) );
  end );

############################################################################
##
#F  internal function
##
LPRES_AdjustIntegralObject := function( obj, b, Basis, Gens, QSPccol, QPccol )
  local i,j,ev,ev1;

  # exponent of the object in the old collector
  ev1 := ExponentsByObj( QSPccol, obj );

  # tail vector (i.e. element in the module)
  ev := ev1{ [b.. QS.Pccol![ PC_NUMBER_OF_GENERATORS ] ] };

  # group element on top
  ev1 := ev1{[1..b-1]};

  # reduce the element w.r.t. the basis
  for i in [1..Length(ev)] do
    if not IsZero( ev[i] ) then 
      j := Position( Basis.Heads, i );
      if j <> fail then 
        ev := ev - ev[i] / Basis.mat[j][i] * Basis.mat[j];
      fi;
    fi;
  od;

  # append the reduced tail vector to the group element
  Append( ev1, List( ev{Gens}, Int ) );

  return( ObjByExponents( QPccol, ev1 ) );
end;
