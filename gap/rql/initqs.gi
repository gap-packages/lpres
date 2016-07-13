############################################################################
##
#W gap/rql/initqs.gi			LPRES				René Hartung
##

############################################################################
## 
#F internal function
## 
# the epimorphism into the new presentation
LPRES_EpimorphismFromQS := function( Q ) 
  local imgs,i;

  imgs := [];
  for i in [ 1 .. Length(Q.Imgs) ] do
    if IsInt( Q.Imgs[i] ) then
      Add( imgs, PcpElementByGenExpList( Q.Pccol, [Q.Imgs[i],1] ) );
    else
      Add( imgs, PcpElementByGenExpList( Q.Pccol, Q.Imgs[i] ) );
    fi;
  od;
  return( GroupHomomorphismByImagesNC( Q.Lpres,
                                       PcpGroupByCollector( Q.Pccol ),
                                       FreeGeneratorsOfLpGroup( Q.Lpres ),
                                       imgs ) );
  end;

############################################################################
##
#F  internal function
##
LPRES_AdjustIntegralObject := function( obj, HNF, Gens, QSPccol, QPccol )
  local b,i,j,ev,ev1;

  if IsEmpty( HNF.mat ) then 
    return( obj );
  fi;

  # first tail in the exponent vector
  b := QSPccol![ PC_NUMBER_OF_GENERATORS ] - Length( HNF.mat[1] ) + 1;

  # exponent of the object in the old collector
  ev1 := ExponentsByObj( QSPccol, obj );

  # tail vector (i.e. element in the module)
  ev := ev1{ [b.. QSPccol![ PC_NUMBER_OF_GENERATORS ] ] };

  # group element on top
  ev1 := ev1{[1..b-1]};

  # reduce the element w.r.t. the basis
  for i in [1..Length(ev)] do
    if not IsZero( ev[i] ) then 
      j := Position( HNF.Heads, i );
      if j <> fail then 
        if IsInt( ev[i] / HNF.mat[j][i] ) then 
          ev := ev - ev[i] / HNF.mat[j][i] * HNF.mat[j];
        elif ev[i] > 0 then 
          ev := ev - ( QuoInt( ev[i], HNF.mat[j][i] ) ) * HNF.mat[j]; 
        elif ev[i] < 0 then 
          ev := ev - ( QuoInt( ev[i], HNF.mat[j][i] ) - 1 ) * HNF.mat[j];
        fi;
      fi;
    fi;
  od;

  # append the reduced tail vector to the group element
  Append( ev1, List( ev{Gens}, Int ) );

  return( ObjByExponents( QPccol, ev1 ) );
end;

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
        Q,		# new quotient system
        n,		# number of generators of L
        ev,evn,	# exponent vectors for spinning algorithm
        rel,		# loop variable for (iterated) relators 
        map,		# loop variable for endomorphisms 
        i,j,k,	# loop variables
        HNF,		# Hermite normal form of the relations
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
  
  # Hermite normal form of the exponent vectors
  HNF := rec( mat := [], Heads := [] );

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
      Add( HNF.mat, ev );
    fi;
  od;

  # set up the basis
  HNF.mat := HermiteNormalFormIntegerMat( HNF.mat );
  HNF.mat := Filtered( HNF.mat, x -> not IsZero( x ) );
  HNF.Heads := List( HNF.mat, PositionNonZero );

  # map the endomorphisms to endomorphisms of the elementary abelian group
  endos:=[];
  for map in EndomorphismsOfLpGroup(G) do
    mat := NullMat( n, n );
    obj := List( GeneratorsOfGroup(G), x->ExtRepOfObj(UnderlyingElement(x)^map) );

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
        if LPRES_AddRow( HNF, evn ) then 
          Add( stack, evn );
        fi;
      od;
    fi;
  od;
  
  # add the (fixed) relators
  for rel in FixedRelatorsOfLpGroup(G) do
    ev := ListWithIdenticalEntries( n, 0 );
    obj := ExtRepOfObj( rel );
    for i in [1,3..Length(obj)-1] do 
      ev[obj[i]] := ev[obj[i]] + obj[i+1];
    od;
    LPRES_AddRow(HNF,ev);
  od;

  # smith normal form to extract the torsion of G/G'
  # ( SNF.rowtrans x HNF.mat x SNF.coltrans = SNF.normal )
  SNF := SmithNormalFormIntegerMatTransforms( HNF.mat );

  # generators w/ finite order
  Torsion := Filtered( [1..Length(SNF.normal)], x-> SNF.normal[x][x] > 0 );

  # remove the torsion in this section
  if not IsEmpty( Torsion ) then 
    # determine the inverse of the coltrans matrix 
    # the first |Torsion| rows are the generators of the torsion subgroup
    mat := SNF.coltrans ^ -1;
   
    # add the torsion generators to the HNF:
    Append( HNF.mat, mat{[1..Length(Torsion)]} );
    HNF.mat := HermiteNormalFormIntegerMat( HNF.mat );
    HNF.mat := Filtered( HNF.mat, x -> not IsZero( x ) );
    HNF.Heads := List( HNF.mat, PositionNonZero );
  fi;

  # we have factored out the torsion of G/G' and continue as in NQL:
  # generators with power relations:
  Gens := HNF.Heads{ Filtered( [1..Length(HNF.Heads)], x->HNF.mat[x][HNF.Heads[x]]<>1) };
  
  # infinite generators;
  Append( Gens, Filtered( [1..n], x -> not x in HNF.Heads ) );
  Sort( Gens );

  # set up the quotient system
  Q := rec( Lpres := G,
            Pccol := FromTheLeftCollector( Length( Gens ) ),
            Imgs := [],
            Weights := ListWithIdenticalEntries( Length( Gens ), 1 ),
            Definitions := [ ]
            );

# set up the collector
  ftl := FromTheLeftCollector( n );
  for i in [ 1 .. Length( Gens ) ] do
    j := Position( HNF.Heads, Gens[i] );
    if j <> fail then 
      # obj of the left hand side of the power relation
      SetRelativeOrder( Q.Pccol, i, HNF.mat[ j ][ Gens[i] ] );

      obj := LPRES_AdjustIntegralObject( [ Gens[i], HNF.mat[ j ][ Gens[i] ] ], HNF, Gens, ftl, Q.Pccol );
      SetPower( Q.Pccol, i, obj );
    fi;
  od;
  UpdatePolycyclicCollector( Q.Pccol );

  # set up the images
  for i in [1..n] do
    if i in Gens then 
      k:= i - Length( Filtered([ 1..Length(HNF.Heads) ], x -> HNF.mat[x][HNF.Heads[x]] = 1 and HNF.Heads[x]<i));
      Q.Imgs[i] := k;
    else
      obj := LPRES_AdjustIntegralObject( [ i, 1 ], HNF, Gens, ftl, Q.Pccol );
      Q.Imgs[i] := obj;
    fi;
  od;

  # set up the definitions
  Q.Definitions := Filtered( [1..Length(Q.Imgs)], x -> not IsList(Q.Imgs[x]) );

  # set up the epimorphism from the LpGroup onto the PcpGroup
  Q.Epimorphism := LPRES_EpimorphismFromQS( Q );
                                         
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
