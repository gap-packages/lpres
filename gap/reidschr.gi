############################################################################
##
#W  reidschr.gi			The LPRES-package	        René Hartung
##

############################################################################
##
#M PermutationRepresentation
##
############################################################################
InstallMethod( FactorCosetAction,
  "for a group and a subgroup of an LpGroup", true,
  [ IsLpGroup, IsSubgroupLpGroup ], 0,
  function( G, U )
  local F,	# the underlying free group 
	Tab,	# coset table of <U> in <G>
	Sym;

  # initialization
  F := FreeGroupOfLpGroup( G );
  Tab := CosetTableInWholeGroup( U );
  Sym := Group( List( Tab, PermList ) );

  return( GroupHomomorphismByImages( F, Sym, GeneratorsOfGroup( F ), 
          List( Tab{[1,3..Length(Tab)-1]}, PermList ) ) );
  end );

############################################################################
##
## PeriodicityOfSubgroupPres
##
############################################################################
InstallMethod( PeriodicityOfSubgroupPres,
  "for a subgroup of an LpGroup", true,
  [ IsSubgroupLpGroup ], 0,
  function( U )
  local G,      # the parent
        sig,  	# the iterating endomorphism 
 	Tab,	# coset table in whole group
	phi,	# the permutation representation given by <Tab>
	i,j, 	# periodicities
	Maps, 	# collection of homomorphisms
	img,  	# images of the free generators 
	prd, 	# periodicity
	map, 	# a group homomorphism
	u,v;	# subgroups of the symmetric group
  
  G := Parent( U );

  # catch the non-trivial case
  if Length( EndomorphismsOfLpGroup( G ) ) <> 1 then 
    Error("this won't do currently");
  fi;

  # initialization
  sig := EndomorphismsOfLpGroup( G )[1];
  Tab := CosetTableInWholeGroup( U );
  phi := FactorCosetAction( G, U );

  # check for 'reduction' as in [Har10b]
  img := FreeGeneratorsOfLpGroup( G );
  Maps := [ phi ];
  j := 1;;
  prd := 0;;
  repeat 
    img := List( img, x -> Image( sig, x ) );
    j := j+1;

    Add( Maps, GroupHomomorphismByImagesNC( FreeGroupOfLpGroup( G ),
                                SymmetricGroup( IndexInWholeGroup( U ) ), 
                                FreeGeneratorsOfLpGroup( G ), 
                                List( img, x -> Image( phi, x ) ) ) );

    for i in [ 1 .. j-1 ] do 
      u := Image( Maps[i] );
      v := Image( Maps[j] );
      map := GroupHomomorphismByImages( u, v, 
	                      MappingGeneratorsImages( Maps[i] )[2],
	                      MappingGeneratorsImages( Maps[j] )[2] );

      if map <> fail then 
        return( [ i-1, j-1 ] );
      fi;
    od;
  until prd > 0;
  end );

############################################################################
##
#M SchreierData
##
############################################################################
InstallMethod( SchreierData,
  "for a subgroup of an LpGroup", true,
  [ IsSubgroupLpGroup ], 0,
  function( U )
  local G, 	# parent of <U> 
	F, 	# the underlying free group
	Tab,	# coset table of <U> in <G>
        ind, 	# index of <U> in <G>
   	Trans,	# the Schreier transversal
	t, 	# an element of the transversal <Trans>
	Gens, 	# Schreier generators (as a list)
	FGens,	# Schreier generators (subset of <F>)
	g, 	# a Schreier generator
	i,j,x; 	# loop variables

  # initialization
  G   := Parent( U );
  F   := FreeGroupOfLpGroup( G );
  Tab := CosetTableInWholeGroup( U );
  ind := IndexInWholeGroup( U );
  if ind = 1 then Error( "no proper subgroup given" ); fi;

  # compute a Schreier transversal
  Trans := ListWithIdenticalEntries( ind, 0 );
  Trans[1] := One( F );
  repeat
    for i in [ 1 .. ind ] do
      if Trans[i] <> 0 then 
        for x in GeneratorsOfGroup( F ) do 
          t := Trans[i] * x;
          j := TraceCosetTableLpGroup( Tab, x, i );
          if Trans[j] = 0 then Trans[j] := t; fi;
        od;
      fi;
    od;
  until ForAll( Trans, x -> x <> 0 );

  # compute the Schreier generating set
  Gens  := [];
  FGens := [];;
  for i in [ 1 .. ind ] do 
    for x in FreeGeneratorsOfGroup( F ) do 
      j := TraceCosetTableLpGroup( Tab, x, i );
      g := Trans[i] * x * Trans[j]^-1;
      if not IsOne( g ) then 
        Add( Gens, [ i, x ] );
        Add( FGens, g );
      fi;
    od;
  od;

  return( rec( trans   := Trans,
               gens    := Gens,
               fgens   := FGens,
	       FreeGrp := FreeGroup( List( [ 1 .. Length( Gens ) ], 
                                           i -> Concatenation( "x", String( i ) ) ) ) ) );
  end);


############################################################################
##
#M  FreeGeneratorsOfFullPreimage
##
## computes the free generator of the preimage in the free group
##
############################################################################
InstallMethod( FreeGeneratorsOfFullPreimage,
  "for a subgroup of an LpGroup", true, 
  [ IsSubgroupLpGroup ], 0,
  U -> SchreierData( U ).fgens );

############################################################################
##
## Reidemeister rewriting
##
InstallMethod( ReidemeisterRewriting, 
  "for a subgroup of an LpGroup and an element of the free group", true,
  [ IsSubgroupLpGroup, IsElementOfFreeGroup ], 0,
  function( U, w )
  local ext,	# external representation of <w>
	fam, 	# family object of <w>
	Gens, 	# Schreier generators of the subgroup
	FGens, 	# the new generators of the free group
	g, 	# a Schreier generator
	x, 	# the Reidemeister rewriting of <w>
	pos, 	# position in a list
	Tab, 	# coset table of <U> in the whole group
	i,j,t; 	# loop variables


  # intialization
  Gens  := SchreierData( U ).gens;
  FGens := FreeGeneratorsOfGroup( SchreierData( U ).FreeGrp ); 
  Tab   := CosetTableInWholeGroup( U );
  ext   := ShallowCopy( ExtRepOfObj( w ) );
  fam   := FamilyObj( w );

  # compute the Reidemeister rewriting of <w>
  x := One( SchreierData( U ).FreeGrp );

  j := 1; i := 1;
  while i <= Length(ext)-1 do
    if ext[i+1] > 0 then 
      g := [ j, ObjByExtRep( fam, [ext[i],1] )];
      pos := Position( Gens, g );
      if pos <> fail then 	# otherwise, this generator is trivial
        x := x * FGens[pos]; 
      fi;			
      j := TraceCosetTableLpGroup( Tab, ObjByExtRep( fam, [ext[i],1] ), j );
      ext[i+1] := ext[i+1] - 1;
    elif ext[i+1] < 0 then 
      # gamma( t, x^-1 ) = gamma( tx^-1, x ) ^ -1
      t := TraceCosetTableLpGroup( Tab, ObjByExtRep( fam, [ext[i],-1] ), j );
      g := [ t, ObjByExtRep( fam, [ext[i],1] )];
      pos := Position( Gens, g );
      if pos <> fail then 
        x := x * FGens[pos]^-1;
      fi;
      j := TraceCosetTableLpGroup( Tab, ObjByExtRep( fam, [ext[i],-1] ), j );
      ext[i+1] := ext[i+1] + 1;
    else 
      i := i + 2;
    fi;
  od;

  return( x );
  end );

############################################################################
##
#M ReidemeisterMap
##
############################################################################
InstallMethod( ReidemeisterMap,
  "for a subgroup of an LpGroup", true,
  [ IsSubgroupLpGroup ], 0,
  function( U )
  local H,	# subgroup of the free group
	F;	# image of the Reidemeister rewriting

  H := Subgroup( FreeGroupOfWholeGroup( U ), FreeGeneratorsOfFullPreimage( U ) );
  F := SchreierData( U ).FreeGrp;

  return( GroupHomomorphismByImagesNC( H, F, SchreierData( U ).fgens, FreeGeneratorsOfGroup( F ) ) );
  end);

############################################################################
##
#M FreeProductOp
##
############################################################################
InstallOtherMethod( FreeProductOp,
  "for two LpGroups", true,
  [ IsLpGroup, IsLpGroup ], 0,
  function( G, H )
  local Lp, 		# the free product of <G> and <H>
	F,		# the underlying free group
	fgens, 		# the basis of <F>
	FG, FH,		# the underlying free groups of <G> and <H>
	mapG,mapH,	# embeddings of the free groups
	endo,endos,	# endomorphisms of the free group(s)
	irels,frels;	# relations of the <LpGroup>

  # initialization
  FG    := FreeGroupOfLpGroup( G );;
  FH    := FreeGroupOfLpGroup( H );;
  F     := FreeGroup( Rank( FG ) + Rank( FH ) );
  fgens := FreeGeneratorsOfGroup( F );

  # isomorphisms of the underlying free groups
  mapG := GroupHomomorphismByImagesNC( FG, F, FreeGeneratorsOfGroup( FG ),
                                       fgens{ [ 1 .. Rank( FG ) ] } );
  
  mapH := GroupHomomorphismByImagesNC( FH, F, FreeGeneratorsOfGroup( FH ), 
                                       fgens{ [ Rank( FG ) + 1 .. Rank( F ) ] } );

  # lift of the endomorphisms 
  endos := [];
  for endo in EndomorphismsOfLpGroup( G ) do 
    Add( endos, GroupHomomorphismByImagesNC( F, F, fgens, 
         Concatenation( List( FreeGeneratorsOfGroup( FG ), x -> Image( mapG, Image( endo, x ) ) ),
                        fgens{[ Rank(FG) + 1 .. Rank(F) ] } ) ) );
  od;
  for endo in EndomorphismsOfLpGroup( H ) do 
    Add( endos, GroupHomomorphismByImagesNC( F, F, fgens, 
         Concatenation( fgens{ [ 1 .. Rank( FG ) ] }, 
         List( FreeGeneratorsOfGroup( FH ), x -> Image( mapH, Image( endo, x ) ) ) ) ) );
  od;

  # compute the embeddings of the relations
  frels := Concatenation( List( FixedRelatorsOfLpGroup( G ), x -> Image( mapG, x ) ),
                          List( FixedRelatorsOfLpGroup( H ), x -> Image( mapH, x ) ) );
  irels := Concatenation( List( IteratedRelatorsOfLpGroup( G ), x -> Image( mapG, x ) ),
                          List( IteratedRelatorsOfLpGroup( H ), x -> Image( mapH, x ) ) );

  # create the LpGroup and store the factors and the embedding as attributes
  Lp := LPresentedGroup( F, frels, endos, irels );
  SetFreeFactors( Lp, [ G, H ] );
  SetEmbeddingIntoFreeProduct( Lp, [ mapG, mapH ] );

  return( Lp );
  end );

InstallMethod( FreeProductOp, 
  "for an LpGroup and an FpGroup", true,
  [ IsLpGroup, IsFpGroup ], 0, 
  function( G, H )
  local Lp,		# the free product
	FG,FH,F,	# underlying free groups,
	fgens,		# free generators of <F>
	mapG,mapH,	# embeddings of <FG> and <FG> into <F>
	frels, irels,	# fixed and iterated relations of <Lp>
	endos,endo;	# iterating endomorphisms of <G>

# avoid IsomorphismLpGroup as this returns < X | {} | {id} | R >
  
  # initialization
  FG := FreeGroupOfLpGroup( G );
  FH := FreeGroupOfFpGroup( H );
  F  := FreeGroup( Concatenation( List( FreeGeneratorsOfGroup( FG ), String ),
                                  List( FreeGeneratorsOfGroup( FH ), String ) ) ); 
  fgens := FreeGeneratorsOfGroup( F );;

  # isomorphisms of the underlying free groups
  mapG := GroupHomomorphismByImagesNC( FG, F, FreeGeneratorsOfGroup( FG ),
                                       fgens{ [ 1 .. Rank( FG ) ] } );

  mapH := GroupHomomorphismByImagesNC( FH, F, FreeGeneratorsOfGroup( FH ),
                                       fgens{ [ Rank( FG ) + 1 .. Rank( F ) ] } );

  # lift of the iterating endomorphisms of <G>
  endos := [];
  for endo in EndomorphismsOfLpGroup( G ) do
    Add( endos, GroupHomomorphismByImagesNC( F, F, fgens,
         Concatenation( List( FreeGeneratorsOfGroup( FG ), x -> Image( mapG, Image( endo, x ) ) ),
                        fgens{[ Rank(FG) + 1 .. Rank(F) ] } ) ) );
  od;

  # compute the embeddings of the relators
  frels := List( FixedRelatorsOfLpGroup( G ), x -> Image( mapG, x ) );;
  irels := Concatenation( List( IteratedRelatorsOfLpGroup( G ), x -> Image( mapG, x ) ),
                          List( RelatorsOfFpGroup( H ), x -> Image( mapH, x ) ) );

  # create the LpGroup and store the factors and the embedding as attributes
  Lp := LPresentedGroup( F, frels, endos, irels );
  SetFreeFactors( Lp, [ G, H ] );
  SetEmbeddingIntoFreeProduct( Lp, [ mapG, mapH ] );

  return( Lp );
  end );

InstallMethod( FreeProductOp,
  "for an FpGroup and an LpGroup", true,
  [ IsFpGroup, IsLpGroup ], 0,
  function( H, G )
  local fac, emb, Lp;
  Lp := FreeProductOp( G, H ) ;
  emb := EmbeddingIntoFreeProduct( Lp );
  fac := FreeFactors( Lp );
  ResetFilterObj( Lp, EmbeddingIntoFreeProduct );;
  SetEmbeddingIntoFreeProduct( Lp, emb{[2,1]} );;
  ResetFilterObj( Lp, FreeFactors );;
  SetFreeFactors( Lp, fac{[2,1]} );;

  return( Lp );;
  end );

############################################################################
##
#M AmalgamatedFreeProduct( LpGroup1, LpGroup2, Map1, Map2 )
##
## <Map1> is a homomorphism from a subgroup of the free group of <LpGroup1>
## into the free group of <LpGroup2>; similarly for <Map2>
##
############################################################################
#InstallMethod( AmalgamatedFreeProductOp,
#  "for two LpGroups and two homomorphisms", true,
#  [ IsLpGroup, IsGeneralMapping, IsLpGroup, IsGeneralMapping ], 0,
#  function( G, H, mapG, mapH )
#  local Fp,	# the free product of <G> and <H>
#
#  Fp := FreeProductOp( G, H );
#  embG := EmbeddingIntoFreeProduct( G );
#  embH := EmbeddingIntoFreeProduct( H );
#
#  return( true );
#  end );


# list contains list[1] and list[2] which are identified pointwise
# in the amalgamated free product
LPRES_AmalgamatedFreeProduct := function( G, listG, H, listH )
  local Fp,		# the free product of <G> and <H>
	embG,embH;	# embeddings into the free product

  if Length( listG ) <> Length( listH ) then 
    Error( "listG and listH must have the same length" );
  fi;

  # initialization
  Fp   := FreeProductOp( G, H );
  embG := EmbeddingIntoFreeProduct( Fp )[1];
  embH := EmbeddingIntoFreeProduct( Fp )[2];

  return( LPresentedGroup( FreeGroupOfLpGroup( Fp ),
          Concatenation( FixedRelatorsOfLpGroup( Fp ), 
             List( [ 1 .. Length( listG ) ],
                   i -> Image( embG, listG[i] ) / Image( embH, listH[i] ) ) ), 
          EndomorphismsOfLpGroup( Fp ),
          IteratedRelatorsOfLpGroup( Fp ) ) );
  end;

LPRES_WordLetterRepToExtRepOfObj := function( list )
  local obj,i;
  
  # catch the trivial case
  if Length( list ) = 0 then return( list ); fi;
  
  # determine the external representation
  obj := [ AbsInt( list[1] ), SignInt( list[1] )];
  for i in [ 2 .. Length( list ) ] do
    if Length(obj)>0 and obj[Length(obj)-1] =  AbsInt( list[i] ) then
      obj[Length(obj)] := obj[Length(obj)] + SignInt( list[i] );
      if obj[Length(obj)] = 0 then
        obj := obj{[1..Length(obj)-2]};
      fi;
    else 
      Append( obj, [ AbsInt( list[i] ), SignInt( list[i] )] );
    fi;
  od;
  return( obj );
  end;

############################################################################
##
#M IsomorphismLpGroup
##
############################################################################
InstallMethod( IsomorphismLpGroup,
  "for a subgroup of an LpGroup", true,
  [ IsSubgroupLpGroup ], 1,
  function( U )
  local G,	# the L-presented image of <U>
	FG,	# the free group of <G>
	ell,	# periodicity of the subgroup presentation
 	sig,	# the iterating endomorphism
	phi, 	# permutation representation
	orbs,	# orbits of the free group under <sigma>^<i>
	F,	# underlying free group of <U>
	L, 	# the <sigma>^<ell> invariant subgroup
	InvL,	# image of <L> under <sigma>^i
	RW,	# the Reidemeister map of <U>
 	i,j,	# periodicities of the subgroup presentation
	U0,	# the finitely presented part
	rels,	# relators of <U0>
    	endo,	# an endomorphism of the underlying free group (a power of <sig>)
	r,q,	# relators of the whole group
	Trans,	# a (full) Schreier transversal
	SigTrans,
	orb,g,
   	fgens,	# free generators of <InvL>
  	irels,
	imgInvL,
	eps,embT,embU,Fp,
   	imgs,
	t,
	Ut, Ft, fam, 
	k,l,m;	# loop variables
  
  G   := Parent( U );
  
  # catch the trivial case
  if IndexInWholeGroup( U ) = 1 then return( IdentityMapping( G ) ); fi;

  # catch the non-trivial case :)
  if Length( EndomorphismsOfLpGroup( G ) ) > 1 then
    TryNextMethod( );
  fi;

  # initialization
  i   := PeriodicityOfSubgroupPres( U )[1];;
  j   := PeriodicityOfSubgroupPres( U )[2];;
  ell := j-i;;
  sig := EndomorphismsOfLpGroup( G )[1];;
  F   := FreeGroupOfWholeGroup( U );
  phi := FactorCosetAction( G, U );
  RW  := ReidemeisterMap( U );
  Trans := SchreierData( U ).trans;

  # determine the finitely presented group <U0>
  rels := [];;
  for q in FixedRelatorsOfLpGroup( G ) do
    Append( rels, List( Trans, t -> ReidemeisterRewriting( U, t * q * t^-1 ) ) );
  od;
  for r in IteratedRelatorsOfLpGroup( G ) do
    endo := IdentityMapping( F );
    for k in [ 0 .. i - 1 ] do
      Append( rels, List( Trans, t -> ReidemeisterRewriting( U, t * Image( endo, r ) * t^-1 ) ) );
      endo := endo * sig;
    od;
  od;
  U0 := Range( RW ) / rels;

  # compute the invariant subgroup
  if i = 0 then 
    InvL := Kernel( phi );
  else
    L    := Kernel( sig^i * phi );
    InvL := Image( sig^i, L );
  fi;
  fgens := FreeGeneratorsOfGroup( InvL );
  if not ForAll( fgens, x -> Image( sig^ell, x ) in InvL ) then
    Error( "the subgroup <InvL> is not invariant" );
  fi;
  
  # compute the induced iterating endomorphism <sig>^<ell>
  imgs := List( fgens, x -> LPRES_WordLetterRepToExtRepOfObj(
                            AsWordLetterRepInFreeGenerators( Image( sig^ell, x ), InvL ) ) );

  # compute the induced iterated relators as words over <fgens>
# irels := [];;
# for r in IteratedRelatorsOfLpGroup( G ) do
#   Append( irels, List( [ i .. j-1 ], x -> LPRES_WordLetterRepToExtRepOfObj( 
#                  AsWordLetterRepInFreeGenerators( Image( sig^x, r ), InvL ) ) ) );
# od;

  # mapping the rewriting of <w> into the free group <Ft>
  imgInvL := function( Ft, w )
    local obj, fam;
#   obj := ExtRepOfObj( ReidemeisterRewriting( U, w ) );
    obj := LPRES_WordLetterRepToExtRepOfObj( 
           AsWordLetterRepInFreeGenerators( w, InvL ) );
    fam := ElementsFamily( FamilyObj( Ft ) );
    return( ObjByExtRep( fam, obj ) );
    end;

  # orbits of <F> acting via <sigma>^<i> on UK\F
  SigTrans := LPRES_Orbits( F, [1..IndexInWholeGroup(U)], FreeGeneratorsOfGroup( F ), 
                          List( FreeGeneratorsOfGroup( F ), x -> Image( sig^i * phi, x ) ) );

  # compute the iterated amalgamated free product
  embU := RW;
  for l in [ 1 .. Length( SigTrans.orbits ) ] do
    orb := SigTrans.orbits[l];
    t := Trans[orb[1]];;
    Ft := FreeGroup( List( [1..Rank( InvL )], x -> Concatenation( "c", String(x) ) ) );
    fam   := ElementsFamily( FamilyObj( Ft ) );;

    # compute the induced iterated endomorphism <sigma>^<ell> as given by <imgs>
    endo  := GroupHomomorphismByImagesNC( Ft, Ft, FreeGeneratorsOfGroup( Ft ),
                                          List( imgs, x -> ObjByExtRep( fam, x ) ) );

    # compute the iterated relations (also conjugates by orbit-reps via sigma^i)
#   irels := [];;
#   for g in SigTrans.reps[l] do 
#     for k in [ i .. j-1 ] do 
#       Append( irels, List( IteratedRelatorsOfLpGroup( G ), x -> imgInvL( Ft, t * g * Image( sig^k, x ) * g^-1 * t^-1) ) );
#     od;
#   od;
    irels := [];;
    for g in SigTrans.reps[l] do 
      for k in [ i .. j-1 ] do 
        Append( irels, List( IteratedRelatorsOfLpGroup( G ), r -> ObjByExtRep( fam, LPRES_WordLetterRepToExtRepOfObj( 
                       AsWordLetterRepInFreeGenerators( t * g * Image( sig^k, r ) * g^-1 * t^-1, InvL ) ) ) ) );
      od;
    od;
    
    # construct the iterated amalgamated free product
    Ut   := LPresentedGroup( Ft, [], [endo], irels );
    Fp   := FreeProductOp( U0, Ut );
    embU := embU * EmbeddingIntoFreeProduct( Fp )[1];
    embT := EmbeddingIntoFreeProduct( Fp )[2];
    U0   := LPresentedGroup( FreeGroupOfLpGroup( Fp ), 
            Concatenation( FixedRelatorsOfLpGroup( Fp ),
                List( [1..Length(fgens)], 
                      x -> Image( embT, FreeGeneratorsOfGroup( Ft )[x] ) / 
                           Image( embU, t * fgens[x] * t^-1 ) ) ),
            EndomorphismsOfLpGroup( Fp ),
            IteratedRelatorsOfLpGroup( Fp ) );
  od;
 

# return( GroupHomomorphismByImagesNC( U, U0, GeneratorsOfGroup( U ),
#         List( GeneratorsOfGroup( U ), x -> Image( embU, ReidemeisterRewriting( U, UnderlyingElement( x ) ) ) ) ) );
  return( GroupHomomorphismByImagesNC( U, U0, GeneratorsOfGroup( U ), 
          List( GeneratorsOfGroup( U ), x -> Image( embU, UnderlyingElement( x ) ) ) ) );
  end);

############################################################################
##
#F LPRES_OrbStab .  .  .  .  .  orbit-stabilizer algorithm
##
############################################################################
InstallGlobalFunction( LPRES_OrbStab,
  function( F, pnt, fgens, gensact )
  local orb,	# the orbit
	Reps,	# representatives of the action
	Stab,	# generators of the stabilizer
	done,		# finished the while-loop?
	i,j,k,m;	# loop variables

  if Length( fgens ) <> Length( gensact ) then Error(); fi;

  # initialization
  orb  := [ pnt ];
  Reps := [ One( F ) ];
  Stab := [];

  done := false;
  while not done do
    done := true;
    for i in [ 1 .. Length( orb ) ] do
      for j in [ 1 .. Length( gensact) ] do
        k := orb[i] ^ gensact[j];
        m := Position( orb, k );
        if m = fail then 
          done := false;
          Add( orb, k );
          Add( Reps, Reps[i] * fgens[j] );
        else
          k := Reps[i] * fgens[j] * Reps[m]^-1;
          if not IsOne( k ) then
            Add( Stab, k );
          fi;
        fi;
      od;
    od;
  od;

  return( rec( orbit := orb, reps := Reps, stab := Subgroup( F, Stab ) ) );
  end);

############################################################################
##
#F LPRES_Orbits
##
############################################################################
InstallGlobalFunction( LPRES_Orbits,
  function( F, Omega, fgens, gensact )
  local O,	# structural copy of <Omega>
	pnt,	# point to act on
	orb,	# orbit of <pnt>
	reps,	# list of orbit-representatives
	orbs;	# the orbits

  # initialization
  O    := StructuralCopy( Omega );
  orbs := [];
  reps := [];
 
  # compute the partion into F-orbits
  while IsBound( O[1] ) do 
    pnt :=  O[1];
    orb := LPRES_OrbStab( F, pnt, fgens, gensact );
    Add( orbs, orb.orbit );
    Add( reps, orb.reps );
    O := Difference( O, orb.orbit );
  od;

  return( rec( orbits := orbs, reps := reps ) );
  end);


#RepAct := function( F, pnt1, pnt2, gensact )
#  local orb,G,fgens,finished,i,ell,j,m;
#
#  fgens := GeneratorsOfGroup( F );
#  if Length( fgens ) <> Length( gensact ) then Error(); fi;
#  G := Group( gensact );
#  orb := rec( orbit := [ pnt1 ],
#              stabilizer := [ ],
#              transversal := [ One( F ) ] );
#
#  if pnt1 = pnt2 then return( One(F) ); fi;
#
#  repeat
#    finished := true;
#    for i in [1..Length(orb.orbit)] do
#      for ell in [1..Length(gensact)] do
#        j := orb.orbit[i] ^ gensact[ell];
#        m := Position( orb.orbit, j );
#        if m = fail then 
#          finished := false;
#          Add( orb.orbit, j );
#          Add( orb.transversal, orb.transversal[i] * fgens[ell] );
#          if j = pnt2 then return( orb.transversal[i] * fgens[ell] ); fi;
#        else
#          j := orb.transversal[i] * fgens[ell] * orb.transversal[m]^-1;
#          if not IsOne( j ) then Add( orb.stabilizer, j ); fi;
#        fi;
#      od;
#    od;
#  until finished;
#
#  return( fail );
#  end;
