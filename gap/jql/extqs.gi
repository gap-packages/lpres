############################################################################
##
#W gap/jql/extqs.gi			LPRES				René Hartung
##

############################################################################
##
#F  LPRES_JenningsSeries( <QS> )
##
## computes the p-Jennings series of a p-Jennings quotient given by the 
## quotient system <QS>.
## TODO: Recycle the method from the p-quotient algorithm?
##
InstallGlobalFunction( LPRES_JenningsSeries,
  function(Q)
  local weights,	# weights-function of <Q>
       	i,		     # loop variable
       	c, 		    # nilpotency class of <Q>
       	H,		     # nilpotent presentation corr. to <Q>
       	gens,	   # generators of <H>
        pcs;		   # lower central series of <H>

  # the p-quotient
  H := PcpGroupByCollectorNC( Q.Pccol );

  # generators of <H>
  gens := GeneratorsOfGroup( H );

  # weights function of the given quotient system
  weights := Q.Weights;

  # nilpotency class of <Q>
  c := Q.Class;

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
  end );

############################################################################
##
#M  ExtendJenningsQuotientSystem ( <QS> )
##
## extends the quotient system for G/phi_i(G) to a consistent quotient
## system for G/phi_{i+1}(G).
##
InstallGlobalFunction( ExtendJenningsQuotientSystem,
  function( Q )
  local c,       # nilpotency class 
        class,
       	weights,	# weight of the generators
        Defs,	   # definitions of each (pseudo) generator and tail
        Imgs,	   # images of the generators of the LpGroup
        ftl,	    # collector of the covering group
        b,		     # number of "old" generators + 1 
        Gens,
        Basis,		 # Hermite normal form of the consistency rels/relators
        A,		     # record: endos as matrices and (it.) rels as exp.vecs
        i,		     # loop variable
        stack,		 # stack for the spinning algorithm
        ev,evn,		# exponent vectors for the spinning algorithm
        QS,		    # (confluent) quotient system for G/\gamma_i+1(G)
        QSnew,
        mat, 
        rel,
        Qnew,
        time;

  # p-class
  c := Maximum( Q.Weights );
  class := Q.Class;

  # weights, definitions and images of the quotient system
  QS := rec( Lpres       := Q.Lpres,
             Prime       := Q.Prime,
             Class       := Q.Class + 1,
             Weights     := ShallowCopy( Q.Weights ),
             Definitions := ShallowCopy( Q.Definitions ),
             Imgs        := ShallowCopy( Q.Imgs ) );

  # make definitions/images mutable lists (in case we reuse a quotient system from 
  # a stored attribute)
  for i in [1..Length(QS.Definitions)] do
    if IsList( QS.Definitions[i] ) then 
      QS.Definitions[i] := ShallowCopy( QS.Definitions[i] );
    fi;
  od;
  for i in [1..Length(QS.Imgs)] do
    if IsList( QS.Imgs[i] ) then
      QS.Imgs[i] := ShallowCopy( QS.Imgs[i] );
    fi;
  od;

  # build a (possibly inconsistent) nilpotent presentation for the 
  # p-covering group with respect to the given quotient system
  # `Description of groups of prime power order', Neumann, Nickel,
  # Niemeyer, 1998
  time := Runtime();
  LPRES_JenningsCoveringGroupByQSystem( Q, QS );
# PrintPcpPresentation( PcpGroupByCollectorNC( QS.Pccol ) );
  Info( InfoLPRES, 2, "Time spent for the tails routine: ", StringTime( Runtime()-time ) );
  Info( InfoLPRES, 2, "Tails introduced in the tails routine: ", Length( Filtered( QS.Weights, x -> x = c+1 ) ) );

  # position of the first pseudo generator/tail
  b := Position( QS.Weights, Maximum( QS.Weights ) );
  
  # enforce consistency
  Basis := LPRES_ConsistencyChecks( QS );
# Display( "Consistencies:" );
# for i in [1..Length(Basis.mat)] do
#   Display( List( Basis.mat[i], Int ) );
# od;

  # throw away the zero entries not in the module
  for i in [1..Length(Basis.mat)] do
    if not IsZero(Basis.mat[i]{[1..b-1]}) then 
      Error("in ExtendJenningsQuotientSystem: wrong basis from consistency check");
    fi;

    # forget the first b-1 (zero) entries
    Basis.mat[i]   := Basis.mat[i]{[b..Length(QS.Weights)]};
    Basis.Heads[i] := Basis.Heads[i]-b+1;
  od;
    
  # induce the substitutions of the L-presentation to the module
  time := Runtime();
  A := rec( Relations := [],
            Substitutions := [],
            IteratedRelations := [] );
  LPRES_InduceSpinning( QS, A );
# Display("Relations:");
# for i in [1..Length(A.Relations)] do
#   Display( List( A.Relations[i], Int ) );
# od;
# Display("ItRelations:");
# for i in [1..Length(A.IteratedRelations)] do
#   Display( List( A.IteratedRelations[i], Int ) );
# od;
# Display("Map:");
# for i in [1..Length(A.Substitutions)] do
#   Display( List( A.Substitutions[i], x -> List( x, Int ) ) );
# od;
  if LPRES_COMPUTE_RECURSIVE_IMAGE then 
    Info( InfoLPRES, 3, "Time spent to induce the endomorphisms and relations (recursive): ", StringTime( Runtime()-time ) );
  else
    Info( InfoLPRES, 3, "Time spent to induce the endomorphisms and relations: ", StringTime( Runtime()-time ) );
  fi;

  # run the spinning algorithm
  time:=Runtime();
  stack:=A.IteratedRelations;
  for i in [1..Length(stack)] do 
    if not IsZero( stack[i] ) then 
      LPRES_AddPRow( Basis, stack[i] );
    fi;
  od;

  while not IsEmpty(stack) do
    Info( InfoLPRES, 4, "Spinning stack has size ", Length(stack) );
    ev := Remove( stack, 1 );
    if not IsZero(ev) then 
      for mat in A.Substitutions do 
        evn := ev * mat;
        if LPRES_AddPRow( Basis,evn ) then 
          Add( stack, evn );
        fi;
      od;
    fi;
  od;

  # add the fixed relations
  for rel in A.Relations do
    LPRES_AddPRow( Basis, rel );
  od;
  Info(InfoLPRES,2,"Time spent for spinning algorithm: ", StringTime(Runtime()-time));

  # check if we end up in the (expected) spanning set
  b := Position( QS.Weights, Maximum( QS.Weights ) );
  Gens := Filtered( [1..Length(QS.Weights)-b+1], x -> not x in Basis.Heads );
  if not ForAll( Gens, i -> ( IsList( QS.Definitions[i+b-1] ) and 
                              QS.Weights[ QS.Definitions[i+b-1][1] ] = QS.Class - 1 and
                              QS.Weights[ QS.Definitions[i+b-1][2] ] = 1 ) or
                            ( IsInt( QS.Definitions[i+b-1] ) and
                              QS.Definitions[i+b-1] < 0 and
                              QS.Prime * QS.Weights[ -QS.Definitions[i+b-1] ] = QS.Class ) ) then 
    Error( "Element outside the spanning set survives" );
  fi;

  # use the basis to create the new quotient system
  QSnew := LPRES_CreateNewQuotientSystem( QS, Basis );
  QSnew.Class := QS.Class;
  if Length( QSnew.Weights ) - Length( Q.Weights ) > InfoLPRES_MAX_GENS then 
    Info( InfoLPRES, 1, "Class ", QSnew.Class, ": ", Length(QSnew.Weights)-Length(Q.Weights), " generators");
  else
    Info( InfoLPRES, 1, "Class ", QSnew.Class, ": ", Length(QSnew.Weights)-Length(Q.Weights),
  	       " generators with relative orders: ", RelativeOrders(QSnew.Pccol){[Length(Q.Weights)+1..Length(QSnew.Weights)]});
  fi;
  return( QSnew );
  end );

############################################################################
##
#F  LPRES_JenningsCoveringGroupByQSystem( <oldQS>, <newQS> )
##
InstallGlobalFunction( LPRES_JenningsCoveringGroupByQSystem,
  function( Q, QS ) 
  local i,j,k,
        x,y,z,u,
        n,
        c,                   # exponent p class
        class,
        b, 
        ev1,ev2,
        m;

  # exponent p-class
  c := Maximum( QS.Weights );
  class := Q.Class;

  # number of generators of the group
  n := Q.Pccol![ PC_NUMBER_OF_GENERATORS ];
  if n = 0 then 
    Error( "Do not call this function with trivial quotient system" );
  fi;

  # Note that the order is important here - e.g. we wish to 
  # remove the tail which stems from an image using the 
  # TriangulizeMat function, so these generators need to show 
  # up earlier than those which we wish to survive (e.g. generators
  # of the form [a_j,a_i] with w(a_j) = c and w(a_i) = 1)

  # start with the non-defining images
  for i in Filtered( [1..Length(Q.Imgs)], x->IsList(Q.Imgs[x]) ) do
    Add( QS.Definitions, i );
    Add( QS.Weights, c+1 );
  od;

  # The number of relations in R which are not 
  # definitions is s = n (n-1) / 2 + d.
  #
  # Let E be the set of new generators in A whose 
  # definition is a relation with left-hand-side 
  # either a p-th power or a commutator [j,i] with 
  # w(a_i) = 1 and j>i.
  # 
  # Let F be the set of those elements t in E for 
  # which t is defined as a commutator or as the p-th
  # power of a generator y, where y itself is defined
  # as a p-th power.

  # continue with the set F descibed in NNN98
  # commutator relations [a_j,a_i] with w(a_i) = 1
  # BEWARE of the order [a_j,a_i] with w(a_j) = b
  # and w(a_i) = 1 are among the spanning set of 
  # \varphi_b / \varphi_{b+1}

  # we sort the tails in ascending order by their pseudo weight
  #                w(a_k)        if 1<k<n
  # \hat w(a_k) =  w(a_i)+1      if 1<k<n and a_k := a_i^p
  #                w(a_i)+w(a_j) if a_k := [a_j,a_i] with w(a_i) = 1
  # 
  # Sorted in order to apply the TriangulizeMat later one (those which
  # we wish to survice succeed the others)!
  for j in Filtered( [1..n], x -> Q.Weights[x]*Q.Prime < Q.Class+1 ) do
    # add tails to those the power relator which aren't definitions and 
    # which aren't trivial in the Jennings series
    if not -j  in Q.Definitions then 
#     if not IsList( Q.Definitions[j] ) then 
        Add( QS.Definitions, -j );
        Add( QS.Weights, c+1 );
#     fi;
    fi;
  od;
  for j in Filtered( [1..n], x-> Q.Weights[x] < Q.Class ) do
    # add the commutator relations [a_j,a_i] with w(a_i) = 1
    for i in [1..j-1] do
      if Q.Weights[i] > 1 then 
        # generators are ordered by their weight
        break;
      fi;
      if not [j,i] in Q.Definitions then 
        Add( QS.Definitions, [j,i] );
        Add( QS.Weights, c+1 );
      fi;
    od;
  od;

  for j in Filtered( [1..n], x -> Q.Weights[x]*Q.Prime = Q.Class+1 ) do
    # add tails to those the power relator which aren't definitions and 
    # which aren't trivial in the Jennings series
    # NOTE we also add tails to power relations of commutators here
    if not -j  in Q.Definitions then 
#     if not IsList( Q.Definitions[j] ) then 
        Add( QS.Definitions, -j );
        Add( QS.Weights, c+1 );
#     fi;
    fi;
  od;
  for j in Filtered( [1..n], x-> Q.Weights[x] = Q.Class ) do
    # add the commutator relations [a_j,a_i] with w(a_i) = 1
    for i in [1..j-1] do
      if Q.Weights[i] > 1 then 
        # generators are ordered by their weight
        break;
      fi;
      if not [j,i] in Q.Definitions then 
        Add( QS.Definitions, [j,i] );
        Add( QS.Weights, c+1 );
      fi;
    od;
  od;

  # FromTheLeftCollector for the extended quotient system
  QS.Pccol := FromTheLeftCollector( Length( QS.Definitions ) );

  # copy the old collector into the new one (the relations just differ by tails and 
  # will be complete with the tails routine)
  for i in [1..Q.Pccol![ PC_NUMBER_OF_GENERATORS ] ] do
    SetRelativeOrder( QS.Pccol,i, Q.Prime );
    SetPower( QS.Pccol, i, GetPower( Q.Pccol, i ) );
    for j in [i+1..Q.Pccol![ PC_NUMBER_OF_GENERATORS ] ] do
      SetConjugate( QS.Pccol, j, i, GetConjugate( Q.Pccol, j, i ) );
    od;
  od;

  # set up the definitions as defined above in QS.Definitions
  for i in [1..Length(QS.Definitions)] do
    # each generator has relative order <Q.Prime>
    SetRelativeOrder( QS.Pccol, i, Q.Prime );

    if IsList( QS.Definitions[i] ) then 
      # we add a definition as a commutator a_i := [a_j,a_k] 
      j := QS.Definitions[i][1];
      k := QS.Definitions[i][2]; 

      if QS.Weights[i] = c+1 then 
        # add a tail to an existing relation [a_j,a_k] = v_{[a_j,a_k]} a_i
        SetConjugate( QS.Pccol, j, k,
           Concatenation( GetConjugate( Q.Pccol, j, k ), [ i, 1 ] ) );
      else  
        # this was already a definition (don't modify this one)
        SetConjugate( QS.Pccol, j, k, GetConjugate( Q.Pccol, j, k ) );
      fi;
    elif IsPosInt( QS.Definitions[i] ) then 
      # we add the i-th tail to the image of the
      # QS.Definitions[i]-th generator
      if IsList( QS.Imgs[ QS.Definitions[i] ] ) then 
        Append( QS.Imgs[ QS.Definitions[i] ], [i,1] );
      fi;
    else
      # negative integer - a power relation
      if QS.Weights[i] = c+1 then 
        # add the i-th tail to the -QS.Definitions[i] power relation
        SetRelativeOrder( QS.Pccol, -QS.Definitions[i], Q.Prime );
        SetPower( QS.Pccol, -QS.Definitions[i],
                    Concatenation( GetPower( Q.Pccol, -QS.Definitions[i] ),
                                   [ i, 1 ] ) );
      else
        # this was already a definition (don't modify this one)
        SetRelativeOrder( QS.Pccol, -QS.Definitions[i], Q.Prime );
        SetPower( QS.Pccol, -QS.Definitions[i],
                    GetPower( Q.Pccol, -QS.Definitions[i] ) );
      fi;
    fi;
  od;
  SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
# SetFeatureObj( QS.Pccol, UseLibraryCollector, true );
  FromTheLeftCollector_SetCommute( QS.Pccol );
  FromTheLeftCollector_CompletePowers( QS.Pccol );

  # run the tails routine ALGORITHM 3 in NNN98
  for b in [c+1,c..2] do

    # if u has weight b-1, u^p has weight p*(b-1),
    # then the definition [z,y] =: u has weight w(z) = p*(b-1)-1 and w(y) = 1
#   for u in Filtered( [1..n], x-> QS.Weights[x] = b-1 and Q.Prime * QS.Weights[x] < Q.Class+1 )  do
#     # Compute tails for p-th powers u^p with w(u) = b-1 and 
#     # u is defined as a commutator [z,y] =: u
#     if IsList( QS.Definitions[u] ) then 
#       z := QS.Definitions[u][1];
#       y := QS.Definitions[u][2];

#       # z^p-1 ( zy )
#       repeat 
#         ev1 := ExponentsByObj( QS.Pccol, [ z, Q.Prime - 1 ] );
#         repeat 
#           ev2 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
#         until CollectWordOrFail( QS.Pccol, ev2, [ y, 1 ] ) <> fail;
#       until CollectWordOrFail( QS.Pccol, ev1, ObjByExponents( QS.Pccol, ev2 ) ) <> fail;

#       # z^p y
#       repeat 
#         ev2 := ExponentsByObj( QS.Pccol, GetPower( QS.Pccol, z ) );
#       until CollectWordOrFail( QS.Pccol, ev2, [ y, 1 ] ) <> fail;

#       # t := ( z^{p-1} (zy))^{-1} ((z^p)y )
#       ev1 := ( ev2 - ev1 ) mod Q.Prime;

#       # Add to \hat R the relation u^p = v_{u^p} t
#       SetPower( QS.Pccol, u, Concatenation( GetPower( Q.Pccol, u ), ObjByExponents( QS.Pccol, ev1 ) ) );
#       SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
#       FromTheLeftCollector_SetCommute( QS.Pccol );
#     fi;
#   od;

    m:=1;
    while b-m >= m+1 do
      # [i..j] should be the generators of pseudo weigth m+1
      i := Position( QS.Weights, m+1 );
      j := Position( QS.Weights, m+2 )-1;

      # Compute tails for commutators [z,u] with w(z) = b-m and w(u) = m+1
      for u in [i..j] do 
        if IsList( QS.Definitions[u] ) then 
          # case 1: u is defined by a commutator
          y := QS.Definitions[u][1];
          x := QS.Definitions[u][2];

          # for each z of w(z) = b-m with z>u
          for z in Filtered( [u+1..n], x -> QS.Weights[x] = b-(m+1) ) do
            # z (yx) 
            repeat 
              repeat 
                ev2 := ExponentsByObj( QS.Pccol, [ y, 1 ] );
              until CollectWordOrFail( QS.Pccol, ev2, [ x, 1 ] ) <> fail;
              ev1 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
            until CollectWordOrFail( QS.Pccol, ev1, ObjByExponents( QS.Pccol, ev2 ) ) <> fail;

            # (zy)x
            repeat 
              repeat 
                ev2 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
              until CollectWordOrFail( QS.Pccol, ev2, [ y, 1 ] ) <> fail;
            until CollectWordOrFail( QS.Pccol, ev2, [ x, 1 ] ) <> fail;

            # t := (z(yx))^{-1} ((zy)x)
            ev1 := ( ev2 - ev1 ) mod Q.Prime;

            # Add to \hat R the relation [z,u] = v_[z,u] t
            SetConjugate( QS.Pccol, z, u, Concatenation( GetConjugate( Q.Pccol, z, u ), ObjByExponents( QS.Pccol, ev1 ) ) );
            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
            FromTheLeftCollector_SetCommute( QS.Pccol );
          od;
        elif IsInt( QS.Definitions[u] ) and QS.Definitions[u] < 0 then 
          y := -QS.Definitions[u];

          # for each z of w(z) = b-m with z>u
          for z in Filtered( [u+1..n], x -> QS.Weights[x] = b-(m+1) ) do
            # z(y^p)
            repeat 
              ev1 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
            until CollectWordOrFail( QS.Pccol, ev1, GetPower( QS.Pccol, y ) ) <> fail; 

            # (zy) y^{p-1}
            repeat
              ev2 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
            until CollectWordOrFail( QS.Pccol, ev2, [ y, 1, y, Q.Prime-1 ] ) <> fail;

            # t := ( z(y^p) )^-1 ( (zy)y^{p-1} )
            ev1 := ( ev2 - ev1 ) mod Q.Prime;

            # Add to \hat R the relation [z,u] = v_{[z,u]} t
            SetConjugate( QS.Pccol, z, u, Concatenation( GetConjugate( Q.Pccol, z, u ), ObjByExponents( QS.Pccol, ev1 ) ) );
            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
            FromTheLeftCollector_SetCommute( QS.Pccol );
          od;
        else 
          Error( "wrong definition" );
        fi;
      od;
      m := m + 1;
    od;
  od;

  FromTheLeftCollector_SetCommute( QS.Pccol );
  SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
  FromTheLeftCollector_CompletePowers( QS.Pccol );
  FromTheLeftCollector_CompleteConjugate( QS.Pccol );
  SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
  end );
