############################################################################
##
#W gap/rql/extqs.gi			LPRES				René Hartung
##

############################################################################
##
#F  LPRES_RationalLowerCentralSeries( <QS> )
##
## computes the rational lower central series of the quotient given by the 
## quotient system <QS>.
## TODO: Recycle the method from the p-quotient algorithm?
##
InstallGlobalFunction( LPRES_RationalLowerCentralSeries,
  function(Q)
  local i,		     # loop variable
       	H,		     # nilpotent presentation corr. to <Q>
       	gens,	   # generators of <H>
        pcs;		   # lower central series of <H>

  # the rational lcs quotient
  H := PcpGroupByCollectorNC( Q.Pccol );

  # generators of <H>
  gens := GeneratorsOfGroup( H );

  # build the exponent-p central series
  pcs:=[];
  pcs[1]   := H;
 
  i := 1;
  repeat 
    i := i+1;
    pcs[i] := SubgroupByIgs( H, gens{Filtered([1..Length(gens)],x->Q.Weights[x]>=i)} );
  until Size( pcs[i] ) = 1;

  return( pcs );
  end );

############################################################################
##
#M  ExtendRationalQuotientSystem ( <QS> )
##
## extends the quotient system for G/G_i to a consistent quotient
## system for G/G_{i+1}.
##
InstallGlobalFunction( ExtendRationalQuotientSystem,
  function( Q )
  local c,       # nilpotency class 
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
        Torsion,SNF,
        rel,
        Qnew,
        time;

  # class
  c := Maximum( Q.Weights );

  # weights, definitions and images of the quotient system
  QS := rec( Lpres       := Q.Lpres,
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
  # covering group with respect to the given quotient system
  time := Runtime();
  LPRES_RationalCoveringGroupByQSystem( Q, QS );
  Info( InfoLPRES, 2, "Time spent for the tails routine: ", StringTime( Runtime()-time ) );
  Info( InfoLPRES, 2, "Number of tails introduced: ", Length( Filtered( QS.Weights, x -> x = c+1 ) ) );

  # position of the first pseudo generator/tail
  b := Position( QS.Weights, Maximum( QS.Weights ) );
  
  # enforce consistency
  Basis := LPRES_ConsistencyChecks( QS, Integers );

  # induce the substitutions of the L-presentation to the module
  time := Runtime();
  A := rec( Relations := [],
            Substitutions := [],
            IteratedRelations := [] );
  LPRES_InduceSpinning( QS, A, Integers );
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
      LPRES_AddRow( Basis, stack[i] );
    fi;
  od;

  while not IsEmpty(stack) do
    Info( InfoLPRES, 4, "Spinning stack has size ", Length(stack) );
    ev := Remove( stack, 1 );
    if not IsZero(ev) then 
      for mat in A.Substitutions do 
        evn := ev * mat;
        if LPRES_AddRow( Basis,evn ) then 
          Add( stack, evn );
        fi;
      od;
    fi;
  od;

  # add the fixed relations
  for rel in A.Relations do
    LPRES_AddRow( Basis, rel );
  od;
  Info(InfoLPRES,2,"Time spent for spinning algorithm: ", StringTime(Runtime()-time));

  # smith normal form to extract the torsion of G/G'
  # ( SNF.rowtrans x Basis.mat x SNF.coltrans = SNF.normal )
  time := Runtime();
  SNF := SmithNormalFormIntegerMatTransforms( Basis.mat );

  # generators w/ finite order
  Torsion := Filtered( [1..Length(SNF.normal)], x-> SNF.normal[x][x] > 0 );

  # remove the torsion in this section
  if not IsEmpty( Torsion ) then 
    # determine the inverse of the coltrans matrix 
    # the first |Torsion| rows are the generators of the torsion subgroup
    mat := SNF.coltrans ^ -1;
   
    # add the torsion generators to the HNF:
    Append( Basis.mat, mat{[1..Length(Torsion)]} );
    Basis.mat := HermiteNormalFormIntegerMat( Basis.mat );
    Basis.mat := Filtered( Basis.mat, x -> not IsZero( x ) );
    Basis.Heads := List( Basis.mat, PositionNonZero );
  fi;
  Info(InfoLPRES,2,"Time spent to eliminate torsion (", Length(Torsion), "): ", StringTime(Runtime()-time));

  # use the new basis to create the new quotient system
  QSnew := LPRES_CreateNewRationalQuotientSystem( QS, Basis );
  SetRationalLowerCentralSeries( Range( QSnew.Epimorphism ), LPRES_RationalLowerCentralSeries( QSnew ) );
  if Length( QSnew.Weights ) - Length( Q.Weights ) > InfoLPRES_MAX_GENS then 
    Info( InfoLPRES, 1, "Class ", Maximum( QSnew.Weights ), ": ", Length(QSnew.Weights)-Length(Q.Weights), " generators");
  else
    Info( InfoLPRES, 1, "Class ", Maximum( QSnew.Weights ), ": ", Length(QSnew.Weights)-Length(Q.Weights),
  	       " generators with relative orders: ", RelativeOrders(QSnew.Pccol){[Length(Q.Weights)+1..Length(QSnew.Weights)]});
  fi;
  return( QSnew );
  end );

############################################################################
##
#F  LPRES_RationalCoveringGroupByQSystem( <oldQS>, <newQS> )
##
InstallGlobalFunction( LPRES_RationalCoveringGroupByQSystem,
  function( Q, QS )
  local c,		# nilpotency class
        n,		# number of generators <pccol>
        d,  # number of generators of weight 1
        t,		# counter for the tails
        orders,		# relative orders of <pccol>
        NewGens,	# total number of generators of the covering group
        rhs,
        obj,
        b,
        i,j,k;		# loop variables
  
  # class
  c := Maximum( Q.Weights );
    
  # number of generator 
  n := Q.Pccol![ PC_NUMBER_OF_GENERATORS ];

  # number of generators for G/G'
  d := Length( Filtered( Q.Weights, x -> x = 1 ) );

  # relative orders 
  orders := RelativeOrders( Q.Pccol );
  
  # determine the number of generators of the covering group:
  # number of new (pseudo-) generators coming from commutator relations
  ## NOTE: d        - weight 1 gens,
  ##       n        - total num of gens,
  ##       (d-1)d/2 - commutators of weight 1,
  ##       (n-d)d   - commutators of larger weight and weight 1
  ##       n-d      - amongst these commutators are the definitions
  ## plus: power relations and non-defining images
# NewGens := ((n-1)*(n)-(n-d-1)*(n-d))/2-n+d;
  NewGens := (d-1)*d/2+(n-d)*d-(n-d);

  # number of new pseudo-generators coming from power relations
  NewGens := NewGens + Length( Filtered( orders, x -> x <> 0 ) );

  # number of new pseudo-generators coming from the epimorphism
  NewGens := NewGens + Length( Filtered( Q.Imgs, IsList ) ); 
  
  # the new collector
  QS.Pccol := FromTheLeftCollector( n + NewGens ); 

  # counter for the tails
  t := n+1;

  # define the new images
  for i in [ 1 .. Length( QS.Imgs ) ] do
    if IsList( QS.Imgs[i] ) then 
      QS.Imgs[i] := Concatenation( QS.Imgs[i], [t,1] );
      Add( QS.Weights, c+1 );
      Add( QS.Definitions, i );
      t := t+1;
    fi;
  od;

  # new power relations
  for i in Filtered( [1..n], x -> orders[x] <> 0 ) do
    SetRelativeOrder( QS.Pccol, i, orders[i] );
    SetPower( QS.Pccol, i, Concatenation( GetPower( Q.Pccol, i ), [t,1] ) );
    Add( QS.Weights, c+1 );
    Add( QS.Definitions, -i );
    t := t+1;
  od;

  # new commutator relations which define pseudo-generators; i.e.
  # ([a_j,a_i] with w(a_j) < c)
  for i in Filtered( [1..Length(orders)], x-> QS.Weights[x] = 1) do
    for j in Filtered([i+1..Length(orders)],x-> not [x,i] in QS.Definitions and QS.Weights[x]<c) do
      SetConjugate(QS.Pccol,j,i,Concatenation(GetConjugate(Q.Pccol,j,i),[t,1]));
      Add(QS.Weights,c+1);
      Add(QS.Definitions,[j,i]);
      t:=t+1;
    od;
  od;

  # new commutator relations which define gens
  for i in Filtered([1..Length(orders)],x->QS.Weights[x]=1) do
    for j in Filtered([1..Length(orders)],x-> not [x,i] in QS.Definitions and
                                              QS.Weights[x]=c and x>i) do
      SetConjugate(QS.Pccol,j,i,Concatenation(GetConjugate(Q.Pccol,j,i),[t,1]));
      Add(QS.Weights,c+1);
      Add(QS.Definitions,[j,i]);
      t:=t+1;
    od;
  od; 

  if LPRES_TEST_ALL then 
    if not t-1 = NumberOfGenerators( QS.Pccol ) then 
      Error("Number of new generators might be wrong");
    fi;
  fi;

  # set up the definitions
  for i in [1..n] do 
    if IsList( QS.Definitions[i] ) then 
      SetConjugate( QS.Pccol, QS.Definitions[i][1], QS.Definitions[i][2],
                    GetConjugate( Q.Pccol, QS.Definitions[i][1], QS.Definitions[i][2] ) );
    fi;
  od;
  
  # remaining relations (completed by the tails routine)
  for i in [1..n] do
    for j in [i+1..n] do
      if QS.Weights[i] > 1 and QS.Weights[i] + QS.Weights[j] < c+1 then 
        SetConjugate( QS.Pccol, j, i, GetConjugate( Q.Pccol, j, i ) );
      fi;
      if orders[i] = 0 then 
        SetConjugate( QS.Pccol, j, -i, GetConjugate( Q.Pccol, j, -i ) );
      fi;
      if orders[j] = 0 then 
        SetConjugate( QS.Pccol, -j, i, GetConjugate( Q.Pccol, -j, i ) );
        if orders[i] = 0 then 
          SetConjugate( QS.Pccol, -j, -i, GetConjugate( Q.Pccol, -j, -i ) );
        fi;
      fi;
    od;
  od;
  SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
# SetFeatureObj( QS.Pccol, UseLibraryCollector, true );
  FromTheLeftCollector_SetCommute( QS.Pccol );
  FromTheLeftCollector_CompletePowers( QS.Pccol );

  # update the relative orders to those of the covering group
  orders := RelativeOrders( QS.Pccol );

  # THE TAILS ROUTINE
  b := c+1;
  while b > 1 do
    for i in [1..QS.Pccol![ PC_NUMBER_OF_GENERATORS ] ] do
      for j in [ i+1 .. QS.Pccol![ PC_NUMBER_OF_GENERATORS ] ] do 
        if QS.Weights[i] + QS.Weights[j] = b then 
          # update the commutator [aj,ai] if w(ai) > 1
          if QS.Weights[i] > 1 then
            rhs := LPRES_Tails_lji( QS.Pccol, QS.Definitions[i], j, i );
            for k in [ 1,3..Length(rhs)-1 ] do
              if orders[ rhs[k] ] <> 0 and rhs[k+1] < 0 then 
                if not IsEmpty( GetPower( QS.Pccol, k ) ) then 
                  Error("rhs not trivial in rational tails routine");
                else
                  rhs[k+1] := rhs[k+1] mod orders[ rhs[k] ];
                fi;
              fi;
            od;
            rhs := Concatenation( GetConjugate( QS.Pccol, j, i ), rhs );
            SetConjugateNC( QS.Pccol, j, i, rhs );
            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
            FromTheLeftCollector_SetCommute( QS.Pccol );
          fi;

          # update the inverse conjugacy relations
          if orders[i] = 0 then 
            obj := LPRES_Tails_lkk( QS.Pccol, j, -i );
            repeat
              rhs := ListWithIdenticalEntries( QS.Pccol![ PC_NUMBER_OF_GENERATORS ], 0 );
            until CollectWordOrFail( QS.Pccol, rhs, obj ) <> fail;
            rhs := ObjByExponents( QS.Pccol, rhs );
            SetConjugateNC( QS.Pccol, j, -i, rhs );
            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
          fi;
          if orders[j]=0 then  
            obj := LPRES_Tails_llk( QS.Pccol, -j, i );
            repeat
              rhs := ListWithIdenticalEntries( QS.Pccol![ PC_NUMBER_OF_GENERATORS ], 0 );
            until CollectWordOrFail( QS.Pccol, rhs, obj ) <> fail;
            rhs := ObjByExponents( QS.Pccol, rhs );
            SetConjugateNC( QS.Pccol, -j, i, rhs );
            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );

            if orders[i] = 0 then
              obj := LPRES_Tails_llk( QS.Pccol, -j, -i );
              repeat
                rhs := ListWithIdenticalEntries( QS.Pccol![ PC_NUMBER_OF_GENERATORS ], 0 );
              until CollectWordOrFail( QS.Pccol, rhs, obj ) <> fail;
              rhs := ObjByExponents( QS.Pccol, rhs );
              SetConjugateNC( QS.Pccol, -j, -i, rhs );
              SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
            fi;
          fi;
        elif QS.Weights[i] + QS.Weights[j] > b then 
          break;
        fi;
      od; 
    od; 
    b := b-1;
  od;
  end);

############################################################################
##
#F  LPRES_CreateNewQuotientSystem
##
## with respect to the old quotient systems QS and the basis of the submodule
##
InstallGlobalFunction( LPRES_CreateNewRationalQuotientSystem,
  function( QS, Basis ) 
  local Q,
        i,j,k,
        b,
        n,m,
        ev,ev1,
        obj,
        Imgs,
        Gens,
        H;

  # position of the first tail
  b := Position( QS.Weights, Maximum( QS.Weights ) );

  # number of generators of the quotient system
  n := QS.Pccol![ PC_NUMBER_OF_GENERATORS ];

  # new generators of the updated quotient system
  Gens := [ ];
  for i in [1..n-b+1] do
    j := Position( Basis.Heads, i );
    if j = fail then
      Add( Gens, i ); # it's an infinite gens
    elif Basis.mat[j][i] > 1 then 
      Add( Gens, i ); # it's a finite gens
    fi;
  od;

  # set up the new quotient system
  Q := rec( Lpres       := QS.Lpres,
            Definitions := ShallowCopy( QS.Definitions{[1..b-1]} ),
            Imgs        := ShallowCopy( QS.Imgs ),
            Pccol       := FromTheLeftCollector( b-1+Length(Gens) ),
            Weights     := Concatenation( QS.Weights{[1..b-1]}, QS.Weights{List(Gens,x->b-1+x)} )
            );

  # add the definitions of the surviving generators
  for i in Gens do 
    Add( Q.Definitions, QS.Definitions[b+i-1] );
  od;

  # set up the images
  for i in [1..Length(Q.Imgs)] do
    if IsList( Q.Imgs[i] ) then 
      Q.Imgs[i] := LPRES_AdjustIntegralObject( QS.Imgs[i], Basis, Gens, QS.Pccol, Q.Pccol );
    fi;
  od;

  # number of gens of the new collector
  m := Q.Pccol![ PC_NUMBER_OF_GENERATORS ];

  # set up the power relation and conjugacy relations of the first b-1 generators
  # the new generators [b..m] are central 
  for i in [1..b-1] do
    if IsBound( QS.Pccol![ PC_EXPONENTS ][i] ) then 
      SetRelativeOrder( Q.Pccol, i, QS.Pccol![ PC_EXPONENTS ][i] );
      obj := GetPower( QS.Pccol, i );
      obj := LPRES_AdjustIntegralObject( obj, Basis, Gens, QS.Pccol, Q.Pccol );
      SetPower( Q.Pccol, i, obj );
    fi;

    for j in [i+1..b-1] do
      obj := GetConjugate( QS.Pccol, j, i );
      obj := LPRES_AdjustIntegralObject( obj, Basis, Gens, QS.Pccol, Q.Pccol );
      SetConjugate( Q.Pccol, j, i, obj );

      # include the inverses
      if not IsBound( QS.Pccol![ PC_EXPONENTS ][i] ) then 
        obj := GetConjugate( QS.Pccol, j, -i );
        obj := LPRES_AdjustIntegralObject( obj, Basis, Gens, QS.Pccol, Q.Pccol );
        SetConjugate( Q.Pccol, j, -i, obj );
      fi;
      if not IsBound( QS.Pccol![ PC_EXPONENTS ][j] ) then 
        obj := GetConjugate( QS.Pccol, -j, i );
        obj := LPRES_AdjustIntegralObject( obj, Basis, Gens, QS.Pccol, Q.Pccol );
        SetConjugate( Q.Pccol, -j, i, obj );
        if not IsBound( QS.Pccol![ PC_EXPONENTS ][i] ) then 
          obj := GetConjugate( QS.Pccol, -j, -i );
          obj := LPRES_AdjustIntegralObject( obj, Basis, Gens, QS.Pccol, Q.Pccol );
          SetConjugate( Q.Pccol, -j, -i, obj );
        fi;
      fi;
    od;
  od;

  # set the order of the remaining generators (tails), they are central by def
  for i in [1..Length(Gens)] do
    j := Position( Basis.Heads, Gens[i] );
    if j <> fail then 
      if Basis.mat[j][Gens[i]] > 1 then 
        SetRelativeOrder( Q.Pccol, b+i-1, Basis.mat[j][Gens[i]] );
        obj := [ b+Gens[i]-1, Basis.mat[j][Gens[i]] ];
        obj := LPRES_AdjustIntegralObject( obj, Basis, Gens, QS.Pccol, Q.Pccol );
        SetPower( Q.Pccol, b+i-1, obj );
      fi;
    fi;
  od;
  FromTheLeftCollector_SetCommute( Q.Pccol );
  SetFeatureObj( Q.Pccol, IsUpToDatePolycyclicCollector, true );
  FromTheLeftCollector_CompletePowers( Q.Pccol );
  FromTheLeftCollector_CompleteConjugate( Q.Pccol );
  SetFeatureObj( Q.Pccol, IsUpToDatePolycyclicCollector, true );
# UpdatePolycyclicCollector( Q.Pccol );

  if LPRES_TEST_ALL then
    if not IsConfluent( Q.Pccol ) then 
      Error( "Inconsistent presentation" );
    fi;
  fi;

  # the epimorphism into the new presentation
  Imgs:=[];
  for i in [1..Length(Q.Imgs)] do
    if IsInt(Q.Imgs[i]) then
      Add( Imgs, PcpElementByGenExpList( Q.Pccol, [ Q.Imgs[i], 1 ] ) );
    else
      Add( Imgs, PcpElementByGenExpList( Q.Pccol, Q.Imgs[i] ) );
    fi;
  od;
  H := PcpGroupByCollectorNC(Q.Pccol);

  Q.Epimorphism := GroupHomomorphismByImagesNC( Q.Lpres, H, 
                                                GeneratorsOfGroup(Q.Lpres), Imgs);
  return( Q );
  end );
