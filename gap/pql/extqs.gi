############################################################################
##
#W extqs.gi			LPRES				René Hartung
##

# help function
LPRES_PrintPcp := function( ftl ) 
  local i,j,k,n,ev,H,gens;

  H := PcpGroupByCollector( ftl );
  gens := GeneratorsOfGroup( H );
  n := ftl![ PC_NUMBER_OF_GENERATORS ];

  Print( "orders = ", RelativeOrders( ftl ), "\n" );
 
  for i in [1..n] do
    Print( "i^{", RelativeOrders( ftl )[i], "} = ", gens[i]^RelativeOrders(ftl)[i], "\n" );
  od;

  for i in [1..n] do
    for j in [i+1..n] do
      if not IsOne( Comm( gens[j], gens[i] ) ) then 
        Print( "[", j, ",", i, "] = ", ObjByExponents( ftl, Exponents( Comm( gens[j], gens[i] ) ) ), "\n" );
      fi;
    od;
  od;

  end;

############################################################################
##
#M  ExtendPQuotientSystem ( <quo> )
##
## Extends the quotient system for G/gamma_i(G) to a consistent quotient
## system for G/gamma_{i+1}(G).
##
InstallGlobalFunction( ExtendPQuotientSystem,
# "using a single-core machine", true,
# [ IsObject ], 0, 
  function( Q )
  local c,       # nilpotency class 
       	weights,	# weight of the generators
        Defs,	   # definitions of each (pseudo) generator and tail
        Imgs,	   # images of the generators of the LpGroup
        ftl,	    # collector of the covering group
        b,		     # number of "old" generators + 1 
        Basis,		 # Hermite normal form of the consistency rels/relators
        A,		     # record: endos as matrices and (it.) rels as exp.vecs
        i,		     # loop variable
        stack,		 # stack for the spinning algorithm
        ev,evn,		# exponent vectors for the spinning algorithm
        QS,		    # (confluent) quotient system for G/\gamma_i+1(G)
        mat, 
        rel,
        Qnew,
        time;

  # p-class
  c := Maximum( Q.Weights );

# Print( "Prev Defs: ", Q.Definitions, "\n" );

  # weights, definitions and images of the quotient system
  QS := rec( Lpres       := Q.Lpres,
             Prime       := Q.Prime,
             Weights     := ShallowCopy( Q.Weights ),
             Definitions := ShallowCopy( Q.Definitions ),
             Imgs        := ShallowCopy( Q.Imgs ) );

  # make definitions images mutable lists (in case we reuse a quotient system from 
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
  LPRES_PCoveringGroupByQSystem( Q, QS );
  Info( InfoLPRES, 2, "Time spent for the tails routine: ", StringTime( time-Runtime() ) );
  Info( InfoLPRES, 2, "Tails introduced in the covering quotient system: ", Length( Filtered( QS.Weights, x -> x = c+1 ) ) );

  # position of the first new (pseudo) generator/tail
  b := Position( QS.Weights, Maximum( QS.Weights ) );
  
  # enforce consistency
  Basis := LPRES_ConsistencyChecks( QS );

# Print( "Definitions: ", QS.Definitions, "\n" );
# Print( "Images: ", QS.Imgs, "\n" );

  # throw away the zero entries not in the module
  for i in [1..Length(Basis.mat)] do
    if not IsZero(Basis.mat[i]{[1..b-1]}) then 
      Error("in ExtendPQuotientSystem: wrong basis from consistency check");
    fi;

    # forget the first b-1 (zero) entries
    Basis.mat[i]   := Basis.mat[i]{[b..Length(QS.Weights)]};
    Basis.Heads[i] := Basis.Heads[i]-b+1;
  od;
    
  # induce the substitutions of the L-presentation to the module
  A := rec( Relations := [],
            Substitutions := [],
            IteratedRelations := [] );

  time := Runtime();
  LPRES_InduceSpinning( QS, A );
  Info( InfoLPRES, 3, "Time spent to induce the endomorphisms and relations: ", StringTime( Runtime()-time ) );

  time:=Runtime();
    
  # run the spinning algorithm
  stack:=A.IteratedRelations;
  for i in [1..Length(stack)] do 
    if not IsZero( stack[i] ) then 
      LPRES_AddPRow( Basis, stack[i] );
    fi;
  od;

  while not IsEmpty(stack) do
    Info(InfoLPRES,4,"Spinning stack has size ",Length(stack));
    ev:=stack[1];
    Remove( stack, 1 );
    if not IsZero(ev) then 
      for mat in A.Substitutions do 
        evn := ev * mat;
        if LPRES_AddPRow( Basis,evn ) then 
          Add( stack, evn );
        fi;
      od;
    fi;
  od;

  # add the non-iterated relations
  for rel in A.Relations do
    LPRES_AddPRow( Basis, rel );
  od;
  
  Info(InfoLPRES,2,"Time spent for spinning algorithm: ", StringTime(Runtime()-time));

# Display( Basis.mat );

# Error();
  return( LPRES_CreateNewQuotientSystem( QS, Basis ) );
#   if Length(HNF.mat)=0 then 
#     # the presentation ftl satisfy the relations and is consistent
#     QS:=rec();
#     QS.Lpres:=Q.Lpres;
#     QS.Weights:=weights;
#     QS.Definitions:=Defs;
#     QS.Pccol:=ftl;
#     QS.Imgs:=ShallowCopy(Imgs);
#     Imgs:=[];
#     for i in [1..Length(QS.Imgs)] do 
#       if IsInt(QS.Imgs[i]) then 
#         Add(Imgs,PcpElementByGenExpList(QS.Pccol,[QS.Imgs[i],1]));
#       else
#         Add(Imgs,PcpElementByGenExpList(QS.Pccol,QS.Imgs[i]));
#       fi;
#     od;
#     QS.Epimorphism:=GroupHomomorphismByImagesNC(Q.Lpres,
#                                 PcpGroupByCollectorNC(QS.Pccol),
# 				  GeneratorsOfGroup(Q.Lpres),Imgs);
#     return(QS);
#   else 
#     # use the Hermite normal form to compute a consistent presentation 
#     # that satisfy the relations
#     return(LPRES_BuildNewCollector(Q,ftl,HNF,weights,Defs,Imgs));
#   fi;
  end );

############################################################################
##
#F  LPRES_PCoveringGroupByQSystem( <oldQS>, <newQS> )
##
## ALGORITHM 3 in 'Neumann, Nickel, Niemeyer 1998'
##
# Init \hat A := A
# Init \hat R := set of definitions of R
# Add to \hat R all relations of R with left-hand-side [j,i] with 
# w_i + w_j > c+1 
# for b in [c+1,c,..2] do
#   Append the new gens of weight b in F to \hat A
#   Add to R the definitions of those new gens in F with pseudo weight b
#   // Compute tails for p-th powers u^p with w(u) = b-1 and 
#   // u is defined as a commutator [z,y] =: u
#   for each u in A of weight b-1
#     if [z,y] =: u then 
#      t := ( z^{p-1} (zy))^{-1} ((z^p)y )
#      Add to \hat R the rel u^p = v_{u^p} t
#     fi;
#   od
#   m := 1
#   // Compute tails for commutators [z,u] with w(z) = b-1 and 
#   // w(u) = m+1
#   while b-m >= m+1 do
#     for each u \in A with w(u) = m+1 do
#       if [y,x] =: u then 
#         # case 1: u is defined by a commutator
#         for each z of w(z) = b-m with z>u
#           t := ((z(yx))^{-1} ((zy)x)
#           Add to \hat R the relation [z,u] = v_[z,u] t
#         od
#       elif y^p =: u then
#         # case 2: u is defined by a power relation
#         for each z of w(z) = b-m with z>u
#           t := ( z^{p-1} (zy))^{-1} ((z^p)y )
#           Add to \hat R the relation [z,u] = v_[z,u] t
#         od;
#       fi;
#     od;
#     m++
#   od
# od
# Add to \hat R the relations [a_r,a_i] = 1 with a_r \in F
# Add to \hat R the relations a_r^p = 1 with a_r \in F
#
# notion w/in Definitions:
# - i and w(a_j) = 1    : defined as an image of a generator  a_j := x_i^\pi
# - i and w(a_j) > 1    : tail added to the image x_i^\pi = u_i t_i
# - -i and w(a_j) > 1   : defined as a power relation         a_j := a_i^p
# - [l,k] and w(a_j) > 1: defined as a commutator relation    a_j := [a_l,a_k]
InstallGlobalFunction( LPRES_PCoveringGroupByQSystem,
  function( Q, QS ) 
  local i,j,k,
        x,y,z,u,
        n,
        c,                   # exponent p class
        b, 
        ev1,ev2,
        m;

  # exponent p-class
  c := Maximum( QS.Weights );

  # number of generators of the group
  n := Q.Pccol![ PC_NUMBER_OF_GENERATORS ];
  if n = 0 then 
    Error( "do not call this function with trivial quotient system" );
  fi;

  # Note that the order is important here - e.g. we wish to 
  # remove the tail which stems from an image using the 
  # TriangulizeMat function

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
  for j in [1..n] do
    # add the power relator
    if not -j  in Q.Definitions then 
      if not IsList( Q.Definitions[j] ) then 
        Add( QS.Definitions, -j );
        Add( QS.Weights, c+1 );
      fi;
    fi;
  od;
  for j in [1..n] do
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

# Print( "Definitions: ", QS.Definitions, "\n" );
# Print( "Weights: ", QS.Weights, "\n" );

  # FromTheLeftCollector for the extended quotient system
  QS.Pccol := FromTheLeftCollector( Length( QS.Definitions ) );

  # copy the old collector into the new one (the relations just differ by tails and 
  # will be complete with the tails routine
  for i in [1..Q.Pccol![ PC_NUMBER_OF_GENERATORS ] ] do
    SetRelativeOrder( QS.Pccol,i, Q.Prime );
    SetPower( QS.Pccol, i, GetPower( Q.Pccol, i ) );
    for j in [i+1..Q.Pccol![ PC_NUMBER_OF_GENERATORS ] ] do
      SetConjugate( QS.Pccol, j, i, GetConjugate( Q.Pccol, j, i ) );
    od;
  od;


# Print( "Set up the definitions...\n" );
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
# Print( j, "^", k, " = ", GetConjugate( QS.Pccol, j, k ), "\n" );
    elif IsPosInt( QS.Definitions[i] ) then 
      # we add the i-th tail to the image of the
      # QS.Definitions[i]-th generator
      if IsList( QS.Imgs[ QS.Definitions[i] ] ) then 
        Append( QS.Imgs[ QS.Definitions[i] ], [i,1] );
      fi;
# Print( QS.Definitions[i], "^\pi = ", QS.Imgs[ QS.Definitions[i] ], "\n" );
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
# Print( -QS.Definitions[i], "^{p} = ", GetPower( QS.Pccol, -QS.Definitions[i] ), "\n" );
    fi;
  od;
# Print( "Imgs: ", QS.Imgs, "\n" );
  SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
# SetFeatureObj( QS.Pccol, UseLibraryCollector, true );
  FromTheLeftCollector_SetCommute( QS.Pccol );
  FromTheLeftCollector_CompletePowers( QS.Pccol );

  # run the tails routine ALGORITHM 3 in NNN98
  for b in [c+1,c..2] do
# Print( "b = ", b, "\n" );
    # [i..j] should be the generators of pseudo weigth b-1
#   i := Position( QS.Weights, b-1 );
#   j := Position( QS.Weights, b )-1;

# Display( Filtered( [1..n], x-> QS.Weights[x] = b-1 ) );
    for u in Filtered( [1..n], x-> QS.Weights[x] = b-1 )  do
      # Compute tails for p-th powers u^p with w(u) = b-1 and 
      # u is defined as a commutator [z,y] =: u
      if IsList( QS.Definitions[u] ) then 
        z := QS.Definitions[u][1];
        y := QS.Definitions[u][2];

        # z^p-1 ( zy )
        repeat 
          ev1 := ExponentsByObj( QS.Pccol, [ z, Q.Prime - 1 ] );
          repeat 
            ev2 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
          until CollectWordOrFail( QS.Pccol, ev2, [ y, 1 ] ) <> fail;
        until CollectWordOrFail( QS.Pccol, ev1, ObjByExponents( QS.Pccol, ev2 ) ) <> fail;

        # z^p y
        repeat 
          ev2 := ExponentsByObj( QS.Pccol, GetPower( Q.Pccol, z ) );
#         Display( ev2 );
        until CollectWordOrFail( QS.Pccol, ev2, [ y, 1 ] ) <> fail;

        # t := ( z^{p-1} (zy))^{-1} ((z^p)y )
        ev1 := ( ev2 - ev1 ) mod Q.Prime;

        # Add to \hat R the relation u^p = v_{u^p} t
        SetPower( QS.Pccol, u, Concatenation( GetPower( Q.Pccol, u ), ObjByExponents( QS.Pccol, ev1 ) ) );
# Print( "TAIL ", u, "^{p} = ", GetPower( QS.Pccol, u ), "\t", List( [1,3..Length(GetPower(QS.Pccol,u))-1], x->QS.Definitions[ GetPower( QS.Pccol, u )[x] ] ), "\n" );

        SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
        FromTheLeftCollector_SetCommute( QS.Pccol );
      fi;
    od;

    m:=1;
    while b-m >= m+1 do
      # [i..j] should be the generators of pseudo weigth m+1
      i := Position( QS.Weights, m+1 );
      j := Position( QS.Weights, m+2 )-1;
# Print( "m = ", m, "  b = ", b, "  c = ", Maximum( QS.Weights ), "  " , [i..j], "  ", QS.Weights{[i..j]}, "\n" );

      # Compute tails for commutators [z,u] with w(z) = b-m and w(u) = m+1
      for u in [i..j] do 
        if IsList( QS.Definitions[u] ) then 
          # case 1: u is defined by a commutator
          y := QS.Definitions[u][1];
          x := QS.Definitions[u][2];


          # for each z of w(z) = b-m with z>u
#         Display( Filtered( [u+1..n], x -> QS.Weights[x] = b-(m+1) ) );
          for z in Filtered( [u+1..n], x -> QS.Weights[x] = b-(m+1) ) do
# Display( [ z, u, [ y, x ] ] );
            if z = 3 and y = 2 and x = 1 then
              Error( "foo" );
            fi;
            # z (yx) 
            repeat 
              repeat 
                ev2 := ExponentsByObj( QS.Pccol, [ y, 1 ] );
              until CollectWordOrFail( QS.Pccol, ev2, [ x, 1 ] ) <> fail;
              ev1 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
            until CollectWordOrFail( QS.Pccol, ev1, ObjByExponents( QS.Pccol, ev2 ) ) <> fail;
#           Display( ev1 );

            # (zy)x
            repeat 
              repeat 
                ev2 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
              until CollectWordOrFail( QS.Pccol, ev2, [ y, 1 ] ) <> fail;
            until CollectWordOrFail( QS.Pccol, ev2, [ x, 1 ] ) <> fail;
#           Display( ev2 );

            # t := ((z(yx))^{-1} ((zy)x)
            ev1 := ( ev2 - ev1 ) mod Q.Prime;

            # Add to \hat R the relation [z,u] = v_[z,u] t
            SetConjugate( QS.Pccol, z, u, Concatenation( GetConjugate( Q.Pccol, z, u ), ObjByExponents( QS.Pccol, ev1 ) ) );
# Print( "TAIL ", z, "^", u, " = ", GetConjugate( QS.Pccol, z, u ), "\t", List( [1,3..Length(GetConjugate(QS.Pccol,z,u))-1], x->QS.Definitions[ GetConjugate( QS.Pccol,z,u )[x] ] ), "\n" );

            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
            FromTheLeftCollector_SetCommute( QS.Pccol );
          od;
        elif IsInt( QS.Definitions[u] ) and QS.Definitions[u] < 0 then 
          y := -QS.Definitions[u];

          # for each z of w(z) = b-m with z>u
#         Display( Filtered( [u+1..n], x -> QS.Weights[x] = b-(m+1) ) );
          for z in Filtered( [u+1..n], x -> QS.Weights[x] = b-(m+1) ) do
#         for z in Filtered( [j+1..n], x -> QS.Weights[x] = b-m ) do
# Display( [ z, u, [ y ] ] );

            # z(y^p)
            repeat 
              ev1 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
            until CollectWordOrFail( QS.Pccol, ev1, GetPower( QS.Pccol, y ) ) <> fail; 
#           Display( ev1 );

            # (zy) y^{p-1}
            repeat
              ev2 := ExponentsByObj( QS.Pccol, [ z, 1 ] );
            until CollectWordOrFail( QS.Pccol, ev2, [ y, 1, y, Q.Prime-1 ] ) <> fail;
#           Display( ev2 );

            # t := ( z(y^p) )^-1 ( (zy)y^{p-1} )
            ev1 := ( ev2 - ev1 ) mod Q.Prime;
#           Display( ev1 );

            # Add to \hat R the relation [z,u] = v_{[z,u]} t
            SetConjugate( QS.Pccol, z, u, Concatenation( GetConjugate( Q.Pccol, z, u ), ObjByExponents( QS.Pccol, ev1 ) ) );
# Print( "TAIL ", z, "^", u, " = ", GetConjugate( QS.Pccol, z, u ), "\t", List( [1,3..Length(GetConjugate(QS.Pccol,z,u))-1], x->QS.Definitions[ GetConjugate( QS.Pccol,z,u )[x] ] ), "\n" );

            SetFeatureObj( QS.Pccol, IsUpToDatePolycyclicCollector, true );
            FromTheLeftCollector_SetCommute( QS.Pccol );
          od;
        else 
          Error();
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

  # Dump the covering group to some file
  FTLCollectorPrintTo( "/tmp/coll.g", "FTL", QS.Pccol );
  end );

############################################################################
##
#F  LPRES_ConsistencyChecks( <quotient system> )
##
## Checks the local confluence of the rewriting system given by the weighted 
## nilpotent presentation. It implements the check from Nickel: "Computing 
## nilpotent quotients of finitely presented groups"
##
##	k ( j i ) = ( k j ) i,	                i < j < k, w_i+w_j+w_k <= c
##          i i^m = i^m i,                i in I, 2 w_i <= c
##          j^m i = j^(m-1) ( j i ),      i < j, j in I,   w_j+w_i <= c
##          j i^m = ( j i ) i^(m-1),      i < j, i in I,   w_j+w_i <= c
## 
## not needed (finite quotients)
##            j   = ( j i^-1 ) i,         i < j, i not in I, w_i+w_j <= c
## 
InstallGlobalFunction( LPRES_ConsistencyChecks,
  function( QS ) 
  local i,j,k,
        w,
        n,          # number of generators of the FromTheLeftCollector
        ev1, ev2,
        c,
        mat,
        Basis,
        prime,
        F,
        num,
        time;

  # keep track of the runtime for the checks
  time := Runtime();
  num := 0;

  # matrix of consequences 
  mat := [];

  # number of generators    
  n := QS.Pccol![ PC_NUMBER_OF_GENERATORS ];

  # nilpotency class 
  c := Maximum( QS.Weights);

  # prime - every generator has prime (relative) order
  prime := RelativeOrders( QS.Pccol )[1];

  # field with <prime> elements
  F := GF( prime );

  # k (j i) = (k j) i
  for k in [n,n-1..1] do
    for j in [k-1,k-2..1] do
      for i in [1..j-1] do
        if QS.Weights[i] + QS.Weights[j] + QS.Weights[k] <= c then 
# TODO: This should be equivalent to
#         repeat 
#           ev1 := ExponentsByObj( QS.Pccol, [k,1] );
#         until CollectWordOrFail( QS.Pccol, ev1, GetConjugateNC( QS.Pccol, j, i ) ) <> fail;
          repeat
            ev1 := ListWithIdenticalEntries( n, 0 );
          until CollectWordOrFail( QS.Pccol, ev1, [j,1,i,1] ) <> fail;

          w := ObjByExponents( QS.Pccol, ev1 );
          repeat
              ev1 := ExponentsByObj( QS.Pccol, [k,1] );
          until CollectWordOrFail( QS.Pccol, ev1, w )<>fail;
              
          repeat 
            ev2 := ExponentsByObj( QS.Pccol, [ k, 1 ] );
          until CollectWordOrFail( QS.Pccol, ev2, [j,1,i,1] )<>fail;
#           ev2 := ListWithIdenticalEntries( n, 0 );
#         until CollectWordOrFail( QS.Pccol, ev2, [k,1,j,1,i,1] )<>fail;
              
#         if k = 3 and j = 2 and i = 1 then 
#           Print( "ev1 = ", ev1, "\n" );
#           Print( "ev2 = ", ev2, "\n" );
#           Display( (ev1-ev2 ) );
#           Error();
#         fi;
          if not IsZero( ev1-ev2 ) then 
            Add( mat, One(F) * ( ev1-ev2 ) );
          fi;
          num := num + 1;
        else 
          # the weight function is an increasing function! 
          break;
        fi;
      od;
    od;
  od;

  # j^m i = j^(m-1) (j i)
  for j in [n,n-1..1] do
    if IsBound(QS.Pccol![ PC_EXPONENTS ][j]) then # not needed everything's finite
      for i in [1..j-1] do
        if QS.Weights[j]+QS.Weights[i]<=c then 
          repeat 
            ev1 := ListWithIdenticalEntries( n, 0 );
          until CollectWordOrFail( QS.Pccol, ev1, [j, QS.Pccol![ PC_EXPONENTS ][j]-1, j, 1, i,1] )<>fail;
              
          repeat
            ev2 := ListWithIdenticalEntries( n, 0 );
          until CollectWordOrFail( QS.Pccol, ev2, [j,1,i,1] )<>fail;
  
          w := ObjByExponents( QS.Pccol, ev2 );
          repeat 
            ev2 := ExponentsByObj( QS.Pccol, [j,QS.Pccol![ PC_EXPONENTS ][j]-1] );
          until CollectWordOrFail( QS.Pccol, ev2, w )<>fail;
  
          if not IsZero( ev1-ev2) then 
            Add( mat, One( F ) * ( ev1 - ev2 ) );
          fi;
          num := num + 1;
        else 
          break;
        fi;
      od;
    fi;
  od;

  # j i^m = (j i) i^(m-1)
  for i in [1..n] do
    if IsBound(QS.Pccol![ PC_EXPONENTS ][i]) then # not needed everything's finite
      for j in [i+1..n] do
        if QS.Weights[i]+QS.Weights[j]<=c then 
          if IsBound( QS.Pccol![ PC_POWERS ][i] ) then
            repeat 
              ev1 := ExponentsByObj( QS.Pccol, [j,1] );
            until CollectWordOrFail( QS.Pccol, ev1, QS.Pccol![ PC_POWERS ][i] );
          else 
            ev1 := ExponentsByObj( QS.Pccol, [j,1] );
          fi;
          
          repeat
            ev2 := ListWithIdenticalEntries( n, 0 );
          until CollectWordOrFail(QS.Pccol,ev2,[j,1,i,QS.Pccol![PC_EXPONENTS][i]] ) <>fail;
        
          if not IsZero( ev1-ev2 ) then 
            Add( mat, One( F ) * ( ev1-ev2 ) ); 
          fi;
          num := num + 1;
        else 
          break;
        fi;
      od;
    fi;
  od;

  # i^m i = i i^m
  for i in [1..n] do
    if IsBound( QS.Pccol![ PC_EXPONENTS ][i] ) then # not needed everything's finite
      if 2*QS.Weights[i]<=c then
        repeat
          ev1 := ListWithIdenticalEntries( n, 0 );
        until CollectWordOrFail(QS.Pccol,ev1,[i,QS.Pccol![ PC_EXPONENTS ][i]+1])<>fail;
            
        if IsBound( QS.Pccol![ PC_POWERS ][i] ) then
          repeat
            ev2 := ExponentsByObj( QS.Pccol, [i,1] );
          until CollectWordOrFail( QS.Pccol, ev2, QS.Pccol![ PC_POWERS ][i] ) <> fail;
        else
          ev2 := ExponentsByObj( QS.Pccol, [i,1] );
        fi;
            
        if not IsZero( ev1 - ev2 ) then 
          Add( mat, One(F) * ( ev1-ev2 ) );
        fi;
        num := num + 1;
      else 
        break;
      fi;
    fi;
  od;

  # only finite quotients turn up
# # j = (j -i) i 
# for i in [1..n] do
#   if not IsBound( QS.Pccol![ PC_EXPONENTS ][i] ) then
#     for j in [i+1..n] do
#       if QS.Weights[i]+QS.Weights[j]<=c then 
#         repeat
#           ev1 := ListWithIdenticalEntries( n, 0 );
#         until CollectWordOrFail( QS.Pccol, ev1, [j,1,i,-1,i,1] )<>fail;
# 
#         ev1[j] := ev1[j] - 1;
#         if not IsZero( ev1) then 
#         Add(mat,ev1);
#         fi;
#       else 
#         break;
#       fi;
#     od;
#   fi;
# od;

  # only finite quotients turn up
# # i = -j (j i)
# for j in [1..n] do
#   if not IsBound( QS.Pccol![ PC_EXPONENTS ][j] ) then
#     for i in [1..j-1] do
#       if QS.Weights[i]+QS.Weights[j]<=c then
#         repeat
#           ev1 := ListWithIdenticalEntries( n, 0 );
#         until CollectWordOrFail( QS.Pccol, ev1, [ j,1,i,1 ] )<>fail;
#   
#         w := ObjByExponents( QS.Pccol, ev1 );
#         repeat
#           ev1 := ExponentsByObj( QS.Pccol, [j,-1] );
#         until CollectWordOrFail( QS.Pccol, ev1, w )<>fail;
#             
#         if not IsZero( ev1 - ExponentsByObj( QS.Pccol, [i,1] )) then 
#         Add(mat, ev1 - ExponentsByObj( QS.Pccol, [i,1] ));
#         fi;
#           
#         # -i = -j (j -i)
#         if not IsBound( QS.Pccol![ PC_EXPONENTS ][i] ) then
#           repeat
#             ev1 := ListWithIdenticalEntries( n, 0 );
#           until CollectWordOrFail( QS.Pccol, ev1, [ j,1,i,-1 ] )<>fail;
# 
#           w := ObjByExponents( QS.Pccol, ev1 );
#           repeat
#             ev1 := ExponentsByObj( QS.Pccol, [j,-1] );
#           until CollectWordOrFail( QS.Pccol, ev1, w )<>fail;
#                 
#         if not IsZero( ExponentsByObj( QS.Pccol, [i,-1] ) - ev1) then 
#           Add( mat, ExponentsByObj( QS.Pccol, [i,-1] ) - ev1);
#           fi;
#         fi;
#       else 
#         break;
#       fi;
#     od;
#   fi;
# od;

  TriangulizeMat( mat );
  Basis := rec( mat := Filtered( mat, x -> not IsZero( x ) ) );
  Basis.Heads := List( Basis.mat, PositionNonZero );

  Info( InfoLPRES, 2, "Time spent for ", num, " consistency checks: ", StringTime( Runtime() - time ) );

  return( Basis );
  end );

# help function
LPRES_MapGenExpList := function( imgs, obj, res ) 
  local i;

  for i in [1,3..Length(obj)-1] do
    res := res * imgs[ obj[i] ] ^ obj[i+1];
  od;
  return( res );
end;

############################################################################
##
#F  LPRES_InduceSpinning
##
InstallGlobalFunction( LPRES_InduceSpinning,
  function( QS, A ) 
  local imgs, hom, F, sub, obj, i, j, k, b, mat, myField, H, G, c, fam, time;

  myField := GF( QS.Prime );

  F := FreeGroupOfLpGroup( QS.Lpres );

# 
  fam := FamilyObj( GeneratorsOfGroup( QS.Lpres )[1] );

  c := Maximum( QS.Weights )-1;
  b := Position( QS.Weights, c+1 );

  imgs := [];
  for i in [1..Length(QS.Imgs)] do
    if IsInt( QS.Imgs[i] ) then 
      imgs[i] := PcpElementByGenExpList( QS.Pccol, [ QS.Imgs[i], 1 ] );
    else
      imgs[i] := PcpElementByGenExpList( QS.Pccol, QS.Imgs[i] );
    fi;
  od;

  H := PcpGroupByCollectorNC( QS.Pccol );

  # use the latter endomorphism as this seems to be faster (polycyclic issue?)
# hom := GroupHomomorphismByImagesNC( F, H, GeneratorsOfGroup( F ), imgs );
  hom := GroupHomomorphismByImagesNC( QS.Lpres, H, GeneratorsOfGroup( QS.Lpres ), imgs );

# Append( A.Relations, List( FixedRelatorsOfLpGroup( QS.Lpres ),
#                            x -> One(myField)*Exponents( x^hom ){[b..Length(QS.Weights)]} ) );
  A.Relations := List( FixedRelatorsOfLpGroup( QS.Lpres ),
                       x -> One(myField)*Exponents( ElementOfLpGroup( fam, x )^hom ){[b..Length(QS.Weights)]} );

# Append( A.IteratedRelations, List( IteratedRelatorsOfLpGroup( QS.Lpres ),
#                                    x -> One(myField)*Exponents( x ^ hom ){ [b..Length(QS.Weights) ]} ) );
  time := Runtime();
  A.IteratedRelations := List( IteratedRelatorsOfLpGroup( QS.Lpres ),
                               x -> One(myField)*Exponents( ElementOfLpGroup( fam, x ) ^ hom ){ [b..Length(QS.Weights) ]} );
  Info( InfoLPRES, 3, "Time spent to map the iterated relations: ", StringTime( Runtime()-time ) );
 
  for sub in EndomorphismsOfLpGroup( QS.Lpres ) do
    imgs := [];
    mat := [];
    for i in [1..Length(QS.Definitions)] do
#     Display( QS.Definitions[i] );
      if IsPosInt( QS.Definitions[i] ) then
        # generator defined as an image of a generator of the LpGroup
        if IsList( QS.Imgs[QS.Definitions[i]] ) then 
          # tail added to an image x_i^\pi = w_i t <-> t = w_i^-1 * x_i^\pi
          obj := QS.Imgs[ QS.Definitions[i] ]{ [1..Length(QS.Imgs[ QS.Definitions[i] ])-2] }; # cut the tail
# Display( obj );
          imgs[i] := LPRES_MapGenExpList( imgs, obj, One(H) );
# Display( imgs[i] );
# Display( imgs[ QS.Definitions[i] ] );
          imgs[i] := imgs[i]^-1 * ElementOfLpGroup( fam, GeneratorsOfGroup( F )[ QS.Definitions[i] ]^sub ) ^hom;
        else 
          # one-to-one with the generator
          imgs[i] := ElementOfLpGroup( fam, GeneratorsOfGroup( F )[ QS.Definitions[i] ] ^ sub ) ^ hom;
        fi;
#       Display( imgs[i] );
      elif IsInt( QS.Definitions[i] ) then
        # power relation a_i^p = v_ii t <-> t = v_ii^-1 a_i^p 
        obj := GetPower( QS.Pccol, -QS.Definitions[i] );
        obj := obj{[1..Length(obj)-2]}; # cut the tail
        imgs[i] := LPRES_MapGenExpList( imgs, obj, One( H ) );
        imgs[i] := imgs[i]^-1 * imgs[ -QS.Definitions[i] ]^QS.Prime;
      elif IsList( QS.Definitions[i] ) then
        # commutator a_j^-1 a_k a_j = v_jk t <-> t = v_jk^{-1} a_j^-1 a_k a_j
        k := QS.Definitions[i][1];
        j := QS.Definitions[i][2];
        obj := GetConjugate( QS.Pccol, k, j );
        obj := obj{[1..Length(obj)-2]}; # cut the tail
        imgs[i] := LPRES_MapGenExpList( imgs, obj, One(H) );
        imgs[i] := imgs[i]^-1 * imgs[j]^-1 * imgs[k] * imgs[j];
      fi;
#     Print( imgs, "\n" );
      if QS.Weights[i] = c+1 then
        if not IsZero( Exponents( imgs[i] ){ [1..b-1] } ) then
          Error( "The L-presentation seems to be non-invariant @ i = ", i , "\n" );
        fi;
        Add( mat, One( myField ) * Exponents( imgs[i] ){[b..Length(QS.Weights)]} );
      fi;
    od;
#   Display( mat );
    Add( A.Substitutions, mat );
  od;

  end );

# help function
LPRES_AdjustObject := function( obj, b, n, Basis, Gens, F, QSPccol, QPccol )
  local i,j,ev,ev1;

  ev1 := ExponentsByObj( QSPccol, obj );
  ev := One(F) * ev1{ [b..n] };
  ev1 := ev1{[1..b-1]};

  for i in [1..Length(ev)] do
    if not IsZero( ev[i] ) then 
      j := Position( Basis.Heads, i );
      if j <> fail then 
        ev := ev - ev[i] / Basis.mat[j][i] * Basis.mat[j];
      fi;
    fi;
  od;

  Append( ev1, List( ev{Gens}, Int ) );

  return( ObjByExponents( QPccol, ev1 ) );
end;

############################################################################
##
#F  LPRES_CreateNewQuotientSystem
##
InstallGlobalFunction( LPRES_CreateNewQuotientSystem,
  function( QS, Basis ) 
  local Q,
        i,j,k,
        b,
        n,m,
        ev,ev1,
        obj,
        F,
        Imgs,
        Gens;

  # position of the first tail
  b := Position( QS.Weights, Maximum( QS.Weights ) );

  n := QS.Pccol![ PC_NUMBER_OF_GENERATORS ];

  F := GF( QS.Prime );

  Gens := Filtered( [1..Length(QS.Weights)-b+1], x -> not x in Basis.Heads );

  Q := rec( Lpres       := QS.Lpres,
            Prime       := QS.Prime,
            Definitions := ShallowCopy( QS.Definitions{[1..b-1]} ),
            Imgs        := ShallowCopy( QS.Imgs ),
            Pccol       := FromTheLeftCollector( b-1+Length(Gens) ),
            Weights     := Concatenation( QS.Weights{[1..b-1]}, QS.Weights{List(Gens,x->b-1+x)} )
            );

  for i in Gens do 
    Add( Q.Definitions, QS.Definitions[b+i-1] );
  od;

  # set up the images
  for i in [1..Length(Q.Imgs)] do
    if IsList( Q.Imgs[i] ) then 
      Q.Imgs[i] := LPRES_AdjustObject( QS.Imgs[i], b, n, Basis, Gens, F, QS.Pccol, Q.Pccol );
#     Display( Q.Imgs[i] );
    fi;
  od;
# Print( "Images: ", Q.Imgs, "\n" );

  m := Q.Pccol![ PC_NUMBER_OF_GENERATORS ];

  # set up the power relation and conjugacy relations of the first b generators
  # the new generators [b..m] are central (and have order p with trivial power
  # relation)
  for i in [1..b-1] do
    SetRelativeOrder( Q.Pccol, i, Q.Prime );
    obj := GetPower( QS.Pccol, i );
    obj := LPRES_AdjustObject( obj, b, n, Basis, Gens, F, QS.Pccol, Q.Pccol );
    SetPower( Q.Pccol, i, obj );

    for j in [i+1..b-1] do
      obj := GetConjugate( QS.Pccol, j, i );
      obj := LPRES_AdjustObject( obj, b, n, Basis, Gens, F, QS.Pccol, Q.Pccol );
      SetConjugate( Q.Pccol, j, i, obj );
    od;
  od;

  # set the order of the remaining generators (tails), they are central by def
  for i in [b..m] do
    SetRelativeOrder( Q.Pccol, i, Q.Prime );
  od;

  FromTheLeftCollector_SetCommute( Q.Pccol );
  SetFeatureObj( Q.Pccol, IsUpToDatePolycyclicCollector, true );
  FromTheLeftCollector_CompletePowers( Q.Pccol );
  FromTheLeftCollector_CompleteConjugate( Q.Pccol );
  SetFeatureObj( Q.Pccol, IsUpToDatePolycyclicCollector, true );
# UpdatePolycyclicCollector( Q.Pccol );

# if LPRES_TEST_ALL then
    if not IsConfluent( Q.Pccol ) then 
      Error( "Inconsistent presentation" );
    fi;
# fi;


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
  return( Q );
  end );
