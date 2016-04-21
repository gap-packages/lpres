############################################################################
##
#W pargap/consist.gi		LPRES				René Hartung
##

############################################################################
##
#F  LPRESPar_ListOfConsistencyChecks( <coll>, <weights> )
##
## parallel version for consistency-checks.
##
InstallGlobalFunction( LPRESPar_ListOfConsistencyChecks,
  function( coll, weights )
  local Checks, # list of consistency checks
	n,	# number of generators
	I, 	# list of generators with finite relative order
	c, 	# the nilpotency class
  	i,j,k;	# loop variables

  # the number of generators
  n := coll![ PC_NUMBER_OF_GENERATORS ];

  # generators with finite relative order
  I := Filtered( [1..n], x -> IsBound( coll![ PC_EXPONENTS ][x] ) );

  # the nilpotency class
  c := Maximum( weights ); 

  # initialize list of consistency checks which we send to the slaves
  Checks := [];

  # k ( j i ) = ( k j ) i
  for k in [ n, n-1 .. 1 ] do
    for j in [ k-1, k-2 .. 1 ] do 
      for i in [ 1 .. j-1 ] do
        if weights[i] + weights[j] + weights[k] <= c then 
          Add( Checks, [ k, j, i ] );
        else
          break;
        fi;
      od;
    od;
  od;

  # j ^ m i = j ^ (m-1) ( j i )
  for j in Reversed( I ) do
    for i in [ 1 .. j-1 ] do 
      if weights[j] + weights[i] <= c then 
        Add( Checks, [ -j, i ] );
      else
        break;
      fi;
    od;
  od;

  # j i ^ m  = ( j i ) i ^ (m-1)
  for i in I do
    for j in [ i+1 .. n ] do 
      if weights[i] + weights[j] <= c then 
        Add( Checks, [ j, -i ] );
      else
        break;
      fi;
    od;
  od;

  # i ^ m i = i i ^ m 
  for i in [ 1 .. n ] do
    if IsBound(  coll![ PC_EXPONENTS ][i] ) then 
      if 2 * weights[i] <= c then 
        Add( Checks, [ i ] );
      else
        break;
      fi;
    fi;
  od;

  # j = ( j i ^ -1 ) i 
  for i in [ 1 .. n ] do 
    if not IsBound( coll![ PC_EXPONENTS ][i] ) then 
      for j in [ i+1 .. n ] do 
        if weights[i] + weights[j] <= c then 
          Add( Checks, [ -j, -i ] );
        else
          break;
        fi;
      od;
    fi;
  od;

  # i = j ^ -1 ( j i )
  for j in [ 1 .. n ] do
    if not IsBound( coll![ PC_EXPONENTS ][j] ) then 
      for i in [ 1 .. j-1 ] do 
        if weights[i] + weights[j] <= c then 
          Add( Checks, [ -i, -j ] );

          if not IsBound( coll![ PC_EXPONENTS ][i] ) then 
            # i ^ -1 = j ^ -1 ( j i ^ -1 )
            Add( Checks, [ j, i ] );
          fi;
        else
          break;
        fi;
      od;
    fi;
  od;

  return( Checks );
  end);

############################################################################
##
#F  LPRESPar_CheckConsRel( <coll>, <job> )
##
## function for checking overlaps on the slaves.
##
InstallGlobalFunction( LPRESPar_CheckConsRel,
  function( coll, job )
  local ev1,ev2,# exponent vectors
	w,	# ExtRepOfObj of a PcpElement
	n,	# number of generators
	m,	# relative order of a generator
	i,j,k;	# loop variables
  
  # number of generators
  n := coll![ PC_NUMBER_OF_GENERATORS ];

  if Length( job ) = 3 then 
    # k ( j i ) = ( k j ) i
    k := job[1]; j := job[2]; i := job[3];
    repeat
      ev1 := ListWithIdenticalEntries( n, 0 );;
    until CollectWordOrFail( coll, ev1, [ j, 1, i, 1 ] ) <> fail;
  
    w := ObjByExponents( coll, ev1 );
    repeat
      ev1 := ExponentsByObj( coll, [ k, 1 ] );
    until CollectWordOrFail( coll, ev1, w ) <> fail;

    repeat
      ev2 := ListWithIdenticalEntries( n, 0 );
    until CollectWordOrFail( coll, ev2, [ k, 1, j, 1, i, 1 ] ) <> fail;;

    return( ev1 - ev2 );
  fi;

  if Length( job ) = 2 then 
    if ( IsPosInt( - job[1] ) and IsPosInt( job[2] ) ) and 
                   - job[1] > job[2]  then 

      # j ^ m i = j ^ (m-1) ( j i )
      j := -job[1]; i := job[2];
      m := coll![ PC_EXPONENTS ][j];

      repeat
        ev1 := ListWithIdenticalEntries( n, 0 );
      until CollectWordOrFail( coll, ev1, [ j, m-1, j, 1, i, 1 ] ) <> fail;

      repeat
        ev2 := ListWithIdenticalEntries( n, 0 );
      until CollectWordOrFail( coll, ev2, [ j, 1, i, 1 ] ) <> fail;

      w := ObjByExponents( coll, ev2 );
      repeat
        ev2 := ExponentsByObj( coll, [ j, m-1 ] );
      until CollectWordOrFail( coll, ev2, w ) <> fail;
      
      return( ev1 - ev2 );
    elif ( IsPosInt( job[1] ) and IsPosInt( - job[2] ) ) and 
                     job[1] > - job[2] then 
      # j i ^ m  = ( j i ) i ^ (m-1)
      j := job[1]; i := - job[2];
      m := coll![ PC_EXPONENTS ][i];

      if IsBound( coll![ PC_POWERS ][i] ) then 
        repeat
          ev1 := ExponentsByObj( coll, [ j, 1 ]);
        until CollectWordOrFail( coll, ev1, coll![ PC_POWERS ][i] ) <> fail;
      else
        ev1 := ExponentsByObj( coll, [ j, 1 ] );
      fi;
  
      repeat
        ev2 := ListWithIdenticalEntries( n, 0 );
      until CollectWordOrFail( coll, ev2, [ j, 1, i, m ] ) <> fail;

      return( ev1 - ev2 );
    elif ( IsPosInt( - job[1] ) and IsPosInt( - job[2] ) ) then
      if - job[1] > - job[2] then 
        # j = ( j i ^ -1 ) i
        j := - job[1]; i := - job[2];
        repeat
          ev1 := ListWithIdenticalEntries( n, 0 );
        until CollectWordOrFail( coll, ev1, [ j, 1, i, -1, i, 1 ] ) <> fail;
        ev1[j] := ev1[j] - 1;

        return( ev1 );
      elif - job[1] < - job[2] then 
        # i = j ^ -1 ( j i )
        j := - job[2]; i := - job[1];
        repeat
          ev1 := ListWithIdenticalEntries( n, 0 );
        until CollectWordOrFail( coll, ev1, [ j, 1, i, 1 ] ) <> fail;
        
        w := ObjByExponents( coll, ev1 );
        repeat
          ev1 := ExponentsByObj( coll, [ j, -1 ] );
        until CollectWordOrFail( coll, ev1, w ) <> fail;
  
        return( ev1 - ExponentsByObj( coll, [ i, 1 ] ) );
      else
        Error("in LPRESPar_CheckConsistencyRelations");
      fi; 
    elif IsPosInt( job[1] ) and IsPosInt( job[2] ) then
      #  i ^ -1 = j ^ -1 ( j i ^-1 )
      j := job[1]; i := job[2];

      repeat
        ev1 := ListWithIdenticalEntries( n, 0 );
      until CollectWordOrFail( coll, ev1, [ j, 1, i, -1 ] ) <> fail;

      w := ObjByExponents( coll, ev1 );
      repeat
        ev1 := ExponentsByObj( coll, [ j, -1 ] );
      until CollectWordOrFail( coll, ev1, w ) <> fail;
     
      return( ExponentsByObj( coll, [ i, -1 ] ) - ev1 );
    fi;
  fi;
  
  if Length( job ) = 1 then
    # i ^ m i = i i ^ m
    i := job[1];
    m := coll![ PC_EXPONENTS ][i];
    
    repeat
      ev1 := ListWithIdenticalEntries( n, 0 );
    until CollectWordOrFail( coll, ev1, [ i, m+1 ] ) <> fail;

    if IsBound( coll![ PC_POWERS ][i] ) then 
      repeat
        ev2 := ExponentsByObj( coll, [ i, 1 ] );
      until CollectWordOrFail( coll, ev2, coll![ PC_POWERS ][i] ) <> fail;
    else
      ev2 := ExponentsByObj( coll, [ i, 1 ] );
    fi;
    return( ev1 - ev2 );
  fi;

  Error("still missing consistency check");
  end);

############################################################################,
##
#F  LPRESPar_MSCheckConsistencyRelations( <weights> )
##
ParInstallTOPCGlobalFunction( "LPRESPar_MSCheckConsistencyRelations",
  function( weights )
  local	HNF,	# the Hermite normal form
  	Checks,	# a list of consistency checks
	b,	# position of the first tail
  	n,	# number of generators of the collector
	SubmitTaskInput, DoTask, CheckTaskResult; # MasterSlave-functions

  # initialization
  HNF := rec( mat := [], Heads := [] );
  n := ReadEvalFromString( "ftl" )![ PC_NUMBER_OF_GENERATORS ];
  b := Position( weights, Maximum( weights ));

  Checks := LPRESPar_ListOfConsistencyChecks( ReadEvalFromString("ftl"), 
                                            weights ); 

  SubmitTaskInput := TaskInputIterator( Checks );

  DoTask := x -> LPRESPar_CheckConsRel( ReadEvalFromString("ftl"), x ){[b..n]};

  CheckTaskResult := function( input, output )
    local ev;
    for ev in output do LPRES_AddRow( HNF, ev ); od;
    return NO_ACTION;
    end;

  MasterSlave( SubmitTaskInput, DoTask, CheckTaskResult, Error, 5 );

  return( HNF );
  end);


############################################################################
##
#F LPRESPar_CheckConsistencyRelations( <coll>, <weights> )
##
InstallGlobalFunction( LPRESPar_CheckConsistencyRelations,
  function( ftl, weights )
  local HNF;

  # define the collector on the slaves
  ParEval( PrintToString( "fnc:=", LPRESPar_CollectorToFunction( ftl ) ) );
  ParEval( "ftl := fnc();");
  ParEval( "Unbind( fnc );" );
  
  # parallel consistency checks
  HNF := LPRESPar_MSCheckConsistencyRelations( weights );

  return( HNF );
  end);
