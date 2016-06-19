############################################################################
##
#W misc.gi			LPRES				René Hartung
##

############################################################################
##
#F  LPRES_WordsOfLengthAtMostN( <list>, <n> )
##   
## returns a list of all words of <list> of length at most <n>
##
InstallGlobalFunction( LPRES_WordsOfLengthAtMostN,
  function ( list, n )
  local Words,	# list of all words
	i,g;	# loop variables

# Words:=[ [ list[1]^0 ], list ];
  Words:=[ [ One( list[1] ) ], list ];
  for i in [ 3 .. n+1 ] do 
    Add( Words, [] );
    for g in list do 
      Append( Words[i], List( Words[i-1], x -> x*g ) );
    od;
  od;
  return( Concatenation( Words ) );
  end);
