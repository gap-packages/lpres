#############################################################################
##
#W    ace.g             The GAP 4 package LPRES
##
##  Package extension, read by GAP once the ACE package is loaded: use ACE
##  for coset enumeration, unless the user replaced the default.
##

if LPRES_CosetEnumerator = CosetTableInWholeGroup then
  LPRES_CosetEnumerator := function( h )
    local f, rels, gens;

    f    := FreeGeneratorsOfFpGroup( Parent( h ) );
    rels := RelatorsOfFpGroup( Parent( h ) );
    gens := List( GeneratorsOfGroup( h ), UnderlyingElement );
    return ACECosetTable( f, rels, gens : silent, hard, max := 10^8, Wo := 10^8 );
    end;
fi;
