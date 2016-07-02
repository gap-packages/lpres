#############################################################################
##
#W    read.g            The GAP 4 package LPRES                    René Hartung
##
#H   @(#)$Id: read.g,v 1.7 2010/03/17 13:09:28 gap Exp $
##


if TestPackageAvailability( "Polycyclic", "2.5" ) <> fail then 
  # problem with polycyclic's Igs vs. Cgs
  USE_CANONICAL_PCS := true;
fi;

BindGlobal( "LPRES_TEST_ALL", false);

# L-presentations in GAP
ReadPackage( LPRESPkgName, "gap/misc.gi");
ReadPackage( LPRESPkgName, "gap/lpres.gi");
ReadPackage( LPRESPkgName, "gap/examples.gi");

# SUBGROUP METHODS
# coset enumeration for (finite index) subgroups of LpGroups
LPRES_TCSTART := 2;
if TestPackageAvailability( "ACE", "5.0" ) <> fail then
  LPRES_CosetEnumerator := function( h )
    local f, rels, gens;

    f    := FreeGeneratorsOfFpGroup( Parent( h ) );
    rels := RelatorsOfFpGroup( Parent( h ) );
    gens := List( GeneratorsOfGroup( h ), UnderlyingElement );
    return( ACECosetTable( f, rels, gens : silent, hard, max := 10^8, Wo := 10^8 ) );
    end;
else
  LPRES_CosetEnumerator := CosetTableInWholeGroup;
fi;
ReadPackage( LPRESPkgName, "gap/subgrps.gi" );
ReadPackage( LPRESPkgName, "gap/reidschr.gi" );

if TestPackageAvailability( "Polycyclic", "2.5" ) <> fail then 
  # nilpotent quotient algorithm as in NQL
  ReadPackage( LPRESPkgName, "gap/nql/misc.gi");
  ReadPackage( LPRESPkgName, "gap/nql/homs.gi");
  ReadPackage( LPRESPkgName, "gap/nql/subgrps.gi" );
  ReadPackage( LPRESPkgName, "gap/nql/hnf.gi");
  ReadPackage( LPRESPkgName, "gap/nql/initqs.gi");
  ReadPackage( LPRESPkgName, "gap/nql/tails.gi");
  ReadPackage( LPRESPkgName, "gap/nql/consist.gi");
  ReadPackage( LPRESPkgName, "gap/nql/cover.gi");
  ReadPackage( LPRESPkgName, "gap/nql/endos.gi");
  ReadPackage( LPRESPkgName, "gap/nql/buildnew.gi");
  ReadPackage( LPRESPkgName, "gap/nql/extqs.gi");
  ReadPackage( LPRESPkgName, "gap/nql/quotsys.gi");
  ReadPackage( LPRESPkgName, "gap/nql/nq.gi");
  ReadPackage( LPRESPkgName, "gap/nql/nq_non.gi");
  ReadPackage( LPRESPkgName, "gap/nql/lpres.gi");

  ReadPackage( LPRESPkgName, "gap/nql/subgroups.gi" );
  
  # p-quotient algorithm
  ReadPackage( LPRESPkgName, "gap/pql/initqs.gi" );
  ReadPackage( LPRESPkgName, "gap/pql/extqs.gi" );
  ReadPackage( LPRESPkgName, "gap/pql/pq.gi" );

  # Jennings quotient
  ReadPackage( LPRESPkgName, "gap/jql/extqs.gi" );
  ReadPackage( LPRESPkgName, "gap/jql/jq.gi" );
  
  # approximating the Schur multiplier
  ReadPackage( LPRESPkgName, "gap/schumu/schumu.gi" );
  
  # parallel version of LPRES's nilpotent quotient algorithm
  if TestPackageAvailability( "ParGap", "1.1.2" ) <> fail then
    LPRESPar_StoreResults := true;
    ReadPackage( LPRESPkgName, "gap/pargap/misc.gi" );
    ReadPackage( LPRESPkgName, "gap/pargap/consist.gi" );
    ReadPackage( LPRESPkgName, "gap/pargap/induce.gi" );
    ReadPackage( LPRESPkgName, "gap/pargap/pargap.gi" );
    ReadPackage( LPRESPkgName, "gap/pargap/storing.gi" );
  fi;
fi;

