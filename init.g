#############################################################################
##
#W    init.g               GAP 4 package LPRES                   Rene Hartung  
##

############################################################################
## 
## Put the name of the package into a single variable. This makes it 
## easier to change it to something else if necessary.
##
LPRESPkgName:="lpres";

############################################################################
##
#D Read .gd files
##
ReadPackage( LPRESPkgName, "gap/lpres.gd");
ReadPackage( LPRESPkgName, "gap/hnf.gd");
ReadPackage( LPRESPkgName, "gap/initqs.gd");
ReadPackage( LPRESPkgName, "gap/tails.gd");
ReadPackage( LPRESPkgName, "gap/consist.gd");
ReadPackage( LPRESPkgName, "gap/cover.gd");
ReadPackage( LPRESPkgName, "gap/endos.gd");
ReadPackage( LPRESPkgName, "gap/buildnew.gd");
ReadPackage( LPRESPkgName, "gap/extqs.gd");
ReadPackage( LPRESPkgName, "gap/misc.gd");
ReadPackage( LPRESPkgName, "gap/quotsys.gd");
ReadPackage( LPRESPkgName, "gap/nq.gd");
ReadPackage( LPRESPkgName, "gap/nq_non.gd");
ReadPackage( LPRESPkgName, "gap/examples.gd");
ReadPackage( LPRESPkgName, "gap/subgrps.gd" );
ReadPackage( LPRESPkgName, "gap/reidschr.gd" );

# approximating the Schur multiplier
ReadPackage( LPRESPkgName, "gap/schumu/schumu.gd" );

# parallel version of LPRES's nilpotent quotient algorithm
if TestPackageAvailability( "ParGap", "1.1.2" ) <> fail then
  ReadPackage( LPRESPkgName, "gap/pargap/pargap.gd" );
fi;
