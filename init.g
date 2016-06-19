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
##    Declare the package and test for the existence of the package 
##    polycyclic.
##
DeclarePackage( LPRESPkgName , "0.0",
  function()
    
    if TestPackageAvailability( "polycyclic", "1.0" ) = fail then
      Info( InfoWarning, 1, 
           "Loading the LPRES package: package polycyclic must be available" );
      return fail;
    fi;

    return true;
end );

# install the documentation
DeclarePackageDocumentation( LPRESPkgName, "doc", "lpres", 
        "Computation of nilpotent quotients" );


############################################################################
## 
#D Require other packages (polycyclic)
##
if IsList( TestPackageAvailability( "polycyclic", "1.0" ) ) then
    HideGlobalVariables( "BANNER" );
    BANNER := false;
    LoadPackage( "polycyclic" );
    UnhideGlobalVariables( "BANNER" );
fi;

############################################################################
##
#D Read .gd files
##
# L-presentations in GAP
ReadPackage( LPRESPkgName, "gap/lpres.gd");
ReadPackage( LPRESPkgName, "gap/homs.gd");
ReadPackage( LPRESPkgName, "gap/examples.gd");

# nilpotent quotient algorithm
ReadPackage( LPRESPkgName, "gap/nql/hnf.gd");
ReadPackage( LPRESPkgName, "gap/nql/initqs.gd");
ReadPackage( LPRESPkgName, "gap/nql/tails.gd");
ReadPackage( LPRESPkgName, "gap/nql/consist.gd");
ReadPackage( LPRESPkgName, "gap/nql/cover.gd");
ReadPackage( LPRESPkgName, "gap/nql/endos.gd");
ReadPackage( LPRESPkgName, "gap/nql/buildnew.gd");
ReadPackage( LPRESPkgName, "gap/nql/extqs.gd");
ReadPackage( LPRESPkgName, "gap/nql/misc.gd");
ReadPackage( LPRESPkgName, "gap/nql/quotsys.gd");
ReadPackage( LPRESPkgName, "gap/nql/nq.gd");
ReadPackage( LPRESPkgName, "gap/nql/nq_non.gd");

# p-quotient algorithm
ReadPackage( LPRESPkgName, "gap/pql/initqs.gd" );

# subgroup methods
ReadPackage( LPRESPkgName, "gap/subgrps.gd" );
ReadPackage( LPRESPkgName, "gap/reidschr.gd" );

# approximating the Schur multiplier
ReadPackage( LPRESPkgName, "gap/schumu/schumu.gd" );

# parallel version of LPRES's nilpotent quotient algorithm
if TestPackageAvailability( "ParGap", "1.1.2" ) <> fail then
  ReadPackage( LPRESPkgName, "gap/pargap/pargap.gd" );
fi;