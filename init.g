#############################################################################
##
#W    init.g               GAP 4 package LPRES                     Rene Hartung  
##
#H   @(#)$Id: init.g,v 1.6 2010/03/17 13:09:28 gap Exp $
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
ReadPkg( LPRESPkgName, "gap/lpres.gd");
ReadPkg( LPRESPkgName, "gap/hnf.gd");
ReadPkg( LPRESPkgName, "gap/initqs.gd");
ReadPkg( LPRESPkgName, "gap/homs.gd");
ReadPkg( LPRESPkgName, "gap/tails.gd");
ReadPkg( LPRESPkgName, "gap/consist.gd");
ReadPkg( LPRESPkgName, "gap/cover.gd");
ReadPkg( LPRESPkgName, "gap/endos.gd");
ReadPkg( LPRESPkgName, "gap/buildnew.gd");
ReadPkg( LPRESPkgName, "gap/extqs.gd");
ReadPkg( LPRESPkgName, "gap/misc.gd");
ReadPkg( LPRESPkgName, "gap/quotsys.gd");
ReadPkg( LPRESPkgName, "gap/nq.gd");
ReadPkg( LPRESPkgName, "gap/nq_non.gd");
ReadPkg( LPRESPkgName, "gap/examples.gd");
ReadPkg( LPRESPkgName, "gap/subgrps.gd" );

# approximating the Schur multiplier
ReadPkg( LPRESPkgName, "gap/schumu/schumu.gd" );

# approximating the outer automorphism group
ReadPkg( LPRESPkgName, "gap/misc/autseq.gd" );

# parallel version of LPRES's nilpotent quotient algorithm
if TestPackageAvailability( "ParGap", "1.1.2" ) <> fail then
  ReadPkg( LPRESPkgName, "gap/pargap/pargap.gd" );
fi;
