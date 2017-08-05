############################################################################
##
#W  testall.gi  		The LPRES-package		René Hartung
##

LoadPackage("LPRES");
dir := DirectoriesPackageLibrary( "LPRES", "tst" );
TestDirectory(dir, rec(exitGAP := true,
    testOptions:=rec(compareFunction := "uptowhitespace")));
FORCE_QUIT_GAP(1);
