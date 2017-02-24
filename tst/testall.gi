############################################################################
##
#W  testall.gi  		The LPRES-package		René Hartung
##

LoadPackage("LPRES");
dir := DirectoriesPackageLibrary( "LPRES", "tst" );

# examples from the manual
ReadTest( Filename( dir, "manual.tst" ) );

# results for self-similar groups from ExamplesOfLPresentations
ReadTest( Filename( dir, "res.tst" ) );
