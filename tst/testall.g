############################################################################
##
#W  testall.gi  		The LPRES-package		René Hartung
##

LoadPackage("LPRES");
dir := DirectoriesPackageLibrary( "LPRES", "tst" );

# examples from the manual
Test(Filename(dir, "manual.tst"), rec(compareFunction := "uptowhitespace"));

# results for self-similar groups from ExamplesOfLPresentations
Test(Filename(dir, "res.tst"), rec(compareFunction := "uptowhitespace"));
