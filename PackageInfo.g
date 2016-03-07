#############################################################################
##  
##  PackageInfo.g                  NQL                          René Hartung 
##  
#H   @(#)$Id: PackageInfo.g,v 1.23 2010/04/01 06:53:24 gap Exp $ 
##
##  Based on Frank Luebeck's template for PackageInfo.g.
##  

SetPackageInfo( rec(

PackageName := "lpres",
Subtitle := "Nilpotent Quotients of L-Presented Groups",
Version := "0.1.0",
Date    := "06/03/2016",

Persons := [
  rec(
  LastName      := "Hartung",
  FirstNames    := "René",
  IsAuthor      := true,
  IsMaintainer  := false,
  )
  rec(
  LastName      := "Bartholdi",
  FirstNames    := "Laurent",
  IsAuthor      := false,
  IsMaintainer  := true
  )
],

#Status         := "accepted",
#CommunicatedBy := "Alexander Konovalov (St Andrews)",
#AcceptDate     := "02/2009",

PackageWWWHome := "http://github.com/...",

ArchiveFormats := ".tar.gz",
ArchiveURL     := Concatenation( ~.PackageWWWHome, "lpres-",~.Version),
README_URL     := Concatenation( ~.PackageWWWHome, "README.lpres" ),
PackageInfoURL := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),

AbstractHTML   := Concatenation( 
               "The NQL Package defines new GAP objects to work with ",
               "L-presented groups. The main part of the package is a ",
               "nilpotent quotient algorithm for L-presented groups. ",
               "That is an algorithm which takes as input an L-presented ",
               "group L and a positive integer c. It computes a polycyclic ",
               "presentation for the lower central series quotient ",
               "L/gamma_c(L)."),

                  
PackageDoc := rec(
  BookName  := "lpres",
  ArchiveURLSubset := [ "doc", "htm" ],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Nilpotent Quotient Algorithm for L-presented Groups",
  Autoload  := false 
),

AvailabilityTest := function()
    return true;
end,

Dependencies := rec(
  GAP                    := ">= 4.4",
  NeededOtherPackages    := [ ["polycyclic", ">= 2.5"], 
                              ["FGA", ">= 1.1.0.1"] ], 
  SuggestedOtherPackages := [ ["ParGAP", ">= 1.1.2" ],
                              ["AutPGrp", ">= 1.4"],
                              ["ACE", ">= 5.0" ] ],
  ExternalConditions     := [ ]
),

Autoload := false,

Keywords := [ "nilpotent quotient algorithm",
              "nilpotent presentations",
              "finitely generated groups",
              "Grigorchuk group",
              "Gupta-Sidki group",
              "L-presented groups",
              "finite index subgroup of L-presented groups", 
              "coset enumeration",
              "recursively presented groups",
              "infinite presentations",
              "commutators",
              "lower central series",
              "Free Engel groups", "Free Burnside groups",
              "computational", "parallel computing" ]
));
