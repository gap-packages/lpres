#############################################################################
##  
##  PackageInfo.g                  LPRES                         René Hartung 
##  
##  Based on Frank Luebeck's template for PackageInfo.g.
##  

SetPackageInfo( rec(

PackageName := "lpres",
Subtitle := "Nilpotent Quotients of L-Presented Groups",
Version := "1.1.1",
Date    := "12/07/2024", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
  LastName      := "Hartung",
  FirstNames    := "René",
  IsAuthor      := true,
  IsMaintainer  := false,
  ),
  rec(
  LastName      := "Bartholdi",
  FirstNames    := "Laurent",
  IsAuthor      := false,
  IsMaintainer  := true,
  Email         := "laurent.bartholdi@gmail.com",
  WWWHome       := "https://www.math.uni-sb.de/ag/bartholdi",
  PostalAddress := Concatenation( [
                       "FR Mathematik+Informatik\n",
                       "Universität des Saarlandes\n",
                       "D-66041 Saarbrücken\n",
                       "Germany" ] ),
  Place         := "Saarbrücken",
  Institution   := "Universität des Saarlandes"
  )
],

Status         := "accepted",
CommunicatedBy := "Olexandr Konovalov (St Andrews)",
AcceptDate     := "09/2018",

SourceRepository := rec(
    Type := "git",
    URL := Concatenation( "https://github.com/gap-packages/", ~.PackageName ),
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := Concatenation( "https://gap-packages.github.io/", ~.PackageName ),
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

AbstractHTML   := "The LPRES Package defines new GAP objects to work with \
L-presented groups, namely groups given by a finite generating set and a \
possibly-infinite set of relations given as iterates of finitely many \
seed relations by a finite set of endomorphisms. The package implements \
nilpotent quotient, Todd-Coxeter and Reidemeister-Schreier algorithms \
for L-presented groups.",

PackageDoc := rec(
  BookName  := ~.PackageName,
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := ~.Subtitle,
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation("Loading ", ~.PackageName, " ", String( ~.Version ), " ...\n"),

Dependencies := rec(
  GAP                    := ">= 4.9",
  NeededOtherPackages    := [ ["polycyclic", ">= 2.5"], 
                              ["FGA", ">= 1.1.0.1"] ], 
  SuggestedOtherPackages := [ ["ParGAP", ">= 1.1.2" ],
                              ["AutPGrp", ">= 1.4"],
                              ["ACE", ">= 5.0" ] ],
  ExternalConditions     := [ ]
),

Autoload := false,

TestFile := "tst/testall.g",

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
