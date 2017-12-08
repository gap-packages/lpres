#############################################################################
##  
##  PackageInfo.g                  LPRES                         René Hartung 
##  
##  Based on Frank Luebeck's template for PackageInfo.g.
##  

SetPackageInfo( rec(

PackageName := "lpres",
Subtitle := "Nilpotent Quotients of L-Presented Groups",
Version := "0.4.2",
Date    := "08/12/2017",

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
  WWWHome       := "http://www.uni-math.gwdg.de/laurent",
  PostalAddress := Concatenation( [
                       "Mathematisches Institut\n",
                       "Bunsenstraße 3-5\n",
                       "D-37073 Göttingen\n",
                       "Germany" ] ),
  Place         := "Göttingen",
  Institution   := "Georg-August Universität zu Göttingen"
  )
],

Status         := "deposited",
CommunicatedBy := "Alexander Konovalov (St Andrews)",
AcceptDate     := "02/2009",

SourceRepository := rec(
    Type := "git",
    URL := Concatenation( "https://github.com/gap-packages/", ~.PackageName ),
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := Concatenation( "https://gap-packages.github.io/", ~.PackageName ),
README_URL      := Concatenation( ~.PackageWWWHome, "/README" ),
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
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := ~.Subtitle,
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation("Loading ", ~.PackageName, " ", String( ~.Version ), " ...\n"),

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
