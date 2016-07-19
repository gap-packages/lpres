############################################################################
##
#W  pjql.tst 			The LPRES-package	        René Hartung
##

gap> START_TEST("Checking the new quotient methods...");
gap> IL := InfoLevel( InfoLPRES );;
gap> G := ExamplesOfLPresentations(1);;
gap> SetInfoLevel( InfoLPRES,1 );
gap> Size( NqPQuotient( G, 2, 16 ) ) = Size( JenningsQuotient( G, 2, 16 ) );
#I  2-Class 2: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 3: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 4: 1 generators with relative orders: [ 2 ]
#I  2-Class 5: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 6: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 7: 1 generators with relative orders: [ 2 ]
#I  2-Class 8: 1 generators with relative orders: [ 2 ]
#I  2-Class 9: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 10: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 11: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 12: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 13: 1 generators with relative orders: [ 2 ]
#I  2-Class 14: 1 generators with relative orders: [ 2 ]
#I  2-Class 15: 1 generators with relative orders: [ 2 ]
#I  2-Class 16: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 2: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 3: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 4: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 5: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 6: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 7: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 8: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 9: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 10: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 11: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 12: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 13: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 14: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 15: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 16: 2 generators with relative orders: [ 2, 2 ]
true
gap> Size( NqPQuotient( G, 2, 32 ) ) = Size( JenningsQuotient( G, 2, 32 ) );
#I  2-Class 17: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 18: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 19: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 20: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 21: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 22: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 23: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 24: 2 generators with relative orders: [ 2, 2 ]
#I  2-Class 25: 1 generators with relative orders: [ 2 ]
#I  2-Class 26: 1 generators with relative orders: [ 2 ]
#I  2-Class 27: 1 generators with relative orders: [ 2 ]
#I  2-Class 28: 1 generators with relative orders: [ 2 ]
#I  2-Class 29: 1 generators with relative orders: [ 2 ]
#I  2-Class 30: 1 generators with relative orders: [ 2 ]
#I  2-Class 31: 1 generators with relative orders: [ 2 ]
#I  2-Class 32: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 17: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 18: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 19: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 20: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 21: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 22: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 23: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 24: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 25: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 26: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 27: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 28: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 29: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 30: 2 generators with relative orders: [ 2, 2 ]
#I  Jennings-Class 31: 1 generators with relative orders: [ 2 ]
#I  Jennings-Class 32: 2 generators with relative orders: [ 2, 2 ]
true
gap> Size( NqPQuotient( G, 2, 8 ) ) = Size( JenningsQuotient( G, 2, 8 ) );
true
gap> H := JenningsQuotient(G,2,32);
Pcp-group with orders [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ]
gap> jcs := JenningsSeries( H );;
gap> iso := IsomorphismPcGroup( H );
[ g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, g16, g17, g18, g19, g20, g21, g22, g23, g24, g25, g26, 
  g27, g28, g29, g30, g31, g32, g33, g34, g35, g36, g37, g38, g39, g40, g41, g42, g43, g44, g45, g46, g47, g48, g49, g50 ] -> 
[ f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, 
  f27, f28, f29, f30, f31, f32, f33, f34, f35, f36, f37, f38, f39, f40, f41, f42, f43, f44, f45, f46, f47, f48, f49, f50 ]
gap> JCS := JenningsSeries( Range( iso ) );;
gap> ForAll( [1..Length(jcs)], x -> Image( iso, jcs[x] ) = JCS[x] );
true
gap> h := NqPQuotient( G, 2, 32 );;
gap> pcs := PCentralSeries( h );;
gap> isoh := IsomorphismPcGroup( h );;
gap> PCS := PCentralSeries( Range( isoh ));;
gap> ForAll( [1..Length(pcs)], x -> Image( isoh, pcs[x] ) = PCS[x] );
true
gap> G := QuaternionGroup( 2^10 );
<pc group of size 1024 with 10 generators>
gap> SetInfoLevel( InfoLPRES, 1 );
gap> H := JenningsQuotient( G, 2 );
#I  Jennings-Class 2: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 3
#I  Jennings-Class 3: 0 generators with relative orders: [  ]
#I  Jennings-Class 4: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 7
#I  Jennings-Class 7: 0 generators with relative orders: [  ]
#I  Jennings-Class 8: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 15
#I  Jennings-Class 15: 0 generators with relative orders: [  ]
#I  Jennings-Class 16: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 31
#I  Jennings-Class 31: 0 generators with relative orders: [  ]
#I  Jennings-Class 32: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 63
#I  Jennings-Class 63: 0 generators with relative orders: [  ]
#I  Jennings-Class 64: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 127
#I  Jennings-Class 127: 0 generators with relative orders: [  ]
#I  Jennings-Class 128: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 255
#I  Jennings-Class 255: 0 generators with relative orders: [  ]
#I  Jennings-Class 256: 1 generators with relative orders: [ 2 ]
#I  Trivial section found - enlarged the class to 511
#I  Jennings-Class 511: 0 generators with relative orders: [  ]
#I  The group has a maximal Jennings-quotient of Jennings-class 256
Pcp-group with orders [ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ]
gap> jcs := JenningsSeries( H );;
gap> iso := IsomorphismPcGroup( H );;
gap> JCS := JenningsSeries( Range( iso ) );;
gap> ForAll( [1..Length(jcs)], x -> Image( iso, jcs[x] ) = JCS[x] );
true
gap> G := Source( EmbeddingOfIASubgroup( AutomorphismGroup( FreeGroup( 3 ) ) ) );
<invariant LpGroup on the generators [ C(1,2), C(1,3), C(2,1), C(2,3), C(3,1), C(3,2), M(1,[2,3]), M(2,[1,3]), M(3,[1,2]) ]>
gap> H := NilpotentQuotient( G, 3 );
#I  Class 1: 9 generators with relative orders: [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
#I  Class 2: 18 generators
#I  Class 3: 69 generators
Pcp-group with orders [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 4, 2, 12, 2, 3, 
  2, 0, 2, 2, 4, 12, 0, 2, 12, 0, 0, 2, 2, 2, 4, 12, 4, 4, 12, 2, 0, 12, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 6, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
gap> h := TorsionFreeNilpotentQuotient( G, 3 );
#I  Rational-Class 2: 18 generators
#I  Rational-Class 3: 66 generators
Pcp-group with orders [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 3, 
  2, 0, 2, 2, 2, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
gap> lcs := LowerCentralSeriesOfGroup( H );;
gap> rcs := RationalLowerCentralSeries( h );;
gap> List( [1..Length(lcs)-1], x-> AbelianInvariants( lcs[x]/lcs[x+1] ) );
[ [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3 ] ]
gap> List( [1..Length(rcs)-1], x-> AbelianInvariants( rcs[x]/rcs[x+1] ) );
[ [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] ]
gap> List( last2, Collected );
[ [ [ 0, 9 ] ], [ [ 0, 18 ] ], [ [ 0, 43 ], [ 2, 14 ], [ 3, 9 ] ] ]
gap> List( last2, Collected );
[ [ [ 0, 9 ] ], [ [ 0, 18 ] ], [ [ 0, 43 ] ] ]
gap> SetInfoLevel( InfoLPRES, IL );
gap> STOP_TEST( "pjql.tst", 100000);
