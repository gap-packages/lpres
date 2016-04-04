############################################################################
##
#W  consist.gd 			LPRES				René Hartung
##

############################################################################
##
#F  LPRES_CheckConsistencyRelations ( <coll> , <weights> )
##
## This function checks the local confluence (or consistency) of a weighted 
## nilpotent presentation. It implements the check from Nickel: "Computing 
## Nilpotent Quotients of Finitely Presented Groups"
##
##	k ( j i ) = ( k j ) i,	          i < j < k, w_i=1, w_i+w_j+w_k <= c
##          j^m i = j^(m-1) ( j i ),      i < j, j in I,   w_j+w_i <= c
##          j i^m = ( j i ) i^(m-1),      i < j, i in I,   w_j+w_i <= c
##          i i^m = i^m i,                i in I, 2 w_i <= c
##            j   = ( j i^-1 ) i,         i < j, i not in I, w_i+w_j <= c
## 
DeclareGlobalFunction( "LPRES_CheckConsistencyRelations" );
