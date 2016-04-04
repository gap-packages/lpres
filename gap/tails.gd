############################################################################
##
#W tails.gd			LPRES				René Hartung
##

############################################################################
## 
#F  LPRES_Tails_lji ( <coll> , <Def of k> , <l> , <k>)  
##
## computes t_{kl}^{++}
##
DeclareGlobalFunction( "LPRES_Tails_lji" );

############################################################################
## 
#F  LPRES_Tails_lkk ( <coll> , <l> , <k>) 
##
## computes t_{kl}^{-+}
##
DeclareGlobalFunction( "LPRES_Tails_lkk" );

############################################################################
##  
#F  LPRES_Tails_llk ( <coll> , <l> , <k>) 
##
## computes t_{kl}^{+-} AND t_{kl}^{--}
##
DeclareGlobalFunction( "LPRES_Tails_llk" );

############################################################################
##
#M  UpdateNilpotentCollector ( <coll>, <weights>, <defs> )
##
DeclareOperation( "UpdateNilpotentCollector",
  	[ IsFromTheLeftCollectorRep, IsList, IsList]);
