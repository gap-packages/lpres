############################################################################
##
#W hnf.gd			NQL				René Hartung
##
#H   @(#)$Id: hnf.gd,v 1.2 2008/08/28 08:04:20 gap Exp $
##
Revision.("nql/gap/hnf_gd"):=
  "@(#)$Id: hnf.gd,v 1.2 2008/08/28 08:04:20 gap Exp $";


############################################################################
##
#F  LPRES_PowerRelationsOfHNF ( <rec> )
##
DeclareGlobalFunction( "LPRES_PowerRelationsOfHNF" );

############################################################################
##
#F  LPRES_ReduceHNF ( <mat> , <int> )
##
DeclareGlobalFunction( "LPRES_ReduceHNF" );

############################################################################
##
#F  LPRES_AddRow ( <mat> , <evec> )
##
DeclareGlobalFunction( "LPRES_AddRow" );

############################################################################
##
#F  LPRES_RowReduce( <ev>, <HNF> )
##
DeclareGlobalFunction( "LPRES_RowReduce" );
