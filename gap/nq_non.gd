############################################################################
##
#W nq_non.gd			NQL				René Hartung
##
#H   @(#)$Id: nq_non.gd,v 1.2 2008/08/28 08:59:06 gap Exp $
##
Revision.("nql/gap/nq_non_gd"):=
  "@(#)$Id: nq_non.gd,v 1.2 2008/08/28 08:59:06 gap Exp $";


############################################################################
##
#F  LPRES_TerminatedNonInvariantNQ( <LpGroup>, <QS> )
##
DeclareGlobalFunction("LPRES_TerminatedNonInvariantNQ");

BindGlobal( "InfoLPRES_MAX_GENS", 10);
