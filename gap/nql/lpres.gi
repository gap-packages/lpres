############################################################################
##
#W gap/nql/lpres.gi			LPRES				René Hartung
##

#############################################################################
##
#M  IsInvariantLPresentation
##
## check whether the endomorphism of the free group induce endomorphisms of
## the multiplier and if we can extend the quotient system
## (this method may determine whether the group is NOT invariantly L-presented)
InstallMethod( IsInvariantLPresentation,
  "using the nilpotent quotient algorithm", true,
  [ IsLpGroup ], 0,
  function( G )
  local g,	# a copy of the original LpGroup <g>
        H;	# nilpotent quotients of <g>
  
  Info( InfoWarning, 1, "using the nilpotent quotient algorithm" );
  g:= LPresentedGroup( FreeGroupOfLpGroup(G), 
                       FixedRelatorsOfLpGroup(G),
                       EndomorphismsOfLpGroup(G),
                       IteratedRelatorsOfLpGroup(G) );
  
  # try the nilpotent quotient algorithm assuming that <G> is invariant
  SetIsInvariantLPresentation( g, true );

  H := NilpotentQuotient( g );
  if H = fail then 
    return(false);
  fi;
  end);

#############################################################################
##
#M  \= ( <elm1>, <elm2> )  . . . . . for two elements of an L-presented group 
##
InstallMethod( \=, 
   "for elements of an L-presented group",
   IsIdenticalObj, 
   [ IsElementOfLpGroup, IsElementOfLpGroup ], 0,
   function( left, right )
   local epi, 		 # epimorphism onto a nilpotent quotient of the group
         Grp,		  # the LpGroup of <left>
       	 truth, 	# the TRUTH ;)		
	        c;	    	# class of the nilpotent quotient 

   if UnderlyingElement(left) = UnderlyingElement(right) then 
     return(true);
   else
     # use the nilpotent quotient algorithm and seek for a quotient 
     # where both elements differ
     Info( InfoWarning, 1, "EQ using the nilpotent quotient algorithm" );

     # recover the group from the element
     Grp:=CollectionsFamily(FamilyObj(left))!.wholeGroup;
  
     c:=1;
     while true do 
       epi:=NqEpimorphismNilpotentQuotient(Grp,c);
       if left^epi <> right^epi then 
         return(false);
       elif HasLargestNilpotentQuotient(Grp) then 
         Info(InfoWarning,1,"EQ fails (found a maximal nilpotent quotient)");
         TryNextMethod();
       fi;
       c:=c+1;
     od;
   fi;
   end );
