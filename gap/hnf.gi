############################################################################
##
#W hnf.gi			LPRES				René Hartung
##

############################################################################
##
#F  LPRES_PowerRelationsOfHNF ( <rec> )
##
## computes the power relations w.r.t. the Hermite normal form <rec>.
##
InstallGlobalFunction( LPRES_PowerRelationsOfHNF,
  function(HNF)
  local i,j,	# loop variables
	A;	# matrix of power relations w.r.t. HNF

  A:=ShallowCopy(HNF.mat);

  # determine the power relations a_i^m_i=w(a_{i+1}..a_n) from the 
  # Hermite normal form 
  for i in [1..Length(HNF.Heads)] do
    if A[i][HNF.Heads[i]]>1 then 
      for j in [1..i-1] do
        if A[j][HNF.Heads[i]]<>0 then
          if IsInt(A[j][HNF.Heads[i]]/A[i][HNF.Heads[i]]) then 
            A[j]:=A[j]-A[j][HNF.Heads[i]]/A[i][HNF.Heads[i]] * A[i];
          elif A[j][HNF.Heads[i]]>0 then    
            A[j]:=A[j]-(QuoInt(A[j][HNF.Heads[i]],A[i][HNF.Heads[i]])+1) * A[i];
          elif A[j][HNF.Heads[i]]<-A[i][HNF.Heads[i]] then 
            A[j]:=A[j]-(QuoInt(A[j][HNF.Heads[i]],A[i][HNF.Heads[i]])) * A[i];
          fi;
        fi;
  
        if LPRES_TEST_ALL then 
          if not A[j][HNF.Heads[i]]<=0 or 
             not A[j][HNF.Heads[i]]>-A[i][HNF.Heads[i]] then 
            Error("in LPRES_PowerRelationsOfHNF");
          fi;
        fi;
      od;
    fi;
  od;

  return(A);
  end);

############################################################################
##
#F  LPRES_AddRow ( <mat> , <evec> )
##
## adds the row <evec> to the Hermite normal form <mat> and returns
## whether <mat> has changed.
##
InstallGlobalFunction( LPRES_AddRow,
  function( HNF, ev )
  local i, val, l;

  if Size( HNF.mat ) = 0 and not IsZero( ev ) then 
    Add( HNF.mat, ev );
    Add( HNF.Heads, PositionNonZero( ev ) );
    return( true );
  fi;

  # reduce a vector with the HNF
  for i in [1..Length(ev)] do
   if ev[i]<>0 then 
     l:=Position(HNF.Heads,i);
     if l<>fail then 
       val := ev[i]/HNF.mat[l][i];
       if IsInt(val) then 
         ev := ev - val*HNF.mat[l];
       else 
         break;
       fi;
     else
       break;
     fi;
   fi;
  od;
   
  if IsZero( ev ) then 
    return( false );
  fi;

  Add( HNF.mat, ev );
  HNF.mat := HermiteNormalFormIntegerMat( HNF.mat );
  HNF.mat := Filtered( HNF.mat, x -> not IsZero( x ) );
  HNF.Heads := List( HNF.mat, PositionNonZero );
   
  return( true );
  end);

############################################################################
##
#F  LPRES_RowReduce( <ev>, <HNF> )
##
## reduces the exponent vector <ev> via the Hermite normal form <HNF>.
##
InstallGlobalFunction( LPRES_RowReduce,
  function(ev,HNF)
  local i,l;	# loop variables
  
  if HNF.mat=[] then 
    return(ev);
  fi;
  
  # reduce a vector with the HNF
  for i in [1..Length(ev)] do
   if ev[i]<>0 then 
     l:=Position(HNF.Heads,i);
     if l<>fail then 
       if IsInt(ev[i]/HNF.mat[l][i]) then 
         ev:=ev-ev[i]/HNF.mat[l][i]*HNF.mat[l];
       elif ev[i]>0 then
         ev:=ev-(QuoInt(ev[i],HNF.mat[l][i]))*HNF.mat[l];
       elif ev[i]<0 then
         ev:=ev-(QuoInt(ev[i],HNF.mat[l][i])-1)*HNF.mat[l];
       fi;
     fi;
   fi;
  od;

  return(ev);
  end);
