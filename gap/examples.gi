############################################################################
##
#W examples.gi			LPRES				René Hartung
##

############################################################################
##
#F  ExamplesOfLPresentations ( <int> )
##
## returns some important examples of L-presented groups (e.g. Grigorchuk).
##
InstallGlobalFunction(ExamplesOfLPresentations,
  function( n )
  local F,F2,		# free group
        rels,		# fixed relators
        sigma,tau,	# endomorphism
        endos,		# set of endomorphisms
        itrels,		# iterated relators
        a,b,c,d,	# free group generators
        t,r,u,v, 	# free group generators
        T,U,V, 		# free group generators
        G,		# L-presented group
	e,f,g,h,i,	# for abbreviation in the Hanoi tower group
	IL;		# info level of InfoLPRES


  if n=1 then 
    # The Grigorchuk group on 4 generators
    Info(InfoLPRES,1,"The Grigorchuk group on 4 generators from [Lys85]");
    F:=FreeGroup("a","b","c","d");
    a:=F.1;;b:=F.2;;c:=F.3;;d:=F.4;;
    rels:=[a^2,b^2,c^2,d^2,b*c*d];
    sigma:=GroupHomomorphismByImagesNC(F,F,[a,b,c,d],[c^a,d,b,c]);
    endos:=[sigma];
    itrels:=[Comm(d,d^a),Comm(d,d^(a*c*a*c*a))];
    
    G:=LPresentedGroup(F,rels,endos,itrels);
    SetIsInvariantLPresentation(G,true);
    SetSize( G, infinity );
  elif n=2 then
    # The Grigorchuk group on 3 generators
    Info(InfoLPRES,1,"The Grigorchuk group on 3 generators");
    F:=FreeGroup("a","c","d");
    a:=F.1;;c:=F.2;;d:=F.3;;
    rels:=[];
    sigma:=GroupHomomorphismByImagesNC(F,F,[a,c,d],[c^a,c*d,c]);
    endos:=[sigma];
    itrels:=[a^2,Comm(d,d^a),Comm(d,d^(a*c*a*c*a))];
  
    G:=LPresentedGroup(F,rels,endos,itrels);
    SetSize( G, infinity );
  elif n=3 then 
    # The lamplighter group \Z_2 \wr \Z
    Info(InfoLPRES,1,"The lamplighter group on two lamp states");
    IL := InfoLevel( InfoLPRES );
    SetInfoLevel( InfoLPRES, 0 );
    G := LamplighterGroup( IsLpGroup, 2 );
    SetInfoLevel( InfoLPRES, IL );
    SetSize( G, infinity );
  elif n=4 then 
    # Brunner-Sidki-Vieira group
    Info(InfoLPRES,1,"The Brunner-Sidki-Vieira group");
    F:=FreeGroup("a","b");
    a:=F.1;;b:=F.2;;
    rels:=[];
    sigma:=GroupHomomorphismByImagesNC(F,F,[a,b],[b^2*a^(-1)*b^2,b^2]);
    endos:=[sigma];
    itrels:=[Comm(a,a^b),Comm(a,a^(b^3))];
  
    G:=LPresentedGroup(F,rels,endos,itrels);
    SetSize( G, infinity );
  elif n=5 then 
    # The Grigorchuk supergroup
    Info(InfoLPRES,1,"The Grigorchuk supergroup");
    F:=FreeGroup("a","b","c","d");
    a:=F.1;;b:=F.2;;c:=F.3;;d:=F.4;;
    rels:=[];
    sigma:=GroupHomomorphismByImagesNC(F,F,[a,b,c,d],[a*b*a,d,b,c]);
    endos:=[sigma];
    itrels:=[a^2,Comm(b,c),Comm(c,c^a),Comm(c,d^a),Comm(d,d^a),Comm(c^(a*b),
             (c^(a*b))^a),Comm(c^(a*b),(d^(a*b))^a),Comm(d^(a*b),(d^(a*b))^a)];
  
    G:=LPresentedGroup(F,rels,endos,itrels);
    SetSize( G, infinity );
  elif n=6 then 
    # The Fabrykowski-Gupta group
    Info(InfoLPRES,1,"The Fabrykowski-Gupta group");
    G:=GeneralizedFabrykowskiGuptaLpGroup( 3 );
    SetSize( G, infinity );
  elif n=7 then 
    # The Gupta-Sidki group
    Info(InfoLPRES,1,"The Gupta-Sidki group");
    F:=FreeGroup("a","t","u","v");
    a:=F.1;;t:=F.2;;   u:=F.3;;   v:=F.4;;
            T:=F.2^-1;;U:=F.3^-1;;V:=F.4^-1;;

    rels:=[a^3,t^3,t^a/u,t^(a^2)/v];

    itrels:=[ u*T*v*T*U*V*t*v*U*t*U*V*T*u*T*u*v*t*U*t*V*U*v*t*u*V*u*T, 
            u*T*v*T*U*V*T*U*v*U*T*V*u*T*u*v*t*U*t*V*u, 
            u*T*v*T*U*V*t*U*T*u*v*t*U*t*V*U*t*v*u*T*u*V, 
            v*T*u*T*V*U*t*v*U*t*U*V*T*u*t*U*v*t*u*V*u*T, 
            v*T*u*T*V*U*T*U*v*U*T*V*u*t*u, v*T*u*T*V*U*t*U*t*U*t*v*u*T*u*V, 
            T*v*U*t*U*V*T*u*T*v*u*t*V*t*u*v*t*u*V*u*T, U*v*U*T*V*u*T*v*u*t*V*t, 
 	    T*U*T*v*u*t*V*t*u*t*v*u*T*u*V, u*T*v*T*U*V*T*V*u*V*T*U*v*t*v, 
            u*T*v*T*U*V*t*u*V*t*V*U*T*v*t*V*u*t*v*U*v*T, 
            u*T*v*T*U*V*t*V*t*V*t*u*v*T*v*U,
            v*T*u*T*V*U*T*V*u*V*T*U*v*T*v*u*t*V*t*U*v, 
            v*T*u*T*V*U*t*u*V*t*V*U*T*v*T*v*u*t*V*t*U*V*u*t*v*U*v*T, 
            v*T*u*T*V*U*t*V*T*v*u*t*V*t*U*V*t*u*v*T*v*U, 
            V*u*V*T*U*v*T*u*v*t*U*t, T*V*T*u*v*t*U*t*v*t*u*v*T*v*U, 
            T*u*V*t*V*U*T*v*T*u*v*t*U*t*v*u*t*v*U*v*T, 
            t*U*v*U*T*V*u*v*T*u*T*V*U*t*U*t*v*u*T*u*V*T*v*u*t*V*t*U, 
            t*U*v*U*T*V*u*T*U*t*v*u*T*u*V*T*u*v*t*U*t*V, 
            t*U*v*U*T*V*U*T*v*T*U*V*t*U*t*v*u*T*u*V*t, 
            U*v*T*u*T*V*U*t*U*v*t*u*V*u*t*v*u*t*V*t*U,
            U*T*U*v*t*u*V*u*t*u*v*t*U*t*V, T*v*T*U*V*t*U*v*t*u*V*u,
            v*U*t*U*V*T*u*v*T*u*T*V*U*t*u*T*v*u*t*V*t*U, 
            v*U*t*U*V*T*u*T*u*T*u*v*t*U*t*V, v*U*t*U*V*T*U*T*v*T*U*V*t*u*t, 
            t*U*v*U*T*V*U*V*t*V*U*T*v*u*v, t*U*v*U*T*V*u*V*u*V*u*t*v*U*v*T,
            t*U*v*U*T*V*u*t*V*u*V*T*U*v*u*V*t*u*v*T*v*U, 
            V*t*V*U*T*v*U*t*v*u*T*u, U*V*U*t*v*u*T*u*v*u*t*v*U*v*T,
            U*t*V*u*V*T*U*v*U*t*v*u*T*u*v*t*u*v*T*v*U, 
            v*U*t*U*V*T*U*V*t*V*U*T*v*U*v*t*u*V*u*T*v, 
            v*U*t*U*V*T*u*V*U*v*t*u*V*u*T*V*u*t*v*U*v*T, 
            v*U*t*U*V*T*u*t*V*u*V*T*U*v*U*v*t*u*V*u*T*V*t*u*v*T*v*U, 
            V*T*V*u*t*v*U*v*t*v*u*t*V*t*U, 
            V*u*T*v*T*U*V*t*V*u*t*v*U*v*t*u*v*t*U*t*V, T*u*T*V*U*t*V*u*t*v*U*v, 
            t*V*u*V*T*U*v*T*V*t*u*v*T*v*U*T*v*u*t*V*t*U, 
            t*V*u*V*T*U*v*u*T*v*T*U*V*t*V*t*u*v*T*v*U*T*u*v*t*U*t*V, 
            t*V*u*V*T*U*V*T*u*T*V*U*t*V*t*u*v*T*v*U*t, 
            u*V*t*V*U*T*v*T*v*T*v*u*t*V*t*U,  u*V*t*V*U*T*V*T*u*T*V*U*t*v*t, 
            u*V*t*V*U*T*v*u*T*v*T*U*V*t*v*T*u*v*t*U*t*V, 
            V*U*V*t*u*v*T*v*u*v*t*u*V*u*T, U*t*U*V*T*u*V*t*u*v*T*v, 
            V*t*U*v*U*T*V*u*V*t*u*v*T*v*u*t*v*u*T*u*V, 
            t*V*u*V*T*U*v*U*v*U*v*t*u*V*u*T, t*V*u*V*T*U*V*U*t*U*V*T*u*v*u,
            t*V*u*V*T*U*v*t*U*v*U*T*V*u*v*U*t*v*u*T*u*V, 
            u*V*t*V*U*T*v*U*V*u*t*v*U*v*T*U*v*t*u*V*u*T, 
            u*V*t*V*U*T*V*U*t*U*V*T*u*V*u*t*v*U*v*T*u, 
            u*V*t*V*U*T*v*t*U*v*U*T*V*u*V*u*t*v*U*v*T*U*t*v*u*T*u*V ];

    endos:=[ GroupHomomorphismByImagesNC( F, F, [a,t,u,v],
                                          [a,t,T*v*u*t*V*t*U,T*u*v*t*U*t*V]) ];
    G:=LPresentedGroup(F,rels,endos,itrels);
  
    SetUnderlyingInvariantLPresentation(G,
           LPresentedGroup(F,[a^3],endos,itrels));
    SetSize( G, infinity );
  elif n=8 then 
    # An index-3 subgroups of the Gupta-Sidki group
    Info(InfoLPRES,1,"An index-3 subgroup of the Gupta-Sidki group");
    F:=FreeGroup("t","u","v");
    t:=F.1;    u:=F.2;    v:=F.3; 
    T:=F.1^-1; U:=F.2^-1; V:=F.3^-1; 
    rels:=[];
    endos:=[GroupHomomorphismByImagesNC(F,F,[t,u,v],
                                            [t,T*v*u*t*V*t*U,T*u*v*t*U*t*V]),
            GroupHomomorphismByImagesNC(F,F,[t,u,v],[u,v,t])];

    itrels:=[ t^3,#u^3, v^3,
            u*T*v*T*U*V*t*v*U*t*U*V*T*u*T*u*v*t*U*t*V*U*v*t*u*V*u*T, 
            u*T*v*T*U*V*T*U*v*U*T*V*u*T*u*v*t*U*t*V*u, 
            u*T*v*T*U*V*t*U*T*u*v*t*U*t*V*U*t*v*u*T*u*V, 
            v*T*u*T*V*U*t*v*U*t*U*V*T*u*t*U*v*t*u*V*u*T, 
            v*T*u*T*V*U*T*U*v*U*T*V*u*t*u, v*T*u*T*V*U*t*U*t*U*t*v*u*T*u*V, 
            T*v*U*t*U*V*T*u*T*v*u*t*V*t*u*v*t*u*V*u*T, U*v*U*T*V*u*T*v*u*t*V*t, 
 	    T*U*T*v*u*t*V*t*u*t*v*u*T*u*V, u*T*v*T*U*V*T*V*u*V*T*U*v*t*v, 
            u*T*v*T*U*V*t*u*V*t*V*U*T*v*t*V*u*t*v*U*v*T, 
            u*T*v*T*U*V*t*V*t*V*t*u*v*T*v*U,
            v*T*u*T*V*U*T*V*u*V*T*U*v*T*v*u*t*V*t*U*v, 
            v*T*u*T*V*U*t*u*V*t*V*U*T*v*T*v*u*t*V*t*U*V*u*t*v*U*v*T, 
            v*T*u*T*V*U*t*V*T*v*u*t*V*t*U*V*t*u*v*T*v*U, 
            V*u*V*T*U*v*T*u*v*t*U*t, T*u*V*t*V*U*T*v*T*u*v*t*U*t*v*u*t*v*U*v*T, 
            T*V*T*u*v*t*U*t*v*t*u*v*T*v*U, 
            t*U*v*U*T*V*u*v*T*u*T*V*U*t*U*t*v*u*T*u*V*T*v*u*t*V*t*U, 
            t*U*v*U*T*V*u*T*U*t*v*u*T*u*V*T*u*v*t*U*t*V, 
            t*U*v*U*T*V*U*T*v*T*U*V*t*U*t*v*u*T*u*V*t, 
            U*v*T*u*T*V*U*t*U*v*t*u*V*u*t*v*u*t*V*t*U,
            U*T*U*v*t*u*V*u*t*u*v*t*U*t*V, T*v*T*U*V*t*U*v*t*u*V*u,
            v*U*t*U*V*T*u*v*T*u*T*V*U*t*u*T*v*u*t*V*t*U, 
            v*U*t*U*V*T*u*T*u*T*u*v*t*U*t*V, v*U*t*U*V*T*U*T*v*T*U*V*t*u*t, 
            t*U*v*U*T*V*U*V*t*V*U*T*v*u*v, t*U*v*U*T*V*u*V*u*V*u*t*v*U*v*T,
            t*U*v*U*T*V*u*t*V*u*V*T*U*v*u*V*t*u*v*T*v*U, 
            V*t*V*U*T*v*U*t*v*u*T*u, U*V*U*t*v*u*T*u*v*u*t*v*U*v*T,
            U*t*V*u*V*T*U*v*U*t*v*u*T*u*v*t*u*v*T*v*U, 
            v*U*t*U*V*T*U*V*t*V*U*T*v*U*v*t*u*V*u*T*v, 
            v*U*t*U*V*T*u*V*U*v*t*u*V*u*T*V*u*t*v*U*v*T, 
            v*U*t*U*V*T*u*t*V*u*V*T*U*v*U*v*t*u*V*u*T*V*t*u*v*T*v*U, 
            V*T*V*u*t*v*U*v*t*v*u*t*V*t*U, 
            V*u*T*v*T*U*V*t*V*u*t*v*U*v*t*u*v*t*U*t*V, T*u*T*V*U*t*V*u*t*v*U*v, 
            t*V*u*V*T*U*v*T*V*t*u*v*T*v*U*T*v*u*t*V*t*U, 
            t*V*u*V*T*U*v*u*T*v*T*U*V*t*V*t*u*v*T*v*U*T*u*v*t*U*t*V, 
            t*V*u*V*T*U*V*T*u*T*V*U*t*V*t*u*v*T*v*U*t, 
            u*V*t*V*U*T*v*T*v*T*v*u*t*V*t*U,  u*V*t*V*U*T*V*T*u*T*V*U*t*v*t, 
            u*V*t*V*U*T*v*u*T*v*T*U*V*t*v*T*u*v*t*U*t*V, 
            V*U*V*t*u*v*T*v*u*v*t*u*V*u*T, U*t*U*V*T*u*V*t*u*v*T*v, 
            V*t*U*v*U*T*V*u*V*t*u*v*T*v*u*t*v*u*T*u*V, 
            t*V*u*V*T*U*v*U*v*U*v*t*u*V*u*T, t*V*u*V*T*U*V*U*t*U*V*T*u*v*u,
            t*V*u*V*T*U*v*t*U*v*U*T*V*u*v*U*t*v*u*T*u*V, 
            u*V*t*V*U*T*v*U*V*u*t*v*U*v*T*U*v*t*u*V*u*T, 
            u*V*t*V*U*T*V*U*t*U*V*T*u*V*u*t*v*U*v*T*u, 
            u*V*t*V*U*T*v*t*U*v*U*T*V*u*V*u*t*v*U*v*T*U*t*v*u*T*u*V ];
  
    G := LPresentedGroup(F,rels,endos,itrels);
    SetSize( G, infinity );
  elif n=9 then 
    # The Basilica group
    Info(InfoLPRES,1,"The Basilica group");
    F:=FreeGroup("a","b");
    a:=F.1; b:=F.2;
    rels:=[];
    endos:=[GroupHomomorphismByImagesNC(F,F,[a,b],[b^2,a])];
    itrels:=[Comm(a,a^b)];
    G := LPresentedGroup( F, rels, endos, itrels );
    SetSize( G, infinity );
  elif n=10 then 
    # Gilbert Baumslag's group
    Info(InfoLPRES,1,"Baumslag's group");
    F:=FreeGroup("a","b","t","u");
    a:=F.1; b:= F.2; t:=F.3; u:=F.4;
    rels:=[u/b];
    endos:=[GroupHomomorphismByImagesNC(F,F,[a,b,t,u],[a,b,t,u^t]), 
            GroupHomomorphismByImagesNC(F,F,[a,b,t,u],[a,b,t,u^(t^-1)])];
    itrels:=[ a^t/a^4, (b^2)^t/b, Comm(a,u) ];
    G := LPresentedGroup( F, rels, endos, itrels );
    SetIsInvariantLPresentation( G, false );  # as proved in [Har08];
    SetSize( G, infinity );
  elif n = 11 then 
    Info( InfoLPRES, 1, "The modified L-presentation of the Basilica Group" );
    F := FreeGroup( "a", "b" );;
    a := F.1; b := F.2;;
    rels := [];;
    endos := [ GroupHomomorphismByImagesNC( F, F, [a,b], [b^2,a] ),
               GroupHomomorphismByImagesNC( F, F, [a,b], [a*b,a^2] ) ];;
    itrels := [ Comm( a, a^b ) ];;
    G := LPresentedGroup( F, rels, endos, itrels );
    SetSize( G, infinity );
  elif n = 12 then 
    Info( InfoLPRES, 1, "The Hanoi-Tower group from [BSZ09]" );
    # as determined in Bartholdi, Siegenthaler, Zalesski, 2009
    F := FreeGroup( "a", "b", "c" );;
    a := F.1;; b := F.2;; c := F.3;;
    d := Comm( a, b );
    e := Comm( b, c );
    f := Comm( c, a );
    g := d ^ c;;
    h := e ^ a;;
    i := f ^ b;;
    rels := [ a^2, b^2, c^2 ];
    endos := [ GroupHomomorphismByImagesNC( F, F, [a,b,c], [a,b^c,c^b] ) ];
    itrels := [ d^-1*e*f*i^-1*g*e, h*e^-1*d^-1*f*d*i^-1, e^-1*g^-1*f^-1*e*g*f,
                e^-1*d*h*e^-2*d^-1*h^2, h*g*d^-2*f^-1*g*f*e^-1 ];
    G := LPresentedGroup( F, rels, endos, itrels );
    SetIsInvariantLPresentation( G, true );
    SetSize( G, infinity );
  else
    Error("<n> must be an integer less than 12");
  fi;

  return(G);
  end);


############################################################################
##
#O  FreeEngelGroup ( <num>, <n> )
##
## returns an L-presentation for the Free n-th Engel Group on <num> 
## generators; see Section~2.4 of [Har08].
##
InstallMethod( FreeEngelGroup, 
  "for positive integers", 
  true,
  [IsPosInt,IsPosInt], 0,
  function( n, c )
  local L,	# L-presented Group
        F,	# free group
	gens,	# generators of the free group
	itrel,	# commutators/iterated relator
   	i,	# loop variable
	imgs,	# loop variable to build Endos
        Endos;	# the endomorphism of the free group F 

  Info(InfoLPRES,1,"Free ",c,"-Engel group on ",n," generators");
  
  # construct an L-presentation by introducing two "stable letters"
  F:=FreeGroup( n + 2 );

  # generators of the free group
  gens:=GeneratorsOfGroup(F);

  # build the iterated relator ( [u,[u,..[u,v]]] )
  itrel:=Comm(gens[n+1],gens[n+2]);
  for i in [1..c-1] do
    itrel:=Comm(itrel,gens[n+2]);
  od;
  
  # build the endomorphisms
  Endos:=[];
  for i in [1..n] do
    imgs:=ShallowCopy(gens{[1..n]});
    Append(imgs,[gens[i]*gens[n+1],gens[n+2]]);
    Add(Endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));

    imgs:=ShallowCopy(gens{[1..n]});
    Append(imgs,[gens[n+1],gens[i]*gens[n+2]]);
    Add(Endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));

    imgs:=ShallowCopy(gens{[1..n]});
    Append(imgs,[gens[i]^-1*gens[n+1],gens[n+2]]);
    Add(Endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));

    imgs:=ShallowCopy(gens{[1..n]});
    Append(imgs,[gens[n+1],gens[i]^-1*gens[n+2]]);
    Add(Endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));
  od;
  
  return(LPresentedGroup(F,[gens[n+1],gens[n+2]],Endos,[itrel]));
  end);


############################################################################
##
#O  FreeBurnsideGroup( <num>, <exp> )
##
## returns an $L$-presentation for the free Burnside group B(m,n) on
## <num> generators with exponent <exp>; see Section~2.4 of [Har08].
##
InstallMethod( FreeBurnsideGroup,
  "for positive integers",
  true,
  [IsPosInt,IsPosInt], 0,
  function(m,n)
  local F,	# underlying free group  
	gens,	# generators of the free group F
	rels,	# fixed relators
	itrels,	# iterated relators
	endos,	# substitutions of the $L$-presentations
	imgs,	# generators images of a substitution
	j;	# loop variable

  Info(InfoLPRES,1,"The Free Burnside Group B(",m,",",n,")\n");

  # introduce a "stable letter"
  F:=FreeGroup(m+1);

  gens:=GeneratorsOfGroup(F);
  rels:=[gens[m+1]];
  itrels:=[gens[m+1]^n];
  endos:=[];
  for j in [1..m] do 
    imgs:=ShallowCopy(gens);
    imgs[m+1]:=imgs[m+1]*gens[j];
    Add(endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));

    imgs:=ShallowCopy(gens);
    imgs[m+1]:=imgs[m+1]*gens[j]^-1;
    Add(endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));
  od;
   
  return(LPresentedGroup(F,rels,endos,itrels));
  end);

############################################################################
##
#O  FreeNilpotentGroup( <num>, <c> )
##
## returns an L-presentation for the free nilpotent group of class <c> 
## on <num> generators; see Section~2.4 of [Har08].
##
InstallMethod(FreeNilpotentGroup,
  "for positive integers",
  true,
  [ IsPosInt, IsPosInt ], 0,
  function(n,c)
  local F, 	# underlying free group
	gens, 	# free generators
	i,j,	# loop variables
	rels,	# fixed relators
	itrels,	# iterated relators
	imgs,	# images under the epimorphism
	endos,	# endomorphisms
	L;	# L presented group

   Info(InfoLPRES,1,"Free nilpotent group on ",n," generators of class ",c);

   # underlying free group <n> gens + <c+1> gens for the iterated rels
   F:=FreeGroup(n+c+1);

   # free generators
   gens:=GeneratorsOfGroup(F);
   
   rels:=gens{[n+1..n+c+1]};
   itrels:=[LeftNormedComm(gens{[n+1..n+c+1]})];
   endos:=[];
   for i in [n+1..n+c+1] do
     for j in [1..n] do 
       imgs:=ShallowCopy(gens);
       imgs[i]:=imgs[i]*gens[j];
       Add(endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));

       imgs:=ShallowCopy(gens);
       imgs[i]:=imgs[i]*gens[j]^-1;
       Add(endos,GroupHomomorphismByImagesNC(F,F,gens,imgs));
     od;
   od;
   
   return(LPresentedGroup(F,rels,endos,itrels));
  end);

############################################################################
##
#O  GeneralizedFabrykowskiGuptaLpGroup ( <n> )
##
## returns an L-presentation for the generalized Fabrykowski-Gupta group for
## a positive integer <n>; for details on the L-presentation see [BEH].
##
InstallMethod( GeneralizedFabrykowskiGuptaLpGroup,
  "for a positive integer", true,
  [ IsPosInt ], 0,
  function( p )
  local F,	# underlying free group
	a,r,	# free group generators
	itrels,	# iterated relators
	endos,	# set of endomorphisms
	s,	# list of r^(a^i)'s
	i,j,m,n;# loop variables
 
  F:=FreeGroup("a","r");
  a:=F.1;; r:=F.2;
  s:=List([0..p-1],i-> r^(a^i));;
  Append(s,s);

  itrels:=[a^p];

  for m in [0..p-1] do 
    for n in [0..p-1] do 
      for i in [1..p] do 
        for j in [1..p] do 
          if AbsInt(i-j) in [2..p-2] then
           Add(itrels,Comm( s[i+1]^(s[i]^n), 
                            s[j+1]^(s[j]^m)));
          fi;
        od;   
        Add(itrels,
            s[i+1]^(s[i]^(n+1))/(s[i+1]^(s[i]^n*s[i]^((a^1*s[i]*a^-1)^m))));
      od;
    od;
  od;
  endos:=[ GroupHomomorphismByImagesNC( F, F, [a,r], [ r^(a^-1), r ]) ];
  return( LPresentedGroup( F, [], endos, itrels ) );
  end);

############################################################################
##
#M  LamplighterGroup( <fil>, <int> )
#M  LamplighterGroup( <fil>, <PcGroup> )
##
## returns an L-presentation for the lamplighter group Z_<int> \wr Z
##
InstallMethod( LamplighterGroup,
  "for the filter IsLpGroup and a positive integer",
  [ IsLpGroup, IsPosInt ], 0,
  function( filter, c )
  local F,	# underlying free group
	a,t,u,	# free group generators
	rels,	# fixed relators
	itrels,	# iterated relators
	endos,	# set of endomorphisms
	G;	# the LpGroup 

  Info(InfoLPRES,1,"The lamplighter group on ",c," lamp states");
  F:=FreeGroup( "a", "t", "u" );
  a:=F.1;;t:=F.2;;u:=F.3;;
  rels:=[ a^-1*u ];
  endos:=[ GroupHomomorphismByImagesNC(F,F,[a,t,u],[a,t,u^t]) ];
  itrels:=[a^c,Comm(a,u)];
  
  G:=LPresentedGroup( F, rels, endos, itrels );
  SetUnderlyingInvariantLPresentation(G, UnderlyingAscendingLPresentation( G ));
  return( G );
  end);

############################################################################
##
#M  LamplighterGroup( <fil>, <PcGroup> )
##
## returns an L-presentation for the lamplighter group <PcGroup> \wr Z
##
InstallMethod( LamplighterGroup,
  "for the filter IsLpGroup and a cyclic PcGroup",
  [ IsLpGroup, IsPcGroup ], 0, 
  function( filter, C )
  if not IsCyclic(C) then 
    TryNextMethod();
  else 
     return( LamplighterGroup(IsLpGroup,Size(C)) );
  fi;
  end);

############################################################################
##
#M SymmetricGroupCons
##
## `economical' L-presentations for the symmetric groups by L. Bartholdi.
##
############################################################################
InstallMethod( SymmetricGroupCons,
  "for an LpGroup and a positive integer", true,
  [ IsLpGroup, IsPosInt ], 0,
  function( filter, n )
  local F, rels, map, PHI, gens;

  if n < 3 then return( fail ); fi;

  F    := FreeGroup( n-1 );
  rels := [ F.1^2, (F.1*F.2)^3, (F.1*F.3)^2 ];

  gens := GeneratorsOfGroup( F );

  # for p = (1..n)
  PHI :=[ GroupHomomorphismByImagesNC( F, F, gens,
          Concatenation( gens{[2..n-1]}, [ F.1^Product( gens{[2..n-1]} ) ] ) ),

  # for p = (1,2)
          GroupHomomorphismByImagesNC( F, F, gens,
          Concatenation( [ F.1, F.2^F.1 ], gens{[3..n-1]} ) ),

  # for p = (3..n)
          GroupHomomorphismByImagesNC( F, F, gens,
          Concatenation( [ F.1, F.2^F.3 ], gens{[4..n-1]}, 
                         [ F.3^Product( gens{[4..n-1]} ) ] ) ) ];;

  return( LPresentedGroup( F, [], PHI, rels ) );
  end);
  
############################################################################
##
#M IA group
##
############################################################################
InstallMethod( EmbeddingOfIASubgroup, "for a free group automorphism group",
        [ IsAutomorphismGroupOfFreeGroup ],
        function(A)
# conventions on generators:
# M[i][j][k] is M_{x_i,[x_j,x_k]} if all variables i,j,k are <= n. x_i^-1 is represented as i+n.
# M[i][j][k] is a variable only if j<k. If j>k, it is stored as M[i][k][j]^-1.
# C[i][j] is C_{x_i,x_j} if i,j <= n. If j>n, then it is C_{x_i,x_{j-n}}^-1.
#
# Beware that GAP composes maps left-to-right, while the article composes maps right-to-left.
# GAP's commutator is [g,h] = g^-1h^-1gh, while the article's commutator is ghg^-1h^-1.
# Therefore, in converting an article formula to a GAP formula, multiplication order must be reversed,
# and commutators must be switched from [g,h] to [h,g].
#
    local n, F, G, C, M, i, p, q, r, s, t, u, v, rels, endos, Gendos, epi, alpha;
    
    G := Source(One(A)); # the free group
    n := RankOfFreeGroup(G);
    
    C := List(Arrangements([1..n],2),p->Concatenation("C(",String(p[1]),",",String(p[2]),")"));
    M := [];
    for p in Arrangements([1..n],3) do
        if p[2]>p[3] then continue; fi;
        Add(M,Concatenation("M(",String(p[1]),",[",String(p[2]),",",String(p[3]),"])"));
    od;
    F := FreeGroup(Concatenation(C,M));
    i := 1;
    epi := [];
    C := List([1..n],i->[]);
    for p in Arrangements([1..n],2) do
        C[p[1]][p[2]] := F.(i);
        C[p[1]][p[2]+n] := F.(i)^-1;
        q := ShallowCopy(GeneratorsOfGroup(G));
        q[p[1]] := q[p[2]]*q[p[1]]/q[p[2]];
        Add(epi,GroupHomomorphismByImages(G,q));
        i := i+1;
    od;
    M := List([1..2*n],i->List([1..2*n],j->[]));
    for p in Arrangements([1..n],3) do
        if p[2]>p[3] then continue; fi;
        M[p[1]][p[2]][p[3]] := F.(i);
        M[p[1]][p[3]][p[2]] := F.(i)^-1;
        q := ShallowCopy(GeneratorsOfGroup(G));
        q[p[1]] := Comm(q[p[2]]^-1,q[p[3]]^-1)*q[p[1]];
        Add(epi,GroupHomomorphismByImages(G,q));
        i := i+1;
            
        M[p[1]][p[3]][p[2]+n] := M[p[1]][p[2]][p[3]]^C[p[1]][p[2]];
        M[p[1]][p[2]][p[3]+n] := M[p[1]][p[3]][p[2]]^C[p[1]][p[3]];
            
        M[p[1]][p[3]+n][p[2]+n] := M[p[1]][p[2]][p[3]+n]^C[p[1]][p[2]];
        M[p[1]][p[2]+n][p[3]+n] := M[p[1]][p[3]][p[2]+n]^C[p[1]][p[3]];
            
        M[p[1]][p[3]+n][p[2]] := M[p[1]][p[2]][p[3]]^C[p[1]][p[3]];
        M[p[1]][p[2]+n][p[3]] := M[p[1]][p[3]][p[2]]^C[p[1]][p[2]];
        
        for s in [0,n] do for t in [0,n] do
            M[p[1]+n][p[2]+s][p[3]+t] := Comm(C[p[1]][p[2]+s]^-1,C[p[1]][p[3]+t]^-1) / M[p[1]][p[2]+s][p[3]+t];            
            M[p[1]+n][p[3]+s][p[2]+t] := Comm(C[p[1]][p[3]+s]^-1,C[p[1]][p[2]+t]^-1) / M[p[1]][p[3]+s][p[2]+t];            
        od; od;
    od;
    
    rels := [];
    
    # R0: M(x_a^alpha,[x_b^beta,x_c^gamma]) * M(x_a^alpha,[x_c^gamma,x_b^beta])
    # not necessary anymore, since we put only half the generators
    if true then
    for p in Arrangements([1..n],3) do
        for s in Tuples([0,n],3) do
            Add(rels,M[p[1]+s[1]][p[2]+s[2]][p[3]+s[3]]*M[p[1]+s[1]][p[3]+s[3]][p[2]+s[2]]);
        od;
    od;
    fi;
    
    # R1: [C(x_a,x_b),C(x_c,x_d)]
    for p in Arrangements([1..n],2) do
        if p<>[1,2] then continue; fi;
        for q in Arrangements([1..n],2) do
            if p[1]<>q[1] and p[1]<>q[2] and p[2]<>q[1] then
                Add(rels,Comm(C[p[1]][p[2]],C[q[1]][q[2]]));
            fi;
        od;
    od;
    
    # R2: [M(x_a^alpha,[x_b^beta,x_c^gamma]),M(x_d^delta,[x_e^epsilon,x_f^zeta])]
    for p in Arrangements([1..n],3) do for s in Tuples([0,n],3) do
        if p{[1,2]}<>[1,2] then continue; fi;
        for q in Arrangements([1..n],3) do for t in Tuples([0,n],3) do
            if q[2]>q[3] then continue; fi;
            if p[1]+s[1]<>q[1]+t[1] and not p[1] in q{[2,3]} and not q[1] in p{[2,3]} then
                Add(rels,Comm(M[p[1]+s[1]][p[2]+s[2]][p[3]+s[3]],M[q[1]+t[1]][q[2]+t[2]][q[3]+t[3]]));
            fi;    
        od; od;
    od; od;
    
    # R3: [C(x_a,x_b),M(x_c^gamma,[x_d^delta,x_e^epsilon])]
    for p in Arrangements([1..n],2) do
        if p<>[1,2] then continue; fi;
        for q in Arrangements([1..n],3) do for t in Tuples([0,n],3) do
            if not p[1] in q and not q[1] in p then
                Add(rels,Comm(C[p[1]][p[2]],M[q[1]+t[1]][q[2]+t[2]][q[3]+t[3]]));
            fi;
        od; od;
    od;
    
    # R4: [C(x_c,x_b)*C(x_a,x_b),C(x_c,x_a)]
    for p in Arrangements([1..n],3) do
        if p{[1,2]}<>[1,2] then continue; fi;
        Add(rels,Comm(C[p[3]][p[2]]*C[p[1]][p[2]],C[p[3]][p[1]]));
    od;
    
    # R5: M(x_a^alpha,[x_b^beta,x_c^gamma])^C(x_a,x_b^beta) / M(x_a^alpha,[x_c^gamma,x_b^-beta])
    # not necessary anymore, since we put only a quarter of the generators
    if true then
    for p in Arrangements([1..n],3) do for s in Tuples([0,n],3) do
        if p{[1,2]}<>[1,2] then continue; fi;
        Add(rels,M[p[1]+s[1]][p[2]+s[2]][p[3]+s[3]]^C[p[1]][p[2]+s[2]]/M[p[1]+s[1]][p[3]+s[3]][p[2]+n-s[2]]);
    od; od;
    fi;

    # R6: M(x_a^-alpha,[x_b^beta,x_c^gamma])*M(x_a^alpha,[x_b^beta,x_c^gamma]) / [C(x_a,x_c^gamma)^-1,C(x_a,x_b^beta)^-1]
    for p in Arrangements([1..n],3) do for s in Tuples([0,n],3) do
        if p{[1,2]}<>[1,2] then continue; fi;
        Add(rels,M[p[1]+n-s[1]][p[2]+s[2]][p[3]+s[3]]*M[p[1]+s[1]][p[2]+s[2]][p[3]+s[3]]/Comm(C[p[1]][p[2]+s[2]]^-1,C[p[1]][p[3]+s[3]]^-1));
    od; od;
    
    # R7: [C(x_a,x_b^beta)^-1,M(x_a^-alpha,[x_b^beta,x_c^gamma])] / [C(x_a,x_d^delta)^-1,C(x_a,x_c^gamma)^-1]
    for p in Arrangements([1..n],4) do for s in Tuples([0,n],4) do
        if p{[1,2]}<>[1,2] then continue; fi;
        Add(rels,Comm(C[p[1]][p[2]+s[2]]^-1,M[p[2]+s[2]][p[3]+s[3]][p[4]+s[4]]^-1)/Comm(C[p[1]][p[3]+s[3]]^-1,C[p[1]][p[4]+s[4]]^-1));
    od; od;
    
    # R8: M(x_a^alpha,[x_c^gamma,x_b^beta])*M(x_d^delta,[x_a^alpha,x_e^epsilon])*M(x_a^alpha,[x_b^beta,x_c^gamma])/M(x_d^delta,[x_c^gamma,x_b^beta])^C(x_d,x_e^-epsilon)/M(x_d^delta,[x_a^alpha,x_e^epsilon])/M(x_d^delta,[x_b^beta,x_c^gamma])
    for p in Arrangements([1..n],4) do for q in [1..n] do for s in Tuples([0,n],5) do p[5] := q;
        if p{[1,2]}<>[1,2] then continue; fi;
        if p[5] in p{[1,4]} then continue; fi;
        t := C[p[4]][p[5]]; if s[5]=n then t := t^-1; fi;
        Add(rels,M[p[1]+s[1]][p[3]+s[3]][p[2]+s[2]]*M[p[4]+s[4]][p[1]+s[1]][p[5]+s[5]]*
            M[p[1]+s[1]][p[2]+s[2]][p[3]+s[3]]/M[p[4]+s[4]][p[3]+s[3]][p[2]+s[2]]^C[p[4]][p[5]+n-s[5]]/M[p[4]+s[4]][p[1]+s[1]][p[5]+s[5]]/M[p[4]+s[4]][p[2]+s[2]][p[3]+s[3]]);
    od; od; od;
    
    # R9: M(x_c^gamma,[x_a^alpha,x_b^beta])^-C(x_a,x_b^beta) * M(x_c^gamma,[x_b^beta,x_a^alpha])*M(x_c^gamma,x[x_a^alpha,x_d^delta])*M(x_c^gamma,[x_a^alpha,x_b^beta])^-C(x_c,x_d^delta)
    for p in Arrangements([1..n],3) do for q in [1..n] do for s in Tuples([0,n],4) do p[4] := q;
        if p{[1,2]}<>[1,2] then continue; fi;
        if p[4] in p{[1,3]} then continue; fi;
        Add(rels,M[p[3]+s[3]][p[2]+s[2]][p[1]+s[1]]*M[p[3]+s[3]][p[1]+s[1]][p[4]+s[4]]*
            M[p[3]+s[3]][p[1]+s[1]][p[2]+s[2]]^C[p[3]][p[4]+n-s[4]]/M[p[3]+s[3]][p[1]+s[1]][p[4]+s[4]]^C[p[1]][p[2]+s[2]]);
    od; od; od;
    
    endos := [];
    Gendos := [];
    # I_1
    t := []; u := [];
    for q in Arrangements([1..n],2) do
        Add(t,C[q[1]][q[2]]);
        if 1=q[2] then Add(u,C[q[1]][q[2]]^-1); else Add(u,C[q[1]][q[2]]); fi;
    od;
    for q in Arrangements([1..n],3) do for s in Tuples([0,n],3) do
        if q[2]>q[3] or s<>[0,0,0] then continue; fi;
        Add(t,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]);
        if 1 in q then s[Position(q,1)] := n-s[Position(q,1)]; fi;
        Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]);
    od; od;
    Add(endos,GroupHomomorphismByImages(F,t,u));
    t := ShallowCopy(GeneratorsOfGroup(G));
    t[1] := t[1]^-1;
    Add(Gendos,GroupHomomorphismByImages(G,t));
    
    # P_s for all permutations s on {1..n}
    for p in GeneratorsOfGroup(SymmetricGroup(n)) do
        t := []; u := [];
        for q in Arrangements([1..n],2) do
            Add(t,C[q[1]][q[2]]);
            Add(u,C[q[1]^p][q[2]^p]);
        od;
        for q in Arrangements([1..n],3) do for s in Tuples([0,n],3) do
            if q[2]>q[3] then continue; fi;
            Add(t,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]);
            Add(u,M[q[1]^p+s[1]][q[2]^p+s[2]][q[3]^p+s[3]]);
        od; od;
        Add(endos,GroupHomomorphismByImages(F,t,u));
        Add(Gendos,GroupHomomorphismByImages(G,Permuted(GeneratorsOfGroup(G),p^-1)));
    od;
    
    # M_{x_1^alpha,x_2^beta}
    for p in Tuples([0,n],2) do
        if p[1]=0 then alpha := 1; else alpha := -1; fi;
        t := []; u := [];
        for q in Arrangements([1..n],2) do
            Add(t,C[q[1]][q[2]]);
            if q[2]=1 and q[1]<>2 then # C(x_c,x_a)
                Add(u,(C[q[1]][2+p[2]]*C[q[1]][1+p[1]])^alpha);
            elif q[1]=1 and q[2]<>2 then # C(x_a,x_c)
                Add(u,M[1+p[1]][2+n-p[2]][q[2]]*C[1][q[2]]);
            elif q[1]=2 and q[2]<>1 then # C(x_b,x_c)
                Add(u,M[1+p[1]][2+n-p[2]][q[2]+n]*C[2][q[2]]);
            elif q=[2,1] then # C(x_b,x_a)
                Add(u,(C[2][1+p[1]]*C[1][2+p[2]])^alpha);
            else
                Add(u,C[q[1]][q[2]]);
            fi;
        od;
        for q in Arrangements([1..n],3) do for s in Tuples([0,n],3) do
            if q[2]>q[3] or s<>[0,0,0] then continue; fi;
            Add(t,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]);
            if q[1]+s[1]=1+p[1] and not 2 in q then # M(x_a^alpha,[x_c^gamma,x_d^delta])
                Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]^C[1][2+p[2]]);
            elif q[2]+s[2]=1+p[1] and not 2 in q then # M(x_c^gamma,[x_a^alpha,x_d^delta])
                Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]^C[q[1]][2+n-p[2]]*M[q[1]+s[1]][2+p[2]][q[3]+s[3]]);
            elif q[2]+s[2]=1+n-p[1] and not 2 in q then # M(x_c^gamma,[x_a^-alpha,x_d^delta])
                Add(u,M[q[1]+s[1]][2+n-p[2]][q[3]+s[3]]^(C[q[1]][1]^alpha)*M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]);
            elif q[1]+s[1]=2+p[2] and not 1 in q then # M(x_b^beta,[x_c^gamma,x_d^delta])
                Add(u,(M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]*M[1+n-p[1]][q[2]+s[2]][q[3]+s[3]])^C[1][2+p[2]]);
            elif q[1]+s[1]=2+n-p[2] and not 1 in q then # M(x_b^-beta,[x_c^gamma,x_d^delta])
                Add(u,M[2+n-p[2]][q[2]+s[2]][q[3]+s[3]]*M[1+p[1]][q[2]+s[2]][q[3]+s[3]]);
            elif q[1]+s[1]=1+p[1] and q[2]+s[2]=2+p[2] then # M(x_a^alpha,[x_b^beta,x_c^gamma])
                Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]^C[1][2+p[2]]);
            elif q[1]+s[1]=1+p[1] and q[2]+s[2]=2+n-p[2] then # M(x_a^alpha,[x_b^-beta,x_c^gamma])
                Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]^C[1][2+p[2]]);
            elif q[1]+s[1]=2+p[2] and q[2]+s[2]=1+p[1] then # M(x_b^beta,[x_a^alpha,x_c^gamma])
                Add(u,M[q[1]+n-s[1]][q[2]+s[2]][q[3]+n-s[3]]*C[2][q[3]+n-s[3]]*M[q[2]+s[2]][q[1]+n-s[1]][q[3]+s[3]]*C[1][q[3]+s[3]]);
            elif q[1]+s[1]=2+p[2] and q[2]+s[2]=1+n-p[1] then # M(x_b^beta,[x_a^-alpha,x_c^gamma])
                Add(u,(C[1][q[3]+n-s[3]]*M[1+p[1]][q[3]+s[3]][2+n-p[2]]*C[2][q[3]+s[3]]*M[2+n-p[2]][q[3]+n-s[3]][1+p[1]])^(C[q[3]][1+n-p[1]]*C[q[3]][2+n-p[2]]));
            elif q[1]+s[1]=2+n-p[2] and q[2]+s[2]=1+p[1] then # M(x_b^-beta,[x_a^alpha,x_c^gamma])
                Add(u,((C[2][q[3]+s[3]]*M[q[2]+s[2]][q[3]+s[3]][q[1]+s[1]])^C[q[3]][2+p[2]]*M[q[1]+s[1]][q[3]+s[3]][q[2]+n-s[2]])^C[q[3]][1+p[1]]*C[1][q[3]+n-s[3]]);
            elif q[1]+s[1]=2+n-p[2] and q[2]+s[2]=1+n-p[1] then # M(x_b^-beta,[x_a^-alpha,x_c^gamma])
                Add(u,(C[1][q[3]+s[3]]^C[q[3]][1+n-p[1]]*M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]])^C[q[3]][2+n-p[2]]*M[1+p[1]][q[1]+s[1]][q[3]+s[3]]*C[2][q[3]+n-s[3]]);
            elif q[2]+s[2]=1+p[1] and q[3]+s[3]=2+p[2] then # M(x_c^gamma,[x_a^alpha,x_b^beta])
                Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]^C[1][2+p[2]]);
            elif q[2]+s[2]=1+p[1] and q[3]+s[3]=2+n-p[2] then # M(x_c^gamma,[x_a^alpha,x_b^-beta])
                Add(u,M[q[1]+s[1]][q[3]+n-s[3]][q[2]+s[2]]);
            else
                Add(u,M[q[1]+s[1]][q[2]+s[2]][q[3]+s[3]]);
            fi;
        od; od;
        Add(endos,GroupHomomorphismByImages(F,t,u));
        u := ShallowCopy(GeneratorsOfGroup(G));
        v := ShallowCopy(GeneratorsOfGroup(G));
        if p[2]=0 then
            u[1] := (u[2]*u[1]^alpha)^alpha;
        else
            u[1] := (u[2]^-1*u[1]^alpha)^alpha;
        fi;
        Add(Gendos,GroupHomomorphismByImages(G,u));
    od;
    F := LPresentedGroup(F,[],endos,rels);
    F!.C := C; # hack: provide access to C and M generators as convenient tables
    F!.M := M;
    F!.Gendos := Gendos; # automorphisms of G such that endos[i] acts on Aut(G) by conjugation by Gendos[i]
    return GroupHomomorphismByImagesNC(F,A,epi);
end);
