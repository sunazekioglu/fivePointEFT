(* vec4num[numDots_] are the independent s-graph dressings at that number of dots.
   vec4stTree[numDots_] are the independent s t A(s,t) at that number of dots in the numerator *)

(*   There's 1 soln w/ 3 dots in the numerator,  3 with 4, 3 with 5, 1 with 6 :

In[102]:= Union[extractType[vec4num[#], a]] & /@ Range[3, 6]

Out[102]= {
	{a["ym"]}, 
	{a["f^3"], a[mD[4], 1], a[mD[4], 2]}, 
	{a["2f^3+f^4"], a[mD[5], 3], a[mD[5], 4]}, 
	{a[mD[6], 1]}
     }

*)

vec4num[3] = -(a["ym"]*ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
        ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]])/2 + 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]])/2 - 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/2 - 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/2 + 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]])/2 + 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]])/2 + 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 + 
     (a["ym"]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 - 
     (a["ym"]*ulp[k[1], k[2]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/4 - 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 - 
     (a["ym"]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 + 
     (a["ym"]*ulp[k[1], k[2]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/4 - 
     (a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]])/2 + 
     (a["ym"]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]])/2 + 
     (a["ym"]*ulp[k[1], k[2]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]])/4 + 
     (a["ym"]*ulp[k[1], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]])/2
 
vec4num[4] = (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]] + (a["f^3"]/2 - a[mD[4], 2]/2)*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]] - 
     (a["f^3"]/2 - a[mD[4], 2]/2)*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] - (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]] + 
     (a["f^3"]/2 - a[mD[4], 1]/2 - a[mD[4], 2])*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + (a["f^3"]/2 - a[mD[4], 2]/2)*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     (a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]])/2 + 
     (a["f^3"]/2 + a[mD[4], 1]/2)*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     (a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]])/2 - 
     (a["f^3"]/2 - a[mD[4], 2]/2)*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + (-a["f^3"]/2 + a[mD[4], 1]/2 + 
       a[mD[4], 2])*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     (a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]])/2 + 
     (a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]])/2 + 
     ((-a[mD[4], 1]/2 - a[mD[4], 2]/2)/2 + (-a["f^3"]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     ((-a[mD[4], 1]/2 - a[mD[4], 2]/2)/2 + (-a["f^3"]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     ((-a["f^3"]/2 + a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     (a[mD[4], 1]/2 + a[mD[4], 2]/2)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     (a[mD[4], 1]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/
      2 + ((-a["f^3"]/2 + a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/
        2)*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     ((a["f^3"]/2 - a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     (a[mD[4], 2]*ulp[k[1], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/
      2 - (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     ((a["f^3"]/2 - a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     ((-a["f^3"]/2 + a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["f^3"]/2 + (-a["f^3"]/2 + a[mD[4], 2]/2)/2 + 
       (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*ulp[k[1], k[2]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a[mD[4], 2]*ulp[k[1], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
       ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]])/
      2 - (a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/
      2 + (a[mD[4], 1]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/
      2 + ((a["f^3"]/2 - a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/
        2)*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["f^3"]/2 + (a["f^3"]/2 - a[mD[4], 2]/2)/2 + 
       (-a[mD[4], 1]/2 - a[mD[4], 2]/2)/2)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     (a[mD[4], 2]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/
      2 + ((a["f^3"]/2 - a[mD[4], 2]/2)/2 + (-a[mD[4], 1]/2 - a[mD[4], 2]/2)/
        2)*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     ((3*a["f^3"])/2 - 2*(a["f^3"]/2 - a[mD[4], 2]/2))*ulp[k[1], k[2]]*
      ulp[k[1], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (a[mD[4], 2]*ulp[k[1], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 + 
     ((-a["f^3"]/2 + a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     (-a[mD[4], 1]/2 - a[mD[4], 2]/2)*ulp[k[1], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     ((-a[mD[4], 1]/2 - a[mD[4], 2]/2)/2 + (-a["f^3"]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a[mD[4], 2]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/
      2 + ((a["f^3"]/2 - a[mD[4], 2]/2)/2 + (-a[mD[4], 1]/2 - a[mD[4], 2]/2)/
        2)*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a["f^3"]/2 + a[mD[4], 2]/2)*ulp[k[1], k[2]]^2*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a["f^3"]*ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
        \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 - 
     (a[mD[4], 2]*ulp[k[1], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 + 
     ((-a[mD[4], 1]/2 - a[mD[4], 2]/2)/2 + (-a["f^3"]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + (-a["f^3"]/2 - a[mD[4], 1]/2)*
      ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     ((a["f^3"]/2 - a[mD[4], 2]/2)/2 + (a[mD[4], 1]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     ((-a[mD[4], 1]/2 - a[mD[4], 2]/2)/2 + (-a["f^3"]/2 + a[mD[4], 2]/2)/2)*
      ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     (a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]])/2 + 
     (-a["f^3"] + 2*(a["f^3"]/2 - a[mD[4], 2]/2))*ulp[k[1], k[2]]*
      ulp[k[1], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]]
 
vec4num[5] = (a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]] + 
     (a["2f^3+f^4"]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]])/2 + (a["2f^3+f^4"]/4 + a[mD[5], 3]/4 + 
       a[mD[5], 4]/4)*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]] + (a["2f^3+f^4"]*ulp[k[1], k[3]]*
       ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]])/2 + 
     (a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]])/2 + (a["2f^3+f^4"]/4 + a[mD[5], 3]/4 + 
       a[mD[5], 4]/4)*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] + (a["2f^3+f^4"]*ulp[k[1], k[3]]*
       ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]])/2 + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] - 
     (a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     (a[mD[5], 4]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]])/2 + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (a["2f^3+f^4"]/4 - a[mD[5], 3]/4 + a[mD[5], 4]/4)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (a[mD[5], 4]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
       ulp[k[3], \[CurlyEpsilon][k[2]]])/2 + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (a["2f^3+f^4"]/4 - a[mD[5], 3]/4 + a[mD[5], 4]/4)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) + a[mD[5], 4]/2)*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (-a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4)*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) + a[mD[5], 4]/2)*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     ((3*a["2f^3+f^4"])/2 - 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     (a["2f^3+f^4"] - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     (a[mD[5], 4]*ulp[k[1], k[3]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/
      2 + (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     ((3*a["2f^3+f^4"])/2 - 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     ((3*a["2f^3+f^4"])/2 - 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - 
       a[mD[5], 4]/2)*ulp[k[1], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"] - 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - 
       a[mD[5], 4]/2)*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (-a["2f^3+f^4"]/2 - a[mD[5], 4])*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - 
       a[mD[5], 4]/2)*ulp[k[1], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) + a[mD[5], 4]/2)*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (a["2f^3+f^4"] - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) + 
       a[mD[5], 4])*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (a[mD[5], 4]*ulp[k[1], k[3]]^2*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/
      2 + (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) + a[mD[5], 4]/2)*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (a["2f^3+f^4"]/2 + a[mD[5], 4])*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) + a[mD[5], 4]/2)*ulp[k[1], k[3]]^2*
      ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 - (3*a[mD[5], 4])/2)*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] + (-2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) - 2*a[mD[5], 4])*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] + (a["2f^3+f^4"]/2 - 
       2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - a[mD[5], 4]/2)*
      ulp[k[1], k[3]]^3*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     ((-3*a["2f^3+f^4"])/2 + 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     ((-3*a["2f^3+f^4"])/2 + 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4))*ulp[k[1], k[2]]*ulp[k[1], k[3]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (-a["2f^3+f^4"]/2 + 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - 
         a[mD[5], 4]/4) + a[mD[5], 4]/2)*ulp[k[1], k[3]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4))*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"] - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) + 
       a[mD[5], 4])*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + ((3*a["2f^3+f^4"])/2 - 
       4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) + a[mD[5], 4]/2)*
      ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - 
       a[mD[5], 4]/2)*ulp[k[1], k[3]]^3*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"]/2 - 2*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - 
       a[mD[5], 4]/2)*ulp[k[1], k[2]]^3*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     (a["2f^3+f^4"] - 4*(a["2f^3+f^4"]/4 - a[mD[5], 3]/4 - a[mD[5], 4]/4) - 
       a[mD[5], 4])*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]]
 
vec4num[6] = -(a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
        ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
        ulp[k[3], \[CurlyEpsilon][k[1]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]])/2 - (a[mD[6], 1]*ulp[k[1], k[2]]*
       ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
       ulp[k[3], \[CurlyEpsilon][k[2]]])/2 - (a[mD[6], 1]*ulp[k[1], k[2]]*
       ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[4]]]*
       ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[k[3], \[CurlyEpsilon][k[2]]])/2 - (a[mD[6], 1]*ulp[k[1], k[2]]^2*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[k[3], \[CurlyEpsilon][k[2]]])/2 + (a[mD[6], 1]*ulp[k[1], k[2]]^2*
       ulp[k[1], k[3]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/
      2 + (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
       ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/2 + 
     (a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/2 + 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
       ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]])/2 + 
     (a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
       ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]])/2 + 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
       ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
       ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
       ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
       ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
       ulp[k[1], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]])/2 + 
     (3*a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]^2*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
        \[CurlyEpsilon][k[3]]])/2 + (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^3*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
        \[CurlyEpsilon][k[3]]])/2 - (a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 - 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^2*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
       ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 - 
     a[mD[6], 1]*ulp[k[1], k[2]]^4*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     (3*a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[1], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
        \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 + 
     (a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^3*ulp[\[CurlyEpsilon][k[1]], 
        \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]])/2 + 
     (a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[1], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
        \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]])/2 + 
     (3*a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], k[3]]^2*
       ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
        \[CurlyEpsilon][k[4]]])/2 + a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]]
 
Attributes[ulp] = {Orderless}
 
ulp[-(a_), b_] := -ulp[a, b]
 
ulp[a_, (b_) + (c_)] := ulp[a, b] + ulp[a, c]
 
ulp[c_, (b_)*(a_)?NumberQ] := a*ulp[b, c]
 
vec4stTree[3] = a["ym"]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a["ym"]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a["ym"]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a["ym"]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a["ym"]*ulp[k[1], k[2]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - a["ym"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a["ym"]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a["ym"]*ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]]
 
vec4stTree[4] = -(a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
       ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
       ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]) + 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] + 2*a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] + 2*a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]] - 
     a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] + a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] + 2*a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + 2*a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[4], 1]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     2*a[mD[4], 2]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[4], 2]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + 2*a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a["f^3"]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     2*a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a["f^3"]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a[mD[4], 1]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + a[mD[4], 2]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 2*a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 2]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["f^3"]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["f^3"]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 2]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["f^3"]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a[mD[4], 2]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["f^3"]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 1]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a["f^3"]*ulp[k[1], k[2]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 1]*ulp[k[1], k[2]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - a[mD[4], 2]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - a["f^3"]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 2]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[4], 2]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 2*a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[4], 1]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[4], 2]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[4], 2]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 2*a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^3*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - 2*a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[4], 2]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[4], 2]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[4], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a["f^3"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 1]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 1]*ulp[k[1], k[2]]^2*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[4], 2]*ulp[k[1], k[2]]^2*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a[mD[4], 2]*ulp[k[1], k[2]]^3*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - a["f^3"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[4], 2]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]]
 
vec4stTree[5] = a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] - a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] - 
     a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] + 
     a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] - a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]] - 
     a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + 2*a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[5], 4]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] - a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] - a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a[mD[5], 3]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a["2f^3+f^4"]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - 2*a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + 2*a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 4]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + a[mD[5], 4]*ulp[k[1], k[2]]^3*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]] - 2*a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     2*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]] - 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a[mD[5], 4]*ulp[k[1], k[2]]^3*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     2*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]^3*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] - 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^3*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[5], 4]*ulp[k[1], k[2]]^3*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     4*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     3*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + a[mD[5], 3]*ulp[k[2], k[3]]^3*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 2*a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] + a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^3*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] + a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^3*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] - a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - a[mD[5], 4]*ulp[k[1], k[2]]^3*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] - 
     a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^3*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^3*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[4]]] + a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^3*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^3*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[5], 3]*ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     3*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 3*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a[mD[5], 4]*ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[5], 3]*ulp[k[2], k[3]]^3*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - 3*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - 3*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - 3*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - 3*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - a[mD[5], 3]*ulp[k[2], k[3]]^4*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 2*a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], k[2]]^3*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     3*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     3*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 3*a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     3*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[2], k[3]]^3*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[2], k[3]]^3*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^3*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]^3*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 4]*ulp[k[1], k[2]]^3*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]^4*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + 3*a[mD[5], 4]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + 2*a["2f^3+f^4"]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]^2*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] + 
     6*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + 3*a[mD[5], 4]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[2], k[3]]^4*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] + 2*a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] + a["2f^3+f^4"]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + 
     2*a[mD[5], 3]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] + 2*a[mD[5], 4]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[5], 4]*ulp[k[1], k[2]]^3*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[5], 4]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[5], 3]*ulp[k[1], k[2]]^3*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[5], 4]*ulp[k[1], k[2]]^3*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - a[mD[5], 3]*ulp[k[1], k[2]]^4*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - 3*a[mD[5], 3]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - 3*a[mD[5], 4]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - a["2f^3+f^4"]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - 3*a[mD[5], 3]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]] - 3*a[mD[5], 4]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]]
 
vec4stTree[6] = a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]] - a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]] - a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[2]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]] + a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[k[3], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]] - a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] - a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[2], k[3]]^2*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] + 2*a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[1], \[CurlyEpsilon][k[2]]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]] + a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     2*a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[3]]]*ulp[k[3], \[CurlyEpsilon][k[2]]]*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[3]]]*
      ulp[k[3], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[4]]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[4]]]*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^3*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^2*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[k[3], \[CurlyEpsilon][k[4]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] + a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[2], k[3]]^3*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[k[3], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[3]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^4*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[4]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[3]]] - a[mD[6], 1]*ulp[k[1], k[2]]^3*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]*ulp[k[3], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     2*a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[1], \[CurlyEpsilon][k[3]]]*
      ulp[k[2], k[3]]^2*ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[6], 1]*ulp[k[1], k[2]]*
      ulp[k[1], \[CurlyEpsilon][k[3]]]*ulp[k[2], k[3]]^3*
      ulp[k[3], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^4*ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], \[CurlyEpsilon][k[4]]] - 
     3*a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - 3*a[mD[6], 1]*ulp[k[1], k[2]]^2*ulp[k[2], k[3]]^3*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] - a[mD[6], 1]*ulp[k[1], k[2]]*ulp[k[2], k[3]]^4*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[3]]]*ulp[\[CurlyEpsilon][k[2]], 
       \[CurlyEpsilon][k[4]]] + a[mD[6], 1]*ulp[k[1], k[2]]^3*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]*ulp[k[2], \[CurlyEpsilon][k[1]]]*
      ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] + a[mD[6], 1]*ulp[k[1], k[2]]^2*
      ulp[k[1], \[CurlyEpsilon][k[2]]]*ulp[k[2], k[3]]^2*
      ulp[k[2], \[CurlyEpsilon][k[1]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^4*ulp[k[2], k[3]]*ulp[\[CurlyEpsilon][k[1]], 
       \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], \[CurlyEpsilon][k[4]]] - 
     a[mD[6], 1]*ulp[k[1], k[2]]^3*ulp[k[2], k[3]]^2*
      ulp[\[CurlyEpsilon][k[1]], \[CurlyEpsilon][k[2]]]*ulp[\[CurlyEpsilon][k[3]], 
       \[CurlyEpsilon][k[4]]]

