(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["SPAUtilities`"]


(* ::Section:: *)
(*Hybrid Sorting: Sorted Merge*)


hybridSort::usage = 
	"hybridSort[list_List,len]
	returns ordered list, using len to subslice list
	"


quickMerge::usage = 
	"quickMerge[alist_List, blist_List]
	returns merged by order list using interchange walk method, 
	"
recursiveMerge::usage = 
	"recursiveMerge[alist_List, blist_List]
	returns merged by order list using recursive method, 
	"
stableFind::usage = 
	"stableFind[list,pos];
	Find the FIRST index, > list[[pos]], insert elem at LEFT of index 
		If list[[All]] == list[[pos]], return Length[[list]]+1;
	"
stableInsert::usage = 
	"stableFind[list,pos];
	Find the FIRST index, > elem, insert elem at LEFT of index 
		If not found, return Length[[list]]+1;
	"
unStableFind::usage =
	"
	unStableFind[list,pos]:=
	Find the FIRST index, >= list[[pos]], insert elem at LEFT of index 
	If not found, return pos;
	"

unStableInsert::usage =
	"
	unStableInsert[list,elem]:=
	Find the FIRST index, >= elem, insert elem at LEFT of index 
	If not found, return pos;
	"


Begin["`Priavte`"]
(*
Find the FIRST index, > list[[pos]], insert elem at LEFT of index 
	If list[[All]] == list[[pos]], return Length[[list]]+1;
*)
stableFind[list_,pos_]:=
Module[
	{start, end, middle, elem },
	elem = list[[pos]];
		start=pos;
		end=Length[list]+1;
		While[ start < end, 
			middle = Floor[(start + end)/2];
			If[list[[middle]] > elem, end = middle, start=middle+1]
		];
	Return[end];
]

(*
Find the FIRST index, > elem, insert elem at LEFT of index 
	If not found, return Length[[list]]+1;
*)
stableInsert[list_,elem_]:=
Module[
	{start, end, middle},
	start=1;end=Length[list]+1;
		While[ start < end, 
			middle = Floor[(start + end)/2];
			If[list[[middle]] > elem, end = middle, start=middle+1]
		];
	Return[end];
]

(*
Find the FIRST index, >= list[[pos]], insert elem at LEFT of index 
	If not found, return pos;
*)
unStableFind[list_,pos_]:=
Module[
	{start, end, middle, elem },
	elem = list[[pos]];
	start=1;end=pos;
		While[ start < end, 
			middle = Floor[(start + end)/2];
			If[list[[middle]] >= elem, end = middle, start=middle+1]
		];
	Return[end];
]

(*
Find the FIRST index, >= elem, insert elem at LEFT of index 
	If not found, return pos;
*)
unStableInsert[list_,elem_]:=
Module[
	{start, end, middle},
	start=1; end=Length[list]+1;
	While[ start < end, 
		middle = Floor[(start + end)/2];
		If[list[[middle]] >= elem, end = middle, start=middle+1]
	];
	Return[end];
]


quickMerge[alist_List, blist_List] :=
Module[
	{start, end, middle, stepfactor=2,
	aindex=1,apivot,bindex=1,bpivot,astack,bstack,tmplist={},flag},
	
	apivot = alist[[aindex]];
	bpivot = blist[[bindex]];
	flag = If[apivot <= bpivot, True, False];
	
	While[True,
		If[flag,
		(*insert b to a*)
			start=1;end=Length[alist]+1;
			While[ start < end, 
				middle = Floor[(start + end)/stepfactor];
				If[list[[alist]] > elem, end = middle, start=middle+1]
			];
		]		
	
	
	
	];
	

]

recursiveMerge[alist_List, blist_List] :=
Module[
	{ma=0,mb=0,posa=0,posb=0, left, middle, right},
	
(*	If[Length[alist] <= 1000 || Length[blist] <= 1000, Return[Sort[{alist, blist}]]];*)
	
	ma = Floor[(1+Length[alist])/2];
	mb = Floor[(1+Length[blist])/2];
	
	(*Print["{ma, mb}=",{ma,mb}];*)
	
	If[ma<2 || mb < 2, Return[Sort[Join[alist, blist]]]];

	If[alist[[ma]] == blist[[mb]], 
	(*True*)
		ma=stableFind[alist,ma];
		mb=unStableFind[blist,mb];
		If[ma==Length[alist]+1,
			posa=unStableFind[alist,ma-1];
		If[posa == 1, 
			Return[Flatten[{blist[[1;;mb-1]],alist[[1;;-1]],blist[[mb;;-1]]}]],
			Return[Flatten[{
					recursiveMerge[alist[[1;;posa-1]],blist[[1;;mb-1]]],
					alist[[posa;;-1]],blist[[mb;;-1]]}]]
		]
		]
	];
	
	(*Print["{ma, mb}=",{ma,mb}];*)
	If[alist[[ma]] > blist[[mb]],
	(*True*)
		posa = stableInsert[alist,blist[[mb]]];
		posb = unStableInsert[blist,alist[[ma]]];
		(*Print["T:{posa, posb}=",{posa,posb}];*)
		If[posa == 1, 
			left=blist[[1;;mb-1]],
			left=recursiveMerge[alist[[1;;posa-1]], blist[[1;;mb-1]]]
		];
		If[posb == Length[blist]+1,
			right=alist[[ma+1;;-1]],
			right=recursiveMerge[alist[[ma+1;;-1]],blist[[posb;;-1]]]
		];
		middle = recursiveMerge[alist[[posa;;ma-1]],blist[[mb+1;;posb-1]]];
		Return[Flatten[{left, {blist[[mb]]},middle,{alist[[ma]]},right}]],
	 (*False*)
	    posa = stableInsert[alist,blist[[mb]]];
		posb = unStableInsert[blist,alist[[ma]]];
		(*Print["F:{posa, posb}=",{posa,posb}];*)
		If[posb == 1, 
			left=alist[[1;;ma-1]],
			left=recursiveMerge[alist[[1;;ma-1]],blist[[1;;posb-1]]]
		];
		If[posa == Length[alist]+1,
			right=blist[[mb+1;;-1]],
			right=recursiveMerge[alist[[posa;;-1]],blist[[mb+1;;-1]]]
		];
		middle = recursiveMerge[alist[[ma+1;;posa-1]],blist[[posb;;mb-1]]];
		Return[Flatten[{left, {alist[[ma]]},middle,{blist[[mb]]},right}]],
	];
	
	
]
End[]


Begin["`Priavte`"]

hybridSort[list_List, len_:10000] :=
Module[
	{merged, templist},
	templist = Table[Sort[list[[i;;Min[i+len-1,Length[list]]]]],{i,1,Length[list],len}];
	merged = templist[[1]];
	For[i=2,i<Length[templist],i++,
		merged = recursiveMerge[merged,templist[[i+1]]]];
	Return[Flatten[merged]];
]
End[]


(* ::Section:: *)
(*Sequence Comparison Matrix*)


compMatrix::usage = 
	"compMatrix[li_List, func_Function:Greater, order_Integer: Infility]
	returns comparison matrix using func to compare, 
	"


Begin["`Priavte`"]

compMatrix[li_List, order_: Infinity, func_Symbol:Greater] :=
Module[
	{n=Length[li], matrix},
	matrix = Table[
		Thread[func[Table[li[[i]],n],li]] /. {True ->0, False ->1},
		{i,n}
	];
	If[order == Infinity, 
		Return[matrix], 
		Return[Table[matrix[[i,i+order]],{i,n-order}]]
	]
]

End[]


(* ::Section:: *)
(*End*)


EndPackage[]
