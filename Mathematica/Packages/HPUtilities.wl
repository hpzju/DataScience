(* ::Package:: *)

(* ::Section::Closed:: *)
(*Begin*)


BeginPackage["HPUtilities`"]


(* ::Section:: *)
(*Initialize Path: *)


(* ::Text:: *)
(*Initialize all the Directory and all sub Directories to $Path*)


initPath::usage = 
	"initPath[Dir_String, level_Integer] adds dir and all sub dirs to $Path"


Begin["`Priavte`"]

initPath[dir_String, level_Integer: Infinity] :=
Module[
	{path},
	SetDirectory[dir];
	path = Select[FileNames["*",dir,level],DirectoryQ];
	path = Select[path, !StringMatchQ[#,RegularExpression[".*\\..*"]]&];
	Map[(AppendTo[$Path, #1])&,path];
]

End[]


(* ::Section:: *)
(*Formatting Base R Output*)


(* ::Text:: *)
(*System built-in BaseForm[] and NumberForm[] with NumberPadding->{"",""} has bugs for fixed-length base R formatting.*)


baseForm::usage = 
	"baseForm[number,base,width] prints number in base form with fixed length width, 
	padding left with 0s if nessesary.
	"


Begin["`Priavte`"]

baseForm[number_,base_:2,width_:0]:=
Module[
	{padding=0 ,len},
	If[number==0,
		padding=width-1;
		Return[
			NumberForm[
				BaseForm[number,base],
				NumberFormat->(If[padding>0,StringJoin[StringRepeat["0",padding],#1],#1]&)
			]
		],
		len =Ceiling[Log[base,number]];
		If[number== base^len,padding=width-(len+1),padding=width-len];
		Return[
			NumberForm[
				BaseForm[number,base],
				NumberFormat->(If[padding>0,StringJoin[StringRepeat["0",padding],#1],#1]&)
			]
		]
	]
]

End[]


(* ::Section:: *)
(*Fastest Growth Function: Ackermann Function*)


(* ::Text:: *)
(*Ackermann function definition:*)
(*	ackermann[0, n] := n+1;*)
(*	ackermann[m, 0] := ackermann[m-1, 1];*)
(*	ackermann[m, n]:= ackermann[m-1, ackermann[m, n-1]]*)


ackermann::usage = 
	"ackermann[m,n] returns Ackermann functions values"


Begin["`Priavte`"]

ackermann[m_Integer, n_Integer] :=
Module[
	{},
	If[m==0, 
		Return[n+1],
		If[n==0,
			Return[ackermann[m-1,1]],
			Return[ackermann[m-1,ackermann[m, n-1]]]
		]			
	]
]

End[]


(* ::Section::Closed:: *)
(*End*)


EndPackage[]
