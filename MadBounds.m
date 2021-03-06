(* ::Package:: *)

BeginPackage["MadBounds`",{"MadBoundsParams`"}]
Print["MadBounds-1.0"];
Print["2018/08"]
Print["Author: Simonas Drauk\[SHacek]as \n"];
(*SetDirectory[NotebookDirectory[]];*)
Print["Working directory: "<>Directory[]];

GetOutput::usage="Runs mg5_aMC with the script file provided by WriteCardScript and get the output";
WriteCardScript::usage="Writes param cards and a little 'script' file for scanning with MadGraph";
ReadCard::usage="Reads the output files and returns a list with widths and branching ratios";
WriteHB::usage="Writes HiggsBounds input files";
ReadHB::usage="Reads HiggsBounds output files";
GetOutputHB::usage="Uses WriteHB and ReadHB, calls HiggsBounds";
Swapper::usage="Swaps scalars and pseudo scalars if needed. Use only right after FlexibleSUSY!";
CallHB::usage="Slightly cooler version of GetOutputHB";
CallMadGraph::usage="Slightly cooler version of GetOutput";
ZMassConstraints::usage="Deletes bad points";
ReadAllCards::usage="Reads all the cards + SM ";

Begin["Private`"]

packdir=StringDrop[$InputFileName,11];
Print["Package directory: "<>packdir];



(* ::Section::Closed:: *)
(*CallHB*)


CallHB[pathHB_,pathin_,list_,pdg_:{{25,35,36},{37}},nsca_:2, options_:"LandH effC"]:=Module[
	{reps, HB, copy},
	reps=Table[StockHB[list[[k]],pdg[[1]],pdg[[2]],Length[pdg[[1]]], Length[pdg[[2]]], nsca], {k, Length[list]}];
	HB=GetOutputHB[pathHB, pathin, reps, Length[pdg[[1]]], Length[pdg[[2]]], options ];
	copy=list;
	Table[
		AppendTo[copy[[k]], HB[[k]]], {k, Length[copy]} 
		]
];


(* ::Section::Closed:: *)
(*CallMadGraph*)


CallMadGraph[modelname_:"Flexible312",pathmg_,pathin_,namein_,blocks_, particles_:"h1 h2 a1",flags_:""]:=Module[
	{reps,out, copied},
	copied=blocks;
	reps=Table[Stock[blocks[[k]]],{k,Length[blocks]}];
	out=GetOutput[modelname,pathmg,pathin,namein,reps,particles,flags];
	Table[
		AppendTo[copied[[k]],{out[[1,k]], {out[[2,1,k]],out[[2,2,k]],out[[2,3,k]]}}],{k,Length[blocks]}
	]
];


(* ::Section::Closed:: *)
(*WriteCardScript*)


WriteCardScript[modelname_,pathmg_,pathin_,namein_,blocks_, particles_,flags_]:=Module[{lh,filename},
	If[DirectoryQ[pathin]===False, CreateDirectory[pathin]];
	
	For[j=1,j<=Length[blocks],j++,
		filename=pathin<>"/"<>ToString[j]<>"_"<>namein;
		If[FileExistsQ[filename],DeleteFile[filename]];
		lh = OpenWrite[filename];
		
		For[i=1, i<=Length[blocks[[j]]],i++,
		
			WriteString[lh,blocks[[j,i,1]]<>"\n"];
				For[k=1, k<=Length[blocks[[j,i,2]]],k++,
					For[m=1,m<=Length[blocks[[j,i,2,k]]],m++,
						WriteString[lh,ToString[FortranForm[blocks[[j,i,2,k,m]]]]<>" "];
					];
					WriteString[lh,"\t#\n"];
				];
		];
		Close[lh];
	];
	
	filename=pathin<>"/"<>"Run_"<>namein;
	If[FileExistsQ[filename],DeleteFile[filename]];
	lh=OpenWrite[filename];
	WriteString[lh, "import model "<>modelname<>"\n"];
	
	For[j=1,j<=Length[blocks],j++,
		filename=pathin<>"/"<>ToString[j]<>"_"<>namein;
		WriteString[lh, "compute_widths "<>particles<>" --path="<>filename<>flags<>" \n"]
	];
	
	WriteString[lh, "exit"];
	Close[lh];
];


(* ::Section::Closed:: *)
(*ReadCard*)


ReadCard[pathin_,namein_,amount_]:=Module[{filename,stre,first, br, width, entry, allentries,second},
	br={};
	width={};
	entry={};
	allentries={};
	For[j=1,j<=amount, j++,
		(*go through all files*)
		filename=pathin<>"/"<>ToString[j]<>"_"<>namein;
		stre=OpenRead[filename];
		Find[stre,"#*************************\n#      Decay widths      *\n#*************************"];
		ReadLine[stre];
		ReadLine[stre];
		
		For[i=0,i<=0,i=i, (*start reading the dacays*)
			first=Read[stre, Word];
			(*Print[ToString[first]<>"   start"];*)
			
				If[ToString[first]==="DECAY", 
					AppendTo[width,ToExpression[Read[stre,Word]]]; (*PDG*)
					AppendTo[width,ToExpression[StringReplace[Read[stre,Word],{"E"->"*10^","e"->"*10^"}]]]; (*Width*)
					AppendTo[width,{}]; (*List for BRs*)
					
					first=Read[stre,Word];
					
					If[ToExpression[first]===EndOfFile, 
						AppendTo[entry,width];
						width={}; 
						i++,
					
					first=first<>Read[stre,Word];
					(*Print[first<>"   DECAy"];*)
					
					If[ToString[first]==="#BR",
					(*check if there are BR ratios calculated*)
						ReadLine[stre]; (*Finish Reading BR NDA ID....line*)
						
						While[first=!="#" && ToExpression[first]=!=EndOfFile,
							(*go through all BRs*)
							AppendTo[br,ToExpression[StringReplace[Read[stre,Word],{"E"->"*10^","e"->"*10^"}]]];
							Skip[stre, Number];
							second=Read[stre,Word];
							
							While[second=!="#" ,
								(*read off a single N body BR *)
								AppendTo[br, ToExpression[second]];
								second=Read[stre,Word];
							]; (*end of N body BR WHILE*)
							
							AppendTo[width[[3]],br];
							br={};
							ReadLine[stre]; (*finish  reading the # line*)
							first=Read[stre,Word];
							SetStreamPosition[stre, StreamPosition[stre]-StringLength[ToString[first]] ];
							(*Print[ToString[first]<>"  end while"];*)
						];(*end of WHILE to go through all BR*)
						,ReadLine[stre] (*go to the next decay if no BRs are present*)
					]; (*BR IF*)
					
					AppendTo[entry,width];
					width={};
					If[ToExpression[first]===EndOfFile, i++];
		
					](*end of file IF*)
				
				](*end DECAY IF*)
			
		]; (*cycle For all decays in a file*)
		AppendTo[allentries,entry];
		entry={};
		Close[stre];
	];(*Cycle for all files*)
	allentries
];
End[]


(* ::Section::Closed:: *)
(*GetOutput From MadGraph*)


GetOutput[modelname_,pathmg_,pathin_,namein_,blocks_, particles_,flags_]:=Module[{nums,lst,dt, SMnums},
	Print["MadGraph will probably take over all cores and threads and the kernel will become unresponsive. Wait until Madgraph is Done"];
	Print["Generating input data"];
	nums=Table[ParamBlocks/.blocks[[k]]/.DefReps,{k,Length[blocks]}];
	Print["Begining to write files"];
	WriteCardScript[modelname,pathmg,pathin,namein,nums,particles,flags];
	(*Create SM files with different Higgs masses*)
	For[m=1,m<=3,m++,
		SMnums=Table[SMParamBlocks/.ReplacePart[blocks[[k]], {21}->mh1->(Keys[blocks[[k,20+m]]]/.blocks[[k,20+m]])]/.DefReps,{k,Length[blocks]}];
		WriteCardScript["SM",pathmg,pathin,ToString[m]<>"_SM.dat",SMnums," h ",flags];
	];
	Print["Done Writting Files!"];
	Print["Launching MadGraph! Wait until MadGraph is Done!"];
	Run[pathmg<>"/bin/mg5_aMC "<>pathin<>"/Run_"<>namein];
	(*Calculate SM widths*)
	For[m=1,m<=3,m++,
		Run[pathmg<>"/bin/mg5_aMC "<>pathin<>"/Run_"<>ToString[m]<>"_SM.dat"];
	];
	Print["Width Calculation Complete!"];
	(*Get all the output! Only the Higgs from SM; {Normal, {SM1,SM2,SM3...}}*)
	ReadAllCards[pathin,namein,Length[nums]]
];


(* ::Section::Closed:: *)
(*ReadAllCards*)


Begin["Private`"]
ReadAllCards[pathin_, namein_, number_]:=Module[{},
{ReadCard[pathin, namein, number ],
	Table[ Cases[ReadCard[pathin, ToString[k]<>"_SM.dat", number ],{25, a_, {b___}},2], {k, 3}]}
];
End[]


(* ::Section::Closed:: *)
(*WriteHB*)


Begin["Private`"];


WriteHB[pathfold_, input_,np_,cp_]:=Module[{file, reps,filename, str},
If[Length[input]>0,
	reps=Table[BoundsParams[np,cp]/.input[[k]]/.DefBounds[np,cp],{k,Length[input]}];
	(*create files and folders*)
	If[!DirectoryQ[pathfold], 
		CreateDirectory[pathfold]; 
		For[i=1,i<=Length[reps[[1]]],i++,
			CreateFile[pathfold<>"/"<>reps[[1,i,1]]] (*create*)
		],
		For[i=1,i<=Length[reps[[1]]],i++,
			If[FileExistsQ[pathfold<>"/"<>reps[[1,i,1]]], DeleteFile[pathfold<>"/"<>reps[[1,i,1]]]] (*delete*)
		];
		For[i=1,i<=Length[reps[[1]]],i++,
			CreateFile[pathfold<>"/"<>reps[[1,i,1]]] (*create*)
		];
	];
	(*write files*)
	(*  i - number of blocks
		j - number of parameter points
		k - number of entries in a line 	
	*)
	For[i=1,i<=Length[reps[[1]]],i++,
		filename=pathfold<>"/"<>reps[[1,i,1]];
		file=OpenAppend[filename];
		For[j=1,j<=Length[reps],j++,
			str="";
			For[k=1,k<=Length[reps[[j,i,2]]],k++,
				str=str<>ToString[
								FortranForm[
									reps[[j,i,2,k]]
								]
						 ]<>" ";
			];
			WriteString[file, ToString[j]<>" "<>str<>"\n"];
		];
		Close[file];
	];
	Print["Done Writing Files for HiggsBounds!"],
	Print["Nothing to write!!!"]
] (*end of if*)
	
];


(* ::Section::Closed:: *)
(*ReadHB*)


ReadHB[pathin_, np_, cp_]:=Module[{out, first, res,file,entry, chan ,obsr, ncomb},
	out={};
	entry={};
	first="";
	If[!DirectoryQ[pathin], Print["Directory "<>pathin<>" Does Not Exist!"],
		If[!FileExistsQ[pathin<>"/HiggsBounds_results.dat"], Print["Output "pathin<>"/HiggsBounds_results.dat was not produced!"],
			file=OpenRead[pathin<>"/HiggsBounds_results.dat"];
			Find[file," #\n #cols:"];
			ReadLine[file];
			first=Read[file,Word];
			(*Print[first<> " firstasd"];*)
			While[ToExpression[first]=!=EndOfFile,
				Read[file, Word*Table[1,{k,np+cp}]];
				res=ToExpression[Read[file, Word]];
				chan=ToExpression[Read[file, Word]];
				obsr=Read[file, Word];
				ncomb=ToExpression[Read[file, Word]];
				entry={ToExpression[first],res,chan,ToExpression[StringReplace[ToString[obsr],{"E"->"*10^","e"->"*10^"}]],ncomb};
				AppendTo[out,entry];
				entry={};
				first=ToString[Read[file,Word]]
				(*Print[first<> " first"]*)
			];
			Close[file];
		]
	];
	out
];
End[];


(* ::Section:: *)
(*Swapper*)


Swapper[data_]:=If[#[[2,3,1]]===0. && #[[2,3,2]]===0.,
					ReplacePart[#,
						{ {1,3}->#[[1,4]], {1,4}->#[[1,3]],
						  {2,3}->#[[2,4]], {2,4}->#[[2,3]]
						}
					], #
				]&/@data;


(* ::Section::Closed:: *)
(*Constraints*)


ZMassConstraints[data_]:=Module[{positive,unitary={},eig, tmp,S21, S20, S01, S00},

	positive=DeleteCases[
					If[ 
						#[[3,1]]>=0 && #[[3,1]]<2*\[Pi] && (*upper bounds 1011.6188*)
						#[[3,2]]>=0 && #[[3,2]]<2*\[Pi] &&
						Abs[#[[3,3]]]<4*\[Pi] &&
						Abs[#[[3,3]]+#[[3,4]]]<4*\[Pi] &&
						Abs[#[[3,5]]]<\[Pi] && Abs[#[[3,6]]]<\[Pi]
						, #
					]&/@data
				,Null];
	For[i=1, i<=Length[positive], i++,
		tmp=positive[[i,3]]; (*https://arxiv.org/pdf/0902.0851.pdf page 10*)
		
		S21={{2*tmp[[1]], 2*tmp[[5]], Sqrt[2]tmp[[6]]},
			{tmp[[5]], tmp[[2]], Sqrt[2]*tmp[[7]]},
			{Sqrt[2]*tmp[[6]], Sqrt[2]*tmp[[7]], tmp[[3]]+tmp[[4]]}
		};
		S20={{tmp[[3]]-tmp[[4]]}};
		S01={
			{tmp[[1]], tmp[[4]], tmp[[6]], tmp[[6]]},
			{tmp[[4]], tmp[[2]], tmp[[7]], tmp[[7]]},
			{tmp[[6]], tmp[[7]], tmp[[3]], 2*tmp[[5]]},
			{tmp[[6]], tmp[[7]], 2*tmp[[5]], tmp[[3]]}	
		};
		S00={
			{6*tmp[[1]], 2*tmp[[3]]+tmp[[4]], 3*tmp[[6]], 3*tmp[[6]]},
			{2*tmp[[3]]+tmp[[4]], 6*tmp[[2]], 3*tmp[[7]], 3*tmp[[7]]},
			{3*tmp[[6]], 3*tmp[[7]], tmp[[3]]+2*tmp[[4]], 6*tmp[[5]]},
			{3*tmp[[6]], 3*tmp[[7]], 6*tmp[[5]], tmp[[3]]+2*tmp[[4]]}
		};
		eig=Flatten[{Eigenvalues[S21],Eigenvalues[S20], Eigenvalues[S01], Eigenvalues[S00]}];
		If[Length[eig]===Length[Select[eig, Abs[#]<16*\[Pi]&]], 
				AppendTo[unitary, positive[[i]]]
		]
	];			
				
	DeleteCases[
					If[ 
						#[[3,3]]>=-2*Sqrt[#[[3,2]]*#[[3,1]]] &&
						4*Abs[#[[3,6]]+#[[3,7]]]>#[[3,1]] +#[[3,2]]+#[[3,3]]+ #[[3,4]] + 2*#[[3,5]] &&
						
						#[[3,8]]+246.22^2*#[[3,3]]/2>200^2 && #[[3,8]]+246.22^2*#[[3,3]]/2<2000^2 &&
						
						#[[3,3]]+#[[3,4]]-2*#[[3,5]] >= -2*Sqrt[#[[3,2]]*#[[3,1]]] &&
						(*mass requirements*)
						 #[[1,3]]>140 && #[[1,4]]>140 && #[[1,6]]>200
						, #
					]&/@unitary
		,Null]
];


(* ::Section::Closed:: *)
(*GetOutput from HiggsBounds*)


GetOutputHB[pathHB_,pathin_,input_,np_:3,cp_:1, options_:"LandH effC"]:=Module[{},
	WriteHB[pathin,input,np,cp];
	If[FileExistsQ[pathin<>"/HiggsBounds_results.dat"], DeleteFile[pathin<>"/HiggsBounds_results.dat"]];
	Run[pathHB<>" "<>options<>" "<>ToString[np]<>" "<>ToString[cp]<>" "<>pathin<>"/"];
	Print[pathHB<>" "<>options<>" "<>ToString[np]<>" "<>ToString[cp]<>" "<>pathin<>"/"];
	ReadHB[pathin,np,cp]
];


(* ::Section::Closed:: *)
(*End*)


EndPackage[]
