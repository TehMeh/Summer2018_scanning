(* ::Package:: *)

BeginPackage["MadParams`"]
ParamBlocks::usage="Has the blocks for printing";
SMParamBlocks::usage="Has the blocks for printing";
DefReps::usage="Default replacement rules";
stock::usage="forms replacement rules for MadGraph input";
BoundsParams::usage="Parameter and block names for HiggsBounds";
DefBounds::usage="Default replacement rules for HiggsBounds";
StockHB::usage="forms replacement rules for HiggsBounds input";


(* ::Section::Closed:: *)
(*Parameter names for MadGraph*)


ParamBlocks:={ 
{"Block ckmblock",{
		{1, 2.277360*10^-1}}
          },         
{"Block pmnsmat", Flatten[Table[{i,k,Re[PMNS[[i,k]]]},{i,3},{k,4}],1]
           },
{"Block impmnsmat", Flatten[Table[{i,k,Im[PMNS[[i,k]]]},{i,3},{k,4}],1]
           },
{"Block pmnsvec", Table[{1,k,Re[PMNS[[4,k]]]},{k,4}]
           },
{"Block impmnsvec", Table[{1,k,Im[PMNS[[4,k]]]},{k,4}]
           },
{"Block mass",{
			{1,md},
			{2,mu},
			{3,ms},
			{4,mc},
			{5,mb},
			{6,mt},
			{11,me},
			{13,mmu},
			{14,mnumu},
			{15,mta},
			{16,mnut},
			{18,mnumaj},
			{23,mz},
			{25,mh1},
			{35,mh2},
			{36,ma1},
			{37,mhp},
			{12,mnue},
			{21,mGlu},
			{22, mPhot},
			{24, MWp}}
            },
{"Block minpar",{
           {1,Z1111IN},
           {2,Z2222IN},
           {3,Z2211IN},
           {4,Z2112IN},
           {5,Z2121IN},
           {6,Z1112IN},
           {7,Z2212IN},
           {8,M222IN},
           {9,M112IN},
           {10,M122IN},
           {11,theta12IN},
           {12,majrIN},
           {20, vevIN}}
          },
{"Block sminputs",{
          {1,aewm1},
          {2,Gf},
          {3,aStr}}
          },
{"Block yukawa",{
		{1,ymdo},
		{2,ymup},
		{3,yms},
		{4,ymc},
		{5,ymb},
		{6,ymt},
		{11,yme},
		{13,ymm},
		{15,ymtau}}
		},
{"Block yukawagdi", Flatten[Table[{i,k,Im[YUKAWAGD[[i,k]]]},{i,3},{k,3}],1]
					},                  
{"Block yukawagdr", Flatten[Table[{i,k,Re[YUKAWAGD[[i,k]]]},{i,3},{k,3}],1]
					},
{"Block yukawagli", Flatten[Table[{i,k,Im[YUKAWAGL[[i,k]]]},{i,3},{k,3}],1]
					},                  
{"Block yukawaglr", Flatten[Table[{i,k,Re[YUKAWAGL[[i,k]]]},{i,3},{k,3}],1]
					},
{"Block yukawagui", Flatten[Table[{i,k,Im[YUKAWAGU[[i,k]]]},{i,3},{k,3}],1]
					},                  
{"Block yukawagur", Flatten[Table[{i,k,Re[YUKAWAGU[[i,k]]]},{i,3},{k,3}],1]
					},
{"Block yukyv", Table[{i,Re[Yv[[i]]]},{i,3}]},     
{"Block imyukyv", Table[{i,Im[Yv[[i]]]},{i,3}]},
{"Block yukgv", Table[{i,Re[Gv[[i]]]},{i,3}]},     
{"Block imyukgv", Table[{i,Im[Gv[[i]]]},{i,3}]},

{"Block QNUMBERS 18",{ 
		{1,0},
		{2,2},
		{3,1},
		{4,0}}
		},
{"Block QNUMBERS 37",{ 
		{1,3},
		{2,1},
		{3,1},
		{4,1}}
		},
{"Block QNUMBERS 35",{
		{1,0},
		{2,1},
		{3,1},
		{4,0}}
		},
{"Block QNUMBERS 36",{ 
		{1,0},
		{2,1},
		{3,1},
		{4,0}}
		},
{"#    PDG    Width", {
							{DECAY, 1, 0}}},
{"#    PDG    Width", {
							{DECAY, 2, 0}}},
{"#    PDG    Width", {
							{DECAY, 3, 0}}},
{"#    PDG    Width", {
							{DECAY, 4, 0}}},
{"#    PDG    Width", {
							{DECAY, 5, 0}}},
{"#    PDG    Width", {
							{DECAY, 6, 1.508336}}},
{"#    PDG    Width", {
							{DECAY, 11, 0}}},
{"#    PDG    Width", {
							{DECAY, 12, wnue}}}, 
{"#    PDG    Width", {
							{DECAY, 13, 0}}},
{"#    PDG    Width", {
							{DECAY, 14, wnumu}}},
{"#    PDG    Width", {
							{DECAY, 15, 0}}},
{"#    PDG    Width", {
							{DECAY, 16, wnunut}}},
{"#    PDG    Width", {
							{DECAY, 18, wnumaj}}},
{"#    PDG    Width", {
							{DECAY, 21, 0}}},
{"#    PDG    Width", {
							{DECAY, 22, 0}}},
{"#    PDG    Width", {
							{DECAY, 23, WZ}}},
{"#    PDG    Width", {
							{DECAY, 24, WW}}},
{"#    PDG    Width", {
							{DECAY, 25, wh1}}},
{"#    PDG    Width", {
							{DECAY, 35, wh2}}},
{"#    PDG    Width", {
							{DECAY, 36, wa1}}},
{"#    PDG    Width", {
							{DECAY, 37, whp}}}
};


(* ::Section::Closed:: *)
(*SM Parameter names for MadGraph*)


SMParamBlocks:={ 
{"Block ckmblock",{
		{1, 2.277360*10^-1}}
          },
(*{"Block loop", {
			{1,mz}}
			},  *)    
{"Block mass",{
			{1,md},
			{2,mu},
			{3,ms},
			{4,mc},
			{5,mb},
			{6,mt},
			{11,me},
			{12,mnue},			
			{13,mmu},
			{14,mnumu},
			{15,mta},
			{16,mnut},
			{23,mz},
			{25,mh1},
			{21,mGlu},
			{22,mPhot},
			{24,MWp}}
            },
{"Block sminputs",{
          {1,aewm1},
          {2,Gf},
          {3,aStr}}
          },
{"Block yukawa",{
		{1,ymdo},
		{2,ymup},
		{3,yms},
		{4,ymc},
		{5,ymb},
		{6,ymt},
		{11,yme},
		{13,ymm},
		{15,ymtau}}
		},
{"Block QNUMBERS 82",{ 
		{1,0},
		{2,1},
		{3,8},
		{4,1}}
		},

{"#    PDG    Width", {
							{DECAY, 1, 0}}},
{"#    PDG    Width", {
							{DECAY, 2, 0}}},
{"#    PDG    Width", {
							{DECAY, 3, 0}}},
{"#    PDG    Width", {
							{DECAY, 4, 0}}},
{"#    PDG    Width", {
							{DECAY, 5, 0}}},
{"#    PDG    Width", {
							{DECAY, 6, 1.508336}}},
{"#    PDG    Width", {
							{DECAY, 11, 0}}},
{"#    PDG    Width", {
							{DECAY, 12, wnue}}}, 
{"#    PDG    Width", {
							{DECAY, 13, 0}}},
{"#    PDG    Width", {
							{DECAY, 14, wnumu}}},
{"#    PDG    Width", {
							{DECAY, 15, 0}}},
{"#    PDG    Width", {
							{DECAY, 16, wnunut}}},
{"#    PDG    Width", {
							{DECAY, 21, 0}}},
{"#    PDG    Width", {
							{DECAY, 22, 0}}},
{"#    PDG    Width", {
							{DECAY, 23, WZ}}},
{"#    PDG    Width", {
							{DECAY, 24, WW}}},
{"#    PDG    Width", {
							{DECAY, 25, wh1}}}
};


(* ::Section::Closed:: *)
(*Default values for MadGraph*)


Begin["Private`"]
DefReps:={
		PMNS -> Table[If[j===k,1,0],{j,4},{k,4}],
		YUKAWAGD -> Table[0,{j,3},{k,3}],
		YUKAWAGL -> Table[0,{j,3},{k,3}],
		YUKAWAGU ->Table[0,{j,3},{k,3}],
		Yv -> Table[0,{j,3}],
		Gv -> Table[0,{j,3}],
		md -> 0,
		mu -> 0,
		ms -> 0.96,
		mc -> 1.28,
		mb -> 4.18,
		mt -> 173.1,
		me -> 0.000511,
		mnue -> 0,
		mmu -> 0.10566,
		mnumu -> 0,
		mta -> 1.7768,
		mnut -> 0,
		mnumaj -> 0,
		mz ->  91.19,
		mh1 -> 125.09,
		mh2 -> 370,
		ma1 -> 268,
		mhp -> 400,
		mGlu -> 0,
		mPhot -> 0,
		MWp -> 80.39,
		(*MINPAR*)
		Z1111IN -> 0,
		Z2222IN -> 0,
		Z2211IN -> 0,
		Z2112IN -> 0,
		Z2121IN -> 0,
		Z1112IN -> 0,
		Z2212IN -> 0,
		M222IN -> 0,
		M112IN -> 0,
		M122IN -> 0,
		theta12IN -> 0, 
		majrIN -> 0,
		vevIN -> 246.22,
		(*SM*)
		aewm1 -> 127.916,
		Gf -> 1.166378700*10^-5,
		aStr -> 0.1182,
		(*Yukawa mass for charged leptons*)
		ymdo ->  0,
		ymup -> 0,
		yms -> 0.96,
		ymc ->  1.28,
		ymb -> 4.18,
		ymt -> 173.1,
		yme -> 0.000511,
		ymm -> 0.10566,
		ymtau -> 1.7768,
		(*some widths*)
		wnue -> 0,
		wnumu ->0,
		wnunut -> 0,
		wnumaj -> 1,
		WZ -> 2.495200,
		WW ->  2.085000,
		wh1 -> 1,
		wh2 -> 1,
		wa1 -> 1,
		whp -> 1
	};
End[]


(* ::Section:: *)
(*Stock[]*)


(*Begin["Private`"]*)
	stock[block_]:={
		PMNS -> block[[4,9]],
		YUKAWAGD -> block[[4,8]],
		YUKAWAGL -> block[[4,2]],
		YUKAWAGU -> block[[4,6]],
		Yv -> block[[4,3]],
		Gv -> block[[4,4]],
		md -> block[[1,17]],
		mu -> block[[1,14]],
		ms -> block[[1,18]],
		mc -> block[[1,15]],
		mb -> block[[1,19]],
		mt -> block[[1,16]],
		me -> block[[1,7]],
		mnue -> block[[1,10]],
		mmu -> block[[1,8]],
		mnumu -> block[[1,11]],
		mta -> block[[1,9]],
		mnut -> block[[1,12]],
		mnumaj -> block[[1,13]],
		mz ->  block[[1,1]],
		mh1 -> block[[1,2]],
		mh2 -> block[[1,3]],
		ma1 -> block[[1,4]],
		mhp -> block[[1,6]],
		mGlu -> 0,
		mPhot -> 0,
		MWp -> block[[1,5]],
		(*MINPAR*)
		Z1111IN -> block[[3,1]],
		Z2222IN -> block[[3,2]],
		Z2211IN -> block[[3,3]],
		Z2112IN -> block[[3,4]],
		Z2121IN -> block[[3,5]],
		Z1112IN -> block[[3,6]],
		Z2212IN -> block[[3,7]],
		M222IN -> block[[3,8]],
		M112IN -> block[[3,9]],
		M122IN -> block[[3,10]],
		theta12IN -> If[block[[2,2,1]]<0,  
						-ArcSin[block[[2,2,2]]]+\[Pi]
						,ArcSin[block[[2,2,2]]]
					], 
		majrIN -> block[[3,11]],
		vevIN -> block[[3,12]],
		(*SM*)
		aewm1 -> 127.916,
		Gf -> 1.166378700*10^-5,
		aStr -> 0.1182,
		(*Yukawa mass for charged leptons*)
		ymdo ->  block[[1,17]],
		ymup -> block[[1,14]],
		yms ->  block[[1,18]],
		ymc ->  block[[1,15]],
		ymb -> block[[1,19]],
		ymt -> block[[1,16]],
		yme -> block[[1,7]],
		ymm -> block[[1,8]],
		ymtau -> block[[1,9]],
		(*some widths*)
		wnue -> 0,
		wnumu ->0,
		wnunut -> 0,
		wnumaj -> 1,
		WZ -> 2.495200,
		WW ->  2.085000,
		wh1 -> 1,
		wh2 -> 1,
		wa1 -> 1,
		whp -> 1
	};
(*End[]*)


(* ::Section::Closed:: *)
(*Parameter Names for HiggsBounds*)


BoundsParams[np_,cp_]:={
	{"MH_GammaTot.dat",
		Flatten[
			{Table[Mh[[j]],{j,np}], Table[MhGamma[[j]],{j,np}] }
		]
	},
	{"MHplus_GammaTot.dat",
		Flatten[
			{Table[Mhplus[[j]],{j,cp}], Table[MhplusGamma[[j]],{j,cp}] }
		]
	},
	{"effC.dat",
		Flatten[
			{Transpose[Table[g2hj[[j,k]],{j,np},{k,18}]], Table[g2hjhiz[[j,k]],{j,np},{k,j}] }
		]
	},
	{"BR_H_NP.dat",
		Flatten[
			{Table[BRhjhh[[j,j]],{j,np}], DeleteCases[Table[If[k=!=j,BRhjhh[[j,k]],{}],{j,np},{k,np}],{}]}
		]
	},
	{"BR_t.dat",
		Flatten[
			{BRtWpb, Table[BRtHpb[[j]],{j,cp}]}
		]
	},
	{"BR_Hplus.dat",
		Flatten[
			Table[{BRHpcs[[j]],BRHpcb[[j]],BRHptaunu[[j]]}, {j,cp}]
		]
	},
	{"LEP_HpHm_CS_ratios.dat",
			Flatten[
				Table[HpHmRatio[[j,k]],{j,cp},{k,cp}]
			]
	}
};


(* ::Section::Closed:: *)
(*Default values for HiggsBounds*)


Begin["Private`"]
DefBounds[np_,cp_]:={
	Mh->Table[0,{j,np}],
	MhGamma -> Table[0,{j,np}],
	Mhplus -> Table[0,{j,cp}],
	MhplusGamma -> Table[0,{j,cp}],
	g2hj -> Table[0,{j,np},{k,18}],
	g2hjhiz -> Table[0,{j,np},{k,np}],
	BRhjhh -> Table[0,{j,np},{k,np}],
	BRtWpb -> 0,
	BRtHpb -> Table[0,{j,cp}],
	BRHpcs -> Table[0,{j,cp}],
	BRHpcb -> Table[0,{j,cp}],
	BRHptaunu -> Table[0,{j,cp}],
	HpHmRatio -> Table[0,{j,cp},{k,cp}]
};
End[]


(* ::Section:: *)
(*StockHB[]*)


Begin["Private`"]
	StockHB[inpt_, npdg_, cpdg_, np_, cp_, nsca_] := Module[
	{lst, smdecays, fermionmass,
	tMh, tMhGamma, tMhplus, tMhplusGamma, tg2hj, tg2hjhiz,
	tBRhjhh, tBRtWpb, tBRtHpb, tBRHpcs, tBRHpcb, tBRHptaunu,
	SMGamma, BR, BRSM, case={},
	tHpb={}, neut={}, tHff={}, tBos={},
	finalrep},
	
	lst={inpt[[1]],inpt[[2]],inpt[[3]],inpt[[4]],inpt[[5,1]]};
	smdecays={#[[1]], #[[2]], Flatten[{#[[1]],Sort[ #[[2;;All]] ]},1]& /@Abs[#[[3]]] }& /@ inpt[[5,2]]; (*separate and prepare for usage. Sort Abs[PDGs]*)
	SMGamma={smdecays[[1,2]],smdecays[[2,2]],smdecays[[3,2]]};
	
	BR=0; BRSM=0;
	
	tMh=lst[[1,2;;(2+np-1)]];
	tMhplus = lst[[1,(2+np+1);;(3+np+cp-1)]];	
	tHpb=Table[Sort[Abs[{cpdg[[j]],5}]],{j,cp}];  (*pdg list for charged higgses and bottom coming from the top*)
	tHff={ {3,3}, {4,4}, {5,5}, {6,6}, {13,13}, { 15, 15} };
	tBos={{24,24}, {23,23}, {22,23}, {22,22}, {21,21}, {21,21,23} };
	fermionmass={ lst[[1,18]],lst[[1,15]],lst[[1,19]],lst[[1,16]],lst[[1,8]],lst[[1,9]]  };
	(*Gammaferm=SMGamma*{0, 0, 5.84*10^-1, 0, 2.18*10^-4, 6.27*10^-2}; (*rpp2018-rev-higgs-boson*)
	Gammabos=SMGamma*{2.14*10^-1, 2.62*10^-2, 1.53*10^-3, 2.27*10^-3, 0, 0};*)
	
	(*setting all the values to 0/1 by default*)
	tMhGamma=Table[1,{j,np}];
	tMhplusGamma = Table[1,{j,cp}];
	tg2hj = Table[0,{j,np},{k,18}];
	tg2hjhiz = Table[0,{j,np},{k,np}];
	tBRhjhh = Table[0,{j,np},{k,np}];
	tBRtWpb=0; (*24 5*)
	tBRtHpb=Table[0,{j,cp}]; (*chH 5*)
	tBRHpcs=Table[0,{j,cp}]; (* 4 3 *)
	tBRHpcb=Table[0,{j,cp}]; (* 4 5*)
	tBRHptaunu=Table[0,{j,cp}]; (*-15 16 *)
	
	
	(*get branching ratios*)
	(*  top quark  *)
	If[MemberQ[lst[[5]], {6 ,a_ ,{b___}}], (*check if particle exists*)
		case=Cases[lst[[5]],{6 ,a_ ,{b___}}]; 
		case={#[[1]], #[[2]], Flatten[{#[[1]],Sort[ #[[2;;All]] ]},1]& /@Abs[#[[3]]] }& /@ case; (*prepare data for readout*)
		If[MemberQ[Abs[case[[1,3]]],{a_, 5,24}], (*check for the needed branching ratio*)
			tBRtWpb=Flatten[
						Cases[Abs[case[[1,3]]],{a_, 5,24}]
					][[1]] (*readout the value*)
		];
		For[j=1, j<=cp,j++,
			If[MemberQ[Abs[case[[1,3]]], Flatten[{a_, tHpb[[j]]}] ], (*check for the needed branching ratio*)
				tBRtHpb[[j]]=Flatten[
							Cases[ Abs[case[[1,3]]],Flatten[{a_, tHpb[[j]]}]  ]
						][[1]] (*readout the value*)
			];			
		];
	];
	case={};
	(*Hp to cs, to cb, and to taunu*)
	For[j=1, j<=cp,j++,
		If[MemberQ[lst[[5]], {cpdg[[j]] ,a_ ,{b___}}], (*check if particle exists*)
			case=Cases[lst[[5]],{cpdg[[j]] ,a_ ,{b___}}];
			tMhplusGamma[[j]]=case[[1,2]];
			case={#[[1]], #[[2]], Flatten[{#[[1]],Sort[ #[[2;;All]] ]},1]& /@Abs[#[[3]]] }& /@ case; (*prepare data for readout*)
			If[MemberQ[Abs[case[[1,3]]],{a_, 3,4}], (*check for the needed branching ratio*)
				tBRHpcs=Flatten[
							Cases[Abs[case[[1,3]]],{a_, 3,4}]
						][[1]] (*readout the value*)
			];
			If[MemberQ[Abs[case[[1,3]]],{a_, 4,5}], (*check for the needed branching ratio*)
				tBRHpcb=Flatten[
							Cases[Abs[case[[1,3]]],{a_, 4,5}]
						][[1]] (*readout the value*)
			];
			If[MemberQ[Abs[case[[1,3]]],{a_, 15,16}], (*check for the needed branching ratio*)
				tBRHptaunu=Flatten[
							Cases[Abs[case[[1,3]]],{a_, 15, 16}]
						][[1]] (*readout the value*)
			];
		]		
	];
	(*scalar Higgses*)
	For[j=1, j<=np, j++,
		If[MemberQ[lst[[5]], {npdg[[j]] ,a_ ,{b___}}], (*check if particle exists*)
			case=Cases[lst[[5]],{npdg[[j]] ,a_ ,{b___}}]; 
			tMhGamma[[j]]=case[[1,2]];
			case={#[[1]], #[[2]], Flatten[{#[[1]],Sort[ #[[2;;All]] ]},1]& /@Abs[#[[3]]] }& /@ case; (*prepare data for readout*)
			For[k=1, k<=np,k++,
				neut={npdg[[k]],npdg[[k]]};
				(*Higgs to other Higgses branchings*)
				If[MemberQ[Abs[case[[1,3]]], Flatten[{a_, neut}] ], (*check for the needed branching ratio*)
					tBRhjhh[[j,k]]=Flatten[
										Cases[ Abs[case[[1,3]]],Flatten[{a_, neut}]  ]
									][[1]] (*readout the value*)
				];
				neut=Sort[{npdg[[k]], 23}];
				(*Higgs to a Higgs and a Z, no references.... set to 0!*)
				If[MemberQ[Abs[case[[1,3]]], Flatten[{a_, neut}] ], (*check for the needed branching ratio*)
					tg2hjhiz[[j,k]]=Flatten[
										Cases[ Abs[case[[1,3]]],Flatten[{a_, neut}]  ]
									][[1]]*0 (*readout the value*)
				]			
			];
			(*couplings to fermions*)
			For[m=1,m<=Length[tHff], m++,
				If[MemberQ[Abs[case[[1,3]]], Flatten[{a_, tHff[[m]]}] ] &&
					MemberQ[Abs[smdecays[[j,3]]], Flatten[{a_, tHff[[m]]}] ], (*check for the needed branching ratio in current model and SM*)
					BR=Flatten[
							Cases[ Abs[case[[1,3]]],Flatten[{a_, tHff[[m]]}]  ]
						][[1]]; (*readout the value*)
					BRSM=Flatten[
							Cases[ Abs[smdecays[[j,3]]],Flatten[{a_, tHff[[m]]}]  ]
						][[1]];
					If[j<=nsca,
						(*scalar*)
						tg2hj[[j, (2*m-1)]]=BR*tMhGamma[[j]]/(BRSM*SMGamma[[j]])(*;
						Print["scalar!\n","j m: ",j," and ",m,"\n BR: ",BR, "\n BRSM: ",BRSM,"\n tg2hj: ",tg2hj[[j, (2*m-1)]] ]*),
						(*pseudoscalar*)
						tg2hj[[j, 2*m]]=(1-4*fermionmass[[m]]^2/tMh[[j]]^2)*BR*tMhGamma[[j]]/(BRSM*SMGamma[[j]])(*;
						Print["pseudoscalar!\n","j m: ",j," and ",m,"\n BR: ",BR, "\n BRSM: ",BRSM,"\n tg2hj: ",tg2hj[[j, 2*m]] ]*)
					];
					BR=0;
					BRSM=0;
				];
			];
			(*couplings to bosons*)
			For[m=1,m<=Length[tBos], m++,
				If[MemberQ[Abs[case[[1,3]]], Flatten[{a_, tBos[[m]]}] ] &&
					MemberQ[Abs[smdecays[[j,3]]], Flatten[{a_, tBos[[m]]}] ], (*check for the needed branching ratio*)
					BR=Flatten[
							Cases[ Abs[case[[1,3]]],Flatten[{a_, tBos[[m]]}]  ]
						][[1]]; (*readout the value*)
					BRSM=Flatten[
							Cases[ Abs[smdecays[[j,3]]],Flatten[{a_, tBos[[m]]}]  ]
						][[1]];	
						tg2hj[[j, (12+m)]]=BR*tMhGamma[[j]]/(BRSM*SMGamma[[j]]);
						(*Print["bosons\n","j m: ",j," and ",m,"\n BR: ",BR, "\n BRSM: ",BRSM,"\n tg2hj: ",tg2hj[[j, (12+m)]] ];*)
					BR=0;
					BRSM=0;
				]
			]			
		]	
	
	];
	BR=0;
	neut={};
	
	finalrep={
		BRtWpb -> tBRtWpb,
		BRtHpb -> tBRtHpb,
		BRHpcs -> tBRHpcs,
		BRHpcb -> tBRHpcb,
		BRHptaunu -> tBRHptaunu,
		BRhjhh -> tBRhjhh,
		g2hj -> tg2hj,
		g2hjhiz -> tg2hjhiz,
		MhGamma -> tMhGamma,
		MhplusGamma -> tMhplusGamma,
		Mh->tMh,
		Mhplus -> tMhplus
		(*LepHmHp production rates are 0 by default*)
	}	
];
End[]


(* ::Section:: *)
(*End*)


EndPackage[]
