(* ::Package:: *)

(* ::Chapter:: *)
(*Setup and Definitions*)


dir=$HomeDirectory<>"/Desktop/PhysPrograms/FlexibleSUSY-2.1.0";
SetDirectory[dir];
<<"/home/simonas/Desktop/MadWidthScan/MadScan.m";
pathmg= "/home/simonas/Desktop/PhysPrograms/MG5_aMC_v2_6_3_2";
pathhb= "/home/simonas/Desktop/PhysPrograms/HiggsBounds-4.3.1/HiggsBounds";
pathin="/home/simonas/Desktop/OneDrive/Perticles/Vasara2018/FS_MG_HB/NightlyRun-08-23";
name="loop_card.dat";
Get[dir<>"/models/Flexible312/Flexible312_librarylink.m"];
cm=72/2.54; (*e.g. ImageSize\[Rule] 5*cm*)


(* ::Section:: *)
(*ScanOver*)


ScanOver[z_]:=
    Module[{handle,spectrum,output, out},
			handle= FSFlexible312OpenHandle[
               fsSettings -> {
                   precisionGoal -> 1.*^-4,           (* FlexibleSUSY[0] *)
                   maxIterations -> 0,                (* FlexibleSUSY[1] *)
                   solver -> 0,                       (* FlexibleSUSY[2] *)
                   calculateStandardModelMasses -> 1, (* FlexibleSUSY[3] *)
                   poleMassLoopOrder -> 1,            (* FlexibleSUSY[4] *)
                   ewsbLoopOrder -> 1,                (* FlexibleSUSY[5] *)
                   betaFunctionLoopOrder -> 1,        (* FlexibleSUSY[6] *)
                   thresholdCorrectionsLoopOrder ->1,(* FlexibleSUSY[7] *)
                   higgs2loopCorrectionAtAs -> 0,     (* FlexibleSUSY[8] *)
                   higgs2loopCorrectionAbAs -> 0,     (* FlexibleSUSY[9] *)
                   higgs2loopCorrectionAtAt -> 0,     (* FlexibleSUSY[10] *)
                   higgs2loopCorrectionAtauAtau -> 0, (* FlexibleSUSY[11] *)
                   forceOutput -> 0,                  (* FlexibleSUSY[12] *)
                   topPoleQCDCorrections -> 2,        (* FlexibleSUSY[13] *)
                   betaZeroThreshold -> 1.*^-11,      (* FlexibleSUSY[14] *)
                   forcePositiveMasses -> 1,          (* FlexibleSUSY[16] *)
                   poleMassScale -> 0,                (* FlexibleSUSY[17] *)
                   eftPoleMassScale -> 0,             (* FlexibleSUSY[18] *)
                   eftMatchingScale -> 0,             (* FlexibleSUSY[19] *)
                   eftMatchingLoopOrderUp -> 0,       (* FlexibleSUSY[20] *)
                   eftMatchingLoopOrderDown -> 0,     (* FlexibleSUSY[21] *)
                   eftHiggsIndex -> 1,                (* FlexibleSUSY[22] *)
                   calculateBSMMasses -> 1,           (* FlexibleSUSY[23] *)
                   thresholdCorrections -> 123111321, (* FlexibleSUSY[24] *) (* default is fine 123111321. alphaem, sinW, alphas,mZ,mW,mh,mt,mb,mtau*)
                   higgs3loopCorrectionRenScheme -> 0,(* FlexibleSUSY[25] *)
                   higgs3loopCorrectionAtAsAs ->0,   (* FlexibleSUSY[26] *)
                   higgs3loopCorrectionAbAsAs -> 0,   (* FlexibleSUSY[27] *)
                   higgs3loopCorrectionAtAtAs -> 0,   (* FlexibleSUSY[28] *)
                   higgs3loopCorrectionAtAtAt -> 0,   (* FlexibleSUSY[29] *)
                   higgs4loopCorrectionAtAsAsAs -> 0, (* FlexibleSUSY[30] *)
                   parameterOutputScale -> 0          (* MODSEL[12] *)
               },
               fsSMParameters -> {
                   alphaEmMZ -> 1/127.916, (* SMINPUTS[1] *)
                   GF -> 1.166378700*10^-5,  (* SMINPUTS[2] *)
                   alphaSMZ -> 0.1182,     (* SMINPUTS[3] *)
                   MZ -> 91.1876,          (* SMINPUTS[4] *)
                   mbmb -> 4.18,           (* SMINPUTS[5] *)
                   Mt -> 173.1,           (* SMINPUTS[6] *)
                   Mtau -> 1.77686,        (* SMINPUTS[7] *)
                   Mv3 -> 0.05124*10^-9,               (* SMINPUTS[8] *)
                   MW -> 80.385,           (* SMINPUTS[9] *)
                   Me -> 0.000510998902,   (* SMINPUTS[11] *)
                   Mv1 -> 0,               (* SMINPUTS[12] *)
                   Mm -> 0.1056583715,     (* SMINPUTS[13] *)
                   Mv2 -> 0.0086948*10^-9,               (* SMINPUTS[14] *)
                   md2GeV -> 0.00475,      (* SMINPUTS[21] *)
                   mu2GeV -> 0.0024,       (* SMINPUTS[22] *)
                   ms2GeV -> 0.104,        (* SMINPUTS[23] *)
                   mcmc -> 1.27,           (* SMINPUTS[24] *)
                   CKMTheta12 -> 0.227005,
                   CKMTheta13 -> 0.00348523,
                   CKMTheta23 -> 0.0410903,
                   CKMDelta -> 1.23562,
                   PMNSTheta12 -> 0*0.602139,
                   PMNSTheta13 -> 0*0.147306,
                   PMNSTheta23 -> 0*0.715585,
                   PMNSDelta -> 0*1.4,
                   PMNSAlpha1 -> 0,
                   PMNSAlpha2 -> 0,
                   alphaEm0 -> 1/137.035999074,
                   Mh -> 125.09
               },
               fsModelParameters -> {
                   Z1111IN ->z[[1]], (*Z1*) 
                   Z2222IN -> z[[2]], (*Z2*)
                   Z2211IN -> z[[3]], (*ConZ3*)
                   Z2112IN -> z[[4]], (*ConZ4*)
                   Z2121rIN -> z[[5]], (*ConZ5*)
                   Z1112rIN -> z[[6]], (*conZ6*) 
                   Z2212rIN ->z[[7]], (*conZ7*)
                   M222IN -> z[[8]], (*mHpm^2-SMvev^2/2*Z2211IN*)
                   MRrIN -> 0,
                   etaIN -> 0,
                   Qin -> 126,
                   Z2121iIN ->0,
                   Z1112iIN ->0,
                   Z2212iIN -> 0,
                   MRiIN->0,
                   
                   Yv1rIN -> {0,0,0},
                   Yv1iIN -> {0,0,0},
                   
                   Yv2rIN -> {0,0,0},
                   Yv2iIN -> {0,0,0},
                   
                   Ye2rIN -> {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}},
                   Ye2iIN -> {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}},
                   
                   Yd2rIN -> {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}},
                   Yd2iIN -> {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}},
                   
                   Yu2rIN -> {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}},
                   Yu2iIN -> {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
               }
           ];   
              spectrum = FSFlexible312CalculateSpectrum[handle];
              output=FSFlexible312GetInputParameters[handle];
              FSFlexible312CloseHandle[handle];
              out=If[spectrum === $Failed,
                     Array[invalid&, 1],
                     {
                      (*1*){(*1-4*)Pole[M[hh]],(*5-6*)Pole[M[Hm]],(*7-9*)Pole[M[Fe]],(*10-13*)Pole[M[Fv]],(*14-16*)Pole[M[Fu]],(*17-19*)Pole[M[Fd]]},
                      (*2*)Pole[ZH],
                      (*3*){(*1*)Z1111, Z2222, Z2211, Z2112, Z2121, Z1112, Z2212, (*8*)M222, (*9*)M112, (*10*)M12, (*11*)MR,(*12*) v},
                      (*4*){(*1,3x3*)Ye1, (*2,3x3*)Ye2, (*3,1x3*)Yv1, (*4,1x3*)Yv2, (*5,3x3*)Yu1, (*6,3x3*)Yu2, (*7,3x3*)Yd1, (*8,3x3*)Yd2, (*9,4x4*)Pole[Vpmns]}
              
                     }/. (Flexible312 /. spectrum) /.output
                     ]
 ];


(* ::Section::Closed:: *)
(*FindHiggsRelat*)


(* ::Input::Initialization:: *)
FindHiggsRelation[ err_]:=Module[
	{zrandom,zcurr,mhp, mhSM=125.09, vSM=246.22, mhcurr,mhprev, direction,savedirection,
	startstep=0.0001, out2={}, counter=0, exitStatus=0, maxCount=4, local=0,
	mh, mH, mA, teta12,s12,c12,s13,mZ,
derivatives={}},
	zrandom=Table[0,{j,8}];
	zcurr=Table[0,{j,8}];
	While[  If[out2==={} || out2==={invalid}, True,  
				(*check the masses to be reasonable, also a scalar Higgs should be the light one*)
				If[ (out2[[1,1,2]]<94  || out2[[1,1,2]]>900 )  ||
					((*out2[[1,1,3]]<180  ||*) out2[[1,1,3]]>1600 ) ||
					((*out2[[1,1,4]]<180  ||*) out2[[1,1,4]]>1600 )||
					((*out2[[1,2,2]]<200  ||*) out2[[1,2,2]]>2000) || 
					(out2[[2,2,1]]===0.  && out2[[2,2,2]]===0. ) (*|| 
			ZMassConstraints[{out2}]==={}*)   ,True, False
				]
		], (*find a point for the start of the search*)
		(*involve tree level relations here!*)
		mh=RandomReal[{50,800}];
		mH=RandomReal[{180,1000}];
		mA=RandomReal[{180,1000}];
		teta12=RandomReal[{-\[Pi],\[Pi]}];
		s12=Sin[teta12];
		c12=Cos[teta12];
		s13=0;
		mZ=91.1876;
		mhp=RandomReal[{200,2000}];
		            (*tree level Z functions, maybe rederive these from Sarah? the angle seems to be incorrect*)
            (*Z1*)zrandom[[1]]=(1/vSM^2)*(c12^2*(1-s13^2)*mh^2+(1-s13^2)*s12^2*mH^2+s13^2*mA^2)/2;
		zrandom[[2]]=RandomReal[{0,4*\[Pi]}];
		zrandom[[3]]=RandomReal[{0,4*\[Pi]}];
           (*Z4*) zrandom[[4]]=(1/vSM^2)*(c12^2*s13^2*mh^2+c12^2*mH^2+
                               s12^2*(mh^2+s13^2*mH^2)+(1-s13^2)*mA^2-2*mhp^2);
                               
            (*Z5*) zrandom[[5]]=(1/vSM^2)*(c12^2*(-s13^2*mh^2+mH^2)+s12^2*(mh^2-s13^2*mH^2)-(1-s13^2)*mA^2+
                                   I*(2*c12*s12*s13*(-mh^2+mH^2)))/(-2);
                                   
            (*Z6*) zrandom[[6]]=(1/vSM^2)*(c12*Sqrt[(1-s13^2)]*s12*(mh^2-mH^2)+
                                   I*(Sqrt[(1-s13^2)]*s13*(-c12^2*mh^2-s12^2*mH^2+mA^2))); 		
		zrandom[[7]]=RandomReal[{0,2*\[Pi]}];
		mhp=RandomReal[{200,2000}];
		zrandom[[8]]=mhp^2-vSM^2/2*zrandom[[3]];

		out2=ScanOver[zrandom];
		mhprev=If[out2=!={invalid},out2[[1,1,2]],1000];
		mhcurr=mhprev;
	];
	(*Print["Found a sweet spot!: ",zrandom," ",out2];*)

	While[counter<maxCount && Abs[mhcurr-mhSM]>=err (*&& exitStatus===0*),
		direction=Table[0.,{j,8}];
	savedirection=Table[0.,{j,8}];
		exitStatus=0;
		(*Find a direction*)
		For[i=1,i<=8,i++,
			out2=ScanOver[ReplacePart[zrandom, i->zrandom[[i]]+startstep*zrandom[[i]]  ]];
			If[out2=!={invalid},
				mhcurr=out2[[1,1,2]];
				If[(mhprev-mhcurr)=!=0. && (startstep*zrandom[[i]])=!=0.,
			savedirection[[i]]=(mhprev-mhcurr)/(startstep*zrandom[[i]]);
					direction[[i]]=-(1/savedirection[[i]])*Abs[(mhSM-mhcurr)];,
					direction[[i]]=0;
				];
			]
		];
	AppendTo[derivatives, {savedirection,zrandom}]; (*save derivatives and the direction*)
		(*Print["The way to went: ", direction];*)
		direction=direction/Sqrt[direction.direction];
		If[mhcurr<mhSM,direction=-direction ]; (*needs to go in the opposite direction in case one needs to reach the SM mass from below*)
		If[Mean[Abs[direction[[1;;7]]]]>10^-3,
			While[Mean[Abs[direction[[1;;7]]]]>10^-3,
				direction=direction/10;
			];,
			If[Mean[Abs[direction[[1;;7]]]]<10^-3,
				While[Mean[Abs[direction[[1;;7]]]]<10^-3,
					direction=direction*10;
				];
			]
		];
		(*Print["The way to go: ", direction];*)
		mhprev=ScanOver[zrandom][[1,1,2]];
		mhcurr=mhprev;
		If[direction===Table[0.,{j,8}],(*Print["Did not find a valid direction!"];*)exitStatus=1; out2={invalid},
			
			zcurr=zrandom;
		
			While[ If[mhSM<mhcurr,If[mhcurr<=mhprev,True, False], If[mhcurr<=mhprev,False, True]  ] &&
				 Abs[mhcurr-mhSM]>=err && out2=!={invalid}  ,

				mhprev=mhcurr;
				zcurr=zcurr-direction*(1+Abs[mhSM-mhcurr]/mhcurr);
				out2=ScanOver[zcurr];
				If[out2=!={invalid} ,If[out2[[2,2,1]]===0. && out2[[2,2,2]]===0., out2={invalid}] ];
				If[out2=!={invalid} ,
					mhcurr=out2[[1,1,2]];
					If[mhSM<mhcurr, If[mhcurr>mhprev, (*Print["Gettin bigger "];*)exitStatus=1],
									If[mhcurr<mhprev, (*Print["Gettin smaller "];*)exitStatus=1] ],
					exitStatus=1;
				];
				local++;
			];
			If[exitStatus=!=0,(* Print["My exit ", exitStatus];*)
		zrandom=zcurr+direction*(1+Abs[mhSM-mhcurr]/mhcurr)*(local/10+1) ]
			(*Print["Steps taken: ",local, " mh: ", mhcurr];*)
		]; 
		counter++
		(*If[counter===maxCount, If[exitStatus===1,out2={invalid} ]  ]; save everything*)
	];
Print["Steps taken: ",local, " mh: ", mhcurr];
	{derivatives,out2}
];


(* ::Section::Closed:: *)
(*FindHiggsRandom*)


(* ::Input::Initialization:: *)
FindHiggsRandom[ err_]:=Module[
	{zrandom,zcurr,mhp, mhSM=125.09, vSM=246.22, mhcurr,mhprev, direction,
	startstep=0.0001, out2={}, counter=0, exitStatus=0, maxCount=4, local=0,
	mh, mH, mA, teta12,s12,c12,s13,mZ},
	zrandom=Table[0,{j,8}];
	zcurr=Table[0,{j,8}];
	While[  If[out2==={} || out2==={invalid}, True,  
				(*check the masses to be reasonable, also a scalar Higgs should be the light one*)
				If[ (out2[[1,1,2]]<94  || out2[[1,1,2]]>900 )  ||
					(out2[[1,1,3]]<180  || out2[[1,1,3]]>1500 ) ||
					(out2[[1,1,4]]<180  || out2[[1,1,4]]>1500 )||
					(out2[[1,2,2]]<200  || out2[[1,2,2]]>2000) || 
					(out2[[2,2,1]]===0.  || out2[[2,2,2]]===0. )   ,True, False
				]
		], (*find a point for the start of the search*)
		(*involve tree level relations here!*)

		zrandom[[1]]=RandomReal[{0,4*\[Pi]}];
		zrandom[[2]]=RandomReal[{0,4*\[Pi]}];
		zrandom[[3]]=RandomReal[{0,4*\[Pi]}];
		zrandom[[4]]=RandomReal[{0,4*\[Pi]}];
		zrandom[[5]]=RandomReal[{-4*\[Pi],4*\[Pi]}];
		zrandom[[6]]=RandomReal[{0,2*\[Pi]}];
		zrandom[[7]]=RandomReal[{0,2*\[Pi]}];
		mhp=RandomReal[{200,2000}];
		zrandom[[8]]=mhp^2-vSM^2/2*zrandom[[3]];

		out2=ScanOver[zrandom];
		mhprev=If[out2=!={invalid},out2[[1,1,2]],1000];
		mhcurr=mhprev;
	];
	(*Print["Found a sweet spot!: ",zrandom," ",out2];*)

	While[counter<maxCount && Abs[mhcurr-mhSM]>=err (*&& exitStatus===0*),
		direction=Table[0.,{j,8}];
		exitStatus=0;
		(*Find a direction*)
		For[i=1,i<=8,i++,
			out2=ScanOver[ReplacePart[zrandom, i->zrandom[[i]]+startstep*zrandom[[i]]  ]];
			If[out2=!={invalid},
			mhcurr=out2[[1,1,2]];
			If[(mhprev-mhcurr)=!=0.,
				direction[[i]]=-(startstep*zrandom[[i]])/(mhprev-mhcurr)*Abs[(mhSM-mhcurr)];,
				direction[[i]]=0;
			];
			]
		];
		(*Print["The way to went: ", direction];*)
		direction=direction/Sqrt[direction.direction];
		If[mhcurr<mhSM,direction=-direction ]; (*needs to go in the opposite direction in case one needs to reach the SM mass from below*)
		If[Mean[Abs[direction[[1;;7]]]]>10^-3,
			While[Mean[Abs[direction[[1;;7]]]]>10^-3,
				direction=direction/10;
			];,
			If[Mean[Abs[direction[[1;;7]]]]<10^-3,
				While[Mean[Abs[direction[[1;;7]]]]<10^-3,
				direction=direction*10;
			];
			]
		];
		(*Print["The way to go: ", direction];*)
		mhprev=ScanOver[zrandom][[1,1,2]];
		mhcurr=mhprev;
		If[direction===Table[0.,{j,8}],(*Print["Did not find a valid direction!"];*)exitStatus=1; out2={invalid},
			
			zcurr=zrandom;
			local=0;
			While[ If[mhSM<mhcurr,If[mhcurr<=mhprev,True, False], If[mhcurr<=mhprev,False, True]  ] &&
				 Abs[mhcurr-mhSM]>=err && out2=!={invalid}  ,

				mhprev=mhcurr;
				zcurr=zcurr-direction*(1+Abs[mhSM-mhcurr]/mhcurr);
				out2=ScanOver[zcurr];
				If[out2=!={invalid} ,If[out2[[2,2,1]]===0. && out2[[2,2,2]]===0., out2={invalid}] ];
				If[out2=!={invalid} ,
					mhcurr=out2[[1,1,2]];
					If[mhSM<mhcurr, If[mhcurr>mhprev, (*Print["Gettin bigger "];*)exitStatus=1],
									If[mhcurr<mhprev, (*Print["Gettin smaller "];*)exitStatus=1] ],
					exitStatus=1;
				];
				local++;
			];
			If[exitStatus=!=0,(* Print["My exit ", exitStatus];*) zrandom=zcurr+direction*(1+Abs[mhSM-mhcurr]/mhcurr)*(local/10+1) ];
			Print["local: ",local, " mh: ", mhcurr];
		]; 
		counter++
		(*If[counter===maxCount, If[exitStatus===1,out2={invalid} ]  ]; save everything*)
	];
	out2
];


(* ::Section::Closed:: *)
(*Kernels*)


LaunchKernels[];
DistributeDefinitions[{ScanOver, FindHiggsRandom ,FindHiggsRelation}];
SetSharedVariable[monitor];
monitor=0;


(* ::Chapter:: *)
(*Calculations*)


(* ::Section::Closed:: *)
(*Call FS*)


dotimes=2000;
idata=Monitor[ParallelTable[monitor++;
		FindHiggsRelation[0.3],{k, dotimes}
	],{ProgressIndicator[monitor,{0, dotimes}], ToString[monitor]<>"/"<>ToString[dotimes]}];


(* ::Section::Closed:: *)
(*Clean Up FS Output*)


derivatives=Flatten[#[[1]]&/@idata,1];
idatanew=#[[2]]&/@idata;


idata2=DeleteCases[idatanew,{invalid}];
idata3=DeleteCases[If[#[[2,2,2]]=!=0. && #[[2,2,1]]=!=0., #]&/@idata2, Null];
idata4=Swapper[idata3];
idata5={Flatten[#[[1]]],#[[2]],#[[3]],#[[4]]}&/@idata4;
idata6=DeleteCases[If[Abs[#[[1,2]]-125.09]<0.32, #]&/@idata5,Null];
(*idata7=ZMassConstraints[idata6];*)


(* ::Section::Closed:: *)
(*Call MadGraph*)


dalyk=CallMadGraph["Flexible312", pathmg, pathin, name, idata6, " h1 h2 a1 t hp ", " --body_decay=2.0025"];


(* ::Section::Closed:: *)
(*CallHiggsBounds*)


final=CallHB[pathhb, pathin, dalyk];


(* ::Section:: *)
(*Working Area *)
