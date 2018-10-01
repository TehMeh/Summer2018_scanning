(* ::Package:: *)

(* ::Chapter:: *)
(*Setup and Definitions*)


dir=" < Path to /FlexibleSUSY-2.1.0 >";
SetDirectory[dir];
Get[dir<>"/models/Flexible312/Flexible312_librarylink.m"];


(* ::Section:: *)
(*ScanOverNeutrinos*)


ScanOverNeutrinos[z_,mass_,lamb_, phi_, MRin_ ]:=
    Module[{handle,spectrum,output, out,
			mh,mH,mA,mHpm, teta12, s12,c12,s13, fiprime, lambdad, mnu2,mnu3,mnu4, md2,mZ, SMvev,
			f1,f2,f3, d, polyn, a4,a3,a2,a1,a0, dprime, P,Q,disc, solut,dprime1,
			A, B, C1, exp2alpha, tbeta,expdelta,expgamma, cbar,sbar, R, PMNS, PMNSnew,
			Yv1rnum, Yv2rnum, Yv1inum, Yv2inum},
           mh=mass[[1]];
           mH=mass[[2]];
           mA=mass[[3]];
           mHpm=mass[[4]];
           teta12=mass[[5]];
           s12=Sin[teta12];
           c12=Cos[teta12];
           s13=0;
           mnu2=0.0086948*10^-9; (*arxiv:1708.01186 data*)
	       mnu3=0.0504975*10^-9; (*pataisyta*)
           mnu4=MRin;
           fiprime=phi;
           lambdad=lamb;
           md2=lambdad^2*mnu3*mnu4;
           mZ=91.1876;
           (*SMvev=z[[12]];*) (*optional*)
           SMvev=246.22;
           f1=1/(32 \[Pi]^2)*(s12^2*mh^2/mnu4*Log[mnu4^2/mh^2]
               +c12^2*mH^2/mnu4*Log[mnu4^2/mH^2]-mA^2/mnu4*Log[mnu4^2/mA^2]);
           f2=1/(32 \[Pi]^2)*Sqrt[2]*c12*s12*(mh^2/mnu4*Log[mnu4^2/mh^2]
               -mH^2/mnu4*Log[mnu4^2/mH^2]);
           f3=1/(32 \[Pi]^2)*2*(3*mZ^2/mnu4*Log[mnu4^2/mZ^2]-(c12^2*mh^2/mnu4*Log[mnu4^2/mh^2]
               +s12^2*mH^2/mnu4*Log[mnu4^2/mH^2]))-SMvev^2/mnu4;   (*pataisyta*)
            d=Sqrt[SMvev^2/md2*mnu2*mnu3/Abs[f1*f3+f2^2]];
            polyn=a4*dprime^4+a3*dprime^3+a2*dprime^2+a1*dprime+a0;
            a4=f1^2;
            a3=4*Sin[fiprime]*Sqrt[md2]/SMvev*f1*f2;
            a2=2*(d^2*f1^2+(2*f2^2+Cos[2*fiprime]*f1*f3)*md2/SMvev^2);
            a1=4*Sin[fiprime]*Sqrt[md2]/SMvev*f2*(d^2*f1-md2/SMvev^2*f3); (*pataisyta*)
            a0=d^4*f1^2+2*d^2*md2/SMvev^2*f2^2+md2^2/SMvev^4*f3^2-mnu2^2-mnu3^2;
            P=8*a4*a2-3*a3^2;
            Q=64*a4^3*a0-16*a4^2*a2^2+16*a4*a3^2*a2-16*a4^2*a3*a1-3*a3^4;
            If[polyn=!=Indeterminate,
            disc=Discriminant[polyn,dprime];
            solut=If[disc<=0,NSolve[polyn==0, dprime, Reals]/.{dprime->b_}->b,
                         If[P<=0 && Q<=0, NSolve[polyn==0,dprime, Reals]/.{dprime->b_}->b, {-1}]
                    ];
             If[solut==={} || solut==={-1},dprime1={-1},
                           dprime1=Select[Select[solut,Positive],#<=1&]
                     ]; 
                    , dprime=-1;d=0;
                ];   
             If[dprime1==={-1} || dprime1==={},dprime1=-1,dprime1=Min[dprime1]]; 
             dprime=dprime1*Exp[I*fiprime];
             A=d^2*f1;
             B=dprime*d*f1+I*d*Sqrt[md2]/SMvev*f2;
             C1=dprime^2*f1+2*I*dprime*Sqrt[md2]/SMvev*f2+md2/SMvev^2*f3; (*C1 since C seems to be taken by mathematica*)
             exp2alpha=-Sqrt[Abs[f1*f3+f2^2]/(f1*f3+f2^2)]; (*istraukiu sakni ir pasirenku sprendini su minusu*)
             tbeta=-Sqrt[(Abs[A]^2+Abs[B]^2-mnu2^2)/(mnu3^2-Abs[A]^2-Abs[B]^2)]; (*pasirenku sprendini su minusu*)
             expdelta=(Conjugate[A]*B+Conjugate[B]*C1)*tbeta/(mnu2^2-Abs[A]^2-Abs[B]^2);
             expgamma=mnu2/(exp2alpha*(expdelta*A+B*tbeta));(*pakeista i exp2alpha*)
             cbar=1/Sqrt[1+tbeta^2]*expgamma^(1/2)*expdelta^(1/2); (*pataisyta*)
             sbar=tbeta/Sqrt[1+tbeta^2]*expgamma^(1/2)*expdelta^(-1/2); (*pataisyta*)
             If[polyn===Indeterminate || A===Indeterminate || B===Indeterminate || C1===Indeterminate 
                  || exp2alpha===Indeterminate || tbeta===Indeterminate || Im[tbeta]=!=0 || expdelta===Indeterminate
                  || expgamma===Indeterminate || cbar===Indeterminate || sbar===Indeterminate , dprime1=-1,True];
             If[dprime1=!=-1,
                  PMNS={{Cos[th12]*Cos[th13],Cos[th13]*Sin[th12],Exp[-I*CP]*Sin[th13]},
                       {-Cos[th23]*Sin[th12]-Exp[I*CP]*Cos[th12]*Sin[th13]*Sin[th23],Cos[th12]*Cos[th23]-Exp[I*CP]*Sin[th12]*Sin[th13]*Sin[th23],Cos[th13]*Sin[th23]},
                       {-Exp[I*CP]*Cos[th12]*Cos[th23]*Sin[th13]+Sin[th12]*Sin[th23],-Exp[I*CP]*Cos[th23]*Sin[th12]*Sin[th13]-Cos[th12]*Sin[th23],Cos[th13]*Cos[th23]}
                  }/.Thread[{th12,th13,th23,CP}->{34.5*Pi/180,8.44*Pi/180,41.0*Pi/180,(*252*Pi/180*)0}];
				PMNS=SetPrecision[PMNS, 15];
				R=Exp[0*I*Pi]*{{1,0,0},
					Sqrt[exp2alpha]*{0,cbar,-Conjugate[sbar]},
					Sqrt[exp2alpha]*{0,sbar, Conjugate[cbar]}
				};  (*pakeista i Sqrt[exp2alpha] ir gali but padauginta is globalios fazes*)
				(*PMNSnew=Conjugate[PMNS].R; *)  (*Loop corrected Yukwas*)
				PMNSnew=PMNS;   (*Medzio lygmenio Yukavoms - Tree level Yukawas*)
			{Yv1rnum,Yv1inum}={Re[#],Im[#]}&@(Sqrt[md2*2]/SMvev*Transpose[PMNSnew][[3]]);
			{Yv2rnum,Yv2inum}={Re[#],Im[#]}&@(d*Transpose[PMNSnew][[2]]+dprime*Transpose[PMNSnew][[3]]);   
			handle= FSFlexible312OpenHandle[
               fsSettings -> {
                   precisionGoal -> 1.*^-4,           (* FlexibleSUSY[0] *)
                   maxIterations -> 0,                (* FlexibleSUSY[1] *)
                   solver -> 0,                       (* FlexibleSUSY[2] *)
                   calculateStandardModelMasses -> 1, (* FlexibleSUSY[3] *)
                   poleMassLoopOrder -> 1,            (* FlexibleSUSY[4] *)
                   ewsbLoopOrder -> 1,                (* FlexibleSUSY[5] *)
                   betaFunctionLoopOrder -> 1,        (* FlexibleSUSY[6] *)
                   thresholdCorrectionsLoopOrder -> 1,(* FlexibleSUSY[7] *)
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
                   MRrIN -> MRin,
                   etaIN -> 0,
                   Qin -> 126,
                   Z2121iIN ->0,
                   Z1112iIN ->0,
                   Z2212iIN -> 0,
                   MRiIN->0,
                   
                   Yv1rIN -> Yv1rnum,
                   Yv1iIN -> Yv1inum,
                   
                   Yv2rIN -> Yv2rnum,
                   Yv2iIN -> Yv2inum,
                   
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
              ,spectrum=$Failed];
              out=If[spectrum === $Failed,
                     Array[invalid&, 1],
                     {
                      (*1*){(*1-4*)Pole[M[hh]],(*5-6*)Pole[M[Hm]],(*7-9*)Pole[M[Fe]],(*10-13*)Pole[M[Fv]],(*14-16*)Pole[M[Fu]],(*17-19*)Pole[M[Fd]]},
                      (*2*)Pole[ZH],
                      (*3*){(*1*)Z1111, Z2222, Z2211, Z2112, Z2121, Z1112, Z2212, (*8*)M222, (*9*)M112, (*10*)M12, (*11*)MR,(*12*) v},
                      (*4*){(*1,3x3*)Ye1, (*2,3x3*)Ye2, (*3,1x3*)Yv1, (*4,1x3*)Yv2, (*5,3x3*)Yu1, (*6,3x3*)Yu2, (*7,3x3*)Yd1, (*8,3x3*)Yd2, (*9,4x4*)Pole[Vpmns], {PMNSnew, d, dprime}}
              
                     }/. (Flexible312 /. spectrum) /.output
                     ]
 ];


(* ::Section::Closed:: *)
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
                   Z1111IN ->z[[1]], (*Z1*) (* 125.09^2/246.22^2-pars[[6]]/(pars[[9]]/Sqrt[1-pars[[9]]^2])*) (*(pars[[12]]^2/2+Cot[-(ArcSin[pars[[9]]])]^2*pars[[13]]^2)*pars[[9]]^2/246.22^2*)
                   Z2222IN -> z[[2]], (*Z2*)
                   Z2211IN -> z[[3]], (*ConZ3*)
                   Z2112IN -> z[[4]], (*ConZ4*)
                   Z2121rIN -> z[[5]], (*ConZ5*)
                   Z1112rIN -> z[[6]], (*conZ6*)  (*-(pars[[12]]^2-pars[[13]]^2)*Sin[-2*ArcSin[pars[[9]]]]/246.22^2/2*)
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


(* ::Section:: *)
(*Calculate*)


LaunchKernels[];
DistributeDefinitions[ScanOverNeutrinos]


fi=Range[0,2*\[Pi],2*\[Pi]/25]; (*Scan range*)


data=ToExpression[Import["Path to data with Higgs Potential points", "List"]];


neutdata=ParallelTable[{data[[k,1]],fi[[j]],data[[k,2]],ScanOverNeutrinos[data[[k,4]], data[[k,3]], 0.89, fi[[j]], 10^4]}
		,{k,Length[data]-480},{j,Length[fi]}];


neut=DeleteCases[Flatten[neutdata,1]/.{a___,{invalid}}->{}, {}];
flatneut=ReplacePart[#, {4,1}->Flatten[#[[4,1]]]]&/@neut;


(*Export[NotebookDirectory[]<>"fixed_neutrinos_loop2.dat",flatneut, "List"]*)
