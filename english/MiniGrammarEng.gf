--# -path=.:../abstract
concrete MiniGrammarEng of MiniGrammar = open MiniResEng, Prelude in {

    oper Clause : Type;
    oper Clause = {
      ip : Str ;
      subj : Str ;                             -- subject
      isIR : Bool ;
      cs : Case ;
      --verb : Bool => Bool => {fin,inf : Str} ; -- dep. on Pol,Temp, e.g. "does","sleep"
      verb : Bool => TempParam => {fin,inf : Str} ; -- dep. on Pol,Temp, e.g. "does","sleep"
      compl : Str                              -- after verb: complement, adverbs
      } ;

    --NP = {s : Case => Str ; a : Agreement} ;
    oper NounPhrase : Type = {s : Case => Str ; a : Agreement ; isIR : Bool } ;
    --VP = {verb : GVerb ; compl : Str} ;
    oper VerbPhrase : Type = {verb : GVerb ; compl : Str ; isRefl : Bool} ;

  --Number = Sg | Pl ;
  --Person = Per1 | Per2 | Per3 ;
  --Agreement = Agr Number Person ;


	oper 
		reflPron : Agreement => Str = table {
			Agr Sg Per1 => "myself" ;
			Agr Sg Per2     => "yourself" ;
			--AgP3Sg Masc  => "himself" ;
			--AgP3Sg Fem   => "herself" ;
			Agr Sg Per3 => "itself" ;
			Agr Pl Per1     => "ourselves" ;
			Agr Pl Per2     => "yourselves" ;
			Agr _  _   => "themselves"
			} ;


--<isAux : Bool true only when be_Gverb otherwise False  ,input  Always false in questions but regulated by polarity otherwise  , Tense, agreement>
--the nom or Acc will ultimately determine whether who or whom gets used
--whom only gets used with a QCL
--so maybe just make ag a Nom or an Acc. only change when the mkclause with those is used
    oper mkCl : NounPhrase -> VerbPhrase -> Clause ;
    oper mkCl np vp = {
      ip = "";
      subj = np.s ! Nom ;
      isIR = np.isIR ;
      cs = Nom ;
      compl = case vp.isRefl of { 
				False => vp.compl; 
				True => reflPron ! np.a };
      verb = \\plain,tense => case <vp.verb.isAux, plain, tense, np.a> of {
        <False,False,SN,Agr Sg Per3> => {fin = "does" ; inf = vp.verb.s ! VF Inf} ;
        <False,False,SN,_          > => {fin = "do"   ; inf = vp.verb.s ! VF Inf} ;
        <_,_, SN, Agr Sg Per1> => {fin = vp.verb.s ! PresSg1    ; inf = []} ;
        <_,_, SN, Agr Sg Per3> => {fin = vp.verb.s ! VF PresSg3 ; inf = []} ;
        <_,_, SN, _>           => {fin = vp.verb.s ! PresPl     ; inf = []} ;
        <_,_, AN,Agr Sg Per3> => {fin = "has"  ; inf = vp.verb.s ! VF PastPart} ;
        <_,_, AN,_          > => {fin = "have" ; inf = vp.verb.s ! VF PastPart} ;
        <_,_, PS,_         > => {fin = vp.verb.s ! VF Past ; inf = []} ;
        <_,_, PA,_         > => {fin = "had" ; inf = vp.verb.s ! VF PastPart} ;
        <_,_, CS,_         > => {fin = "would" ; inf = vp.verb.s ! PresSg1} ;

        --case for the negative
        --<_,_, CA,_         > => {fin = "would have" ; inf = vp.verb.s ! VF PastPart} 
        <_,_, CA,_         > => {fin = "would" ; inf = "have" ++ vp.verb.s ! VF PastPart} 
      }
    } ;

        --<_,_, PS,_         > => {fin = "vp.verb.s ! VF PastPart" ; inf = []} ;
        --<_,_, PS,_       > => {fin = "" ; inf = vp.verb.s ! VF PastPart} ;


---- more tenses
--  TPastSim : Temp ; -- (slept) drank pres= broke
--	VF Past
	----  TPastAnt : Temp ; -- had (slept) drunk /broken
	--	VF PastPart == broken
	----  TCondSim : Temp ; -- would (sleep) drink break
	--	PressSg1
	--  TCondAnt : Temp ; -- would have (slept) would have broken
	--	VF PastPart
--mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
--      = \inf,pres,part -> lin V (irregVerb inf pres part) ;
--  VForm = Inf | PresSg3 | Past | PastPart | PresPart ; 
--   GVForm = VF VForm | PresSg1 | PresPl | PastPl ;
--      Inf => inf ;
--      PresSg3 => pres ;
--      Past => past ;
--      PastPart => pastpart ;
--      PresPart => prespart
   -- table {
   --   VPresent Sg => "drinks" ;
   --   VPresent Pl => "drink" ;
   --   VPast       => "drank" ;
   --   VPastPart   => "drunk" ;
    --  VPresPart   => "drinking"
    --  }

	param TempParam = SN | AN | PS | PA | CS | CA ;

	--Temp = {s : Str ; tense : TempParam} ;
  oper Temporality : Type = {s : Str ; tense : TempParam} ;
  oper Polarity : Type = {s : Str ; isTrue : Bool} ;

   -- QuestVP    : IP -> VP -> QCl ;      -- who sees me
   -- QuestSlash : IP -> ClSlash -> QCl ; -- whom do I see


--use this for whom cases
  oper revOrder : Temporality -> Polarity -> Clause -> {s : Str} ;
  oper revOrder temp pol cl = let clt = cl.verb ! False ! temp.tense      -- False means that "do" is always used
        in {
          s = pol.s ++ temp.s ++
        cl.ip ++  --whom
        clt.fin ++               -- does
        cl.subj ++               -- she
        negation pol.isTrue ++   -- not
        clt.inf ++               -- drink
        cl.compl                 -- beer
        } ;

  --oper normOrderWho temp pol cl = let clt = cl.verb ! False ! temp.tense      -- False means that "do" is always used, changed false to true
  oper normOrderWho : Temporality -> Polarity -> Clause -> {s : Str} ;
  oper normOrderWho temp pol cl = let clt = cl.verb ! True ! temp.tense      -- False means that "do" is always used
        in {
          s = pol.s ++ temp.s ++
        cl.subj ++               -- she
        clt.fin ++               -- does
        negation pol.isTrue ++   -- not
        clt.inf ++               -- drink
        cl.compl                 -- beer
        } ;

--only use this for normal questions
  oper normOrder : Temporality -> Polarity -> Clause -> {s : Str} ;
  oper normOrder temp pol cl = let clt = cl.verb ! pol.isTrue ! temp.tense      -- False means that "do" is always used
        in {
          s = pol.s ++ temp.s ++
        cl.subj ++               -- she
        clt.fin ++               -- does
        negation pol.isTrue ++   -- not
        clt.inf ++               -- drink
        cl.compl                 -- beer
        } ;

  lincat
    Utt = {s : Str} ;
    Pol = Polarity ;
    Temp = Temporality ;
    S  = {s : Str} ;
    QS = {s : Str} ;
    Cl, QCl = Clause ;
    Imp = {s : Bool => Str} ;
    VP = VerbPhrase;
    --VP = {verb : GVerb ; compl : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = NounPhrase ;
    --NP = {s : Case => Str ; a : Agreement} ;
    Pron = Pronoun ; 
    --Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = {s : Str} ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttQS s = s ;
    UttNP np = {s = np.s ! Acc} ; -- Acc: produce "me" rather than "I"
    UttAdv adv = adv ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.isTrue} ;

    UseCl temp pol cl = normOrder temp pol cl ;

    --UseCl temp pol cl =
    --  let clt = cl.verb ! pol.isTrue ! temp.tense  -- isTrue regulates if "do" is used
    --  in {
    --    s = pol.s ++ temp.s ++    --- needed for parsing: a hack
	  --  cl.subj ++               -- she
	  --  clt.fin ++               -- does
	  --  negation pol.isTrue ++   -- not
	  --  clt.inf ++               -- drink
	  --  cl.compl                 -- beer
    --  } ;

    --UseQCl temp pol cl = revOrder temp pol cl ;

    --UseQCl temp pol cl = case cl.isIR of {
    --  False => revOrder temp pol cl ;
    --  True => normOrder temp pol cl 
    --};

    UseQCl temp pol cl = case cl.isIR of {
      True => case cl.cs of {
        Nom => normOrderWho temp pol cl ;
        Acc => revOrder temp pol cl 
        };
      False => revOrder temp pol cl 
    };

   -- UseQCl temp pol cl =
   --   let clt = cl.verb ! False ! temp.tense      -- False means that "do" is always used
   --   in {
   --     s = pol.s ++ temp.s ++
	 --   clt.fin ++               -- does
	 --   cl.subj ++               -- she
	 --   negation pol.isTrue ++   -- not
	 --   clt.inf ++               -- drink
	 --   cl.compl                 -- beer
   --   } ;

    QuestCl cl = cl ; -- since the parts are the same, we don't need to change anything

    PredVP np vp = mkCl np vp;

    ImpVP vp = {
      s = table {
        True  => vp.verb.s ! VF Inf ++ vp.compl ;    -- in Eng, imperative = infinitive
        False => "do not" ++ vp.verb.s ! VF Inf ++ vp.compl
        }
      } ;

    UseV v = {
      verb = verb2gverb v ;  -- lift ordinary verbs to generalized verbs
      compl = []
      } ** { isRefl = False } ;
      
    ComplV2 v2 np = {
      verb = verb2gverb v2 ;
      compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ** { isRefl = False } ;
      
    UseAP ap = {
      verb = be_GVerb ;     -- the verb is the copula "be"
      compl = ap.s
      } ** { isRefl = False } ;
      
    UseNP np = {
      verb = be_GVerb ;
      compl = np.s ! Nom    -- NP complement is in the nominative
      } ** { isRefl = False } ;
      
    UseAdv adv = {
      verb = be_GVerb ;
      compl = adv.s
      }  ** { isRefl = False } ;

    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ** {isRefl = vp.isRefl} ;
      
    DetCN det cn = {
      s = table {c => det.s ++ cn.s ! det.n} ;
      a = Agr det.n Per3   -- this kind of NP is always third person
      } ** { isIR = False } ;
      
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3 ;
      } ** { isIR = False } ;
      
    --UsePron p = p ;  -- Pron is worst-case NP  
      
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = Agr Sg Per3
      } ** { isIR = False } ;
      
    a_Det = {s = pre {"a"|"e"|"i"|"o" => "an" ; _ => "a"} ; n = Sg} ; --- a/an can get wrong
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "the" ; n = Sg} ;
    thePl_Det = {s = "the" ; n = Pl} ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = [] ; isTrue = True} ;
    PNeg  = {s = [] ; isTrue = False} ;

    TSim  = {s = []    ; tense = SN} ;
    TAnt  = {s = []    ; tense = AN} ;

    and_Conj = {s = "and"} ;
    or_Conj = {s = "or"} ;

    every_Det = {s = "every" ; n = Sg} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

	oper Pronoun : Type ;
	oper Pronoun = {s : Case => Str ; p : Str ; a : Agreement ; isIR : Bool} ;

	oper mkPron : Str ->  Str ->  Str -> Agreement -> Bool -> Pronoun ;
	oper mkPron i me my agr isir = {
		s = table {Nom => i ; Acc => me} ;
		p = my ;     -- posessive
		a = agr ;
    isIR = isir
		};

	oper prToNP : Pronoun -> NounPhrase = \p -> {s = p.s ; a = p.a ; isIR = p.isIR } ;  

	lin 
    UsePron p = {s = p.s ; a = p.a ; isIR = p.isIR
} ;  -- Pron is worst-case NP  
      
    i_Pron = mkPron "I" "me" "my" (Agr Sg Per1) False ;
    youSg_Pron = mkPron "you" "you" "your" (Agr Sg Per2) False ;
    he_Pron = mkPron "he" "him" "his" (Agr Sg Per3) False ;
    she_Pron = mkPron "she" "her" "her" (Agr Sg Per3) False ;
    we_Pron = mkPron "we" "us" "our" (Agr Pl Per1) False ;
    youPl_Pron = mkPron "you" "you" "your" (Agr Pl Per2) False ; 
    they_Pron = mkPron "they" "them" "their" (Agr Pl Per3) False ;

    have_V2 = mkVerb "have" "has" "had" "had" "having" ** {c = []} ;

}
