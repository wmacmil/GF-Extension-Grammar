--# -path=.:../abstract
concrete MiniGrammarEng of MiniGrammar = open MiniResEng, Prelude in {

    oper Clause : Type;
    oper Clause = {
      subj : Str ;                             -- subject
      verb : Bool => Bool => {fin,inf : Str} ; -- dep. on Pol,Temp, e.g. "does","sleep"
      compl : Str                              -- after verb: complement, adverbs
      } ;

    --NP = {s : Case => Str ; a : Agreement} ;
    oper NounPhrase : Type = {s : Case => Str ; a : Agreement} ;
    --VP = {verb : GVerb ; compl : Str} ;
    oper VerbPhrase : Type = {verb : GVerb ; compl : Str} ;

    --oper mkCl : {s : Case => Str ; a : Agreement} -> {verb : GVerb ; compl : Str} -> Clause ;
    oper mkCl : NounPhrase -> VerbPhrase -> Clause ;
    oper mkCl np vp = {
      subj = np.s ! Nom ;
      compl = vp.compl ;
      verb = \\plain,isPres => case <vp.verb.isAux, plain, isPres, np.a> of {

        -- non-auxiliary verbs, negative/question present: "does (not) drink" 
        <False,False,True,Agr Sg Per3> => {fin = "does" ; inf = vp.verb.s ! VF Inf} ;
        <False,False,True,_          > => {fin = "do"   ; inf = vp.verb.s ! VF Inf} ;
	
        -- non-auxiliary, plain present ; auxiliary, all present: "drinks", "is (not)"
        <_,_, True, Agr Sg Per1> => {fin = vp.verb.s ! PresSg1    ; inf = []} ;
        <_,_, True, Agr Sg Per3> => {fin = vp.verb.s ! VF PresSg3 ; inf = []} ;
        <_,_, True, _>           => {fin = vp.verb.s ! PresPl     ; inf = []} ;

        -- all verbs, past: "has (not) drunk", "has (not) been"
        <_,_, False,Agr Sg Per3> => {fin = "has"  ; inf = vp.verb.s ! VF PastPart} ;
        <_,_, False,_          > => {fin = "have" ; inf = vp.verb.s ! VF PastPart} 

        -- the negation word "not" is put in place in UseCl, UseQCl
      }
    } ;


  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; isTrue : Bool} ; -- the s field is empty, but needed for parsing
    Temp = {s : Str ; isPres : Bool} ;
    S  = {s : Str} ;
    QS = {s : Str} ;
    Cl, QCl = Clause ;
   -- Cl, QCl = {   -- word order is fixed in S and QS
   --   subj : Str ;                             -- subject
   --   verb : Bool => Bool => {fin,inf : Str} ; -- dep. on Pol,Temp, e.g. "does","sleep"
   --   compl : Str                              -- after verb: complement, adverbs
   --   } ;
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
    oper mkCl np vp = {
      subj = np.s ! Nom ;
      compl = vp.compl ;
      verb = \\plain,isPres => case <vp.verb.isAux, plain, isPres, np.a> of {
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

    UseCl temp pol cl =
      let clt = cl.verb ! pol.isTrue ! temp.isPres  -- isTrue regulates if "do" is used
      in {
        s = pol.s ++ temp.s ++    --- needed for parsing: a hack
	    cl.subj ++               -- she
	    clt.fin ++               -- does
	    negation pol.isTrue ++   -- not
	    clt.inf ++               -- drink
	    cl.compl                 -- beer
      } ;
      
    UseQCl temp pol cl =
      let clt = cl.verb ! False ! temp.isPres      -- False means that "do" is always used
      in {
        s = pol.s ++ temp.s ++
	    clt.fin ++               -- does
	    cl.subj ++               -- she
	    negation pol.isTrue ++   -- not
	    clt.inf ++               -- drink
	    cl.compl                 -- beer
      } ;

    QuestCl cl = cl ; -- since the parts are the same, we don't need to change anything

    PredVP np vp = mkCl np vp;

--    PredVP np vp = {
--      subj = np.s ! Nom ;
--      compl = vp.compl ;
--<isAux : Bool true only when be_Gverb otherwise False  ,input  Always false in questions but regulated by polarity otherwise  , Tense, agreement>
--      verb = \\plain,isPres => case <vp.verb.isAux, plain, isPres, np.a> of {
--
--        -- non-auxiliary verbs, negative/question present: "does (not) drink" 
--        <False,False,True,Agr Sg Per3> => {fin = "does" ; inf = vp.verb.s ! VF Inf} ;
--        <False,False,True,_          > => {fin = "do"   ; inf = vp.verb.s ! VF Inf} ;
--	
--        -- non-auxiliary, plain present ; auxiliary, all present: "drinks", "is (not)"
--        <_,_, True, Agr Sg Per1> => {fin = vp.verb.s ! PresSg1    ; inf = []} ;
--        <_,_, True, Agr Sg Per3> => {fin = vp.verb.s ! VF PresSg3 ; inf = []} ;
--        <_,_, True, _>           => {fin = vp.verb.s ! PresPl     ; inf = []} ;
--
--        -- all verbs, past: "has (not) drunk", "has (not) been"
--        <_,_, False,Agr Sg Per3> => {fin = "has"  ; inf = vp.verb.s ! VF PastPart} ;
--        <_,_, False,_          > => {fin = "have" ; inf = vp.verb.s ! VF PastPart} 
--
--        -- the negation word "not" is put in place in UseCl, UseQCl
--      }
--    } ;

    ImpVP vp = {
      s = table {
        True  => vp.verb.s ! VF Inf ++ vp.compl ;    -- in Eng, imperative = infinitive
        False => "do not" ++ vp.verb.s ! VF Inf ++ vp.compl
        }
      } ;

    UseV v = {
      verb = verb2gverb v ;  -- lift ordinary verbs to generalized verbs
      compl = []
      } ;
      
    ComplV2 v2 np = {
      verb = verb2gverb v2 ;
      compl = v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseAP ap = {
      verb = be_GVerb ;     -- the verb is the copula "be"
      compl = ap.s
      } ;
      
    UseNP np = {
      verb = be_GVerb ;
      compl = np.s ! Nom    -- NP complement is in the nominative
      } ;
      
    UseAdv adv = {
      verb = be_GVerb ;
      compl = adv.s
      } ;

    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = table {c => det.s ++ cn.s ! det.n} ;
      a = Agr det.n Per3   -- this kind of NP is always third person
      } ;
      
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
      
    --UsePron p = p ;  -- Pron is worst-case NP  
      
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = Agr Sg Per3
      } ;
      
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

    TSim  = {s = []    ; isPres = True} ;
    TAnt  = {s = []    ; isPres = False} ;

    and_Conj = {s = "and"} ;
    or_Conj = {s = "or"} ;

    every_Det = {s = "every" ; n = Sg} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

	oper Pronoun : Type ;
	oper Pronoun = {s : Case => Str ; p : Str ; a : Agreement} ;

	oper mkPron : Str ->  Str ->  Str -> Agreement -> Pronoun ;
	--oper mkPron i me my agr = lin Pronoun {
	oper mkPron i me my agr = {
		s = table {Nom => i ; Acc => me} ;
		p = my ;     -- posessive
		a = agr 
		};

	oper prToNP : Pronoun -> NounPhrase = \p -> {s = p.s ; a = p.a } ;  

	lin 
    UsePron p = {s = p.s ; a = p.a } ;  -- Pron is worst-case NP  

    i_Pron = mkPron "I" "me" "my" (Agr Sg Per1) ;
    youSg_Pron = mkPron "you" "you" "your" (Agr Sg Per2) ;
    he_Pron = mkPron "he" "him" "his" (Agr Sg Per3) ;
    she_Pron = mkPron "she" "her" "her" (Agr Sg Per3) ;
    we_Pron = mkPron "We" "us" "our" (Agr Pl Per1) ;
    youPl_Pron = mkPron "you" "you" "your" (Agr Pl Per2) ; 
    they_Pron = mkPron "they" "them" "their" (Agr Pl Per3) ;


--    i_Pron = {
--      s = table {Nom => "I" ; Acc => "me"} ;
--      a = Agr Sg Per1
--      } ;
--    youSg_Pron = {
--      s = \\_ => "you" ;
--      a = Agr Sg Per2
--      } ;
--    he_Pron = {
--      s = table {Nom => "he" ; Acc => "him"} ;
--      a = Agr Sg Per3
--      } ;
--    she_Pron = {
--      s = table {Nom => "she" ; Acc => "her"} ;
--      a = Agr Sg Per3
--      } ;
--    we_Pron = {
--      s = table {Nom => "we" ; Acc => "us"} ;
--      a = Agr Pl Per1
--      } ;
--    youPl_Pron = {
--      s = \\_ => "you" ;
--      a = Agr Pl Per2
--      } ;
--    they_Pron = {
--      s = table {Nom => "they" ; Acc => "them"} ;
--      a = Agr Pl Per2
--      } ;

    have_V2 = mkVerb "have" "has" "had" "had" "having" ** {c = []} ;

}
