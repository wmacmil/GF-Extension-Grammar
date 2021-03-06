--# -path=.:../abstract
concrete MiniExtEng of MiniExt = MiniLangEng ** open MiniResEng, Prelude, MiniParadigmsEng in {

	lincat

		ClSlash = Clause ;
		IAdv = {s : Str} ; -- interrogative adverbial,  e.g. "where"
		RS = {s : Str} ;   -- relative clause with fixed tense and polarity, e.g. "that I have not seen"
		RCl = Clause ;  -- relative clause,          e.g. "that I see"
		RP = Pronoun ;   -- relative pronoun,         e.g. "that"
		IP = Pronoun ;   -- interrogative pronoun,    e.g. "who"
		Subj = {s : Str} ; -- subjunction,              e.g. "because"
		VS = Verb2 ;   -- sentence-complement verb, e.g. "know"
		VQ = Verb2 ;   -- question-complement verb, e.g. "wonder"
		VV = V ;   -- VP-complement verb,       e.g. "want"
		VA = Verb2 ;   -- AP-complement verb,       e.g. "become"

	oper complV2 : Verb2 -> VerbPhrase = \v2 -> {
		verb = verb2gverb v2 ;
		compl = "";
		isRefl = False
		} ;

lin

  SlashV2 np v2 = mkCl np (complV2 v2) ;

	QuestSlash ipN clslash = {
		ip = clslash.ip ++ ipN.s ! Acc ;
		subj = clslash.subj ;
		isIR = ipN.isIR ;
		cs = Acc ;
		compl = clslash.compl ;
		verb = clslash.verb
	};

	who_IP  = mkPron "who" "whom" "whose" (Agr Sg Per3) True ;
	what_IP  = mkPron "what" "what" "" (Agr Sg Per3) True ;

	QuestVP ip vp = let npip = prToNP ip in ( mkCl npip vp) ;

	QuestIAdv iadv cl = {
		ip = cl.ip ++ iadv.s ;
		subj = cl.subj ;
		isIR = True ;
		cs = Acc ;
		compl = cl.compl ;
		verb = cl.verb
	};

  where_IAdv = {s = "where"} ; 
  why_IAdv   = {s = "why"} ;

	RelVP rp vp = let nprp = prToNP rp in ( mkCl nprp vp) ;

	RelSlash ipN clslash = {
		ip = clslash.ip ++ ipN.s ! Acc ;
		subj = clslash.subj ;
		isIR = ipN.isIR ;
		cs = Acc ;
		compl = clslash.compl ;
		verb = clslash.verb
	};

  oper revOrderThat : Temporality -> Polarity -> Clause -> {s : Str} ;
  oper revOrderThat temp pol cl = let clt = cl.verb ! pol.isTrue ! temp.tense      -- False means that "do" is always used
        in {
          s = pol.s ++ temp.s ++
        cl.ip ++  --whom
        cl.subj ++               -- she
        clt.fin ++               -- does
        negation pol.isTrue ++   -- not
        clt.inf ++               -- drink
        cl.compl                 -- beer
        } ;

  oper normOrderThat : Temporality -> Polarity -> Clause -> {s : Str} ;
  oper normOrderThat temp pol cl = let clt = cl.verb ! pol.isTrue ! temp.tense      -- False means that "do" is always used
        in {
          s = pol.s ++ temp.s ++
        cl.subj ++               -- that
        clt.fin ++               -- does
        negation pol.isTrue ++   -- not
        clt.inf ++               -- drink
        cl.compl                 -- beer
        } ;

lin
	UseRCl temp pol rcl = case rcl.cs of {
			Nom => normOrderThat temp pol rcl ;
			Acc => revOrderThat temp pol rcl 
			};

  that_RP = mkPron "that" "that" "" (Agr Sg Per3) True ;

	--RelCN cn rs = {s = table { Sg => cn.s ! Sg ++ rs.s;
--														 Pl => cn.s ! Pl ++ rs.s } } ;
	--RelCN cn rs = {s = table { x => cn.s ! x ++ rs.s } } ;

	RelCN cn rs = {s = \\x => cn.s ! x ++ rs.s  } ;


	--Adv = {s : Str} ;

	SubjS subj sen = {s = subj.s ++ sen.s} ;

  if_Subj       = mkSubj "if" ;
  because_Subj  = mkSubj "because" ;
  although_Subj = mkSubj "although" ;

	oper mkSubj : Str -> {s : Str} = \str -> {s = str};

lin

	ReflV2 v2 = {
		verb = verb2gverb v2 ;
		compl = ""; 
		isRefl = True
		} ;
 
	PossSgDet pn = {s = pn.p ; n = Sg } ;
	PossPlDet pn = {s = pn.p ; n = Pl } ;

  TPastSim = {s = []    ; tense = PS} ;
  TPastAnt = {s = []    ; tense = PA} ;
  TCondSim = {s = []    ; tense = CS} ;
  TCondAnt = {s = []    ; tense = CA} ;

  ----------------

--lin drink_V2 = mkV2 (mkV "drink" "drank" "drunk") ;

  know_VS = mkV2 (mkV "know" "knew" "known") ;

	ComplVS vs s = {
      verb = verb2gverb vs ;  
      compl = s.s
      } ** { isRefl = False } ;

  become_VA = mkV2 (mkV "become" "became" "become") ;

	ComplVA va ap = {
      verb = verb2gverb va ;  
      compl = ap.s
      } ** { isRefl = False } ;

  wonder_VQ = mkV2 (mkV "wonder") ;

	ComplVQ vq temp pol qcl = let q = evalQCl temp pol qcl in {
      verb = verb2gverb vq ;  
      compl = q.s
      } ** { isRefl = False } ;


--	ComplVV vv vp = "" ;   
---- Structural words
---- content words to test with
--  want_VV   = (mkV "want") ** { c = ToInf } ;
--  want_VV   = (mkV "want") ** { c = ToInf } ;
--    VForm = VF Number Person | VIng | VInf ; -- Present tense forms, participle and infinitive
--    Case = Nom | Acc ;
--    VVCompl = Inf | ToInf | Ing ; 
--    --want_VV = (mkV "want") ** { c = ToInf } ;
--    --VV = { s : VForm => Str ; c : VVCompl } ;
--    --VP = { s : VForm => Str ; adv : Str ; compl : Str } ;
--  must_VV   = "";
--  try_VV    = "";

}
