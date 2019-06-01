--# -path=.:../abstract
concrete MiniExtEng of MiniExt = MiniLangEng ** open MiniResEng, Prelude in {

	lincat

		ClSlash = Clause ;
		IAdv = {s : Str} ; -- interrogative adverbial,  e.g. "where"
		RS = Str ;   -- relative clause with fixed tense and polarity, e.g. "that I have not seen"
		RCl = Str ;  -- relative clause,          e.g. "that I see"
		RP = Str ;   -- relative pronoun,         e.g. "that"
		IP = Pronoun ;   -- interrogative pronoun,    e.g. "who"
		Subj = Str ; -- subjunction,              e.g. "because"
		VS = V ;   -- sentence-complement verb, e.g. "know"
		VQ = V ;   -- question-complement verb, e.g. "wonder"
		VV = V ;   -- VP-complement verb,       e.g. "want"
		VA = V ;   -- AP-complement verb,       e.g. "become"

	oper complV2 : Verb2 -> VerbPhrase = \v2 -> {
		verb = verb2gverb v2 ;
		compl = ""
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

  ----------------

--https://stackoverflow.com/questions/1676632/whats-a-quick-way-to-comment-uncomment-lines-in-vim
--	UseRCl temp pol rcl = "" ;   
--	RelVP rp vp = "" ;   
--	RelSlash rp clslash = "" ;   
--  
--	ComplVS vs s = "" ;   
--	ComplVQ vs s = "" ;   
--	ComplVV vv vp = "" ;   
--	ComplVA va ap = "" ;   
--
--	ReflV2 v2 = "" ;   
--
--	RelCN cn rs = "" ;   
--
--	PossSgDet pron = "" ;
--	PossPlDet pron = "" ;
--
--	SubjS subj s = "" ;   
--
---- more tenses
--  TPastSim = "";
--  TPastAnt = "";
--  TCondSim = "";
--  TCondAnt = "";
--
---- Structural words
--
--  that_RP = "";
--
--  if_Subj       = "";
--  because_Subj  = "";
--  although_Subj = "";
--
---- content words to test with
--  know_VS   = "";
--  wonder_VQ = "";
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
--  become_VA = "";

}
