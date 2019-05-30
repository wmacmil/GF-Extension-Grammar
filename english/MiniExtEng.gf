--# -path=.:../abstract
concrete MiniExtEng of MiniExt = MiniLangEng ** {

lincat

  ClSlash = {s : Str}; -- slash clause,          e.g. "(that) I see"
  IAdv = Str ; -- interrogative adverbial,  e.g. "where"
  RS = Str ;   -- relative clause with fixed tense and polarity, e.g. "that I have not seen"
  RCl = Str ;  -- relative clause,          e.g. "that I see"
  RP = Str ;   -- relative pronoun,         e.g. "that"
  IP = Str ;   -- interrogative pronoun,    e.g. "who"
  Subj = Str ; -- subjunction,              e.g. "because"
  VS = V ;   -- sentence-complement verb, e.g. "know"
  VQ = V ;   -- question-complement verb, e.g. "wonder"
  VV = V ;   -- VP-complement verb,       e.g. "want"
  VA = V ;   -- AP-complement verb,       e.g. "become"

lin

  SlashV2 np v2 = {s = ""};
  --SlashV2 np v2 = {s = np.s ! Nom ++ v2.s !  ; 
  --Verb : Type = {s : VForm => Str} ;
  --Verb2 : Type = Verb ** {c : Str} ;
  --I love that the man with the dog sees himself.
  --so the slashV2 fcn needs to more or less resemble a clause except it doesn't need righthand side
  --need a function that takes an ip to an np
	--need to account for who vs whom

  oper UseP
    UseIP pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
  mkPN : Str -> PN
    = \s -> lin PN {s = s} ;


  



--
--	QuestVP ip vp = "" ;
--	QuestSlash ip clslash = "" ;
--	QuestIAdv iadv cl = "" ;
--
--  who_IP  = "";
--  what_IP = "";
--
--  where_IAdv = "";
--  why_IAdv   = "";

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
