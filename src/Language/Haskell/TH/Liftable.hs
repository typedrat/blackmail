module Language.Haskell.TH.Liftable () where

import Language.Haskell.TH.Syntax

deriving instance Lift AnnTarget
deriving instance Lift Bang
deriving instance Lift Body
deriving instance Lift Callconv
deriving instance Lift Clause
deriving instance Lift Con
deriving instance Lift Dec
deriving instance Lift DerivClause
deriving instance Lift DerivStrategy
deriving instance Lift Exp
deriving instance Lift FamilyResultSig
deriving instance Lift Fixity
deriving instance Lift FixityDirection
deriving instance Lift Foreign
deriving instance Lift FunDep
deriving instance Lift Guard
deriving instance Lift Inline
deriving instance Lift InjectivityAnn
deriving instance Lift Lit
deriving instance Lift Match
deriving instance Lift Name
deriving instance Lift NameSpace
deriving instance Lift Overlap
deriving instance Lift Pat
deriving instance Lift PatSynArgs
deriving instance Lift PatSynDir
deriving instance Lift Phases
deriving instance Lift Pragma
deriving instance Lift Range
deriving instance Lift Role
deriving instance Lift RuleBndr
deriving instance Lift RuleMatch
deriving instance Lift Safety
deriving instance Lift SourceStrictness
deriving instance Lift SourceUnpackedness
deriving instance Lift Stmt
deriving instance Lift ModName
deriving instance Lift NameFlavour
deriving instance Lift OccName
deriving instance Lift PkgName
deriving instance Lift TyLit
deriving instance Lift TySynEqn
deriving instance Lift TyVarBndr
deriving instance Lift TypeFamilyHead
deriving instance Lift Type

instance (Lift a) => Lift (Q a) where
    lift m = AppE <$> [e| (\x -> return x) :: a -> Q a  |] <*> (lift =<< m)
