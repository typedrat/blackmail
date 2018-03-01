{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Blackmail.SMTP.Protocol.TH (mkCmpTF, mkUpdateStateInst) where

import Control.Lens
import Numeric.Lens
import Language.Haskell.TH.Lens

import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import Data.Char (toLower)
import qualified Data.Containers as O
import qualified Data.MonoTraversable.Unprefixed as O
import Data.Maybe (isJust, fromJust)
import Data.Tuple (swap)
import Language.Haskell.TH

mkCmpTF :: Name -> DecsQ
mkCmpTF kind = do
    -- Name type family
    let tfN = mkName $ "Cmp" ++ nameBase kind

    let tvK = promotedT kind
    tvX <- kindedTV <$> newName "x" <*> tvK
    tvY <- kindedTV <$> newName "y" <*> tvK

    -- Make body of type family
    kindI <- reify kind
    let tys :: [(Int, Type)]
        tys = kindI ^.. _TyConI . _DataD . _5  -- Access [Con] inside Info
                      . ifolded . withIndex
                      . alongside
                          id
                          (name . re _PromotedT) -- Extract name, convert to promoted type

        ordToTy :: Ordering -> Type
        ordToTy LT = PromotedT 'LT
        ordToTy EQ = PromotedT 'EQ
        ordToTy GT = PromotedT 'GT

        eqns = flip foldMap tys (\(idx, ty) ->
            tys <&> (\(idx', rhs) ->
                TySynEqn [ty, rhs] (ordToTy $ compare idx idx')
                )
            )

    -- Declare type family
    return [ClosedTypeFamilyD (TypeFamilyHead tfN [tvX, tvY] (KindSig $ PromotedT ''Ordering) Nothing) eqns]

-- I'd try to make this prettier, but that'd require looking at and/or thinking about it.
--
-- **When** it breaks, I'll leave it better than when I found it. (AW, 3/1/18)
mkUpdateStateInst :: Name -> Name -> DecsQ
mkUpdateStateInst aN bN = do
    clsN' <- lookupTypeName "UpdateState"
    unless (isJust clsN') $ fail "UpdateState is not in scope!"
    let clsN = fromJust clsN'
    clsI <- reify clsN
    let clsT = clsI ^?! _ClassI . _1 -- Get class declaration
                      . _ClassD . _2 -- Get name
                      . re _ConT     -- Generate type

    idN' <- lookupTypeName "SMTPStateId"
    unless (isJust idN') $ fail "SMTPStateId is not in scope!"
    let idN = fromJust idN'
    idI <- reify idN
    let order :: [(Name, Int)]
        order = idI ^.. _TyConI . _DataD . _5        -- Access [Con] inside Info
                      . ifolded . withIndex          -- Fold over each element, augmented with the index
                      . to swap . alongside name id  -- convert from (Int, Con) to (Name, Int)

    tyN' <- lookupTypeName "SMTPStateData"
    tyConN' <- lookupValueName "SMTPStateData"
    unless (isJust tyN' && isJust tyConN') $ fail "SMTPStateData is not in scope!"
    let tyN = fromJust tyN'
        tyConN = fromJust tyConN'
    tyI <- reify tyN
    let tyMemTys :: [(Name, Type)]
        tyMemTys = tyI ^.. _TyConI . _DataD . _5     -- Access [Con] inside Info
                         . ifolded . conFields . _2  -- Fold over all types
                         . _AppT . alongside         -- Unwrap first layer of AppT, get state type *name* and value type
                             (singular _AppT . _2 . singular _PromotedT)
                             id

    let aIdx' = lookup aN order
        bIdx' = lookup bN order
    unless (isJust aIdx' && isJust bIdx') $ fail "Input types are not of kind SMTPStateId!"
    let aIdx, bIdx :: Int
        Just aIdx = aIdx'
        Just bIdx = bIdx'

    let startingMemTys = filter (\(k, _) -> fromJust (lookup k order) <= aIdx) tyMemTys
        finalMemTys = filter (\(k, _) -> fromJust (lookup k order) <= bIdx) tyMemTys

    let newMems = finalMemTys `O.difference` startingMemTys
        newMemTys = snd <$> newMems

        scN = mkName "StateChanges"
        scD = TySynInstD scN (TySynEqn [ConT aN, ConT bN] $ foldr (\t acc -> AppT (AppT PromotedConsT t) acc) PromotedNilT newMemTys)

    let patFold :: ([Exp], [Pat], [Pat]) -> (Name, Type) -> Q ([Exp], [Pat], [Pat])
        patFold (ns, tyPs, argPs) (n, t) = newName ((\(c:cs) -> toLower c : cs) . nameBase $ t ^?! _ConT) >>= \n' ->
            case n `lookup` newMems of
                Just _ -> return (VarE n':ns, WildP:tyPs, VarP n':argPs)
                Nothing -> return (VarE n':ns, VarP n':tyPs, argPs)

    (argNs, conPs, argPs) <- foldM patFold mempty tyMemTys

    let fnN = mkName "updateState"
        fnConP = ConP tyConN (reverse conPs)
        fnPs = fnConP : reverse argPs
        fnB = NormalB $ O.foldl' AppE (ConE tyConN) (reverse argNs)
        fnD = FunD fnN [Clause fnPs fnB []]

        sgT = foldr
            (\ty acc -> AppT (AppT ArrowT ty) acc)
            (AppT (ConT tyN) (PromotedT bN))
            (AppT (ConT tyN) (PromotedT aN) : newMemTys)
        sgD = SigD fnN sgT

        instT = AppT (AppT clsT (PromotedT aN)) (PromotedT bN)

    return [InstanceD Nothing [] instT [sgD, fnD, scD]]
