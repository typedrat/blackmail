module Control.FSM.TH (DefnM(), DefnA(), FSMEvent(), FSMNode(), FSMAttribute(), initial, event, state, terminal, transition, attrib, makeFSMTypes) where

import Control.FSM.TH.Description
import Control.FSM.Monad.Internal
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Class (get, put)
import Data.Char (toUpper, toLower)
import Data.Graph.Inductive.Graph hiding ((&))
import Data.Foldable (foldl')
import Data.List (elemIndex, nub, nubBy, (\\))
import Data.Proxy
import Language.Haskell.TH

--

makeStateType :: Machine -> Q ([Dec], [Dec])
makeStateType (Machine name graph) = do
    -- Make the state id type
    let idTypeName = mkName (name ++ "StateId")
        states = snd <$> labNodes graph

        stateNames = mkName . _nName <$> states
        idCons = flip NormalC [] <$> stateNames
        idTy = DataD [] idTypeName [] Nothing idCons []

        stateIdKindInst = TySynInstD ''StateIdKind (TySynEqn [ConT (mkName name)] (ConT idTypeName))

    -- Make the actual core state type, parameterized by the state ID.
    idName <- newName "state"

    let dataTyName = mkName (name ++ "StateData")

        plainBang = Bang NoSourceUnpackedness NoSourceStrictness
        attrToField (FSMAttribute _ ty) = (plainBang, ty)
        stateToCon (FSMNode state attrs _) = GadtC [mkName $ state ++ "Data"] (attrToField <$> attrs) (AppT (ConT dataTyName) (PromotedT $ mkName state))

        dataTy = DataD [] dataTyName [KindedTV idName (ConT idTypeName)] Nothing (stateToCon <$> states) []
        dataDeriving = StandaloneDerivD Nothing [] (AppT (ConT ''Show) (AppT (ConT dataTyName) (VarT idName)))

    -- Make the type that wraps the safe states
        stateConF name = NormalC (mkName $ nameBase name ++ "_") [(plainBang, AppT (ConT dataTyName) (PromotedT name))]
        stateFamInst = DataInstD [] ''StateType [ConT (mkName $ name)] Nothing (stateConF <$> stateNames) [DerivClause Nothing [ConT ''Show]]

    [stateType'] <- [d| type instance StateType' $(conT $ mkName name) = $(conT dataTyName) |]
    let initialState' = ValD (VarP 'initialState) (NormalB (AppE (ConE (mkName "Initial_")) (ConE (mkName "InitialData")))) []
        wrapStateClauses = \(FSMNode name attrs _) -> Clause
            [AsP idName (ConP (mkName $ name ++ "Data") (replicate (length attrs) WildP))]
            (NormalB $ AppE (ConE (mkName $ name ++ "_")) (VarE idName))
            []
        wrapState' = FunD 'wrapState (wrapStateClauses <$> states)

    return ([idTy, stateIdKindInst, dataTy, dataDeriving], [stateType', stateFamInst, initialState', wrapState'])


makeEventType :: Machine -> Q ([Dec], [Dec])
makeEventType (Machine name graph) = do
    -- Make the event id type
    let idTypeName = mkName (name ++ "EventId")
        events = nub $ (\(_, _, x) -> x) <$> labEdges graph
        eventNames = mkName . _eName <$> events
        idCons = flip NormalC [] <$> eventNames
        idTy = DataD [] idTypeName [] Nothing idCons []

        eventIdKindInst = TySynInstD ''EventIdKind (TySynEqn [ConT (mkName name)] (ConT idTypeName))

    -- Make the actual core event type, parameterized by the state ID.
    idName <- newName "event"

    let dataTyName = mkName (name ++ "EventData")

        plainBang = Bang NoSourceUnpackedness NoSourceStrictness
        attrToField (FSMAttribute _ ty) = (plainBang, ty)
        eventToCon (FSMEvent event attrs) = GadtC [mkName $ event ++ "Data"] (attrToField <$> attrs) (AppT (ConT dataTyName) (PromotedT $ mkName event))

        dataTy = DataD [] dataTyName [KindedTV idName (ConT idTypeName)] Nothing (eventToCon <$> events) []
        dataDeriving = StandaloneDerivD Nothing [] (AppT (ConT ''Show) (AppT (ConT dataTyName) (VarT idName)))

    -- Make the type that wraps the safe events
        eventConF name = NormalC (mkName $ nameBase name ++ "_") [(plainBang, AppT (ConT dataTyName) (PromotedT name))]
        eventFamInst = DataInstD [] ''EventType [ConT (mkName $ name)] Nothing (eventConF <$> eventNames) [DerivClause Nothing [ConT ''Show]]

    [eventType'] <- [d| type instance EventType' $(conT $ mkName name) = $(conT dataTyName) |]

    let wrapEventClauses = \(FSMEvent name attrs) -> Clause
            [AsP idName (ConP (mkName $ name ++ "Data") (replicate (length attrs) WildP))]
            (NormalB $ AppE (ConE (mkName $ name ++ "_")) (VarE idName))
            []
        wrapEvent' = FunD 'wrapEvent (wrapEventClauses <$> events)

    return ([idTy, eventIdKindInst, dataTy, dataDeriving], [eventType', eventFamInst, wrapEvent'])


makeOptics :: Machine -> Q [Dec]
makeOptics (Machine name graph) = do
    let states = nub $ snd <$> labNodes graph
        events = nub $ (\(_, _, x) -> x) <$> labEdges graph

        allAttrKeys = nub . fmap _aName $ foldMap _nAttribs states ++ foldMap _eAttribs events

        defnClass :: String -> Q Dec
        defnClass key@(k:ks) = do
            fromTvN <- newName "from"
            typeTvN <- newName "type'"

            let key' = toUpper k:ks
                clsName = mkName (name ++ "Has" ++ key')
                instS = SigD (mkName $ '_':key) (AppT (AppT (ConT ''Lens') (VarT fromTvN)) (VarT typeTvN))
                classD = ClassD [] clsName [PlainTV fromTvN, PlainTV typeTvN] [FunDep [fromTvN] [typeTvN]] [instS]

            return classD

        makeInst :: (String, [FSMAttribute]) -> Type -> FSMAttribute -> Q Dec
        makeInst (inName, attrs) from attr@(FSMAttribute key@(k:ks) res) = do
            let conN = mkName $ inName ++ "Data"
                nameP = varP . mkName $ '_':key
                Just attrIdx = attr `elemIndex` attrs
                nAttrs = length attrs

            varN <- newName "var"

            let getterArgP = replicate attrIdx WildP ++ (VarP varN : replicate (nAttrs - attrIdx - 1) WildP)
                getterP = return $ ConP conN getterArgP

            names <- mapM (newName . pure) (take nAttrs ['a'..])

            let (inP, setterArgP) = (VarP <$> names) & singular (ix attrIdx) <<.~ WildP
                setterP = return $ ConP conN setterArgP
                setterE = return $ foldl' (\acc x -> AppE acc (VarE x)) (ConE conN) names

                key' = toUpper k:ks
                clsTy = ConT $ mkName (name ++ "Has" ++ key')
                instTy = AppT (AppT clsTy from) res

            [def] <- [d| $nameP = lens (\($getterP) -> $(varE varN)) (\($setterP) ($(return inP)) -> $setterE) |]
            return $ InstanceD Nothing [] instTy [def]

        makeStateInsts :: FSMNode -> Q [Dec]
        makeStateInsts (FSMNode state attrs _) = mapM (makeInst (state, attrs) stateTy) attrs
            where
                stateTy = AppT (ConT . mkName $ name ++ "StateData") (PromotedT $ mkName state)

        makeEventInsts :: FSMEvent -> Q [Dec]
        makeEventInsts (FSMEvent event attrs) = mapM (makeInst (event, attrs) eventTy) attrs
            where
                eventTy = AppT (ConT . mkName $ name ++ "EventData") (PromotedT $ mkName event)

    classes <- mapM defnClass allAttrKeys
    stateInsts <- concat <$> mapM makeStateInsts states
    eventInsts <- concat <$> mapM makeEventInsts events

    return (classes ++ stateInsts ++ eventInsts)


makeTransitionInsts :: Machine -> Q [Dec]
makeTransitionInsts (Machine mach graph) = do
    let nodes = labNodes graph
        getNode n = let Just l = lookup n nodes in l

        transitions = filter (uncurry (/=)) . nub $ (\(f, t, _) -> (getNode f, getNode t)) <$> labEdges graph

        mkInst :: (FSMNode, FSMNode) -> Q Dec
        mkInst (FSMNode fromN attrs  _, FSMNode toN   attrs' _) = do
            let machStateT = ConT (mkName $ mach ++ "StateData")
                stateT name = AppT machStateT (PromotedT (mkName name))

                newAttrs = attrs' \\ attrs

                difference = TySynInstD ''FSMTransitionDifference eqn
                    where
                        diff = _aType <$> newAttrs
                        tup = TupleT (length diff)
                        val = foldl' AppT tup diff
                        eqn = TySynEqn [stateT fromN, stateT toN] val

                (inP, newP, outE) = (return in', return new', return out')
                    where
                        in'  = ConP (mkName $ fromN ++ "Data") (fmap attrPat attrs)
                        new' = TupP (fmap attrPat newAttrs)
                        out' = foldl' AppE (ConE . mkName $ toN ++ "Data") (fmap attrExp attrs')

                        attrPat = VarP . mkName . _aName
                        attrExp = VarE . mkName . _aName
                fName = return (VarP '_transition)

            [mapF] <- [d| $fName = setting (\f ($inP) -> let $newP = f () in $outE) |]

            let decl = InstanceD Nothing [] (AppT (AppT (ConT ''FSMTransitionable) (stateT fromN)) (stateT toN)) [difference, mapF]

            return decl

    mapM mkInst transitions

reifyFSM :: Machine -> Q [Dec]
reifyFSM machine@(Machine mach gr) = do
    let machineN = mkName mach
        machineT = DataD [] machineN [] Nothing [] []

    (stateTy, stateClsMems) <- makeStateType       machine
    (eventTy, eventClsMems) <- makeEventType       machine
    optics                  <- makeOptics          machine
    transitionInsts         <- makeTransitionInsts machine

    eventN <- newName "event"
    stateN <- newName "state"
    let nodes = labNodes gr
        edges = labEdges gr

    let instDecl = [InstanceD Nothing [] (AppT (ConT ''FSM) (ConT machineN)) (stateClsMems ++ eventClsMems)]
        validTrans (fromId, toId, (FSMEvent via _)) = InstanceD Nothing [] (AppT (AppT (AppT (AppT (ConT ''FSMValidTransition) (ConT machineN)) (PromotedT fromN)) (PromotedT viaN)) (PromotedT toN)) []
            where
                viaN = mkName via
                Just (FSMNode from _ _) = lookup fromId nodes
                fromN = mkName from
                Just (FSMNode to   _ _) = lookup toId nodes
                toN = mkName to

    return ([machineT] ++ stateTy ++ eventTy ++ optics ++ (validTrans <$> nub edges) ++ instDecl ++ transitionInsts)

-- | Generates the wide variety of types and type class instances that are required to fully define the function of the FSM.
makeFSMTypes :: String -> DefnM a -> Q [Dec]
makeFSMTypes name defn = reifyFSM =<< makeMachine name defn
