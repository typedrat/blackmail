module Control.FSM.TH.Description (Machine(..), DefnM(), DefnA(), FSMEvent(..), FSMNode(..), FSMAttribute(..), initial, makeMachine, event, state, terminal, transition, attrib) where

import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.NodeMap as NM
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Language.Haskell.TH

--

data Machine = Machine
             { _mName :: String
             , _mGraph :: Gr FSMNode FSMEvent
             }
             deriving (Eq, Show)

initial :: FSMNode
initial = FSMNode "Initial" [] False

data FSMEvent = FSMEvent
              { _eName :: String
              , _eAttribs :: [FSMAttribute]
              }
              deriving (Eq, Ord, Show)

data FSMNode = FSMNode
              { _nName :: String
              , _nAttribs :: [FSMAttribute]
              , _nTerminal :: Bool
              }
              | FSMLoopbackNode
              deriving (Eq, Ord, Show)

data FSMAttribute = FSMAttribute
            { _aName :: String
            , _aType :: Type
            }
            deriving (Eq, Ord, Show)


-- | This monad is used to encapsulate the machine definition process.
newtype DefnM a = DefnM { runDefnM :: StateT (NM.NodeMap FSMNode, Gr FSMNode FSMEvent) Q a }
                deriving (Functor, Applicative, Monad)

-- | This monad is used to encapsulate the attribute definition process.
newtype DefnA a = DefnA { runDefnA :: WriterT [FSMAttribute] (StateT (NM.NodeMap FSMNode, Gr FSMNode FSMEvent) Q) a }
                deriving (Functor, Applicative, Monad)

makeMachine :: String -> DefnM a -> Q Machine
makeMachine name (DefnM m) = do
    let (initialNode, initialMap) = NM.mkNode NM.new initial
        initialGraph = mkGraph [initialNode] []
    (_, graph) <- execStateT m (initialMap, initialGraph)

    return (Machine name graph)

-- | This function defines an 'FSMEvent', given a name and an attribute-definition action.
event :: String -> DefnA a -> DefnM FSMEvent
event name (DefnA m) = DefnM $ do
    attrs <- execWriterT m
    return (FSMEvent name attrs)

state' :: Bool -> String -> DefnA a -> DefnM FSMNode
state' terminal name (DefnA m) = DefnM $ do
    (map, graph) <- get
    attrs <- execWriterT m

    let label = FSMNode name attrs terminal
        (node, map') = NM.mkNode map label
        graph' = insNode node graph

    put (map', graph')

    return label

-- | This function defines a non-terminal state, given a name and an attribute-definition action.
state :: String -> DefnA a -> DefnM FSMNode
state = state' False

-- | This function defines a terminal state, given a name and an attribute-definition action.
terminal :: String -> DefnA a -> DefnM FSMNode
terminal = state' True

-- | This function defines a valid transition, given an initial state, the event that causes the transition, and the state that is being transitioned to.
transition :: FSMNode -> FSMEvent -> FSMNode -> DefnM ()
transition from with to = DefnM $ do
    (map, graph) <- get

    let Just edge = NM.mkEdge map (from, to, with)
        graph' = insEdge edge graph

    put (map, graph')

    return ()

-- | This function is used to define the attributes of a state or event. It is intended to be used like this:
--
-- > attrib "clientName" [t| BS.ByteString |] -- using quoting to guarantee that a valid type is being referenced
attrib :: String -> TypeQ -> DefnA ()
attrib name tyM = DefnA $ do
    ty <- lift (lift tyM)
    tell [FSMAttribute name ty]
