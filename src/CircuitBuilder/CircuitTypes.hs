{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitBuilder.CircuitTypes (
	CircuitBuilder, CBState(..), initCBState,
	IWire(..), makeIWire, OWire(..), triIWire, makeOWire, makeOWireTri,
	BasicGate(..), gateWires,
	FromOWire, BitLen, BitPosIn, BitPosOut,
	BlockName, putNamedBlock
	) where

import Prelude as P

import Control.Monad.State
import Data.Word
import Data.Map

import Tools

type FromOWire = ((BitLen, BitPosOut), (BitLen, BitPosIn))

newtype IWire = IWire Word32 deriving (Show, Eq, Ord)
data OWire = OWire Word32 (Maybe IWire) deriving (Show, Eq, Ord)

triIWire :: OWire -> Maybe IWire
triIWire (OWire _ mi) = mi

type BitLen = Word8
type BitPosIn = Word8
type BitPosOut = Word8

type CircuitBuilder = State CBState

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = (`OWire` Nothing) <$> getModify cbsWireNum sccWireNum

makeOWireTri :: IWire -> CircuitBuilder OWire
makeOWireTri i = (`OWire` Just i) <$> getModify cbsWireNum sccWireNum

sccWireNum :: CBState -> CBState
sccWireNum cbs = cbs { cbsWireNum = cbsWireNum cbs + 1 }

type BlockName = String

putNamedBlock :: BlockName -> [IWire] -> [OWire] -> CircuitBuilder ()
putNamedBlock nm iws ows = modify $ putCbsBlock iws ows nm

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OWire BasicGate,
	cbsWireConn :: Map IWire [(OWire, FromOWire)],
	cbsDelay :: Map IWire Word8,
	cbsBlock :: [([IWire], [OWire], String)] }
	deriving Show

initCBState :: CBState
initCBState = CBState {
	cbsWireNum = 0, cbsGate = empty, cbsWireConn = empty, cbsDelay = empty,
	cbsBlock = [] }

putCbsBlock :: [IWire] -> [OWire] -> String -> CBState -> CBState
putCbsBlock iws ows nm cbs = cbs { cbsBlock = (iws, ows, nm) : cbsBlock cbs }

data BasicGate
	= ConstGate Word64
	| IdGate IWire | NotGate IWire
	| AndGate IWire IWire | OrGate IWire IWire
	deriving (Show, Eq, Ord)

gateWires :: BasicGate -> [IWire]
gateWires (ConstGate _) = []
gateWires (IdGate i) = [i]
gateWires (NotGate i) = [i]
gateWires (AndGate a b) = [a, b]
gateWires (OrGate a b) = [a, b]
