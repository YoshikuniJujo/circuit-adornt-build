{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.BuilderCore (
	-- * Circuit Builder
	CircuitBuilder, CBState(..), FromOWire, initCBState,

	-- * Basic Gate
	BasicGate(..),
	constGate, idGate, andGate, orGate, notGate, triGate, cheatGate,
	gateWires,

	-- * Input and Output Wire
	IWire(..), makeIWire, delay,
	OWire(..), triIWire, makeOWire, makeOWireTri,
	Wire11, Wire21, Wire31, Wire41, Wire51, Wire22, Wire32,

	-- * Wire Connection
	connectWire, connectWire0, connectWire64, connectWire0_64,
	BitLen, BitPosIn, BitPosOut,

	-- * Named Block
	putNamedBlock, BlockName
	) where

import Data.Word

import CircuitBuilder.CircuitCore

constGate :: Word64 -> CircuitBuilder OWire
constGate = constGateW

type Wire11 = (IWire, OWire)
type Wire21 = (IWire, IWire, OWire)
type Wire31 = (IWire, IWire, IWire, OWire)
type Wire41 = (IWire, IWire, IWire, IWire, OWire)
type Wire51 = (IWire, IWire, IWire, IWire, IWire, OWire)

type Wire22 = (IWire, IWire, OWire, OWire)
type Wire32 = (IWire, IWire, IWire, OWire, OWire)

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)

connectWire0_64 :: OWire -> IWire -> CircuitBuilder ()
connectWire0_64 o i = connectWire (o, 1, 0) (i, 64, 0)
