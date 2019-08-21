{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Builder (
	CircuitBuilder, CBState(..), initCBState,
	BasicGate(..), gateWires,
	constGate, idGate, andGate, orGate, notGate, triGate, cheatGate,
	IWire(..), makeIWire, delay,
	OWire(..), triIWire, makeOWire, makeOWireTri,
	Wire11, Wire21, Wire31, Wire41, Wire51, Wire22,
	connectWire, FromOWire, BitLen, BitPosIn, BitPosOut,
	connectWire0, connectWire64, connectWire0_64,
	BlockName, putNamedBlock
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

connectWire64 :: OWire -> IWire -> CircuitBuilder ()
connectWire64 o i = connectWire (o, 64, 0) (i, 64, 0)

connectWire0 :: OWire -> IWire -> CircuitBuilder ()
connectWire0 o i = connectWire (o, 1, 0) (i, 1, 0)

connectWire0_64 :: OWire -> IWire -> CircuitBuilder ()
connectWire0_64 o i = connectWire (o, 1, 0) (i, 64, 0)
