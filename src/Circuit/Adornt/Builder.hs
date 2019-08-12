{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Builder (
	CircuitBuilder, CBState(..), initCBState,
	BasicGate(..), gateWires,
	constGate, idGate, andGate, orGate, notGate, triGate,
	IWire(..), makeIWire, delay,
	OWire(..), triIWire, makeOWire, makeOWireTri,
	connectWire,
	FromOWire, BitLen, BitPosIn, BitPosOut,
	) where

import Data.Word

import CircuitBuilder.CircuitCore

constGate :: Word64 -> CircuitBuilder OWire
constGate = constGateW
