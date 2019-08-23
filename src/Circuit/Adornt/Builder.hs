{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Builder (
	-- * Circuit Builder
	CircuitBuilder, CBState, initCBState,

	-- * Basic Gate
	BasicGate,
	constGate, idGate, andGate, orGate, notGate, triGate, cheatGate,

	-- * Input and Output Wire
	IWire, delay, OWire,
	Wire11, Wire21, Wire31, Wire41, Wire51, Wire22, Wire32,

	-- * Wire Connection
	connectWire, connectWire0, connectWire64, connectWire0_64,
	BitLen, BitPosIn, BitPosOut,

	-- * Named Block
	putNamedBlock, BlockName
) where

import Circuit.Adornt.BuilderCore
