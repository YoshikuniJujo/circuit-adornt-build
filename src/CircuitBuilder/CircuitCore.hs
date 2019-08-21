{-# LANGUAGE TupleSections, MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitBuilder.CircuitCore (
	CircuitBuilder,
	IWire(..), OWire(..), BitLen, BitPosIn, BitPosOut,
	constGateW, idGate, notGate, andGate, orGate, triGate, cheatGate,
	connectWire, delay,

	CBState(..), initCBState,
	BasicGate(..),

	FromOWire,

	makeIWire, makeOWire, makeOWireTri, triIWire, gateWires,

	BlockName, putNamedBlock
	) where

import Control.Monad.State
import Data.Word
import Data.Map

import CircuitBuilder.CircuitTypes
import Tools

connectWire :: (OWire, BitLen, BitPosOut) ->
	(IWire, BitLen, BitPosIn) -> CircuitBuilder ()
connectWire (_, obl, obp) (_, ibl, ibp)
	| obl <= 0 || ibl <= 0 =
		error "connectWire: length should be larger than 0"
	| obp < 0 || ibp < 0 =
		error "connectWire: position should be larger than or equal to 0"
	| obl + obp > 64 || ibl + ibp > 64 = error
		$ "connectWire: length + position should be less then or equal to 64\n" ++
			"\tobl: " ++ show obl ++ " obp: " ++ show obp ++ " ibl: " ++ show ibl ++ " ibp: " ++ show ibp
connectWire (o, obl, obp) (i, ibl, ibp) =
	modify $ insConn ((obl, obp), (ibl, ibp))
	where insConn f cbs = cbs {
		cbsWireConn =
			insert i ((o, f) : indexOrEmpty (cbsWireConn cbs) i)
				$ cbsWireConn cbs }

delay :: IWire -> Word8 -> CircuitBuilder ()
delay _ 0 = error "0 delay is not permitted"
delay iw n = modify insDelay
	where insDelay cbs = cbs { cbsDelay = insert iw n $ cbsDelay cbs }

constGateW :: Word64 -> CircuitBuilder OWire
constGateW bs = do
	o <- makeOWire
	modify $ insGate (ConstGate bs) o
	return o

idGate, notGate :: CircuitBuilder (IWire, OWire)
idGate = do
	io@(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (IdGate i) o
	return io

notGate = do
	io@(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (NotGate i) o
	return io

andGate, orGate :: CircuitBuilder (IWire, IWire, OWire)
andGate = do
	abo@(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate a b) o
	return abo
orGate = do
	abo@(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (OrGate a b) o
	return abo

triGate :: CircuitBuilder (IWire, IWire, OWire)
triGate = do
	(a, b) <- (,) <$> makeIWire <*> makeIWire
	o <- makeOWireTri b
	modify $ insGate (IdGate a) o
	return (a, b, o)

cheatGate :: BlockName -> Int -> Int -> ([IWire] -> [OWire]) -> CircuitBuilder ([IWire], [OWire])
cheatGate nm iwn own f = do
	iws <- replicateM iwn makeIWire
	ows <- mapM (cheatGateIndex f iws) [0 .. own - 1]
	putNamedBlock nm iws ows
	return (iws, ows)

cheatGateIndex :: ([IWire] -> [OWire]) -> [IWire] -> Int -> CircuitBuilder OWire
cheatGateIndex f iws owi = do
	o <- makeOWire
	modify $ insGate (CheatGate iws ((!! owi) . f)) o
	return o

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g o cbs = cbs { cbsGate = insert o g $ cbsGate cbs }
