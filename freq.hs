import System.IO
import Control.Monad
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

handleChar :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
handleChar symbol oldMap = M.insertWith (+) symbol 1 oldMap
   
handleString :: B.ByteString -> M.Map Word8 Int
handleString str =
	if B.null str
	then M.empty
	else handleChar (B.head str) (handleString (B.tail str))
   
countFrequences path = withBinaryFile path ReadMode (\h -> B.hGetContents h >>= pure . handleString)