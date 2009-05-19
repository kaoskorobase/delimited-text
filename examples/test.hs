import qualified Data.ByteString			as BS
import qualified Data.ByteString.Lazy   	as B
import qualified Data.ByteString.Lex.Double as L
import System.Environment               	(getArgs)
import Text.Delimited                    	as DT

mainInteract :: [Char] -> IO ()
mainInteract ds = do
    s <- DT.interact id ds `fmap` B.getContents
    case s of
        Right bs -> B.putStr bs
        Left e   -> putStrLn ("Parse failed: " ++ e)

mainPrint :: [Char] -> IO ()
mainPrint ds = do
    s <- DT.decode ds `fmap` B.getContents
    case s of
        Right c -> mapM_ print c
        Left e  -> putStrLn ("Parse failed: " ++ e)

readDouble :: Double -> BS.ByteString -> Double
readDouble d s = case L.readDouble s of
					Nothing     -> d
					Just (a, _) -> a

mainPrintNumbers :: [Char] -> IO ()
mainPrintNumbers ds = do
    s <- DT.decode ds `fmap` B.getContents
    case s of
        Right c -> mapM_ (print . map (readDouble 0)) c
        Left e  -> putStrLn ("Parse failed: " ++ e)

main :: IO ()
main = do
	[cmd, ds] <- getArgs
	case cmd of
		"interact" -> mainInteract ds
		"print"    -> mainPrint ds
		"printnum" -> mainPrintNumbers ds
		otherwise  -> putStrLn ("Unknown command " ++ cmd)
