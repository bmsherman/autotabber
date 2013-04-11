> module Write where
> import qualified Data.ByteString.Lazy as B

> main :: String -> IO ()
> main = do
  contents <- B.getContents
  B.putStr contents
