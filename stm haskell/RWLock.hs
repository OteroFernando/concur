module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Parallel


writer b = wloop 10
		where
		wloop 0 = return ()
		wloop n = do{
		atomically (put b n);
		wloop (n-1)}


reader b = rloop
	where rloop = do{
	v <- atomically (get b);
	hPutStr stdout $ "Consumo "++ (show v) ++ "\n";
	rloop}

mainBuffer = do {
b <- atomically newBuf;
forkIO (reader b);
forkIO (writer b)
}
