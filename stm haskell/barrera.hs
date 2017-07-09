module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

esperar :: TVar Integer -> TVar Integer -> TVar Integer -> IO ()
esperar tope cont rest = do
	atomically(
		do{
			a <- readTVar tope;
			c <- readTVar cont;
			r <- readTVar rest;
			check(a > c && r == 0);
			writeTVar cont (c+1)
		}
		);
	atomically(
		do{
			a <- readTVar tope;
			c <- readTVar cont;
			check(a == c)
		}
		);
	atomically(
		do
		{	
			a <- readTVar tope;
			r <- readTVar rest;
			if r == a-1 
				then	do{
				writeTVar cont (0);
				writeTVar cont (0)
				}
				else	do{
				writeTVar rest (r+1)
				}
		}
		)

barrera :: TVar Integer -> IO ()
barrera n = do {
	v1 <- atomically(newTVar 0);
 	v2 <- atomically(newTVar 0);
	esperar n v1 v2
}

--main :: TVar Integer -> IO ()
main = do {
	a <- atomically(newTVar 2);
 	barrera a }
