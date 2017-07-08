module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM


esperar tope cont rest = do
	atomically(
		do{
			a <- readTVar tope;
			c <- readTVar cont;
			check(tope > cont && rest == 0);
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
			d <- readTVar rest;
			if d == a-1 
				then	do{
				writeTVar cont (0);
				writeTVar cont (0)
				}
				else	do{
				writeTVar rest (rest+1)
				}
		}
		);





crear n = atomically(do {
	v1 <- newTVar 0;
 	v2 <- newTVar 0;
	esperar n v1 v2 });

main = atomically(do {
	a <- newTVar 2;
 	crear a });
