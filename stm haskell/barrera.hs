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
			cont <- writeTVar(c+1)
		}
		);
	atomically(
		do
			check(a == c)
		);
	atomically(
		do
		{	
			if (rest == a-1) then
				do
				cont <- writeTVar(0);
				rest <- writeTVar(0)
			else
				do
				rest <- writeTVar(rest+1)
		}
		);

crear n = esperar n 0 0 


main = crear 2
