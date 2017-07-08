module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM


esperar tope cont rest = do
	atomically(do
		a <- readTVar tope;
		c <- readTVar cont;
		check(tope < cont && rest == 0);
		cont <- writeTVar(c+1)
		);
	atomically(
		do
			check(a == c)
		);
	atomically(
		do
			rest <- writeTVar(rest+1)
			if (rest == a) then
				do
				cont <- writeTVar(0)
				rest <- writeTVar(0)
			else
				idle
		);

crear n = esperar n 0 0 


main = crear 2
