module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM


esperar tope cont rest = do
	atomically(do
		a <- readTvar tope;
		c <- readTvar cont;
		check(tope < cont && rest == 0);
		cont <- writeTvar(c+1)
		);
	atomically(
		do
			check(a == c)
		);
	atomically(
		do
			rest <- writeTvar(rest+1)
			if (rest == a) then
				do
				cont <- writeTvar(0)
				rest <- writeTvar(0)
			else
				idle
		);

crear n = esperar n 0 0 


main = crear 2
