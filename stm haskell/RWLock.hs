module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Parallel

--quite el ejemplo xq no tenia nada q ver. era wr sin lock xD

wlock readCant writeCant = do
	atomically(
		do{
			w <- readTVar writeCant;
			check(w == 0); 	--si alguien escribe tengo q esperar
			writeTVar writeCant (w+1)
		};
		atomically(
		do{
			r <- readTVar readCant;
			check(r == 0); 	--si alguien lee tengo q esperar
		}

wunlock writeCant = do
	atomically(
		do{
			w <- readTVar writeCant;
			writeTVar writeCant (w-1)
		}
		)

rlock readCant writeCant maxLectores = do	--maxLectores es del punto 4
	atomically(
		do{
			w <- readTVar writeCant;
			r <- readTVar readCant;
			check(w == 0); 	--si alguien escribe tengo q esperar
			writeTVar readCant (r+1)
		}
		atomically(	--en el punto 3 este atomically no va
		do{
			r <- readTVar readCant;
			n <- readTVar maxLectores;
			check(r < n); 	--si hay mas de n lectores tengo q esperar
		}

runlock readCant = do
	atomically(
		do{
			r <- readTVar readCant;
			writeTVar readCant (r-1)
		}
		)