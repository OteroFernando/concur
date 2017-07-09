module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM


wlock :: TVar Integer -> TVar Integer -> IO ()
wlock readCant writeCant = do
	atomically(
		do{
			w <- readTVar writeCant;
			check(w == 0); 	--si alguien escribe tengo q esperar
			writeTVar writeCant (w+1)
		});
		atomically(
		do{
			r <- readTVar readCant;
			check(r == 0); 	--si alguien lee tengo q esperar
		})

wunlock :: TVar Integer -> IO ()
wunlock writeCant = do
	atomically(
		do{
			w <- readTVar writeCant;
			writeTVar writeCant (w-1)
		}
		)

rlock :: TVar Integer -> TVar Integer -> TVar Integer -> IO ()
rlock readCant writeCant maxLectores = do	--maxLectores es del punto 4
	atomically(
		do{
			w <- readTVar writeCant;
			r <- readTVar readCant;
			check(w == 0); 	--si alguien escribe tengo q esperar
			writeTVar readCant (r+1)
		});
		atomically(	--en el punto 3 este atomically no va
		do{
			r <- readTVar readCant;
			n <- readTVar maxLectores;
			check(r < n); 	--si hay mas de n lectores tengo q esperar
		})

runlock :: TVar Integer -> IO ()
runlock readCant = do
	atomically(
		do{
			r <- readTVar readCant;
			writeTVar readCant (r-1)
		}
		)

rwlock :: TVar Integer -> IO ()
rwlock maxLectores = do {
	v1 <- atomically(newTVar 0);
 	v2 <- atomically(newTVar 0);
	rlock maxLectores v1 v2;
	runlock v1
}

--main :: TVar Integer -> IO ()
main = do {
	a <- atomically(newTVar 2);
 	rwlock a }
