{-
esperar b = do{
	atom(do{
		a,c <- readTvar b;
		check(a/=c);
		writeTvar(a,c+1)
		});
	atom(
		do{
		check(a = c)
		})
}
-}
import Control.Parallel