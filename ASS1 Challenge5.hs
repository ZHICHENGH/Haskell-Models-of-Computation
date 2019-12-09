import Exp    -- helper module, defines Exp types as in
              -- worksheet 1, but with integer variable
              -- identifiers, instead of characters
              -- (and there's no 'parse' function).
import Hidden -- a hidden module providing a function
              -- 'solve :: [Int] -> Exp -> [Int]', as
              -- described in the problem description.

-- put your solution to challenge 5 here:
findClose:: Int->Int->[Int]
findClose n x
  |(mod x n)==1 =[y|y<-[x-n,x,x+1,x+n],y>0&&y<=n*n]
  |(mod x n)==0 =[y|y<-[x-n,x-1,x,x+n],y>0&&y<=n*n]
  |otherwise =[y|y<-[x-n,x-1,x,x+1,x+n],y>0&&y<=n*n]


getcomb ::Int->[Int]->[[Int]]
getcomb 0 xs=[[]]
getcomb  n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- getcomb (n-1) (drop (i+1) xs) ]

getallcomb ::Int->[Int]->[[Int]]
getallcomb 1 xs=getcomb 1 xs
getallcomb n xs=(getcomb n xs)++(getallcomb (n-2) xs)

getall ::[Int]->[[Int]]
getall xs
  |mod (length xs) 2==0 =getallcomb ((length xs)-1) xs
  |otherwise =getallcomb (length xs) xs

getallcomb1 ::Int->[Int]->[[Int]]
getallcomb1 0 xs=[[]]
getallcomb1 n xs=(getcomb n xs)++(getallcomb1 (n-2) xs)

getall1 ::[Int]->[[Int]]
getall1 xs
  |mod (length xs) 2==1 =getallcomb1 ((length xs)-1) xs
  |otherwise =getallcomb1 (length xs) xs
getpointcomb ::Int->Int->[[Int]]
getpointcomb x n=reverse(getall (findClose n x))

getpointcomb1 ::Int->Int->[[Int]]
getpointcomb1 x n=reverse(getall1 (findClose n x))

sing :: [Int]->[Int]->Exp
sing xs (l:[]) 
  |elem l xs =VAR l
  |otherwise =NOT(VAR l)
sing xs (l:ls)
  |elem l xs =AND (VAR l) (sing xs ls)
  |otherwise =AND (NOT (VAR l)) (sing xs ls)

allsing:: Int->Int->[Exp]
allsing x n=[sing k (findClose n x)|k<-(getpointcomb x n)]

allsing1:: Int->Int->[Exp]
allsing1 x n=[sing k (findClose n x)|k<-(getpointcomb1 x n)]

getonepoint :: [Exp]->Exp
getonepoint (l:[])=l
getonepoint (l:ls)=OR l (getonepoint ls)

comballpoint ::Int->Int->[Int]->Exp
comballpoint 1 n ls
  |elem 1 ls =getonepoint (allsing 1 n)
  |otherwise =getonepoint (allsing1 1 n)
comballpoint x n ls
  |elem x ls =AND (getonepoint (allsing x n)) (comballpoint (x-1) n ls)
  |otherwise =AND (getonepoint (allsing1 x n)) (comballpoint (x-1) n ls)

lights :: Int -> [Int] -> [Int]
lights n configuration
  =solve [1..n*n] (comballpoint (n*n) n configuration)
