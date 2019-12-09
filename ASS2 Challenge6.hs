import DFA    -- hidden module, defines DFA types as in worksheet 3
import RunDFA -- hidden module, contains a completed DFA emulator
import VisDFA -- hidden module, contains DFA visualisation tools
import EqDFA  -- hidden module, required for testing purposes
import Data.List
-- put your solution to challenge 6 here:
multiples :: Int -> DFA
-- you may like to start by hand-building DFAs
-- for 1 <= n <= 6.
-- then, move on to the general case, and/or to
-- making your DFAs minimal.
-- feel free to replace the cases above with a single,
-- general definition if you prefer.
bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + (mod i 10)
multiples n
  |2^(powernumber)==n =powermultiples n powernumber 0
  |mod n 2==0 =([0..(powernumber+(oddpart)-1)], "01",y,oddpart+k-1,[oddpart+k-1])
  |otherwise =([0..n-1], "01", t, 0, [0])
  where
  k=powernumber
  start=oddpart-1
  tmp=[((x,'1'),start)|x<-[start..(start+k)]]++[((x,'0'),(x+1))|x<-[start..(start+k-1)]]++[((start+k,'0'),start+k)]
  oddpart=div n (2^powernumber)
  powernumber=getpowernum n
  t=(generateT [] (n-1) n)
  y=(delete ((0,'0'),0) ((generateT [] (oddpart-1) oddpart)++[((0,'0'),oddpart)]))++(makedelete ((oddpart-1,'0'),0) (makecomb ((0,'0'),(oddpart-1)) tmp))
makecomb :: Transn->[Transn]->[Transn]
makecomb _ []                 = []
makecomb ((a,b),c) (((d,e),f):ys) | c == f    =((d,e),1): makecomb ((a,b),c) ys
                    | otherwise = ((d,e),f) : makecomb ((a,b),c) ys
makedelete :: Transn->[Transn]->[Transn]
makedelete _ []                 = []
makedelete ((a,b),c) (((d,e),f):ys) | a == d    =makedelete ((a,b),c) ys
                    | otherwise = ((d,e),f) : makedelete ((a,b),c) ys
powermultiples :: Int->Int->Int-> DFA
powermultiples n k start=([start..start+k],"01",t,start+k,[start+k])
  where t=[((x,'1'),start)|x<-[start..(start+k)]]++[((x,'0'),(x+1))|x<-[start..(start+k-1)]]++[((start+k,'0'),start+k)]
  
getpowernum ::Int->Int
getpowernum n
  |mod n 2==1 =0
  |otherwise =getpowernum (n`div`2) +1
generateT ::[Transn]->Int->Int->[Transn]
generateT [] 0 n=[((0,'0'),(0 `mod` n)),((0,'1'),(1 `mod` n))]
generateT ls k n=
  tmp++[((k,'0'),x),((k,'1'),y)]
  where tmp=(generateT ls (k-1) n)
        x=(bintodec(read ((getA tmp k)++['0']))) `mod` n
        y=(bintodec(read((getA tmp k)++['1']))) `mod` n
getA ::[Transn]->Int->String
getA ls 0=[]
getA ls n=(getA ls a)++[b]
  where ((a,b),c)=findmatch ls n
findmatch ::[Transn]->Int->Transn
findmatch (l:ls) n
  |c==n =l
  |otherwise =findmatch ls n
  where ((a,b),c)=l
  