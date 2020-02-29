||this is a problem described by Dijkstra in his book, A Discipline of
||Programming, and attributed by him to Dr Hamming, of Bell Labs.
||Print in ascending order all numbers of the form
|| 2**a.3**b.5**c        a,b,c all >=0
||the solution here is based on a method using communicating processes.
||ham is the list of numbers, to see them, say
||  ham
||hit control-C (interrupt) when you have seen enough!

ham = 1 : foldr1 merge [mult 2 ham, mult 3 ham, mult 5 ham]
      where
      mult n x = [n*a|a<-x]
      merge (a:x) (b:y) = a : merge x y,     if a=b
			= a : merge x (b:y), if a<b
			= b : merge (a:x) y, if a>b

||Note  that  there  is  a  function  called  `merge'  in  the  standard
||environment,  but  unlike  the  one  defined  above it does not remove
||duplicates from the lists being merged.
