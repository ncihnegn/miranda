||from Barry Brown, Sierra College -- Aug 2009
||the critical case is test5, below

|| Given a number, return the next number in the Collatz sequence
collatz :: num -> num
collatz n = n div 2, if (n mod 2 = 0)
          = 3*n+1, if (n mod 2 = 1)

|| Given a number, return the whole Collatz sequence starting with that	
|| number. Note that it does not include the '1' on the end, but  that's OK
|| since we're only interested in the length.
collatzseq n = takewhile (>1) (iterate collatz n)

|| Given a number, return a tuple with the starting number and the
|| length of the Collatz sequence. We'll find the maximum tuple using the
|| next function. The number returned will be 1 less than the actual
|| Collatz sequence length, but that's OK for our purposes. One one of them
|| will be the longest.
collatzpair n = (n, #(collatzseq n))

|| Given two tuples, return the greater based on the second term.
maxtuple :: (*,**)->(*,**)->(*,**)
maxtuple x y = x, if (snd x > snd y)
             = y, otherwise


test1 = map collatzpair [1..9]
test2 = foldr maxtuple (1,0) (map collatzpair [1..9])
test3 = foldr maxtuple (1,0) (map collatzpair [1..999])
test4 = foldr maxtuple (1,0) (map collatzpair [1..9999])
test5 = foldl maxtuple (1,0) (map collatzpair [1..999999]) ||segfaults, ok with foldl
