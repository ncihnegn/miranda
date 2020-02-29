> ||this tests that floating point overflow is handled correctly

Date:          Mon, 16 Apr 90 17:15:03 CST
From:          mccrosky@ishmael.usask.ca

Sorry for the delay in sending my bignum problem,  I've been out of town.
 
> keith n = (shownum n) ++ " " ++ (shownum ((log (sumterm n))/n)) where
>           sumterm n = sum (map prodterm [0..entier (n/2)]) where
>                       prodterm p = prod (map term [0..p-1]) where
>                                    term j = x*(x-1)/((p-j)^2) where
>                                             x = n-(2*j)
> prod = foldr (*) 1
> lim = lay (from 1) where from n = (keith n):(from (n*2))
 
 
******** This is the execution:
         (We believe the results up to n=256 are correct).
 
Miranda lim
1 0.0
2 0.549306144334
4 0.736109744792
8 0.876176116589
16 0.966470019952
32 1.021895160467
64 1.054884461071
128 1.07405223475
256 1.084981322415
512
should trap floating point overflow here
