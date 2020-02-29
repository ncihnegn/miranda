||this is a functional version of quicksort, to see it work, say:
||	qsort testdata
||the reason we have to call the function `qsort' rather than `sort' is
||because there is a `sort' already defined in the standard environment

qsort [] = []
qsort (a:x) = qsort [b|b<-x;b<=a] ++ [a] ++ qsort[b|b<-x;b>a]

testdata = f 10
f n = concat(transpose [[0,2..2*n],[2*n-1,2*n-3..1]])

||note that the sort included in the standard environment is merge-sort
