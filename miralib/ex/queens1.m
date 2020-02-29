||This finds one solution to the eight queens problem,  using  a
||different method from that of the previous script, "queens.m".
||To run it, say
||	output
||This time the backtracking is programmed explicitly

output = concat [c:shownum r++" "|(c,r)<-zip2 "rnbqkbnr" soln]
soln = until full extend emptyboard
extend board = until safe alter (addqueen board)
addqueen board = 1:board
emptyboard = []
full board = # board=8
alter (q:board) = q+1:board, if q<8
                = alter board, otherwise ||backtrack
safe (q:board) = and [~checks q board i|i<-index board]
checks q board i = q=board!i \/ abs(q-board!i)=i+1
