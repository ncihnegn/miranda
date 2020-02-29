||defines ackermann's function, beloved of recursion theorists.  Example
||	ack 3 3
||should  yield 61, after doing a huge amount of recursion.  Can only be
||called for small arguments, because the values get so big.

ack 0 n = n+1
ack (m+1) 0 = ack m 1
ack (m+1) (n+1) = ack m (ack (m+1) n)
ack m n = error "ack applied to -ve or fractional arg"
