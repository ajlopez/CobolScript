data division.
working-storage section.
01 n.

procedure division.
perform show-factorial varying n from 1 to 10.

show-factorial locals result.
perform factorial using n giving result.
display n "! = " result.

factorial using n locals m.
if n = 1 then return n.
subtract 1 from n giving m.
perform factorial using m giving m.
multiply n by m.
return m.


