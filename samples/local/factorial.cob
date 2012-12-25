
* using a local variable instead of an item 01 in working-storage section

local n.
perform show-factorial using n varying n from 1 to 10.

show-factorial using n.
local result.
perform factorial using n giving result.
display n "! = " result.

factorial using n.
local m.
if n = 1 then return n.
subtract 1 from n giving m.
perform factorial using m giving m.
multiply n by m.
return m.


