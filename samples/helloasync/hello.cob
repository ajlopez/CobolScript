procedure division.
perform sayhello async.
local name.
perform getname async giving name.
display name.

sayhello async.
display "Hello, " with no advancing.

getname asynchronous.
return "World".
