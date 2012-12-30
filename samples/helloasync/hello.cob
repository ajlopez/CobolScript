procedure division.
perform sayhello async.
local name.
perform getname async giving name.
display name.

sayhello section async.
display "Hello, " with no advancing.

getname section asynchronous.
return "World".
