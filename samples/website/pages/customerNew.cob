data division.
linkage section.
01 require.
01 request.
01 response.
procedure division.
local mysql.

perform require using "mysql" giving mysql.

local name.
local address.
local notes.

move name in body in request to name.
move address in body in request to address.
move notes in body in request to notes.

local connection.

perform insert-customer.

insert-customer section.
local options.
move object to options.
move "root" to user in options.
move "" to password in options.
move "cobolscriptwebsite" to database in options.

perform createConnection in mysql using options giving connection.
perform connect in connection.

local datavalues.
move array to datavalues.
perform push in datavalues using name.
perform push in datavalues using address.
perform push in datavalues using notes.

perform query in connection using "insert customers set Name = ?, Address = ?, Notes = ?" datavalues insert-end.

insert-end section using err, result.
if err then
    display "Error".
    stop run.
end-if.
local headers.
move object to headers.
move "/customer" to headers("Location").
perform writeHead in response using 302 headers.
perform end in connection.
stop run.
