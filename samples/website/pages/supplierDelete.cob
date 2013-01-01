data division.
linkage section.
01 require.
01 request.
01 response.
procedure division.
local mysql.

perform require using "mysql" giving mysql.

local id.

move id in query in request to id.

local connection.

perform delete-supplier.

delete-supplier section.
local options.
move object to options.
move "root" to user in options.
move "" to password in options.
move "cobolscriptwebsite" to database in options.

perform createConnection in mysql using options giving connection.
perform connect in connection.

local datavalues.
move array to datavalues.
perform push in datavalues using id.

perform query in connection using "delete from suppliers where Id = ?" datavalues delete-end.

delete-end section using err, result.
if err then
    display "Error".
    stop run.
end-if.
local headers.
move object to headers.
move "/supplier" to headers("Location").
perform writeHead in response using 302 headers.
perform end in connection.
stop run.
