
global require.
local http.
local server.

perform require using "http" giving http.
perform createServer in http using doget giving server.
perform listen in server using 8000.
display "listening on port 8000".

doget using request, response.
perform write in response using "<h1>Hello, world</h1>".
perform end in response.