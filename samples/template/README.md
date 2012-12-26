# Template Sample

CobolScript code can be embedded in a text file.

## Setup

Install [Node.js](http://nodejs.org).

## Run

With the command line:

```
node run webserver.cobt
```

(actually, `.cobt` extension is reserved to template files).

The template file:
```
<#
data division.
working-storage section.
01 n.

procedure division.
#>
Factorial
---------

<#
perform show-factorial varying n from 1 to 10.

show-factorial.
local result.
perform factorial using n giving result.
#>
${n}!= ${result}
<#
.
factorial using n.
local m.
if n = 1 then return n.
subtract 1 from n giving m.
perform factorial using m giving m.
multiply n by m.
return m.
#>
```

CobolScript code is embedded between `<#` and `#>`. CobolScript expressions are embedded between `${` and `}`.

