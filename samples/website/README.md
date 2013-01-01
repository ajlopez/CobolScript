# Web Site Sample

CobolScript can access MySQL server using a Node.js module, 
and listing databases, tables and columns in dynamic web pages. It uses Twitter bootstrap, and SimpleWeb middleware layer.

## Setup

### Node

Install [Node.js](http://nodejs.org).

Then, execute at command line:
```
npm install
```
This command installs the `mysql`, `simpleweb` modules, according to the dependecies described in `package.json` file.

### MySQL

You must create a database `cobolscriptwebsite` in your MySQL database, and then execute the commands at `database.sql`. These
commands defines the tables and initial data to be used by the sample.

## Run

With the command line:
```
node server
```

The output:
```
Server started, listening at port 8000
```

The program connects to the MySQL server at localhost and list its database. The server must be running.

In the pages, the code
```
perform require using "mysql" giving mysql.
```
loads the `mysql` module.

Then
```
local options.

move object to options.
move "root" to user in options.
move "" to password in options.
move "cobolscriptwebsite" to database options.

local connection.

perform createConnection in mysql using options giving connection.
```
opens the connection.

The code
```
perform query in connection using "select Id, Name, Address, Notes from customers order by Id" showcustomers.

showcustomers section using err, rows, fields.
* ....
```
pass `showcustomers` as a callback to asynchronous method `query` in `connection`. 
The callback accept three parameters: `err`, `rows`, `fields`.

When the sql is an update command:
```
perform query in connection using "insert customers set Name = ?, Address = ?, Notes = ?" datavalues showcustomers.
```
then the callback receives two parameters: `err`, `result`.

The `datavalues` parameters is an array, with the values to be passed to `?` placeholders.
See [node-sql](https://github.com/felixge/node-mysql) for more detailed information.

