# Simple Web Sample

CobolScript can access MySQL server using a Node.js module, 
and listing databases, tables and columns in dynamic web pages. It uses Twitter bootstrap, and SimpleWeb middleware layer.

## Setup

Install [Node.js](http://nodejs.org).

Then, execute at command line:
```
npm install
```
This command installs the `mysql`, `simpleweb` modules, according to the dependecies described in `package.json` file.

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

local connection.

perform createConnection in mysql using options giving connection.
```
opens the connection.

The code
```
perform query in connection using "show databases" showdbs.

showdbs section using err, rows, fields.
* ....
```
pass `showdbs` as a callback to asynchronous method `query` in `connection`. The callbacks accept three parameters.

