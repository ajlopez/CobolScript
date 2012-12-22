
var cobs = require('../'),
    assert = require('assert');

// Working storage with one variable
 
var parser = new cobs.Parser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 ITEM.\r\n\
    ');

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.data);
assert.ok(program.data.working_storage);
assert.ok(typeof(program.data.working_storage.item) != 'undefined');

// Working storage with two variables
 
var parser = new cobs.Parser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 ITEM1.\r\n\
01 ITEM2.\r\n\
    ');

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.data);
assert.ok(program.data.working_storage);
assert.ok(typeof(program.data.working_storage.item1) != 'undefined');
assert.ok(typeof(program.data.working_storage.item2) != 'undefined');

// Working storage with group item and two subitems
 
var parser = new cobs.Parser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 GROUP1.\r\n\
02 ITEM1.\r\n\
02 ITEM2.\r\n\
    ');

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.data);
assert.ok(program.data.working_storage);
assert.ok(typeof(program.data.working_storage.group1) != 'undefined');
assert.ok(typeof(program.data.working_storage.group1.items.item1) != 'undefined');
assert.ok(typeof(program.data.working_storage.group1.items.item2) != 'undefined');
