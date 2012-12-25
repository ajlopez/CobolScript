
var cobs = require('../').complete(),
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
assert.ok(typeof(program.data.working_storage.group1.item1) != 'undefined');
assert.ok(typeof(program.data.working_storage.group1.item2) != 'undefined');

// Working storage with two group items and three levels
 
var parser = new cobs.Parser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 GROUP1.\r\n\
02 ITEM1.\r\n\
03 SUBITEM1.\r\n\
03 SUBITEM2.\r\n\
02 ITEM2.\r\n\
01 GROUP2.\r\n\
02 ITEM1.\r\n\
02 ITEM2.\r\n\
    ');

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.data);
assert.ok(program.data.working_storage);
assert.ok(program.data.working_storage.group1);
assert.ok(program.data.working_storage.group1.item1);
assert.ok(typeof(program.data.working_storage.group1.item1.subitem1) != 'undefined');
assert.ok(typeof(program.data.working_storage.group1.item1.subitem2) != 'undefined');
assert.ok(typeof(program.data.working_storage.group1.item2) != 'undefined');
assert.ok(program.data.working_storage.group2);
assert.ok(typeof(program.data.working_storage.group2.item1) != 'undefined');
assert.ok(typeof(program.data.working_storage.group2.item2) != 'undefined');
