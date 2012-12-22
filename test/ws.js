
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

