
var cobs = require('../'),
    assert = require('assert');

// Parser defined

assert.ok(cobs.Parser);

// Parse simple command

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD".');

var cmd = parser.parseCommand();

assert.ok(cmd);

assert.equal('console.log("HELLO, WORLD");', cmd.compile());

