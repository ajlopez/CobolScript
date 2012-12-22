
var cobs = require('../'),
    assert = require('assert');

// Parser defined

assert.ok(cobs.Parser);

// Parse simple command

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD".');

var cmd = parser.parseCommand();

assert.ok(cmd);

assert.equal('console.log("HELLO, WORLD");', cmd.compile());

assert.equal(null, parser.parseCommand());

// Raise if no point at end

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD"');

assert.throws(
    function() {
        parser.parseCommand();
    },
    function(err) {
        return err == "unexpected end of input";
    }
 );

// Raise if extraneous char at end

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD"!');

assert.throws(
    function() {
        parser.parseCommand();
    },
    function(err) {
        return err == "unexpected '!'";
    }
 );
 
// Raise if extra name at end

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD" FOO');

assert.throws(
    function() {
        parser.parseCommand();
    },
    function(err) {
        return err == "unexpected 'FOO'";
    }
 );