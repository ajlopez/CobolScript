
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
 
// Parse Identification Division with Program Id
 
 var parser = new cobs.Parser("\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.");

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.identification);
assert.ok(program.identification.program_id);
assert.equal(program.identification.program_id, "HELLO");
 
// Parse Identification Division with Program Id, Author
 
 var parser = new cobs.Parser("\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.\r\n\
    AUTHOR. A.J.LOPEZ.\r\n\
    INSTALLATION. TEST.\r\n\
    DATE-WRITTEN. 2012-12-22.\r\n\
    DATE-COMPILED. 2012-12-22.\r\n\
    ");

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.identification);
assert.ok(program.identification.program_id);
assert.equal(program.identification.program_id, "HELLO");
assert.equal(program.identification.author, "A.J.LOPEZ");
assert.equal(program.identification.installation, "TEST");
assert.equal(program.identification.date_written, "2012-12-22");
assert.equal(program.identification.date_compiled, "2012-12-22");

