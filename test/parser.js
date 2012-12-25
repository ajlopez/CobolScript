
var cobs = require('../').complete(),
    assert = require('assert');

// Parser defined

assert.ok(cobs.Parser);

// Parse simple command

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD".');
var cmd = parser.parseCommand();
assert.ok(cmd);
assert.equal('runtime.display("HELLO, WORLD");', cmd.compile());
assert.equal(null, parser.parseCommand());

// No point at end

var parser = new cobs.Parser('DISPLAY "HELLO, WORLD"');
var cmd = parser.parseCommand();
assert.ok(cmd);
assert.equal('runtime.display("HELLO, WORLD");', cmd.compile());
assert.equal(null, parser.parseCommand());

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
 
// Parse Identification Division with Program Id
 
 var parser = new cobs.Parser("\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.");

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.identification);
assert.ok(program.identification.program_id);
assert.equal(program.identification.program_id, "HELLO");

// Parse commands

var parser = new cobs.Parser('display "hello". display "world".');

var commands = parser.parseCommands();
assert.ok(commands);
 
// Parse Identification Division
 
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
 
// Parse Identification Division + Environment Division
 
var parser = new cobs.Parser("\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.\r\n\
    AUTHOR. A.J.LOPEZ.\r\n\
    INSTALLATION. TEST.\r\n\
    DATE-WRITTEN. 2012-12-22.\r\n\
    DATE-COMPILED. 2012-12-22.\r\n\
ENVIRONMENT DIVISION.\r\n\
    CONFIGURATION SECTION.\r\n\
        SOURCE-COMPUTER. NODE.\r\n\
        OBJECT-COMPUTER. NODE.\r\n\
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

assert.ok(program.environment);
assert.ok(program.environment.configuration);
assert.equal(program.environment.configuration.source_computer, "NODE");
assert.equal(program.environment.configuration.object_computer, "NODE");

// Parse Identification Division + Environment Division + Empty Data Division + Procedure Division
 
var parser = new cobs.Parser('\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.\r\n\
    AUTHOR. A.J.LOPEZ.\r\n\
    INSTALLATION. TEST.\r\n\
    DATE-WRITTEN. 2012-12-22.\r\n\
    DATE-COMPILED. 2012-12-22.\r\n\
ENVIRONMENT DIVISION.\r\n\
    CONFIGURATION SECTION.\r\n\
        SOURCE-COMPUTER. NODE.\r\n\
        OBJECT-COMPUTER. NODE.\r\n\
DATA DIVISION.\r\n\
PROCEDURE DIVISION.\r\n\
    DISPLAY "HELLO".\r\n\
    ');

var program = parser.parseProgram();

assert.ok(program);
assert.ok(program.identification);
assert.ok(program.identification.program_id);
assert.equal(program.identification.program_id, "HELLO");
assert.equal(program.identification.author, "A.J.LOPEZ");
assert.equal(program.identification.installation, "TEST");
assert.equal(program.identification.date_written, "2012-12-22");
assert.equal(program.identification.date_compiled, "2012-12-22");

assert.ok(program.environment);
assert.ok(program.environment.configuration);
assert.equal(program.environment.configuration.source_computer, "NODE");
assert.equal(program.environment.configuration.object_computer, "NODE");

assert.ok(program.data);
assert.ok(program.command);

