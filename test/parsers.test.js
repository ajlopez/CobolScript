
var parsers = require('../lib/parsers');
const assert = require('assert');

it('Create parser defined', () => {
    expect(!!parsers.createParser).toBe(true);
});

it('Parse simple command', () => {
    var parser = parsers.createParser('DISPLAY "HELLO, WORLD".');
    var cmd = parser.parseCommand();
    expect(!!cmd).toBe(true);
    
    assert.equal('runtime.display("HELLO, WORLD");', cmd.compile());
    assert.equal(null, parser.parseCommand());
});

it('No point at end', () => {
    var parser = parsers.createParser('DISPLAY "HELLO, WORLD"');
    var cmd = parser.parseCommand();
    expect(!!cmd).toBe(true);
    assert.equal('runtime.display("HELLO, WORLD");', cmd.compile());
    assert.equal(null, parser.parseCommand());
});

it('Raise if extraneous char at end', () => {
    var parser = parsers.createParser('DISPLAY "HELLO, WORLD"!');

    assert.throws(
        function() {
            parser.parseCommand();
        },
        function(err) {
            return err == "unexpected '!'";
        }
     );
});
 
it('Parse Identification Division with Program Id', () => {
    var parser = parsers.createParser("\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.");

    var program = parser.parseProgram();

    expect(!!program).toBe(true);
    expect(!!program.identification).toBe(true);
    expect(!!program.identification.program_id).toBe(true);
    expect(program.identification.program_id).toEqual("HELLO")
});

it('Parse commands', () => {
    var parser = parsers.createParser('display "hello". display "world".');

    var commands = parser.parseCommands();
    expect(!!commands).toBe(true);
});
 
it('Parse Identification Division', () => {
    var parser = parsers.createParser("\
IDENTIFICATION DIVISION.\r\n\
    PROGRAM-ID. HELLO.\r\n\
    AUTHOR. A.J.LOPEZ.\r\n\
    INSTALLATION. TEST.\r\n\
    DATE-WRITTEN. 2012-12-22.\r\n\
    DATE-COMPILED. 2012-12-22.\r\n\
    ");

    var program = parser.parseProgram();

    expect(!!program).toBe(true);
    expect(!!program.identification).toBe(true);
    expect(!!program.identification.program_id).toBe(true);
    expect(program.identification.program_id).toEqual("HELLO")
    expect(program.identification.author).toEqual("A.J.LOPEZ")
    expect(program.identification.installation).toEqual("TEST")
    expect(program.identification.date_written).toEqual("2012-12-22")
    expect(program.identification.date_compiled).toEqual("2012-12-22")
});
 
it('Parse Identification Division + Environment Division', () => {
    var parser = parsers.createParser("\
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

    expect(!!program).toBe(true);
    expect(!!program.identification).toBe(true);
    expect(!!program.identification.program_id).toBe(true);
    expect(program.identification.program_id).toEqual("HELLO")
    expect(program.identification.author).toEqual("A.J.LOPEZ")
    expect(program.identification.installation).toEqual("TEST")
    expect(program.identification.date_written).toEqual("2012-12-22")
    expect(program.identification.date_compiled).toEqual("2012-12-22")

    expect(!!program.environment).toBe(true);
    expect(!!program.environment.configuration).toBe(true);
    expect(program.environment.configuration.source_computer).toEqual("NODE")
    expect(program.environment.configuration.object_computer).toEqual("NODE")
});

it('Parse Identification Division + Environment Division + Empty Data Division + Procedure Division', () => { 
    var parser = parsers.createParser('\
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

    expect(!!program).toBe(true);
    expect(!!program.identification).toBe(true);
    expect(!!program.identification.program_id).toBe(true);
    expect(program.identification.program_id).toEqual("HELLO")
    expect(program.identification.author).toEqual("A.J.LOPEZ")
    expect(program.identification.installation).toEqual("TEST")
    expect(program.identification.date_written).toEqual("2012-12-22")
    expect(program.identification.date_compiled).toEqual("2012-12-22")

    expect(!!program.environment).toBe(true);
    expect(!!program.environment.configuration).toBe(true);
    expect(program.environment.configuration.source_computer).toEqual("NODE")
    expect(program.environment.configuration.object_computer).toEqual("NODE")

    expect(!!program.data).toBe(true);
    expect(!!program.command).toBe(true);
});

