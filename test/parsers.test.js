
var parsers = require('../lib/parsers');

it('Create parser defined', () => {
    test.ok(parsers.createParser);
});

it('Parse simple command', () => {
    var parser = parsers.createParser('DISPLAY "HELLO, WORLD".');
    var cmd = parser.parseCommand();
    test.ok(cmd);
    test.equal('runtime.display("HELLO, WORLD");', cmd.compile());
    test.equal(null, parser.parseCommand());
});

it('No point at end', () => {
    var parser = parsers.createParser('DISPLAY "HELLO, WORLD"');
    var cmd = parser.parseCommand();
    test.ok(cmd);
    test.equal('runtime.display("HELLO, WORLD");', cmd.compile());
    test.equal(null, parser.parseCommand());
});

it('Raise if extraneous char at end', () => {
    var parser = parsers.createParser('DISPLAY "HELLO, WORLD"!');

    test.throws(
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

    test.ok(program);
    test.ok(program.identification);
    test.ok(program.identification.program_id);
    test.equal(program.identification.program_id, "HELLO");
});

it('Parse commands', () => {
    var parser = parsers.createParser('display "hello". display "world".');

    var commands = parser.parseCommands();
    test.ok(commands);
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

    test.ok(program);
    test.ok(program.identification);
    test.ok(program.identification.program_id);
    test.equal(program.identification.program_id, "HELLO");
    test.equal(program.identification.author, "A.J.LOPEZ");
    test.equal(program.identification.installation, "TEST");
    test.equal(program.identification.date_written, "2012-12-22");
    test.equal(program.identification.date_compiled, "2012-12-22");
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

    test.ok(program);
    test.ok(program.identification);
    test.ok(program.identification.program_id);
    test.equal(program.identification.program_id, "HELLO");
    test.equal(program.identification.author, "A.J.LOPEZ");
    test.equal(program.identification.installation, "TEST");
    test.equal(program.identification.date_written, "2012-12-22");
    test.equal(program.identification.date_compiled, "2012-12-22");

    test.ok(program.environment);
    test.ok(program.environment.configuration);
    test.equal(program.environment.configuration.source_computer, "NODE");
    test.equal(program.environment.configuration.object_computer, "NODE");
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

    test.ok(program);
    test.ok(program.identification);
    test.ok(program.identification.program_id);
    test.equal(program.identification.program_id, "HELLO");
    test.equal(program.identification.author, "A.J.LOPEZ");
    test.equal(program.identification.installation, "TEST");
    test.equal(program.identification.date_written, "2012-12-22");
    test.equal(program.identification.date_compiled, "2012-12-22");

    test.ok(program.environment);
    test.ok(program.environment.configuration);
    test.equal(program.environment.configuration.source_computer, "NODE");
    test.equal(program.environment.configuration.object_computer, "NODE");

    test.ok(program.data);
    test.ok(program.command);
});

