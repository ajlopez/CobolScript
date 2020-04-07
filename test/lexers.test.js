
var lexers = require('../lib/lexers');
    
var TokenType = lexers.TokenType;

function getToken(text, value, type, test)
{
    var lexer = lexers.createLexer(text);
    assertToken(lexer, value, type, test);
    test.equal(lexer.nextToken(), null);
}

function assertToken(lexer, value, type, test)
{
    var token = lexer.nextToken();
    test.ok(token);
    test.equal(token.value, value);
    test.equal(token.type, type);
}
    
it('Lexer defined', () => {
    test.ok(lexers.createLexer);
});

it('Null token if null text', () => {
    var lexer = lexers.createLexer(null);

    test.equal(lexer.nextToken(), null);
});

it('Null token if empty text', () => {
    var lexer = lexers.createLexer('');

    test.equal(lexer.nextToken(), null);
});

it('Get simple name', () => {
    var lexer = lexers.createLexer('DIVISION');

    assertToken(lexer, 'DIVISION', TokenType.Name, test);

    test.equal(lexer.nextToken(), null);
});

it('Get simple name with spaces', () => {
    var lexer = lexers.createLexer('  DIVISION  ');

    assertToken(lexer, 'DIVISION', TokenType.Name, test);

    test.equal(null, lexer.nextToken());
});

it('Get simple name with digits', () => {
    var lexer = lexers.createLexer('ITEM01');

    assertToken(lexer, 'ITEM01', TokenType.Name, test);

    test.equal(lexer.nextToken(), null);
});

it('Get two simple names', () => {
    var lexer = lexers.createLexer('IDENTIFICATION DIVISION');

    assertToken(lexer, 'IDENTIFICATION', TokenType.Name, test);
    assertToken(lexer, 'DIVISION', TokenType.Name, test);

    test.equal(lexer.nextToken(), null);
});

it('Get name with minus sign', () => {
    var lexer = lexers.createLexer('WORKING-STORAGE');

    assertToken(lexer, 'WORKING-STORAGE', TokenType.Name, test);

    test.equal(null, lexer.nextToken());
});

it('Get integer number', () => {
    var lexer = lexers.createLexer('123');

    assertToken(lexer, '123', TokenType.Integer, test);

    test.equal(null, lexer.nextToken());
});

it('Get integer number with leading zeroes', () => {
    var lexer = lexers.createLexer('003');

    assertToken(lexer, '003', TokenType.Integer, test);

    test.equal(lexer.nextToken(), null);
});

it('Get simple string', () => {
    getToken('"ADAM"', 'ADAM', TokenType.String, test);
});

it('Get simple string with quote', () => {
    getToken('"AD\\\"AM"', 'AD\\\"AM', TokenType.String, test);
});

it('Raise if unclosed string', () => {
    var lexer = lexers.createLexer('"ADAM');

    test.throws(
        function() {
            assertToken(lexer, 'ADAM', TokenType.String, test);
        },
        function(ex) {
            return ex == "unclosed string";
        });
});

it('Raise if unexpected character', () => {
    var lexer = lexers.createLexer('!');

    test.throws(
        function() {
            assertToken(lexer, '!', TokenType.String, test);
        },
        function(ex) {
            return ex == "unexpected '!'";
        });
});

it('Get point as Punctuation', () => {
    getToken('.', '.', TokenType.Punctuation, test);
});

it('Get comma as Punctuation', () => {
    getToken(',', ',', TokenType.Punctuation, test);
});

it('Get parenthesis as Punctuation', () => {
    getToken('(', '(', TokenType.Punctuation, test);
    getToken(')', ')', TokenType.Punctuation, test);
});

it('Skip line comment', () => {
    getToken('* This is a line comment \r\nDIVISION', 'DIVISION', TokenType.Name, test);
});

it('Skip two line comments', () => {
    getToken('* This is a line comment \r\n* This is another line comment \r\nDIVISION', 'DIVISION', TokenType.Name, test);
});

it('Get Phrase', () => {
    var lexer = lexers.createLexer("HELLO.");

    test.equal(lexer.nextPhrase(), "HELLO");
});

it('Get Phrase with initial spaces and end of line', () => {
    var lexer = lexers.createLexer("   HELLO.\r\n");

    test.equal(lexer.nextPhrase(), "HELLO");
});

it('Get Phrase with inner points', () => {
    var lexer = lexers.createLexer("A.J.LOPEZ.\r\n");

    test.equal(lexer.nextPhrase(), "A.J.LOPEZ");
});

it('Get comparison operators', () => {
    getToken('<','<',TokenType.Operator, test);
    getToken('>','>',TokenType.Operator, test);
    getToken('=','=',TokenType.Operator, test);
    getToken('>=','>=',TokenType.Operator, test);
    getToken('<=','<=',TokenType.Operator, test);
});

