
var lexers = require('../lib/lexers'),
    assert = require('assert');
    
var TokenType = lexers.TokenType;

function getToken(text, value, type)
{
    var lexer = lexers.createLexer(text);
    assertToken(lexer, value, type);
    assert.equal(lexer.nextToken(), null);
}

function assertToken(lexer, value, type)
{
    var token = lexer.nextToken();
    assert.ok(token);
    assert.equal(token.value, value);
    assert.equal(token.type, type);
}
    
// Lexer defined

assert.ok(lexers.createLexer);

// Null token if null text

var lexer = lexers.createLexer(null);

assert.equal(lexer.nextToken(), null);

// Null token if empty text

var lexer = lexers.createLexer('');

assert.equal(lexer.nextToken(), null);

// Get simple name

var lexer = lexers.createLexer('DIVISION');

assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(lexer.nextToken(), null);

// Get simple name with spaces

var lexer = lexers.createLexer('  DIVISION  ');

assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get simple name with digits

var lexer = lexers.createLexer('ITEM01');

assertToken(lexer, 'ITEM01', TokenType.Name);

assert.equal(lexer.nextToken(), null);

// Get two simple names

var lexer = lexers.createLexer('IDENTIFICATION DIVISION');

assertToken(lexer, 'IDENTIFICATION', TokenType.Name);
assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(lexer.nextToken(), null);

// Get name with minus sign

var lexer = lexers.createLexer('WORKING-STORAGE');

assertToken(lexer, 'WORKING-STORAGE', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get integer number

var lexer = lexers.createLexer('123');

assertToken(lexer, '123', TokenType.Integer);

assert.equal(null, lexer.nextToken());

// Get integer number with leading zeroes

var lexer = lexers.createLexer('003');

assertToken(lexer, '003', TokenType.Integer);

assert.equal(lexer.nextToken(), null);

// Get simple string

getToken('"ADAM"', 'ADAM', TokenType.String);

// Get simple string with quote

getToken('"AD\\\"AM"', 'AD\\\"AM', TokenType.String);

// Raise if unclosed string

var lexer = lexers.createLexer('"ADAM');

assert.throws(
    function() {
        assertToken(lexer, 'ADAM', TokenType.String);
    },
    function(ex) {
        return ex == "unclosed string";
    });

// Raise if unexpected character

var lexer = lexers.createLexer('!');

assert.throws(
    function() {
        assertToken(lexer, '!', TokenType.String);
    },
    function(ex) {
        return ex == "unexpected '!'";
    });

// Get point as Punctuation

getToken('.', '.', TokenType.Punctuation);

// Get comma as Punctuation

getToken(',', ',', TokenType.Punctuation);

// Get parenthesis as Punctuation

getToken('(', '(', TokenType.Punctuation);
getToken(')', ')', TokenType.Punctuation);

// Skip line comment

getToken('* This is a line comment \r\nDIVISION', 'DIVISION', TokenType.Name);

// Skip two line comments

getToken('* This is a line comment \r\n* This is another line comment \r\nDIVISION', 'DIVISION', TokenType.Name);

// Get Phrase

var lexer = lexers.createLexer("HELLO.");

assert.equal(lexer.nextPhrase(), "HELLO");

// Get Phrase with initial spaces and end of line

var lexer = lexers.createLexer("   HELLO.\r\n");

assert.equal(lexer.nextPhrase(), "HELLO");

// Get Phrase with inner points

var lexer = lexers.createLexer("A.J.LOPEZ.\r\n");

assert.equal(lexer.nextPhrase(), "A.J.LOPEZ");

// Get comparison operators

getToken('<','<',TokenType.Operator);
getToken('>','>',TokenType.Operator);
getToken('=','=',TokenType.Operator);
getToken('>=','>=',TokenType.Operator);
getToken('<=','<=',TokenType.Operator);
