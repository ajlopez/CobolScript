
var cobs = require('../'),
    assert = require('assert');
    
var TokenType = cobs.TokenType;

function assertToken(lexer, value, type)
{
    var token = lexer.nextToken();
    assert.ok(token);
    assert.equal(value, token.value);
    assert.equal(type, token.type);
}
    
// Lexer defined

assert.ok(cobs.Lexer);

// Null token if null text

var lexer = new cobs.Lexer(null);

assert.equal(null, lexer.nextToken());

// Null token if empty text

var lexer = new cobs.Lexer('');

assert.equal(null, lexer.nextToken());

// Get simple name

var lexer = new cobs.Lexer('DIVISION');

assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get simple name with spaces

var lexer = new cobs.Lexer('  DIVISION  ');

assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get simple name with digits

var lexer = new cobs.Lexer('ITEM01');

assertToken(lexer, 'ITEM01', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get two simple names

var lexer = new cobs.Lexer('IDENTIFICATION DIVISION');

assertToken(lexer, 'IDENTIFICATION', TokenType.Name);
assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get name with minus sign

var lexer = new cobs.Lexer('WORKING-STORAGE');

assertToken(lexer, 'WORKING-STORAGE', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Get integer number

var lexer = new cobs.Lexer('123');

assertToken(lexer, '123', TokenType.Integer);

assert.equal(null, lexer.nextToken());

// Get integer number with leading zeroes

var lexer = new cobs.Lexer('003');

assertToken(lexer, '003', TokenType.Integer);

assert.equal(null, lexer.nextToken());

// Get simple string

var lexer = new cobs.Lexer('"ADAM"');

assertToken(lexer, 'ADAM', TokenType.String);

assert.equal(null, lexer.nextToken());

// Raise if unclosed string

var lexer = new cobs.Lexer('"ADAM');

assert.throws(
    function() {
        assertToken(lexer, 'ADAM', TokenType.String);
    },
    function(ex) {
        return ex == "unclosed string";
    });

// Raise if unexpected character

var lexer = new cobs.Lexer('!');

assert.throws(
    function() {
        assertToken(lexer, '!', TokenType.String);
    },
    function(ex) {
        return ex == "unexpected '!'";
    });

// Get point as Punctuation

var lexer = new cobs.Lexer('.');

assertToken(lexer, '.', TokenType.Punctuation);

assert.equal(null, lexer.nextToken());

// Skip line comment

var lexer = new cobs.Lexer('* This is a line comment \r\nDIVISION');

assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(null, lexer.nextToken());

// Skip two line comments

var lexer = new cobs.Lexer('* This is a line comment \r\n* This is another line comment \r\nDIVISION');

assertToken(lexer, 'DIVISION', TokenType.Name);

assert.equal(null, lexer.nextToken());
