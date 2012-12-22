
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
