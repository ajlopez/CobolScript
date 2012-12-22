
var cobs = require('../'),
    assert = require('assert');
    
var TokenType = cobs.TokenType;
    
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

var token = lexer.nextToken();
assert.ok(token);
assert.equal('DIVISION', token.value);
assert.equal(TokenType.Name, token.type);

assert.equal(null, lexer.nextToken());

// Get simple name with digits

var lexer = new cobs.Lexer('ITEM01');

var token = lexer.nextToken();
assert.ok(token);
assert.equal('ITEM01', token.value);
assert.equal(TokenType.Name, token.type);

assert.equal(null, lexer.nextToken());

// Get two simple names

var lexer = new cobs.Lexer('IDENTIFICATION DIVISION');

var token = lexer.nextToken();
assert.ok(token);
assert.equal('IDENTIFICATION', token.value);
assert.equal(TokenType.Name, token.type);

token = lexer.nextToken();
assert.ok(token);
assert.equal('DIVISION', token.value);
assert.equal(TokenType.Name, token.type);

assert.equal(null, lexer.nextToken());

// Get name with minus sign

var lexer = new cobs.Lexer('WORKING-STORAGE');

var token = lexer.nextToken();
assert.ok(token);
assert.equal('WORKING-STORAGE', token.value);
assert.equal(TokenType.Name, token.type);

assert.equal(null, lexer.nextToken());
