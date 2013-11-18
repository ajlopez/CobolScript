
var TokenType = { Integer: 1, String: 2, Name: 3, Punctuation: 4, Operator: 5 };

var punctuations = ".,()";
var operators = [ ">", "<", "=", ">=", "<=", "<>" ];

function Token(value, type) {
    this.value = value;
    this.type = type;
}

function Lexer(text) {
    var position = 0,
        nexts = [];

    function nextChar() {
        return text[position++];
    }

    function pushChar(ch) {
        if (ch) {
            position--;
        }
    }

    function isSpace(ch) {
        if (ch <= ' ') {
            return true;
        }

        return false;
    }

    function isLetter(ch) {
        return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
    }

    function isOperator(ch) {
        return operators.indexOf(ch) >= 0;
    }

    function isDigit(ch) {
        return ch && ch >= '0' && ch <= '9';
    }

    function skipSpaces() {
        while (true) {
            while (position < text.length && isSpace(text[position])) {
                position++;
            }

            if (position < text.length && text[position] === '*') {
                while (position < text.length && text[position] !== '\r' && text[position] !== '\n') {
                    position++;
                }
            } else {
                break;
            }
        }
    }

    function nextFirstChar() {
        if (!text) {
            return null;
        }

        skipSpaces();

        if (position >= text.length) {
            return null;
        }

        return nextChar();
    }

    function nextName(letter) {
        var name = letter,
            ch;

        for (ch = nextChar(); ch && (isLetter(ch) || isDigit(ch) || ch === '-'); ch = nextChar()) {
            name += ch;
        }

        pushChar(ch);

        return new Token(name, TokenType.Name);
    }

    function nextOperator(letter) {
        var ch = nextChar(),
            value = letter + ch;

        if (operators.indexOf(value) >= 0) {
            return new Token(value, TokenType.Operator);
        }

        pushChar(ch);

        return new Token(letter, TokenType.Operator);
    }

    function nextString() {
        var name = '',
            ch;

        for (ch = nextChar(); ch && ch !== '"'; ch = nextChar()) {
            name += ch;
            if (ch === '\\') {
                name += nextChar();
            }
        }

        if (!ch) {
            throw "unclosed string";
        }

        return new Token(name, TokenType.String);
    }

    function nextNumber(digit) {
        var number = digit,
            ch;

        for (ch = nextChar(); ch && isDigit(ch); ch = nextChar()) {
            number += ch;
        }

        pushChar(ch);

        return new Token(number, TokenType.Integer);
    }

    this.nextPhrase = function () {
        var lastchar = null,
            lastposition = null,
            l = text.length,
            initial,
            ch;

        skipSpaces();

        initial = position;

        while (position < l) {
            ch = text[position];

            if (lastchar === '.' && (ch === '\r' || ch === '\n')) {
                return text.slice(initial, lastposition);
            }

            if (!isSpace(ch)) {
                lastchar = ch;
                lastposition = position;
            }

            position++;
        }

        if (lastchar === '.') {
            return text.slice(initial, lastposition);
        }

        throw "unexpected end of input";
    }

    this.nextToken = function () {
        if (nexts.length > 0) {
            return nexts.pop();
        }

        var ch = nextFirstChar();

        if (ch === null) {
            return null;
        }

        if (isLetter(ch)) {
            return nextName(ch);
        }

        if (isDigit(ch)) {
            return nextNumber(ch);
        }

        if (isOperator(ch)) {
            return nextOperator(ch);
        }

        if (ch === '"') {
            return nextString();
        }

        if (punctuations.indexOf(ch) >= 0) {
            return new Token(ch, TokenType.Punctuation);
        }

        throw "unexpected '" + ch + "'";
    }

    this.pushToken = function(token) {
        if (token) {
            nexts.push(token);
        }
    }

    this.peekToken = function () {
        var token = this.nextToken();

        if (token)
            this.pushToken(token);

        return token;
    }
}

function createLexer(text) {
    return new Lexer(text);
}

module.exports = {
    createLexer: createLexer,
    TokenType: TokenType
}

