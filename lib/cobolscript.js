
var cobolscript = (function() {
    var TokenType = { Integer: 1, String: 2, Name: 3, Punctuation: 4 };
    
    var punctuations = ".";
    
    function Token(value, type)
    {
        this.value = value;
        this.type = type;
    };

    function Lexer(text) {
        var position = 0;
        var nexts = [];
        
        this.nextToken = function() {
            if (nexts.length > 0)
                return nexts.pop();

            var ch = nextFirstChar();
            
            if (ch == null)
                return null;
            
            if (isLetter(ch))
                return nextName(ch);
            
            if (isDigit(ch))
                return nextNumber(ch);
                
            if (ch == '"')
                return nextString();
                
            if (punctuations.indexOf(ch) >= 0)
                return new Token(ch, TokenType.Punctuation);
            
            throw "unexpected '" + ch + "'";
        }
        
        this.nextPhrase = function() {
            var lastchar = null;
            var lastposition = null;
            var l = text.length;

            skipSpaces();

            var initial = position;

            while (position < l) {
                var ch = text[position];

                if (lastchar == '.' && (ch == '\r' || ch == '\n'))
                    return text.slice(initial, lastposition);

                if (!isSpace(ch)) {
                    lastchar = ch;
                    lastposition = position;
                }

                position++;
            }
            
            if (lastchar == '.')
                return text.slice(initial, lastposition);
            
            throw "unexpected end of input";
        }
        
        this.pushToken = function(token) {
            if (token)
                nexts.push(token);
        }

        function nextName(letter)
        {
            var name = letter;
            
            for (var ch = nextChar(); ch && (isLetter(ch) || isDigit(ch) || ch == '-'); ch = nextChar())
                name += ch;

            pushChar(ch);
            
            return new Token(name, TokenType.Name);
        }

        function nextString()
        {
            var name = '';
            
            for (var ch = nextChar(); ch && ch != '"'; ch = nextChar())
                name += ch;
            
            if (!ch)
                throw "unclosed string";
                
            return new Token(name, TokenType.String);
        }
        
        function nextNumber(digit)
        {
            var number = digit;
            
            for (var ch = nextChar(); ch && isDigit(ch); ch = nextChar())
                number += ch;
                
            pushChar(ch);
                
            return new Token(number, TokenType.Integer);
        }

        function nextFirstChar() {
            if (!text)
                return null;
                
            skipSpaces();
            
            if (position >= text.length)
                return null;
                
            return nextChar();
        }
        
        function nextChar() {
            return text[position++];
        }

        function pushChar(ch) {
            if (ch)
                position--;
        }
        
        function skipSpaces() {
            while (true) {
                while (position < text.length && isSpace(text[position]))
                    position++;
                    
                if (position < text.length && text[position] == '*') {
                    while (position < text.length && text[position] != '\r' && text[position] != '\n')
                        position++;
                }
                else
                    break;
            }
        }
        
        function isSpace(ch) {
            if (ch <= ' ')
                return true;
                
            return false;
        }

        function isLetter(ch) {
            return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
        }
        
        function isDigit(ch) {
            return ch && ch >= '0' && ch <= '9';
        }
    };
    
    function StringExpression(text)
    {
        this.compile = function() {
            return '"' + text + '"';
        };
    }
    
    function VariableExpression(name)    {        name = normalizeName(name);                this.compile = function(program) {            if (program && program.data && program.data.working_storage)                return compileWs('ws', program.data.working_storage);        };                function compileWs(prefix, items) {            if (typeof(items[name]) != 'undefined')                return prefix + "." + name;                        for (var k in items) {                if (!items[k].items)                    continue;                                    var newprefix = prefix + "." + k + ".items";                                var result = compileWs(newprefix, items[k].items);                                if (result)                    return result;            }        };    }        function DisplayCommand(expr)
    {
        this.compile = function(program) {
            return 'runtime.display(' + expr.compile(program) + ');';
        }
    }
    
    function Parser(text) {
        var lexer = new Lexer(text);
        var self = this;
        
        this.parseProgram = function() {
            var program = { };
            
            program.identification = parseIdentificationDivision();
            program.environment = parseEnvironmentDivision();
            program.data = parseDataDivision();
            program.command = parseProcedureDivision();                        if (!program.command)                program.command = this.parseCommand();
            
            return program;
        }
        
        this.parseCommand = function() {
            if (tryParseName("display"))
            {
                var expr = parseExpression();
                
                parseEndOfCommand();
                
                return new DisplayCommand(expr);
            }
        };
        
        function parseIdentificationDivision() {
            return parseParagraph("identification", "division", ["environment", "data", "procedure"]);
        };
        
        function parseEnvironmentDivision() {
            return parseParagraph("environment", "division", ["data", "procedure"], "section");
        };
        
        function parseDataDivision() {
            return parseParagraph("data", "division", ["procedure"], "section");
        };
        
        function parseProcedureDivision() {
            var token = lexer.nextToken();
            
            if (!token)
                return null;
                
            if (token.type != TokenType.Name || token.value.toLowerCase() != "procedure") {
                lexer.pushToken(token);
                return null;
            }

            parseName("division");
            parsePoint();
            
            var command = self.parseCommand();
            
            if (!command)
                return null;
                
            return command;
        };
        
        function parseParagraph(name, kind, followers, child) {            var object = { };
            if (!tryParseName(name))
                return null;
            parseName(kind);
            parsePoint();

            if (child)
                parseChildren(object, child, followers);
            else
                parseAttributes(object, followers);
            
            return object;
        }
        
        function parseChildren(object, kind, followers) {
            for (var token = lexer.nextToken(); token != null && token.type == TokenType.Name && followers.indexOf(token.value.toLowerCase()) == -1; token = lexer.nextToken())
            {
                var name = normalizeName(token.value);
                parseName(kind);
                parsePoint();
                
                object[name] = { };
                
                if (name == "working_storage" && kind == "section")
                    parseWorkingStorage(object[name]);
                else                
                    parseAttributes(object[name], followers);
            }
            
            if (token)
                lexer.pushToken(token);
        }
        
        function parseAttributes(object, followers) {
            for (var token = lexer.nextToken(); token != null && token.type == TokenType.Name && followers.indexOf(token.value.toLowerCase()) == -1; token = lexer.nextToken())
            {
                var name = normalizeName(token.value);
                parsePoint();
                
                var value = lexer.nextPhrase();
                object[name] = value;
            }
            
            if (token)
                lexer.pushToken(token);
        };
        
        function parseWorkingStorage(object)
        {        
            parseItems(object, "01");
        }
        
        function parseItems(object, level)
        {            while (tryParse(level, TokenType.Integer))
                parseItem(object, level);
        }
        
        function parseItem(object, level)
        {
            var name = normalizeName(getName());
            parsePoint();
            object[name] = null;            
            for (var newlevel = tryGetInteger(); newlevel && newlevel > level; newlevel = tryGetInteger())            {                if (object[name] == null)                    object[name] = { items: { } };                lexer.pushToken(new Token(newlevel, TokenType.Integer));                    
                parseItems(object[name].items, newlevel);            }
                            
            if (newlevel != null)
                lexer.pushToken(new Token(newlevel, TokenType.Integer));
        }
        
        function parseExpression(program) {
            var token = lexer.nextToken();
            
            if (!token)
                return null;
                
            if (token.type == TokenType.String)
                return new StringExpression(token.value);                            if (token.type == TokenType.Name)                return new VariableExpression(token.value);
        };
        
        function parseEndOfCommand() {
            var token = lexer.nextToken();
            
            if (!token)
                throw "unexpected end of input";
            
            if (token.value == "." && token.type == TokenType.Punctuation)
                return;
                
            throw "unexpected '" + token.value + "'";
        };
        
        function parsePoint()
        {
            if (!tryParse('.', TokenType.Punctuation))
                throw "expected '.'";              
        }
        
        function parseName(name) {
            if (!tryParseName(name))
                throw "expected '" + name + "'";
        }
        
        function tryParseName(name) {
            return tryParse(name, TokenType.Name);
            
            return true;
        };
        
        function tryGetInteger() {
            var token = lexer.nextToken();                        if (token == null)                return null;
            
            if (token.type != TokenType.Integer)
            {
                lexer.pushToken(token);
                return null;
            }
            
            return token.value;
        };
                function tryGetName() {            var token = lexer.nextToken();                        if (token == null)                return null;                        if (token.type != TokenType.Integer)            {                lexer.pushToken(token);                return null;            }                        return token.value;        };        
        function tryParse(value, type) {
            var token = lexer.nextToken();
            
            if (token == null || token.type != type || token.value.toLowerCase() != value)
            {
                lexer.pushToken(token);
                return false;
            }
            
            return true;
        };
        
        function getName() {
            var token = lexer.nextToken();
            
            if (token == null)
                throw "unexpected end of input";
            
            if (token.type != TokenType.Name)
            {
                lexer.pushToken(token);
                throw "unexpected '" + token.value + "'";
            }
            
            return token.value;
        };
        
    };
    function normalizeName(name) {        var name = name.toLowerCase();        name = name.replace(/-/g, '_');        return name;    };            
    return {
        Lexer: Lexer,
        TokenType: TokenType,
        Parser: Parser,
        run: function(text, runtime, program) {
            var func = new Function("runtime", "program", "var ws = (program && program.data) ? program.data.working_storage : null;\r\n" + text);
            return func(runtime, program);
        },        compile: function(text) {            var parser = new Parser(text);            return parser.parseProgram();        }
    };
})();

if (typeof(window) === 'undefined') {
	module.exports = cobolscript;
}
