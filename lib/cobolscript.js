
var cobolscript = (function() {
    var TokenType = { Integer: 1, String: 2, Name: 3, Punctuation: 4, Operator: 5 };
    
    var punctuations = ".,";
    var operators = [ ">", "<", "=", ">=", "<=", "<>" ];
    
    var verbs = [
        "display", 
        "add", "subtract", "multiply", "divide", 
        "move", "perform", "compute",
        "if", "else",
        "open", "close", "read", "write"
    ];
    
    var reserved = [
        "to", "from", "by", "into", 
        "no", "with",
        "or", "and",
        "is", "not", "then",
        "advancing",
        "giving", "using", 
        "than",
        "varying",
        "division", "section", 
        "end-if", "end-perform", "end-read"
    ];
    
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
                
            if (isOperator(ch))
                return nextOperator(ch);
                
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
        
        function nextOperator(letter)
        {
            var ch = nextChar();
            var value = letter + ch;
            
            if (operators.indexOf(value) >= 0)
                return new Token(value, TokenType.Operator);
            
            pushChar(ch);
            
            return new Token(letter, TokenType.Operator);
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
        
        function isOperator(ch) {
            return operators.indexOf(ch) >= 0;
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
    
    function IntegerExpression(text)    {        this.compile = function() {            return text;        };    }
    
    function BinaryExpression(left, oper, right) {
        this.compile = function(program, context) {
            return left.compile(program, context) + ' ' + oper + ' ' + right.compile(program.context);
        }
    }        function VariableExpression(name)    {        name = normalizeName(name);
        
        this.compile = function(program, context) {
            if (context && context.arguments && typeof(context.arguments[name]) != 'undefined')
                return name;
                
            if (context && context.locals && typeof(context.locals[name]) != 'undefined')
                return name;

            if (program && program.data && program.data.working_storage)
                return compileWs('ws', program.data.working_storage);
        };

        this.getName = function() { return name; };
        
        function compileWs(prefix, items) {
            if (typeof(items[name]) != 'undefined')
                return prefix + "." + name;
            
            for (var k in items) {
                if (!items[k])
                    continue;
                if (!items[k].items)
                    continue;
                    
                var newprefix = prefix + "." + k + ".items";
                
                var result = compileWs(newprefix, items[k].items);
                
                if (result)
                    return result;
            }
        };
    };
    
    function IfCommand(cond, commands)
    {
        this.compile = function(program, context) {
            var code = 'if (' + cond.compile(program, context) + ') {' + commands.compile(program, context) + '}';
            return code;
        };
    }
    
    function ProcedureCommand(name, commands, args)
    {
        this.compile = function(program, context) {
            var code = 'function ' + name + '(';
            code += compileArguments();
            code += ') {';
            if (commands)
                code += commands.compile(program, getContext());
            code += '};';
            return code;
        };
        
        function getContext() {
            var context = { arguments: { }, locals: { } };
            
            if (!args)
                return context;            
            
            var n = args.length;
            
            for (var k = 0; k < n; k++)
                context.arguments[args[k].getName()] = null;
            
            return context;
        };

        function compileArguments() {
            if (!args)
                return '';
                
            var n = args.length;
            var code = '';
            
            for (var k = 0; k < n; k++) {
                if (code)
                    code += ', ';
                code += args[k].getName();
            }
            
            return code;
        };
    };
    
    function PerformCommand(name, options)
    {
        options = options || { };
        
        if (options.varying) {
            if (options.from == null)
                options.from = new IntegerExpression(1);
            if (options.by == null)
                options.by = new IntegerExpression(1);
        }
        
        this.compile = function(program, context) {
            var code = name + '('
            code += compileArguments(program, context);
            code += ');';
            
            if (options.varying) {
                var variable = options.varying.compile(program, context);
                var step = options.by.compile(program, context);
                var oper = '<=';
                var incr = '+= ' + step;

                if (step == "1")
                    incr = '++';
                else if (step == "-1")
                    incr = '--';

                if (step[0] == '-')
                    oper = '>=';

                code = 'for (' + variable + ' = ' + options.from.compile(program, context) + '; ' + variable + ' ' + oper + ' ' + options.to.compile(program) + '; ' + variable + incr + ') ' + code;
            }
            
            return code;
        };
        
        function compileArguments(program, context)
        {
            if (!options.arguments)
                return '';
                
            var code = '';
            var n = options.arguments.length;
            
            for (var k = 0; k < n; k++) {
                if (code)
                    code += ', ';
                code += options.arguments[k].compile(program, context);
            }
            
            return code;
        }
    }        function CompositeCommand()    {        var commands = [];                this.add = function(cmd) {            commands.push(cmd);        };                this.compile = function(program, context) {            var code = '';            var ncmd = commands.length;            for (var k = 0; k < ncmd; k++)                code += commands[k].compile(program, context) + '\r\n';                            return code;        };    };        function DisplayCommand(expr, noadvancing)
    {
        this.compile = function(program, context) {
            if (noadvancing)
                return 'runtime.write(' + compileExpression(program, context) + ');';
            else
                return 'runtime.display(' + compileExpression(program, context) + ');';
        }

        function compileExpression(program, context) {
            if (!(expr instanceof Array))
                return expr.compile(program, context);
                
            var n = expr.length;
            var code = '';
            
            for (var k = 0; k < n; k++) {
                if (code)
                    code += ', ';
                code += expr[k].compile(program, context);
            }
            
            return code;
        };
    };
        function MoveCommand(expr, variable)    {
        var variables;
        
        if (variable instanceof Array)
            variables = variable;
        else            variables = [ variable ];                this.compile = function(program, context) {            var code = '';            var n = variables.length;                        for (var k = 0; k < n; k++)                code = code + variables[k].compile(program, context) + ' = ';                            code += expr.compile(program, context) + ';';                            return code;        };    };        function BinaryToCommand(expr, variable, oper, giving, from)    {
        if (expr instanceof Array && expr.length == 1)
            expr = expr[0];

        if (variable instanceof Array && variable.length == 1)
            variable = variable[0];

        if (giving instanceof Array && giving.length == 1)
            giving = giving[0];
            
        this.compile = function(program, context) {
            if (!(variable instanceof Array))
                return compileTarget(program, context, variable);
                
            var n = variable.length;
            code = '';
            
            for (var k = 0; k < n; k++)
                code += compileTarget(program, context, variable[k]);
            return code;        };
        
        function compileTarget(program, context, target) {
            var code = target.compile(program, context) + ' = ';

            if (!giving)
                code += target.compile(program, context) + ' ' + oper + ' ';
            else if (from)
                code += from.compile(program, context) + ' ' + oper + ' ';

            code += compileExpression(program, context) + ';';

            return code;
        };

        function compileExpression(program, context) {
            if (!(expr instanceof Array))
                return expr.compile(program, context);
                
            var n = expr.length;
            var code = '(';
            
            for (var k = 0; k < n; k++) {
                if (code)
                    code += ' + ';
                code += expr[k].compile(program, context);
            }
            
            return code + ')';
        };    }    
    function Parser(text) {
        var lexer = new Lexer(text);
        var self = this;
        
        this.parseProgram = function() {
            var program = { };
            
            program.identification = parseIdentificationDivision();
            program.environment = parseEnvironmentDivision();
            program.data = parseDataDivision();
            program.command = parseProcedureDivision();                        if (!program.command)                program.command = this.parseProcedures();
            
            return program;
        }
        
        this.parseProcedures = function() {
            var procedure = this.parseProcedure();
            
            if (procedure == null)
                return null;
                
            var procedures = null;

            for (var proc = this.parseProcedure(); proc != null; proc = this.parseProcedure()) {
                if (!procedures) {
                    procedures = new CompositeCommand();
                    procedures.add(procedure);
                }
                
                procedures.add(proc);
            }
            
            if (procedures)
                return procedures;
            
            return procedure;
        };
        
        this.parseProcedure = function() {
            var token = lexer.nextToken();
            
            if (token == null)
                return null;
            
            if (token.type == TokenType.Name && isVerb(token.value)) {
                lexer.pushToken(token);
                return this.parseCommands();
            }
            
            var arguments = null;
            
            if (tryParseName("using"))
                arguments = parseVariables();
            
            parsePoint();
            
            var cmds = this.parseCommands();
            
            return new ProcedureCommand(token.value, cmds, arguments);
        };                this.parseCommands = function(noend) {
            var command = this.parseCommand(noend);                        if (command == null)                return null;
                            var commands = null;
            
            if (noend && tryParsePoint())
                return command;
                            for (var cmd = this.parseCommand(noend); cmd != null; cmd = this.parseCommand(noend)) {                if (!commands) {                    commands = new CompositeCommand();                    commands.add(command);                }                                commands.add(cmd);
                
                if (noend && tryParsePoint())
                    return commands;            }                        if (commands)                return commands;                        return command;        };
        
        this.parseCommand = function(noend) {
            if (tryParseName("display"))
            {
                var expr = parseTerms();
                
                var withwith = tryParseName("with");
                var noadvancing = tryParseName("no");
                
                if (withwith || noadvancing)
                    parseName("advancing");
                else
                    tryParseName("advancing");
                
                if (!noend)
                    parseEndOfCommand();
                
                return new DisplayCommand(expr, noadvancing);
            }
            
            if (tryParseName("if"))
            {
                var cond = parseSimpleExpression();
                parseName('then');
                var commands = this.parseCommands(true);
                
                return new IfCommand(cond, commands);
            }                        if (tryParseName("add"))            {                var exprs = parseTerms();
                var giving = false;
                
                if (tryParseName("giving"))
                    giving = true;
                else                    parseName("to");
                                    var variable = parseVariables();                var cmd = new BinaryToCommand(exprs, variable, '+', giving);                                if (!noend)
                    parseEndOfCommand();
                                return cmd;            }                        if (tryParseName("subtract"))            {                var exprs = parseTerms();                parseName("from");                var variable = parseVariables();
                var cmd;
                
                if (variable.length == 1 && tryParseName("giving"))
                    cmd = new BinaryToCommand(exprs, parseVariables(), '-', true, variable[0]);
                else                    cmd = new BinaryToCommand(exprs, variable, '-', false);                                if (!noend)
                    parseEndOfCommand();
                                return cmd;            }                        if (tryParseName("multiply"))            {                var expr = parseTerm();                parseName("by");                var variable = new VariableExpression(getName());                var cmd = new BinaryToCommand(expr, variable, '*');                                if (!noend)
                    parseEndOfCommand();
                                return cmd;            }                        if (tryParseName("divide"))            {                var expr = parseTerm();                parseName("into");                var variable = new VariableExpression(getName());                var cmd = new BinaryToCommand(expr, variable, '/');                                if (!noend)
                    parseEndOfCommand();
                                return cmd;            }                        if (tryParseName("move"))            {
                var expr = parseTerm();                parseName("to");                var variables = parseVariables();                var cmd = new MoveCommand(expr, variables);                                if (!noend)
                    parseEndOfCommand();
                                return cmd;            }
            
            if (tryParseName("perform"))
            {
                var name = getName();
                var options = { };
                
                while (true) {
                    if (!options.varying && tryParseName("varying")) {
                        options.varying = parseVariable();
                        if (tryParseName("from"))
                            options.from = parseTerm();
                        if (tryParseName("to"))
                            options.to = parseTerm();
                        if (tryParseName("by"))
                            options.by = parseTerm();
                        continue;
                    }
                    
                    if (!options.arguments && tryParseName("using")) {
                        options.arguments = parseTerms();
                        continue;
                    }
                    
                    break;
                }
                var cmd = new PerformCommand(name, options);

                if (!noend)
                    parseEndOfCommand();

                return cmd;
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
        };
        
        function parseVariables() {
            var list = [];
            list.push(parseVariable());
            
            while (true) {
                while (tryParse(",", TokenType.Punctuation))
                    list.push(parseVariable());

                var token = lexer.nextToken();
                if (token == null)
                    break;
                lexer.pushToken(token);

                if (token.type != TokenType.Name || (token.type == TokenType.Name && isReserved(token.value)))
                    break;
                
                list.push(parseVariable());
            }
            
            return list;
        };
        
        function parseVariable() {
            return new VariableExpression(getName());
        };
        
        function parseSimpleExpression() {
            var left = parseTerm();
                
            var oper = tryGetOperator();
            
            if (!oper)
                return left;
            
            var right = parseTerm();
            
            return new BinaryExpression(left, oper, right);
        };
        
        function parseTerms() {
            var list = [];
            list.push(parseTerm());
        
            while (true) {
                while (tryParse(",", TokenType.Punctuation))
                    list.push(parseTerm());

                var token = lexer.nextToken();
                if (token == null)
                    break;
                lexer.pushToken(token);

                if (token.type == TokenType.Punctuation || (token.type == TokenType.Name && isReserved(token.value)))
                    break;
                
                list.push(parseTerm());
            }
                
            return list;
        };
        
        function parseTerm() {
            var token = lexer.nextToken();
            
            if (!token)
                return null;
                
            if (token.type == TokenType.String)
                return new StringExpression(token.value);                            if (token.type == TokenType.Integer)                return new IntegerExpression(token.value);                            if (token.type == TokenType.Name)                return new VariableExpression(token.value);
        };
        
        function parseEndOfCommand() {
            var token = lexer.nextToken();
            
            if (!token)
                return;
            
            if (token.value == "." && token.type == TokenType.Punctuation)
                return;
                
            if (token.type == TokenType.Name && isVerb(token.value)) {
                lexer.pushToken(token);
                return;
            }
                
            throw "unexpected '" + token.value + "'";
        };
        
        function parsePoint()
        {
            if (!tryParse('.', TokenType.Punctuation))
                throw "expected '.'";              
        }
        
        function tryParsePoint()
        {
            return tryParse('.', TokenType.Punctuation);
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
        
        function tryGetOperator() {
            var token = lexer.nextToken();
            
            if (token == null)
                return null;
            
            if (token.type != TokenType.Operator)
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
        function isReserved(name) {
        name = name.toLowerCase();
        return reserved.indexOf(name) >= 0 || verbs.indexOf(name) >= 0;
    };
    
    function isVerb(name) {
        name = name.toLowerCase();
        return verbs.indexOf(name) >= 0;
    };
    
    return {
        Lexer: Lexer,
        TokenType: TokenType,
        Parser: Parser,
        run: function(text, runtime, program) {
            var func = new Function("runtime", "program", "var ws = (program && program.data) ? program.data.working_storage : null; var aux = {};\r\n" + text);
            return func(runtime, program);
        },        compile: function(text) {            var parser = new Parser(text);            return parser.parseProgram();        }
    };
})();

if (typeof(window) === 'undefined') {
	module.exports = cobolscript;
}
