
'use strict';

var require;

var cobolscript = (function () {
    var fs = (typeof require === 'function') ? require('fs') : null,
        TokenType = { Integer: 1, String: 2, Name: 3, Punctuation: 4, Operator: 5 },
        punctuations = ".,()",
        operators = [ ">", "<", "=", ">=", "<=", "<>" ],
        verbs = [
            "display",
            "add", "subtract", "multiply", "divide",
            "move", "perform", "compute",
            "if", "else",
            "return",
            "local", "locals", "global", "globals",
            "open", "close", "read", "write",
            "stop", "exit"
        ],
        reserved = [
            "to", "from", "by", "into",
            "no", "with",
            "or", "and",
            "is", "not", "then",
            "async", "asynchronous", "error",
            "advancing",
            "giving", "using",
            "than",
            "until",
            "varying",
            "division", "section",
            "end_if", "end_perform", "end_read"
        ];

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

    function StringExpression(text)
    {
        this.compile = function() {
            return '"' + text + '"';
        };
    }

    function IntegerExpression(text)    {        this.compile = function() {            return text;        };    }

    function BinaryExpression(left, oper, right) {
        if (oper === '=') {
            oper = '==';
        } else if (oper === '<>') {
            oper = '!=';
        }

        this.compile = function(program, context) {
            return left.compile(program, context) + ' ' + oper + ' ' + right.compile(program, context);
        }
    }
    
    function IndexedVariableExpression(name, index)
    {
        var variable = new VariableExpression(name);
        
        this.compile = function(program, context) {
            var code = 'runtime.getIndex(' + variable.compile(program, context);
            code += ', ';
            code += index.compile(program, context);
            code += ')';
            return code;
        }
        
        this.compileSet = function(program, context, value) {
            var code = 'runtime.setIndex(' + variable.compile(program, context);
            code += ', ';
            code += index.compile(program, context);
            code += ', ';
            code += value;
            code += ')';
            return code;
        }
    }        function VariableExpression(name)    {        name = normalizeName(name);

        this.compile = function(program, context) {
            if (name === 'object')
                return '{}';
                
            if (name === 'array')
                return '[]';
                
            if (context && context.arguments && typeof context.arguments[name] !== 'undefined')
                return name;

            if (context && context.locals && typeof context.locals[name] !== 'undefined')
                return name;

            if (program && program.procedures && typeof program.procedures[name] !== 'undefined')
                return name;

            if (program && program.data && program.data.linkage && typeof program.data.linkage[name] !== 'undefined')
                return 'runtime.' + name;

            if (program && program.data && program.data.working_storage)
                return compileWs('ws', program.data.working_storage);
        };

        this.getName = function() { return name; };

        function compileWs(prefix, items) {
            if (typeof items[name] !== 'undefined') {
                return prefix + "." + name;
            }
            
            for (var k in items) {
                if (!items[k]) {
                    continue;
                }
                    
                var newprefix = prefix + "." + k;
                
                var result = compileWs(newprefix, items[k]);
                
                if (result) {
                    return result;
                }
            }
        };
    };

    function QualifiedVariableExpression(names)
    {
        var l = names.length;

        for (var k = 0; k < l; k++) {
            names[k] = normalizeName(names[k]);
        }

        var variable = new VariableExpression(names[0]);

        this.compile = function(program, context) {
            var name = variable.compile(program, context);
            return fullName(name);
        };
        
        function fullName(name) {
            for (var k = 1; k < l; k++) {
                name += '.' + names[k];
            }

            return name;
        };

        this.getName = function() { return fullName(names[0]); };
    };

    function LocalCommand(names)
    {
        var n = names.length;

        this.compile = function(program, context) {
            var code = 'var ';
            
            if (!context.locals) {
                context.locals = {};
            }

            for (var k = 0; k < n; k++) {
                if (k)
                    code += ', ';
                var name = names[k];
                context.locals[name] = null;
                code += name;
            }

            return code + ';';
        };
    };
    
    function GlobalCommand(names)
    {
        var n = names.length;
        this.compile = function(program, context) {
            // TODO quick trick, declare as locals
            if (!context.locals) {
                context.locals = {};
            }

            for (var k = 0; k < n; k++) {
                context.locals[names[k]] = null;
            }

            return '';
        };
    };
    
    function IfCommand(cond, thencommands, elsecommands)
    {
        this.compile = function(program, context) {
            var code = 'if (' + cond.compile(program, context) + ') { ' + thencommands.compile(program, context) + ' }';
            if (elsecommands)
                code += ' else { ' + elsecommands.compile(program, context) + ' }';
            return code;
        };
    };
    
    function ReturnCommand(expr) {
        this.endsWithReturn = true;

        this.compile = function(program, context) {
            var codeexpr;

            if (expr)
                codeexpr = expr.compile(program, context);

            var code;

            if (context.async) {
                code = '$cb(';
                if (expr)
                    code += codeexpr;
                code += '); return';
            }
            else {
                code = 'return';
            
                if (expr)
                    code += ' ' + codeexpr;
            }
                
            code += ';';
            return code;
        };
    };
    
    function ProcedureCommand(name, commands, args, options)
    {
        name = normalizeName(name);
        options = options || { };
        
        this.compile = function(program, context) {
            var code = 'function ' + name + '(';
            code += compileArguments();
            code += ') { ';

            var newcontext = getContext(context);

            if (commands)
                code += commands.compile(program, newcontext);

            if (options.async) {
                if (!(commands && commands.endsWithReturn))
                    code += (new ReturnCommand()).compile(program, newcontext);
            }

            code += ' }';
            return code;
        };
        
        function getContext(context) {
            var opts = context.options || { };
            var newcontext = { arguments: { }, locals: { }, options: opts, async: options.async };

            if (context.locals)
                for (var n in context.locals)
                    newcontext.locals[n] = context.locals[n];
            
            if (args) {
                var n = args.length;
                
                for (var k = 0; k < n; k++)
                    newcontext.arguments[args[k].getName()] = null;
            }
            
            return newcontext;
        };

        function compileArguments() {
            if (!args) {
                if (options.async)
                    return '$cb';
                return '';
            }
                
            var n = args.length;
            var code = '';
            
            for (var k = 0; k < n; k++) {
                if (code) {
                    code += ', ';
                }

                code += args[k].getName();
            }

            if (options.async) {
                if (n)
                    code += ', ';
                code += '$cb';
            }
            
            return code;
        };
    };

    function StopCommand() {
        this.compile = function(program, context) {
            return 'runtime.stop(0);';
        }
    }

    function ExitPerformCommand() {
        this.compile = function(program, context) {
            return 'break;';
        };
    }

    function InlinePerformCommand(commands, options)
    {
        if (options.varying) {
            if (options.from == null) {
                options.from = new IntegerExpression(1);
            }
            if (options.by == null) {
                options.by = new IntegerExpression(1);
            }
        }
        
        this.compile = function(program, context) {
            var code = 'while (';
            if (options.varying) {
                var forvariable = options.varying.compile(program, context);
                var step = options.by.compile(program, context);
                var oper = '<=';
                var incr = '+= ' + step;

                if (step === "1") {
                    incr = '++';
                }
                else if (step === "-1") {
                    incr = '--';
                }

                if (step[0] === '-') {
                    oper = '>=';
                }

                code = 'for (' + forvariable + ' = ' + options.from.compile(program, context) + '; ' + forvariable + ' ' + oper + ' ' + options.to.compile(program, context) + '; ' + forvariable + ' ' + incr + ')';
            }
            else {
                code = 'while (';
                if (options.until && !options.testlast)
                    code += '!(' + options.until.compile(program, context) + ')';
                else
                    code += 'true';
                code += ')';
            }

            code += ' { ' + commands.compile(program, context);

            if (options.until && options.testlast)
                code += ' if (!(' + options.until.compile(program, context) + ')) break;'

            code += ' }';
            return code;
        }
    }
    
    function PerformCommand(variable, options)
    {
        options = options || { };
        
        if (options.varying) {
            if (options.from == null) {
                options.from = new IntegerExpression(1);
            }
            if (options.by == null) {
                options.by = new IntegerExpression(1);
            }
        }
        
        if (options.giving && options.giving instanceof Array && options.giving.length == 1) {
            options.giving = options.giving[0];
        }
        
        this.compile = function(program, context) {
            var name = variable.compile(program, context);
            
            // TODO quick hack to detect undefined
            if (!name || name.slice(0,10) === 'undefined.')
                name = variable.getName();
                
            var code = name + '(';

            if (name.slice(-4) === ".new")
                code = "new " + name.slice(0, name.length-4) + "(";
            
            if (options.async) {
                var callback = getCallbackId(context);
                code += compileArguments(program, context, callback);
                code += '); function ' + callback + '(' + compileGiving(options.witherror) + ') {';
                addGivingAsArguments(context, options.witherror);
            }
            else {
                code += compileArguments(program, context);
                code += ');';
            }

            if (options.varying) {
                var forvariable = options.varying.compile(program, context);
                var step = options.by.compile(program, context);
                var oper = '<=';
                var incr = '+= ' + step;

                if (step === "1") {
                    incr = '++';
                }
                else if (step === "-1") {
                    incr = '--';
                }

                if (step[0] === '-') {
                    oper = '>=';
                }

                code = 'for (' + forvariable + ' = ' + options.from.compile(program, context) + '; ' + forvariable + ' ' + oper + ' ' + options.to.compile(program, context) + '; ' + forvariable + incr + ') ' + code;
            }
            else if (!options.async && options.giving) {
                if (!(options.giving instanceof Array)) {
                    code = options.giving.compile(program, context) + ' = ' + code;
                } else {
                    code = 'var $aux = ' + code;
                    var n = options.giving.length;
                    for (var k = 0; k < n; k++)
                        code += options.giving[k].compile(program, context) + ' = $aux;';
                }
            }
            
            return code;
        };
        
        function compileArguments(program, context, callback)
        {
            if (!options.arguments) {
                if (callback)
                    return callback;
                return '';
            }
                
            var code = '';
            var n = options.arguments.length;
            
            for (var k = 0; k < n; k++) {
                if (code)
                    code += ', ';

                code += options.arguments[k].compile(program, context);
            }

            if (callback) {
                if (n)
                    code += ', ';
                code += callback;
            }
            
            return code;
        }
        
        function compileGiving(witherror)
        {
            if (!options.giving) {
                if (witherror)
                    return 'err';
                return '';
            }
                
            var code = '';

            if (witherror)
                code += 'err';

            if (!(options.giving instanceof Array)) {
                if (code)
                    code += ', ';
                code += options.giving.getName();
                return code;
            }

            var n = options.giving.length;
            
            for (var k = 0; k < n; k++) {
                if (code)
                    code += ', ';
                code += options.giving[k].getName();
            }
            
            return code;
        }
        
        function addGivingAsArguments(context, witherror)
        {
            if (!context.arguments)
                context.arguments = {};

            if (witherror)
                context.arguments['err'] = null;

            if (!options.giving)
                return;

            if (!(options.giving instanceof Array)) {
                context.arguments[options.giving.getName()] = null;
                return;
            }

            var n = options.giving.length;
            
            for (var k = 0; k < n; k++) {
                context.arguments[options.giving[k].getName()] = null;
            }
        }
    }

    function getCallbackId(context) {
        if (!context.ncallbacks)
            context.ncallbacks = 1;
        else
            context.ncallbacks++;

        return '$cb' + context.ncallbacks;
    }        function CompositeCommand()    {        var commands = [];
                this.add = function(cmd) {            commands.push(cmd);
            this.endsWithReturn = cmd.endsWithReturn;        };                this.compile = function(program, context) {
            var initialncb = context.ncallbacks;            var code = '';            var ncmd = commands.length;
            for (var k = 0; k < ncmd; k++) {
                if (k)
                    code += ' ';                code += commands[k].compile(program, context);
            }

            if (context && context.ncallbacks) {
                var n = context.ncallbacks;
                if (initialncb)
                    n -= initialncb;
                for (var k = 0; k < n; k++)
                    code += ' }';
                context.ncallbacks = initialncb;
            }                            return code;        };    };        function DisplayCommand(expr, noadvancing)
    {
        this.compile = function(program, context) {
            if (noadvancing) {
                return 'runtime.write(' + compileExpression(program, context) + ');';
            } 
            else {
                return 'runtime.display(' + compileExpression(program, context) + ');';
            }
        }

        function compileExpression(program, context) {
            if (!(expr instanceof Array)) {
                return expr.compile(program, context);
            }
                
            var n = expr.length;
            var code = '';
            
            for (var k = 0; k < n; k++) {
                if (code) {
                    code += ', ';
                }

                code += expr[k].compile(program, context);
            }
            
            return code;
        };
    };
        function MoveCommand(expr, variable)    {
        var variables;
        
        if (variable instanceof Array) {
            variables = variable;
        }
        else {            variables = [ variable ];
        }                this.compile = function(program, context) {            var code,                n = variables.length;
            var exprcode = expr.compile(program, context);

            if (n == 1)
                if (variables[0].compileSet)
                    return variables[0].compileSet(program, context, exprcode) + ';';
                else
                    return variables[0].compile(program, context) + ' = ' + exprcode + ';';

            code = 'var $aux = ' + exprcode + ';';                        for (var k = 0; k < n; k++)
                if (variables[k].compileSet)
                    code = code + ' ' + variables[k].compileSet(program, context, '$aux') + ';';
                else                    code = code + ' ' + variables[k].compile(program, context) + ' = $aux;';
                            return code;        };    };        function BinaryToCommand(expr, variable, oper, giving, from)    {
        if (expr instanceof Array && expr.length === 1) {
            expr = expr[0];
        }

        if (variable instanceof Array && variable.length === 1) {
            variable = variable[0];
        }

        if (giving instanceof Array && giving.length === 1) {
            giving = giving[0];
        }
            
        this.compile = function(program, context) {
            if (!(variable instanceof Array)) {
                return compileTarget(program, context, variable);
            }

            var code = compileExpression(program, context) + ';';
            
            if (from) {
                code = from.compile(program, context) + ' ' + oper + ' ' + code;
            }
            
            code = 'var $aux = ' + code;
                
            var n = variable.length;
            
            for (var k = 0; k < n; k++) {
                if (!giving) {
                    if (variable[k].compileSet)
                        code += variable[k].compileSet(program, context, variable[k].compile(program, context) + ' ' + oper + ' $aux') + ';'
                    else
                        code += variable[k].compile(program, context) + ' ' + oper + '= $aux;';
                }
                else {
                    if (variable[k].compileSet)
                        code += variable[k].compileSet(program, context, '$aux') + ';';
                    else
                        code += variable[k].compile(program, context) + ' = $aux;';
                }
            }
            return code;        };
        
        function compileTarget(program, context, target) {
            var exprcode = compileExpression(program, context);
            var targetcode = target.compile(program, context);
            var code;

            if (target.compileSet) {
                if (oper === '=')
                    return target.compileSet(program, context, exprcode);
                var suboper = oper[0];
                return target.compileSet(program, context, targetcode + ' ' + suboper + ' ' + exprcode);
            }

            code = targetcode + ' = ';

            if (!giving) {
                code += targetcode + ' ' + oper + ' ';
            }
            else if (from) {
                code += from.compile(program, context) + ' ' + oper + ' ';
            }

            code += exprcode + ';';

            return code;
        };

        function compileExpression(program, context) {
            if (!(expr instanceof Array)) {
                return expr.compile(program, context);
            }
                
            var n = expr.length;
            var code = '(';
            
            for (var k = 0; k < n; k++) {
                if (code) {
                    code += ' + ';
                }

                code += expr[k].compile(program, context);
            }
            
            return code + ')';
        };    };
    
    function Program() {
    };
    
    Program.prototype.compileText = function() {
        return this.command.compile(this, {});
    };
    
    Program.prototype.compileFunction = function() {
        var text = this.compileText();
        return compileFunction(text);
    };
    
    Program.prototype.run = function(runtime) {
        var data = cloneData(this.data);
        this.procedure(runtime, this, data);
        return data;
    };

    function cloneData(data) {
        if (!data)
            return null;

        var newdata = {};

        for (var n in data) {
            newdata[n] = data[n];
        }

        if (newdata.working_storage)
            newdata.working_storage = cloneObject(newdata.working_storage);

        return newdata;
    }

    function cloneObject(obj) {
        if (!obj)
            return null;

        if (obj instanceof Array)
            return obj.slice(0);

        var newobj = {};

        for (var n in obj) {
            var value = obj[n];
            if (typeof value === 'object')
                value = cloneObject(value);
            newobj[n] = value;
        }

        return newobj;
    }
    
    function compileFunction(text) {
        var func = new Function("runtime", "program", "data", "var ws = data ? data.working_storage : null;\r\n" + text);
        return func;
    };
    
    function Parser(text) {
        var lexer = new Lexer(text);
        var self = this;
        
        this.parseProgram = function() {
            var program = new Program();
            
            program.identification = parseIdentificationDivision();
            program.environment = parseEnvironmentDivision();
            program.data = parseDataDivision();
            program.procedures = { };
            program.command = parseProcedureDivision(program.procedures);                        if (!program.command)                program.command = this.parseProcedures(program.procedures);
            
            return program;
        }
        
        this.parseProcedures = function(procs) {
            var procedure = this.parseProcedure(procs);
            
            if (procedure == null)
                return null;
                
            var procedures = null;

            for (var proc = this.parseProcedure(procs); proc != null; proc = this.parseProcedure(procs)) {
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
        
        this.parseProcedure = function(procedures) {
            var token = lexer.nextToken();
            
            if (token == null)
                return null;
            
            if (token.type == TokenType.Name && isVerb(token.value)) {
                lexer.pushToken(token);
                return this.parseCommands();
            }

            // If not a section, only a name/title with point, 
            // then their commands are the continuation of the preceding ones
            if (tryParsePoint())
                return this.parseCommands();

            parseName('section');
            
            var args = null;
            var options = { };
            
            while (true) {
                if (!args && tryParseName("using")) {
                    args = parseVariables();
                    continue;
                }
                if (!options.async && (tryParseName("async") || tryParseName("asynchronous"))) {
                    options.async = true;
                    continue;
                }
                break;
            }

            parsePoint();
            
            var cmds = this.parseCommands();

            procedures[normalizeName(token.value)] = null;
            
            return new ProcedureCommand(token.value, cmds, args, options);
        };                this.parseCommands = function(noend, followers) {
            var command = this.parseCommand(noend);                        if (command == null)                return null;
                            var commands = null;
            
            if (followers && tryPeekFollower(followers))
                return command;

             if (noend && tryParsePoint())
                return command;
                           for (var cmd = this.parseCommand(noend); cmd != null; cmd = this.parseCommand(noend)) {                if (!commands) {                    commands = new CompositeCommand();                    commands.add(command);                }                                commands.add(cmd);

                if (followers && tryPeekFollower(followers))
                    return commands;
                
                if (noend && tryParsePoint())
                    return commands;
            }                        if (commands)                return commands;                        return command;        };
        
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
                var thencommands = this.parseCommands(true, ['else', 'end_if']);
                var elsecommands = null;

                if (tryParseName("else"))
                    elsecommands = this.parseCommands(true, ['end_if']);

                tryParseName('end_if');
                
                if (!noend)
                    parseEndOfCommand();
                    
                return new IfCommand(cond, thencommands, elsecommands);
            }
            
            if (tryParseName("return"))
            {
                var expr = parseSimpleExpression();
                if (!noend)
                    parseEndOfCommand();
                    
                return new ReturnCommand(expr);
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

            if (tryParseName("stop"))
            {
                tryParseName("run");
                var cmd = new StopCommand();
                
                if (!noend)
                    parseEndOfCommand();
                
                return cmd;
            }
            
            if (tryParseName("local") || tryParseName("locals"))
            {
                var names = parseNames();
                var cmd = new LocalCommand(names);

                if (!noend)
                    parseEndOfCommand();

                return cmd;
            }
            
            if (tryParseName("global") || tryParseName("globals")) {
                var names = parseNames();
                var cmd = new GlobalCommand(names);

                if (!noend)
                    parseEndOfCommand();

                return cmd;
            }

            if (tryParseName("exit")) {
                parseName('perform');
                var cmd = new ExitPerformCommand();

                if (!noend)
                    parseEndOfCommand();

                return cmd;
            }
            
            if (tryParseName("perform"))
            {
                var options = { };

                if (tryPeekReservedName()) {
                    var token = lexer.nextToken();

                    // TODO to allow 'perform write in perform', using reserved word in qualified invocation
                    if (tryPeekName("in"))
                        lexer.pushToken(token);
                    else {
                        lexer.pushToken(token);

                        while (true) {
                            if (!options.until && tryParseName("until"))
                                options.until = parseSimpleExpression();
                            else if (options.until && !options.testlast && !options.testfirst && tryParseName("with")) {
                                parseName("test");
                                if (tryParseName("last"))
                                    options.testlast = true;
                                else {
                                    parseName("first");
                                    options.testfirst = true;
                                }
                            }
                            else if (options.until && tryParseName("test")) {
                                if (tryParseName("last"))
                                    options.testlast = true;
                                else {
                                    parseName("first");
                                    options.testfirst = true;
                                }
                            }
                            else if (!options.until && !options.varying && tryParseName("varying")) {
                                options.varying = parseVariable();
                                if (tryParseName("from"))
                                    options.from = parseTerm();
                                if (tryParseName("to"))
                                    options.to = parseTerm();
                                if (tryParseName("by"))
                                    options.by = parseTerm();
                            }
                            else
                                break;
                        }

                        var cmds = this.parseCommands(true, ['end_perform']);

                        tryParseName('end_perform');
                        
                        if (!noend)
                            parseEndOfCommand();

                        return new InlinePerformCommand(cmds, options);
                    }
                }
                  
                var variable = parseVariable();

                while (true) {
                    if (!options.giving && !options.varying && !options.async && tryParseName("varying")) {
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

                    if (!options.varying && !options.giving && tryParseName("giving")) {
                        options.giving = parseVariables();
                        continue;
                    }

                    if (!options.varying && (tryParseName("async") || tryParseName("asynchronous"))) {
                        options.async = true;
                        tryParseName("with");
                        options.witherror = tryParseName("error");
                        continue;
                    }

                    break;
                }

                var cmd = new PerformCommand(variable, options);

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
        
        function parseProcedureDivision(procedures) {
            var token = lexer.nextToken();
            
            if (!token)
                return null;
                
            if (token.type != TokenType.Name || token.value.toLowerCase() != "procedure") {
                lexer.pushToken(token);
                return null;
            }

            parseName("division");
            parsePoint();
            
            var command = self.parseProcedures(procedures);
            
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
                else if (name == "linkage" && kind == "section")
                    parseLinkage(object[name]);
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
        
        function parseLinkage(object)
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
            for (var newlevel = tryGetInteger(); newlevel && newlevel > level; newlevel = tryGetInteger())            {                if (object[name] == null)                    object[name] = { };                lexer.pushToken(new Token(newlevel, TokenType.Integer));                    
                parseItems(object[name], newlevel);            }
                            
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
            var name = getName();
            
            if (tryParse('(', TokenType.Punctuation)) {
                var index = parseSimpleExpression();
                parseToken(')', TokenType.Punctuation);
                return new IndexedVariableExpression(name, index);
            }
            
            if (!tryParseName("in"))
                return new VariableExpression(name);

            var names = [ name ];
            
            for (names.unshift(getName()); tryParseName("in");)
                names.unshift(getName());
                
            return new QualifiedVariableExpression(names);
        };
        
        function parseNames() {
            var names = [ getName() ];
            
            var name;
            
            for (tryParse(',', TokenType.Punctuation); (name = tryGetName()) != null; tryParse(',', TokenType.Punctuation))
                names.push(name);

            return names;
        };
        
        function parseSimpleExpression() {
            var left = parseTerm();
            
            if (left == null)
                return null;
                
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
                
            if (token.type === TokenType.String)
                return new StringExpression(token.value);                            if (token.type === TokenType.Integer)                return new IntegerExpression(token.value);                            if (token.type === TokenType.Name) {
                lexer.pushToken(token);
                return parseVariable();
            }
        };
        
        function parseEndOfCommand() {
            var token = lexer.nextToken();
            
            if (!token)
                return;
            
            if (token.value === "." && token.type === TokenType.Punctuation)
                return;
                
            if (token.type === TokenType.Name && isVerb(token.value)) {
                lexer.pushToken(token);
                return;
            }
                
            throw "unexpected '" + token.value + "'";
        };
        
        function parseToken(value, type)
        {
            if (!tryParse(value, type))
                throw "expected '" + value + "'";              
        }
        
        function parsePoint()
        {
            if (!tryParse('.', TokenType.Punctuation))
                throw "expected '.'";              
        }

        function tryPeekFollower(followers)
        {
            if (!followers || !followers.length)
                return false;

            var token = lexer.peekToken();

            if (token == null)
                return false;

            if (token.type != TokenType.Name)
                return false;

            var name = normalizeName(token.value);

            return followers.indexOf(name) >= 0;
        }

        function tryPeekReservedName()
        {
            var token = lexer.peekToken();

            if (token != null && token.type === TokenType.Name && isReserved(token.value))
                return true;

            return false;
        }

        function tryPeekName(name)
        {
            var token = lexer.peekToken();

            if (token != null && token.type === TokenType.Name && normalizeName(token.value) === name)
                return true;

            return false;
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
                function tryGetName() {            var token = lexer.nextToken();                        if (token == null)                return null;                        if (token.type != TokenType.Name || isReserved(token.value))            {                lexer.pushToken(token);                return null;            }
                        return token.value;        };        
        function tryParse(value, type) {
            var token = lexer.nextToken();
            
            if (token == null || token.type != type || normalizeName(token.value) != value)
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
    function normalizeName(name) {
        if (name.indexOf('_') >= 0)
            return name;
        
        var n = name.length;
        var nlower = 0;
        var nupper = 0;
        
        for (var k = 0; k < n; k++)
            if (isUpperCase(name[k]))
                nupper++;
            else if (isLowerCase(name[k]))
                nlower++;
                
        if (nupper && nlower)
            return name;
                var name = name.toLowerCase();        name = name.replace(/-/g, '_');        return name;    };
    
    function isUpperCase(ch)
    {
        return ch >= 'A' && ch <= 'Z';
    };
    
    function isLowerCase(ch)
    {
        return ch >= 'a' && ch <= 'z';
    };
        function isReserved(name) {
        name = normalizeName(name);
        return reserved.indexOf(name) >= 0 || verbs.indexOf(name) >= 0;
    };
    
    function isVerb(name) {
        name = normalizeName(name);
        return verbs.indexOf(name) >= 0;
    };
    
    function compileTemplateText(text) {
        if (!text)
            return '';
            
        text = text.replace(/\"/g, '\\"');
        text = text.replace(/\n/g, '\\n');
        text = text.replace(/\r/g, '\\r');
        return '"' + text + '"';
    };
    
    function compileTemplateExpression(text) {
        var begin = text.indexOf("${");
        
        if (begin < 0)
            return compileTemplateText(text);
            
        var rest = text.slice(begin + 2);
        var end = rest.indexOf("}");
        
        if (begin >= 0 && end >= 0) {
            var left = text.slice(0, begin);
            var right = rest.slice(end + 1);
            
            var result = '';
            
            if (left)
                result += compileTemplateText(left);

            result += ' ' + rest.slice(0, end);
            
            if (right)
                result += ' ' + compileTemplateExpression(right);
                
            return result;
        }
        
        return compileTemplateText(text);
    };
    
    function compileTemplateToCode(text) {
        var begin = text.indexOf("<#");
        var end = text.indexOf("#>");
        
        if (begin >= 0 && end > begin) {
            var left = text.slice(0, begin);
            var right = text.slice(end + 2);
            
            if (right && right[0] == '\r')
                right = right.slice(1);
            if (right && right[0] == '\n')
                right = right.slice(1);
            
            var result = '';
            
            if (left)
                result += 'display ' + compileTemplateExpression(left) + ' with no advancing';
            
            result += text.slice(begin + 2, end);
            
            if (right)
                result += compileTemplateToCode(right);
                
            return result;
        }
        
        return 'display ' + compileTemplateExpression(text) + ' with no advancing';
    };
    
    function compileProgram(text, ws) {
        var parser = new Parser(text);
        var program = parser.parseProgram();
        
        if (ws) {
            program.data = program.data || { };
            program.data.working_storage = ws;
        }

        program.procedure = program.compileFunction();
            
        return program;
    };
    
    function compileTemplate(text, ws) {
        var code = compileTemplateToCode(text);
        return compileProgram(code, ws);
    };
    
    function Runtime()
    {
        this.buffer = '';
        if (typeof global !== 'undefined')
            this.global = global;
        if (typeof window !== 'undefined')
            this.window = window;
    };
    
    Runtime.prototype.display = function() {
        if (arguments && arguments.length)
            for (var k = 0; k < arguments.length; k++)
                if (arguments[k] != null)
                    this.buffer += arguments[k].toString();
                    
        console.log(this.buffer);
        this.buffer = '';        
    };
    
    Runtime.prototype.write = function() {
        if (arguments && arguments.length)
            for (var k = 0; k < arguments.length; k++)
                if (arguments[k] != null)
                    this.buffer += arguments[k].toString();
    };
    
    Runtime.prototype.flush = function() {
        if (this.buffer)
            console.log(this.buffer);
        this.buffer = '';
    };

    Runtime.prototype.stop = function(value) {
        value = value || 0;
        this.flush();
        process.exit(value);
    };

    function getIndex(obj, index) {
        if (typeof index == 'number')
            return obj[index - 1];
        return obj[index];
    }

    function setIndex(obj, index, value) {
        if (typeof index == 'number')
            obj[index - 1] = value;
        else
            obj[index] = value;
    }

    Runtime.prototype.getIndex = getIndex;
    Runtime.prototype.setIndex = setIndex;
    
    function WebRuntime(request, response)
    {
        this.request = request;
        this.response = response;
        if (typeof global !== 'undefined')
            this.global = global;
        if (typeof window !== 'undefined')
            this.window = window;
    };

    WebRuntime.prototype.display = function() {
        if (this.layout) {
            if (this.body == null)
                this.body = '';
            if (arguments && arguments.length)
                for (var k = 0; k < arguments.length; k++)
                    if (arguments[k] != null)
                        this.body += arguments[k].toString();

            this.body += '\r\n';

            return;
        };

        if (arguments && arguments.length)
            for (var k = 0; k < arguments.length; k++)
                if (arguments[k] != null)
                    this.response.write(arguments[k].toString());

        this.response.write('\r\n');
    };

    WebRuntime.prototype.write = function() {
        if (this.layout) {
            if (this.body == null)
                this.body = '';
            if (arguments && arguments.length)
                for (var k = 0; k < arguments.length; k++)
                    if (arguments[k] != null)
                        this.body += arguments[k].toString();

            return;
        };

        if (arguments && arguments.length)
            for (var k = 0; k < arguments.length; k++)
                if (arguments[k])
                    this.response.write(arguments[k].toString());
    };
    
    WebRuntime.prototype.flush = function() {
        // TODO to be reviewed, other use cases
        if (this.layout) {
            var layout = this.layout;
            delete this.layout;
            layout.run(this);
            return;
        }

        this.response.end();
    };

    WebRuntime.prototype.stop = function(value) {
        this.flush();
    };

    WebRuntime.prototype.getIndex = getIndex;
    WebRuntime.prototype.setIndex = setIndex;
    
    function getRuntime(options) {
        var runtime;

        if (options && options.request && options.response)
            runtime = new WebRuntime(options.request, options.response);
        else
            runtime = new Runtime();

        for (var n in options)
            runtime[n] = options[n];

        return runtime;
    };
    
    return {
        compileProgram: compileProgram,
        compileTemplate: compileTemplate,
        compileProgramFile: function(filename, noprocedure) {
            var text = fs.readFileSync(filename).toString();
            return compileProgram(text, noprocedure);
        },
        compileTemplateFile: function(filename, noprocedure) {
            var text = fs.readFileSync(filename).toString();
            return compileTemplate(text, noprocedure);
        },
        getRuntime: getRuntime,
        complete: function() {
            this.Lexer = Lexer;
            this.TokenType = TokenType;
            this.Parser = Parser;
            return this;
        }
    };
})();

if (typeof window === 'undefined') {
	module.exports = cobolscript;
}
