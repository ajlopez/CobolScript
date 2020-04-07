
'use strict';

var require;

var parsers = require('./parsers');

var cobolscript = (function () {
    var fs = (typeof require === 'function') ? require('fs') : null;
   
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
        var parser = parsers.createParser(text);
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
        getRuntime: getRuntime
    };
})();

module.exports = cobolscript;
