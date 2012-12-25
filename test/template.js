
var cobs = require('../'),
    path = require('path'),
    assert = require('assert');
    
function compile(text, ws) {
    var program = cobs.compileTemplate(text, true);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }

    return program.compileText();
}

function compileFile(filename, ws) {
    var program = cobs.compileTemplateFile(filename, true);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
    return program.compileText();
}

// compileTemplate defined

assert.ok(cobs.compileTemplate);

// compile simple text

var text = compile("Hello");
assert.ok(text.indexOf('runtime.write("Hello");') >= 0);

// simple text with \r \n

var text = compile("Hello\r\nWorld");
assert.ok(text.indexOf('runtime.write("Hello\\r\\nWorld");') >= 0);

// simple text with quotes

var text = compile("Hello\"World\"");
assert.ok(text.indexOf('runtime.write("Hello\\\"World\\\"");') >= 0);

// embedded code

var text = compile("<# move 1 to a. #>", { a: null });
assert.ok(text.indexOf("ws.a = 1;") >= 0);
assert.ok(text.indexOf("display") == -1);

// text and embedded code

var text = compile("Hello <# move 1 to a #> world", { a: null });
assert.ok(text.indexOf("ws.a = 1;") >= 0);
assert.ok(text.indexOf('runtime.write("Hello ");') >= 0);
assert.ok(text.indexOf('runtime.write(" world");') >= 0);

// text with expression

var text = compile("Hello ${a}", { a: null });
assert.ok(text.indexOf('runtime.write("Hello ", ws.a);') >= 0);

// text with expression and text

var text = compile("Hello ${a} World", { a: null });
assert.ok(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0);

// text with expression and text from file

var text = compileFile(path.join(__dirname, '/files/hello.cobt'), { a: null });
assert.ok(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0);
