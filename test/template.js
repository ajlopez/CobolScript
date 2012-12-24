
var cobs = require('../'),
    assert = require('assert');
    
function compile(text, ws) {
    var code = cobs.compileTemplate(text);
    var program = cobs.compile(code);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
    var text = program.command.compile(program);
    return text;
}

// compileTemplate defined

assert.ok(cobs.compileTemplate);

// compile simple text

var text = compile("Hello");
assert.ok(text.indexOf('runtime.display("Hello");') >= 0);

// compile simple text with \r \n

var text = compile("Hello\r\nWorld");
assert.ok(text.indexOf('runtime.display("Hello\\r\\nWorld");') >= 0);

// compile simple text with quotes

var text = compile("Hello\"World\"");
assert.ok(text.indexOf('runtime.display("Hello\\\"World\\\"");') >= 0);
