
var cobs = require('../lib/cobolscript'),
    path = require('path');
    
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

it('compileTemplate defined', () => {
    test.ok(cobs.compileTemplate);
});

it('compile simple text', () => {
    var text = compile("Hello");
    test.ok(text.indexOf('runtime.write("Hello");') >= 0);
});

it('simple text with \r \n', () => {
    var text = compile("Hello\r\nWorld");
    test.ok(text.indexOf('runtime.write("Hello\\r\\nWorld");') >= 0);
});

it('simple text with quotes', () => {
    var text = compile("Hello\"World\"");
    test.ok(text.indexOf('runtime.write("Hello\\\"World\\\"");') >= 0);
});

it('embedded code', () => {
    var text = compile("<# move 1 to a. #>", { a: null });
    test.ok(text.indexOf("ws.a = 1;") >= 0);
    test.ok(text.indexOf("display") == -1);
});

it('text and embedded code', () => {
    var text = compile("Hello <# move 1 to a #> world", { a: null });
    test.ok(text.indexOf("ws.a = 1;") >= 0);
    test.ok(text.indexOf('runtime.write("Hello ");') >= 0);
    test.ok(text.indexOf('runtime.write(" world");') >= 0);
});

it('text with expression', () => {
    var text = compile("Hello ${a}", { a: null });
    test.ok(text.indexOf('runtime.write("Hello ", ws.a);') >= 0);
});

it('text with expression and text', () => {
    var text = compile("Hello ${a} World", { a: null });
    test.ok(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0);
});

it('text with expression and text from file', () => {
    var text = compileFile(path.join(__dirname, '/files/hello.cobt'), { a: null });
    test.ok(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0);
});

