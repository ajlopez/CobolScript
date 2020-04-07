
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
    expect(!!cobs.compileTemplate).toBe(true);
});

it('compile simple text', () => {
    var text = compile("Hello");
    expect(text.indexOf('runtime.write("Hello");') >= 0).toBe(true);
});

it('simple text with \r \n', () => {
    var text = compile("Hello\r\nWorld");
    expect(text.indexOf('runtime.write("Hello\\r\\nWorld");') >= 0).toBe(true);
});

it('simple text with quotes', () => {
    var text = compile("Hello\"World\"");
    expect(text.indexOf('runtime.write("Hello\\\"World\\\"");') >= 0).toBe(true);
});

it('embedded code', () => {
    var text = compile("<# move 1 to a. #>", { a: null });
    expect(text.indexOf("ws.a = 1;") >= 0).toBe(true);
    expect((text.indexOf("display") == -1)).toBe(true);
});

it('text and embedded code', () => {
    var text = compile("Hello <# move 1 to a #> world", { a: null });
    expect(text.indexOf("ws.a = 1;") >= 0).toBe(true);
    expect(text.indexOf('runtime.write("Hello ");') >= 0).toBe(true);
    expect(text.indexOf('runtime.write(" world");') >= 0).toBe(true);
});

it('text with expression', () => {
    var text = compile("Hello ${a}", { a: null });
    expect(text.indexOf('runtime.write("Hello ", ws.a);') >= 0).toBe(true);
});

it('text with expression and text', () => {
    var text = compile("Hello ${a} World", { a: null });
    expect(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0).toBe(true);
});

it('text with expression and text from file', () => {
    var text = compileFile(path.join(__dirname, '/files/hello.cobt'), { a: null });
    expect(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0).toBe(true);
});

