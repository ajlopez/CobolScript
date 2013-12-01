
var cobs = require('../'),
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

exports['compileTemplate defined'] = function (test) {
    test.ok(cobs.compileTemplate);
};

exports['compile simple text'] = function (test) {
    var text = compile("Hello");
    test.ok(text.indexOf('runtime.write("Hello");') >= 0);
};

exports['simple text with \r \n'] = function (test) {
    var text = compile("Hello\r\nWorld");
    test.ok(text.indexOf('runtime.write("Hello\\r\\nWorld");') >= 0);
};

exports['simple text with quotes'] = function (test) {
    var text = compile("Hello\"World\"");
    test.ok(text.indexOf('runtime.write("Hello\\\"World\\\"");') >= 0);
};

exports['embedded code'] = function (test) {
    var text = compile("<# move 1 to a. #>", { a: null });
    test.ok(text.indexOf("ws.a = 1;") >= 0);
    test.ok(text.indexOf("display") == -1);
};

exports['text and embedded code'] = function (test) {
    var text = compile("Hello <# move 1 to a #> world", { a: null });
    test.ok(text.indexOf("ws.a = 1;") >= 0);
    test.ok(text.indexOf('runtime.write("Hello ");') >= 0);
    test.ok(text.indexOf('runtime.write(" world");') >= 0);
};

exports['text with expression'] = function (test) {
    var text = compile("Hello ${a}", { a: null });
    test.ok(text.indexOf('runtime.write("Hello ", ws.a);') >= 0);
};

exports['text with expression and text'] = function (test) {
    var text = compile("Hello ${a} World", { a: null });
    test.ok(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0);
};

exports['text with expression and text from file'] = function (test) {
    var text = compileFile(path.join(__dirname, '/files/hello.cobt'), { a: null });
    test.ok(text.indexOf('runtime.write("Hello ", ws.a, " World");') >= 0);
};

