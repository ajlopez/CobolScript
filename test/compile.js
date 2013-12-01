
var cobs = require('../'),
    path = require('path');
    
function compile(code, ws) {
    var program = cobs.compileProgram(code, ws);
    return program.compileText();
}
    
function compileFile(filename, ws) {
    var program = cobs.compileProgramFile(filename, true);

    if (ws) {
        program.data = program.data || { };
        program.data.working_storage = ws;
    }
    
    return program.compileText();
}

exports['compile program defined'] = function (test) {
    test.ok(cobs.compileProgram);
}

exports['simple compile display'] = function (test) {
    var text = compile('display "hello".');
    test.ok(text);
    test.ok(text.indexOf('runtime.display("hello");') >= 0);
}

exports['simple compile display from file'] = function (test) {
    var text = compileFile(path.join(__dirname, '/files/hello.cob'));
    test.ok(text);
    test.ok(text.indexOf('runtime.display("hello");') >= 0);
}

exports['display two values'] = function (test) {
    var text = compile('display "hello" "world".');
    test.ok(text);
    test.ok(text.indexOf('runtime.display("hello", "world");') >= 0);
}

exports['display with advancing'] = function (test) {
    var text = compile('display "hello" "world" with advancing.');
    test.ok(text);
    test.ok(text.indexOf('runtime.display("hello", "world");') >= 0);
}

exports['display with no advancing'] = function (test) {
    var text = compile('display "hello" "world" with no advancing.');
    test.ok(text);
    test.ok(text.indexOf('runtime.write("hello", "world");') >= 0);
}

exports['display no advancing'] = function (test) {
    var text = compile('display "hello" "world" no advancing.');
    test.ok(text);
    test.ok(text.indexOf('runtime.write("hello", "world");') >= 0);
}

exports['display two values comma separated'] = function (test) {
    var text = compile('display "hello", "world".');
    test.ok(text);
    test.ok(text.indexOf('runtime.display("hello", "world");') >= 0);
}

exports['simple compile move'] = function (test) {
    var text = compile('move 1 to a-1.', {a_1: null});
    test.ok(text);
    test.ok(text.indexOf('ws.a_1 = 1;') >= 0);
}

exports['compile simple variable'] = function (test) {
    text = compile('display A.', { a: null });
    test.ok(text);
    test.ok(text.indexOf('runtime.display(ws.a);') >= 0);
}

exports['compile simple variable in program'] = function (test) {
    var text = compile('\
data division.\r\n\
working-storage section.\r\n\
01 a.\r\n\
procedure division.\r\n\
display a.\r\n\
');

    test.ok(text);
    test.ok(text.indexOf('runtime.display(ws.a);') >= 0);
}

exports['compile nested variable in program'] = function (test) {
    var text = compile('\
data division.\r\n\
working-storage section.\r\n\
01 a.\r\n\
02 b.\r\n\
procedure division.\r\n\
display b.\r\n\
');
    test.ok(text);
    test.ok(text.indexOf('runtime.display(ws.a.b);') >= 0);
}

exports['simple compile two move commands'] = function (test) {
    var text = compile('move 1 to a-1. move 2 to a-2.', { a_1: null, a_2: null });
    test.ok(text);
    test.ok(text.indexOf('ws.a_1 = 1;') >= 0);
    test.ok(text.indexOf('ws.a_2 = 2;') >= 0);
}

exports['simple compile two move commands without points'] = function (test) {
    var text = compile('move 1 to a-1 move 2 to a-2', { a_1: null, a_2: null });
    test.ok(text);
    test.ok(text.indexOf('ws.a_1 = 1;') >= 0);
    test.ok(text.indexOf('ws.a_2 = 2;') >= 0);
}

exports['simple compile move to two variables'] = function (test) {
    var text = compile('move 1 to a-1, a-2.', { a_1: null, a_2: null });
    test.ok(text);
    test.ok(text.indexOf('var $aux = 1; ws.a_1 = $aux; ws.a_2 = $aux;') >= 0);
}

exports['simple function'] = function (test) {
    var text = compile('procedure1 section.');
    test.ok(text.indexOf('function procedure1()') >= 0);
}

exports['function with moves'] = function (test) {
    var text = compile('procedure1 section. move 1 to a. move 2 to b.', { a: null, b: null });
    test.ok(text.indexOf('function procedure1() {') >= 0);
    test.ok(text.indexOf('ws.a = 1;') >= 0);
    test.ok(text.indexOf('ws.b = 2;') >= 0);
    test.ok(text.indexOf('}') >= 0);
}

exports['function with parameters'] = function (test) {
    var text = compile('procedure1 section using x, y. move x to a. move y to b.', { a: null, b: null });
    test.ok(text.indexOf('function procedure1(x, y) {') >= 0);
    test.ok(text.indexOf('ws.a = x;') >= 0);
    test.ok(text.indexOf('ws.b = y;') >= 0);
    test.ok(text.indexOf('}') >= 0);
}

exports['if'] = function (test) {
    var text = compile('if a > 0 then move 0 to a.', { a: null });
    test.ok(text.indexOf('if (ws.a > 0) {') >= 0);
    test.ok(text.indexOf('ws.a = 0;') >= 0);
}

exports['if with multiple commands'] = function (test) {
    var text = compile('if a > 0 then move 0 to a move 1 to b.', { a: null, b: null });
    test.equal(text, 'if (ws.a > 0) { ws.a = 0; ws.b = 1; }');
}

exports['if with else'] = function (test) {
    var text = compile('if a > 0 then move 0 to a else move 1 to b.', { a: null, b: null });
    test.equal(text, 'if (ws.a > 0) { ws.a = 0; } else { ws.b = 1; }');
}

exports['if with else end if'] = function (test) {
    var text = compile('if a > 0 then move 0 to a else move 1 to b end-if move 2 to c.', { a: null, b: null, c: null });
    test.equal(text, 'if (ws.a > 0) { ws.a = 0; } else { ws.b = 1; } ws.c = 2;');
}

exports['return with value'] = function (test) {
    var text = compile('return 1.');
    test.ok(text.indexOf('return 1;') >= 0);
}

exports['return'] = function (test) {
    var text = compile('return.');
    test.ok(text.indexOf('return;') >= 0);
}

exports['move to nested item'] = function (test) {
    var text = compile('move 0 to b in a', { a: { b: null } });
    test.ok(text.indexOf('ws.a.b = 0;') >= 0);
}

exports['declare local variable'] = function (test) {
    var text = compile('local a.');
    test.ok(text.indexOf('var a;') >= 0);
}

exports['declare and use local variable'] = function (test) {
    var text = compile('local a. move 1 to a');
    test.ok(text.indexOf('var a;') >= 0);
    test.ok(text.indexOf('a = 1;') >= 0);
}

exports['declare two local variables'] = function (test) {
    var text = compile('locals a b.');
    test.ok(text.indexOf('var a, b;') >= 0);
}

exports['declare and use global variable'] = function (test) {
    var text = compile('global a. move 1 to a');
    test.ok(text.indexOf('var a;') < 0);
    test.ok(text.indexOf('a = 1;') >= 0);
}

exports['compile global invocation'] = function (test) {
    var text = compile('global foo. global require. perform require using "assert" giving foo.');
    test.ok(text.indexOf('foo = require("assert");') >= 0);
}

exports['a linkage item is visible as a runtime field'] = function (test) {
    var text = compile('data division. linkage section. 01 a. procedure division. move 1 to a.');
    test.ok(text.indexOf('runtime.a = 1;') >= 0);
}

exports['call a linkage item as procedure'] = function (test) {
    var text = compile('data division. linkage section. 01 response. procedure division. perform write in response.');
    test.ok(text.indexOf('runtime.response.write()') >= 0);
}

exports['move an empty object'] = function (test) {
    var text = compile('local a. move object to a.');
    test.ok(text.indexOf('a = {};') >= 0);
}

exports['move an empty array'] = function (test) {
    var text = compile('local a. move array to a.');
    test.ok(text.indexOf('a = [];') >= 0);
}

exports['move to an indexed array by name'] = function (test) {
    var text = compile('local a. move 1 to a("foo")');
    test.ok(text.indexOf('runtime.setIndex(a, "foo", 1);') >= 0);
}

exports['stop run'] = function (test) {
    var text = compile('stop run.');
    test.equal(text, 'runtime.stop(0);');
}

