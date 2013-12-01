var cobs = require('../'),
    path = require('path');
    
function compile(code, ws) {
    var program = cobs.compileProgram(code, ws);
    return program.compileText();
}

exports['function call with arguments'] = function (test) {
    var text = compile('perform procedure1 using a, b.', { a: null, b: null });
    test.ok(text.indexOf('procedure1(ws.a, ws.b)') >= 0);
};

exports['perform procedure'] = function (test) {
    var text = compile('perform procedure1.');
    test.ok(text.indexOf('procedure1();') >= 0);
};

exports['perform procedure with procedure'] = function (test) {
    var text = compile('perform procedure1. procedure1 section. move 1 to a. move 2 to b.', { a: null, b: null });
    test.ok(text.indexOf('procedure1();') >= 0);
    test.ok(text.indexOf('function procedure1() {') >= 0);
    test.ok(text.indexOf('ws.a = 1;') >= 0);
    test.ok(text.indexOf('ws.b = 2;') >= 0);
    test.ok(text.indexOf('}') >= 0);
};

exports['perform procedure with varying'] = function (test) {
    var text = compile('perform procedure1 varying k from 1 to 10 by 1. procedure1 section. move 1 to a. move 2 to b.', { k: null, a: null, b: null });
    test.ok(text.indexOf('for (ws.k = 1; ws.k <= 10; ws.k++)') >= 0);
    test.ok(text.indexOf('procedure1();') >= 0);
    test.ok(text.indexOf('function procedure1() {') >= 0);
    test.ok(text.indexOf('ws.a = 1;') >= 0);
    test.ok(text.indexOf('ws.b = 2;') >= 0);
    test.ok(text.indexOf('}') >= 0);
};

exports['perform procedure with giving'] = function (test) {
    var text = compile('perform procedure1 giving k. procedure1 section. return 1.', { k: null });
    test.ok(text.indexOf('ws.k = procedure1();') >= 0);
    test.ok(text.indexOf('function procedure1() {') >= 0);
    test.ok(text.indexOf('return 1;') >= 0);
    test.ok(text.indexOf('}') >= 0);
};

exports['perform procedure with giving many variables'] = function (test) {
    var text = compile('perform procedure-1 giving k, j. procedure-1 section. return 1.', { k: null, j: null });
    test.ok(text.indexOf('var $aux = procedure_1();') >= 0);
    test.ok(text.indexOf('ws.k = $aux;') >= 0);
    test.ok(text.indexOf('ws.j = $aux;') >= 0);
    test.ok(text.indexOf('function procedure_1() {') >= 0);
    test.ok(text.indexOf('return 1;') >= 0);
    test.ok(text.indexOf('}') >= 0);
};

exports['perform procedure with local'] = function (test) {
    var text = compile('\
procedure division.\r\n\
perform procedure-1.\r\n\
perform procedure-2.\r\n\
\r\n\
procedure-1 section. local a.\r\n\
move 1 to a.\r\n\
return a.\r\n\
\r\n\
procedure-2 section. local a.\r\n\
move 2 to a.\r\n\
return a.'
    , { k: null, j: null });
    test.ok(text.indexOf('procedure_1();') >= 0);
    test.ok(text.indexOf('procedure_2();') >= 0);
    test.ok(text.indexOf('function procedure_1() {') >= 0);
    test.ok(text.indexOf('function procedure_2() {') >= 0);
    test.ok(text.indexOf('var a;') >= 0);
    test.ok(text.indexOf('}') >= 0);
};

exports['perform with in'] = function (test) {
    var text = compile('perform func in obj');
    test.ok(text.indexOf('obj.func()') >= 0);
};

exports['use a procedure as parameter'] = function (test) {
    var text = compile('perform proc1 using proc2. proc1 section. return 1. proc2 section. return 2.');
    test.ok(text.indexOf('proc1(proc2)') >= 0);
};

exports['perform with async'] = function (test) {
    var text = compile('perform proc1 async. display "hello"');
    test.equal(text, 'proc1($cb1); function $cb1() { runtime.display("hello"); }');
    text = compile('perform proc1 async giving a. display "hello". display a');
    test.equal(text, 'proc1($cb1); function $cb1(a) { runtime.display("hello"); runtime.display(a); }');
    text = compile('perform proc1 async with error giving a. display "hello". display a');
    test.equal(text, 'proc1($cb1); function $cb1(err, a) { runtime.display("hello"); runtime.display(a); }');
    text = compile('perform proc1 async with error. perform proc2 async with error giving a. display "hello". display a');
    test.equal(text, 'proc1($cb1); function $cb1(err) { proc2($cb2); function $cb2(err, a) { runtime.display("hello"); runtime.display(a); } }');
};

exports['procedure with async'] = function (test) {
    var text = compile('procedure division. local name. perform proc1 async giving name. display "hello " name. proc1 section async. return "world"');
    test.equal(text, 'var name; proc1($cb1); function $cb1(name) { runtime.display("hello ", name); } function proc1($cb) { $cb("world"); return; }'); 
};

exports['perform inline'] = function (test) {
    var text = compile('local a. perform move 1 to a end-perform.');
    test.equal(text, 'var a; while (true) { a = 1; }');
};

exports['perform inline with until'] = function (test) {
    var text = compile('local a. move 1 to a. perform until a > 10 add 1 to a end-perform.');
    test.equal(text, 'var a; a = 1; while (!(a > 10)) { a = a + 1; }');
};

exports['perform inline with until with test first'] = function (test) {
    var text = compile('local a. move 1 to a. perform until a > 10 with test first add 1 to a end-perform.');
    test.equal(text, 'var a; a = 1; while (!(a > 10)) { a = a + 1; }');
};

exports['perform inline with until with test last'] = function (test) {
    var text = compile('local a. move 1 to a. perform until a > 10 with test last add 1 to a end-perform.');
    test.equal(text, 'var a; a = 1; while (true) { a = a + 1; if (!(a > 10)) break; }');
};

exports['perform inline with verying'] = function (test) {
    var text = compile('local a. local k. move 0 to a. perform varying k from 1 to 10 add k to a end-perform.');
    test.equal(text, 'var a; var k; a = 0; for (k = 1; k <= 10; k += 1) { a = a + k; }');

    var text = compile('local a. local k. local n. move 0 to a. move 10 to n. perform varying k from 1 to n add k to a end-perform.');
    test.equal(text, 'var a; var k; var n; a = 0; n = 10; for (k = 1; k <= n; k += 1) { a = a + k; }');
};

exports['exit perform'] = function (test) {
    var text = compile('perform exit perform end-perform.');
    test.equal(text, 'while (true) { break; }');
};

exports['compile new'] = function (test) {
    var text = compile('perform new in Date.');
    test.equal(text, 'new Date();');
};

exports['compile new and giving'] = function (test) {
    var text = compile('local a. perform new in Date giving a.');
    test.equal(text, 'var a; a = new Date();');
};

