
var cobs = require('../lib/cobolscript'),
    path = require('path');
    
it('compile program defined', () => {
    test.ok(cobs.compileProgram);
});

it('data division', () => {
    var program = cobs.compileProgram('data division.procedure division.return.');
    test.ok(program);
    test.notEqual(typeof program.data, 'undefined');
});

it('working-storage section', () => {
    var program = cobs.compileProgram('data division. working-storage section. procedure division.return.');
    test.ok(program);
    test.ok(program.data);
    test.notEqual(typeof program.data.working_storage, 'undefined');
});

it('linkage section', () => {
    var program = cobs.compileProgram('data division. linkage section. procedure division.return.');
    test.ok(program);
    test.ok(program.data);
    test.notEqual(typeof program.data.linkage, 'undefined');
});

it('linkage section with item', () => {
    var program = cobs.compileProgram('data division. linkage section. 01 request. 01 response. procedure division.return.');
    test.ok(program);
    test.ok(program.data);
    test.ok(program.data.linkage);
    test.notEqual(typeof program.data.linkage.request, 'undefined');
    test.notEqual(typeof program.data.linkage.response, 'undefined');
});

