
var cobs = require('../'),
    path = require('path');
    
exports['compile program defined'] = function (test) {
    test.ok(cobs.compileProgram);
};

exports['data division'] = function (test) {
    var program = cobs.compileProgram('data division.procedure division.return.');
    test.ok(program);
    test.notEqual(typeof program.data, 'undefined');
};

exports['working-storage section'] = function (test) {
    var program = cobs.compileProgram('data division. working-storage section. procedure division.return.');
    test.ok(program);
    test.ok(program.data);
    test.notEqual(typeof program.data.working_storage, 'undefined');
};

exports['linkage section'] = function (test) {
    var program = cobs.compileProgram('data division. linkage section. procedure division.return.');
    test.ok(program);
    test.ok(program.data);
    test.notEqual(typeof program.data.linkage, 'undefined');
};

exports['linkage section with item'] = function (test) {
    var program = cobs.compileProgram('data division. linkage section. 01 request. 01 response. procedure division.return.');
    test.ok(program);
    test.ok(program.data);
    test.ok(program.data.linkage);
    test.notEqual(typeof program.data.linkage.request, 'undefined');
    test.notEqual(typeof program.data.linkage.response, 'undefined');
};

