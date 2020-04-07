
var parsers = require('../lib/parsers');

it('Working storage with one variable', () => {
    var parser = parsers.createParser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 ITEM.\r\n\
    ');

    var program = parser.parseProgram();

    test.ok(program);
    test.ok(program.data);
    test.ok(program.data.working_storage);
    test.ok(typeof(program.data.working_storage.item) != 'undefined');
});

it('Working storage with two variables', () => {
    var parser = parsers.createParser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 ITEM1.\r\n\
01 ITEM2.\r\n\
    ');

    var program = parser.parseProgram();

    test.ok(program);
    test.ok(program.data);
    test.ok(program.data.working_storage);
    test.ok(typeof(program.data.working_storage.item1) != 'undefined');
    test.ok(typeof(program.data.working_storage.item2) != 'undefined');
});

it('Working storage with group item and two subitems', () => {
    var parser = parsers.createParser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 GROUP1.\r\n\
02 ITEM1.\r\n\
02 ITEM2.\r\n\
    ');

    var program = parser.parseProgram();

    test.ok(program);
    test.ok(program.data);
    test.ok(program.data.working_storage);
    test.ok(typeof(program.data.working_storage.group1) != 'undefined');
    test.ok(typeof(program.data.working_storage.group1.item1) != 'undefined');
    test.ok(typeof(program.data.working_storage.group1.item2) != 'undefined');
});

it('Working storage with two group items and three levels', () => {
    var parser = parsers.createParser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 GROUP1.\r\n\
02 ITEM1.\r\n\
03 SUBITEM1.\r\n\
03 SUBITEM2.\r\n\
02 ITEM2.\r\n\
01 GROUP2.\r\n\
02 ITEM1.\r\n\
02 ITEM2.\r\n\
    ');

    var program = parser.parseProgram();

    test.ok(program);
    test.ok(program.data);
    test.ok(program.data.working_storage);
    test.ok(program.data.working_storage.group1);
    test.ok(program.data.working_storage.group1.item1);
    test.ok(typeof(program.data.working_storage.group1.item1.subitem1) != 'undefined');
    test.ok(typeof(program.data.working_storage.group1.item1.subitem2) != 'undefined');
    test.ok(typeof(program.data.working_storage.group1.item2) != 'undefined');
    test.ok(program.data.working_storage.group2);
    test.ok(typeof(program.data.working_storage.group2.item1) != 'undefined');
    test.ok(typeof(program.data.working_storage.group2.item2) != 'undefined');
});

