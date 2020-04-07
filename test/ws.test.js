
var parsers = require('../lib/parsers');

it('Working storage with one variable', () => {
    var parser = parsers.createParser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 ITEM.\r\n\
    ');

    var program = parser.parseProgram();

    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(!!program.data.working_storage).toBe(true);
    expect(!!typeof(program.data.working_storage.item) != 'undefined').toBe(true);
});

it('Working storage with two variables', () => {
    var parser = parsers.createParser('\
DATA DIVISION.\r\n\
WORKING-STORAGE SECTION.\r\n\
01 ITEM1.\r\n\
01 ITEM2.\r\n\
    ');

    var program = parser.parseProgram();

    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(!!program.data.working_storage).toBe(true);
    expect(typeof(program.data.working_storage.item1) != 'undefined').toBe(true);
    expect(typeof(program.data.working_storage.item2) != 'undefined').toBe(true);
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

    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(!!program.data.working_storage).toBe(true);
    expect(typeof(program.data.working_storage.group1) != 'undefined').toBe(true);
    expect(typeof(program.data.working_storage.group1.item1) != 'undefined').toBe(true);
    expect(typeof(program.data.working_storage.group1.item2) != 'undefined').toBe(true);
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

    expect(!!program).toBe(true);
    expect(!!program.data).toBe(true);
    expect(!!program.data.working_storage).toBe(true);
    expect(!!program.data.working_storage.group1).toBe(true);
    expect(!!program.data.working_storage.group1.item1).toBe(true);
    expect(typeof(program.data.working_storage.group1.item1.subitem1) != 'undefined').toBe(true);
    expect(typeof(program.data.working_storage.group1.item1.subitem2) != 'undefined').toBe(true);
    expect(typeof(program.data.working_storage.group1.item2) != 'undefined').toBe(true);
    expect(!!program.data.working_storage.group2).toBe(true);
    expect(typeof(program.data.working_storage.group2.item1) != 'undefined').toBe(true);
    expect(typeof(program.data.working_storage.group2.item2) != 'undefined').toBe(true);
});

