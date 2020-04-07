
var cobs = require('../lib/cobolscript');

it('getRuntime is defined', () => {
    test.ok(cobs.getRuntime);
    test.equal(typeof(cobs.getRuntime), 'function');
});

it('getRuntime', () => {
    var runtime = cobs.getRuntime();

    test.ok(runtime);
    test.ok(runtime.display);
    test.equal(typeof(runtime.display), 'function');
    test.ok(runtime.write);
    test.equal(typeof(runtime.write), 'function');
    test.ok(runtime.flush);
    test.equal(typeof(runtime.flush), 'function');
    test.ok(runtime.getIndex);
    test.equal(typeof(runtime.getIndex), 'function');
    test.ok(runtime.setIndex);
    test.equal(typeof(runtime.setIndex), 'function');
    test.ok(runtime.global);
});

it('getRuntime with response, request, require', () => {
    var runtime = cobs.getRuntime( { response: {}, request: {}, require: require });

    test.ok(runtime);
    test.ok(runtime.display);
    test.equal(typeof(runtime.display), 'function');
    test.ok(runtime.write);
    test.equal(typeof(runtime.write), 'function');
    test.ok(runtime.flush);
    test.equal(typeof(runtime.flush), 'function');
    test.ok(runtime.getIndex);
    test.equal(typeof(runtime.getIndex), 'function');
    test.ok(runtime.setIndex);
    test.equal(typeof(runtime.setIndex), 'function');
    test.ok(runtime.response);
    test.ok(runtime.request);
    test.ok(runtime.global);
    test.ok(runtime.require);
});

