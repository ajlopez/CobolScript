
var cobs = require('../'),
    assert = require('assert');

// getRuntime is defined

assert.ok(cobs.getRuntime);
assert.equal(typeof(cobs.getRuntime), 'function');

// getRuntime

var runtime = cobs.getRuntime();

assert.ok(runtime);
assert.ok(runtime.display);
assert.equal(typeof(runtime.display), 'function');
assert.ok(runtime.write);
assert.equal(typeof(runtime.write), 'function');
assert.ok(runtime.flush);
assert.equal(typeof(runtime.flush), 'function');

// getRuntime with response, request

var runtime = cobs.getRuntime( { response: {}, request: {} });

assert.ok(runtime);
assert.ok(runtime.display);
assert.equal(typeof(runtime.display), 'function');
assert.ok(runtime.write);
assert.equal(typeof(runtime.write), 'function');
assert.ok(runtime.flush);
assert.equal(typeof(runtime.flush), 'function');
assert.ok(runtime.response);
assert.ok(runtime.request);


