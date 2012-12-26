
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

