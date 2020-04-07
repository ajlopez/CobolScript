
var cobs = require('../lib/cobolscript');

it('getRuntime is defined', () => {
    expect(!!cobs.getRuntime).toBe(true);
    expect(typeof(cobs.getRuntime)).toEqual('function')
});

it('getRuntime', () => {
    var runtime = cobs.getRuntime();

    expect(!!runtime).toBe(true);
    expect(!!runtime.display).toBe(true);
    expect(typeof(runtime.display)).toEqual('function')
    expect(!!runtime.write).toBe(true);
    expect(typeof(runtime.write)).toEqual('function')
    expect(!!runtime.flush).toBe(true);
    expect(typeof(runtime.flush)).toEqual('function')
    expect(!!runtime.getIndex).toBe(true);
    expect(typeof(runtime.getIndex)).toEqual('function')
    expect(!!runtime.setIndex).toBe(true);
    expect(typeof(runtime.setIndex)).toEqual('function')
    expect(!!runtime.global).toBe(true);
});

it('getRuntime with response, request, require', () => {
    var runtime = cobs.getRuntime( { response: {}, request: {}, require: require });

    expect(!!runtime).toBe(true);
    expect(!!runtime.display).toBe(true);
    expect(typeof(runtime.display)).toEqual('function')
    expect(!!runtime.write).toBe(true);
    expect(typeof(runtime.write)).toEqual('function')
    expect(!!runtime.flush).toBe(true);
    expect(typeof(runtime.flush)).toEqual('function')
    expect(!!runtime.getIndex).toBe(true);
    expect(typeof(runtime.getIndex)).toEqual('function')
    expect(!!runtime.setIndex).toBe(true);
    expect(typeof(runtime.setIndex)).toEqual('function')
    expect(!!runtime.response).toBe(true);
    expect(!!runtime.request).toBe(true);
    expect(!!runtime.global).toBe(true);
    expect(!!runtime.require).toBe(true);
});

