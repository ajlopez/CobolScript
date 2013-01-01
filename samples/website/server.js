
var cobs = require('../..');
    simpleweb = require('simpleweb'),
    path = require('path'),
    http = require('http');
    
var app = simpleweb();

app.use(simpleweb.query());
app.use(simpleweb.body());
app.use(app.router);
app.use(simpleweb.static(path.join(__dirname, 'public')));

function makeTemplatePage(filename, layout) {
    var program = cobs.compileTemplateFile(filename);
    return function (req, res) {
        program.run(cobs.getRuntime({ request: req, response: res, require: require, layout: layout }));
    };
}

function makePage(filename) {
    var program = cobs.compileProgramFile(filename);
    return function (req, res) {
        program.run(cobs.getRuntime({ request: req, response: res, require: require }));
    };
}

function doMappings() {
    var layout = cobs.compileTemplateFile('./pages/layout.cobp');
    app.get('/', makeTemplatePage('./pages/index.cobp', layout));

    app.get('/customer', makeTemplatePage('./pages/customerList.cobp', layout));
    app.get('/customer/new', makeTemplatePage('./pages/customerNew.cobp', layout));
    app.post('/customer/new', makePage('./pages/customerNew.cob', layout));
    app.get('/customer/update', makeTemplatePage('./pages/customerUpdate.cobp', layout));
    app.post('/customer/update', makePage('./pages/customerUpdate.cob', layout));
    app.get('/customer/delete', makePage('./pages/customerDelete.cob', layout));
    app.get('/customer/view', makeTemplatePage('./pages/customerView.cobp', layout));

    app.get('/supplier', makeTemplatePage('./pages/supplierList.cobp', layout));
    app.get('/supplier/new', makeTemplatePage('./pages/supplierNew.cobp', layout));
    app.post('/supplier/new', makePage('./pages/supplierNew.cob', layout));
    app.get('/supplier/update', makeTemplatePage('./pages/supplierUpdate.cobp', layout));
    app.post('/supplier/update', makePage('./pages/supplierUpdate.cob', layout));
    app.get('/supplier/delete', makePage('./pages/supplierDelete.cob', layout));
    app.get('/supplier/view', makeTemplatePage('./pages/supplierView.cobp', layout));
}

doMappings();

app.get('/reload', function (req, res) {
    doMappings();
    res.writeHead(302, { 'Location': '/' });
    res.end();    
});

var server = http.createServer(app).listen(8000);

console.log('listening to http://localhost:8000');
