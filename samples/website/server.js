
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

var layout = cobs.compileTemplateFile('./pages/layout.cobp');
app.get('/', makeTemplatePage('./pages/index.cobp', layout));
app.get('/customer', makeTemplatePage('./pages/customerList.cobp', layout));
app.get('/customer/new', makeTemplatePage('./pages/customerNew.cobp', layout));

var server = http.createServer(app).listen(8000);

console.log('listening to http://localhost:8000');
