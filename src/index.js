'use strict';

require('ace-css/css/ace.css');
require('./index.html');

var Elm = require('./Main.elm');
var localStoragePorts = require('elm-local-storage-ports');

var mountNode = document.getElementById('main');
var app = Elm.Main.embed(mountNode);
localStoragePorts.register(app.ports);
