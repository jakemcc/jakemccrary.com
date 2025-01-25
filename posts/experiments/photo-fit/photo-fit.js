"use strict";
var PhotoFit = /** @class */ (function () {
    function PhotoFit() {
        var _this = this;
        var _a;
        this.image = new Image();
        this.handleFileSelect = function (evt) {
            var _a, _b;
            var files = (_b = (_a = evt) === null || _a === void 0 ? void 0 : _a.target.files) !== null && _b !== void 0 ? _b : new FileList();
            if (files.length === 0) {
                return;
            }
            var file = files[0];
            var reader = new FileReader();
            reader.onload = function (_) {
                var r = reader.result;
                if (typeof r === 'string') {
                    _this.image.src = r;
                }
            };
            reader.readAsDataURL(file);
        };
        var canvas = document.getElementById('canvas');
        var context = canvas.getContext("2d");
        context.lineCap = 'round';
        context.lineJoin = 'round';
        context.strokeStyle = 'black';
        context.lineWidth = 1;
        this.canvas = canvas;
        this.context = context;
        this.resizeCanvas();
        this.image.onload = function () { return _this.redraw(); };
        (_a = document.getElementById('image')) === null || _a === void 0 ? void 0 : _a.addEventListener('change', this.handleFileSelect, false);
    }
    PhotoFit.prototype.devicePixelRatio = function () {
        return window.devicePixelRatio || 1;
    };
    PhotoFit.prototype.width = function () {
        return (window.screen.width || window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth) * this.devicePixelRatio();
    };
    PhotoFit.prototype.height = function () {
        return (window.screen.height || window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight) * this.devicePixelRatio();
    };
    PhotoFit.prototype.resizeCanvas = function () {
        var canvas = this.canvas;
        canvas.width = this.width();
        canvas.height = this.height();
        this.clearCanvas();
    };
    PhotoFit.prototype.canvasWidth = function () {
        return Math.min(this.canvas.width, 1440);
    };
    PhotoFit.prototype.canvasHeight = function () {
        return Math.min(this.canvas.height, 2960);
    };
    PhotoFit.prototype.redraw = function () {
        var _a;
        this.clearCanvas();
        var context = this.context;
        context.fillStyle = 'black';
        context.fillRect(0, 0, this.canvasWidth(), this.canvasHeight());
        var w = this.image.width;
        var h = this.image.height;
        if (w > this.canvasWidth()) {
            var ratio = this.canvasWidth() / w;
            w = this.canvasWidth();
            h = ratio * h;
        }
        var y = (this.canvasHeight() - h) / 2;
        var dx = this.image.width < this.canvasWidth() ? (this.canvasWidth() - this.image.width) / 2.0 : 0;
        context.drawImage(this.image, 0, 0, this.image.width, this.image.height, dx, y, w, h);
        var data = this.canvas.toDataURL('image/png', 1);
        (_a = document.getElementById('display-image')) === null || _a === void 0 ? void 0 : _a.setAttribute('src', data);
        var anchor = document.getElementById('display-image-anchor');
        anchor === null || anchor === void 0 ? void 0 : anchor.setAttribute('href', data);
        anchor === null || anchor === void 0 ? void 0 : anchor.setAttribute('download', Date.now().toString() + '-resized.png');
    };
    PhotoFit.prototype.clearCanvas = function () {
        this.context.clearRect(0, 0, this.canvasWidth(), this.canvasHeight());
    };
    return PhotoFit;
}());
// Check for the various File API support.
if (window.File && window.FileReader && window.FileList && window.Blob) {
    // Great success! All the File APIs are supported.
}
else {
    alert('The File APIs are not fully supported in this browser.');
}
var hidder = document.getElementById('hidder');
if (hidder !== null) {
    hidder.className = "";
}
var _qqq = new PhotoFit();
//# sourceMappingURL=photo-fit.js.map