
"use strict";

exports.fullPagename = function () {
    return window.location.href.split("?")[0];
}

exports.execCopy = function () {
    document.execCommand("copy");
}
