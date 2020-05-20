
"use strict";

exports.fullPagename = function () {
    return window.location.href.split("?")[0];
}

exports.execCopy = function () {
    document.execCommand("copy");
}


const linkifier = function (selector, callback) {
    const links = document.querySelectorAll("a" + selector);
    for (const a of links) {
        const targ = a.getAttribute('href');
        if (targ) {
            const aNew = a.cloneNode(true);
            a.parentNode.replaceChild(aNew, a);
            // aNew.setAttribute('href','#');
            aNew.addEventListener('click', function (e) {
                e.preventDefault();
                callback(targ);
            });
        }
    }
}


exports.linkify = function (dataview, copylink, saveimage) {
    return function () {
        linkifier(".dataview",  s => dataview(s.slice(1))());
        linkifier(".copylink",  () => copyLink());
        linkifier(".saveimage", () => saveimage());
    }
}

exports.cutInnerHTML = function(e) {
    return function() {
        const x = e.innerHTML;
        e.parentNode.removeChild(e);
        return x;
    }
}

exports.toast = function(str) {
    return function () {
      Toastify({
        text: str,
        duration: 2000,
        gravity: "bottom",
        backgroundColor: "#00b09b"
      }).showToast();
    }
}

