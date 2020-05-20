
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


// exports.initLinkifier = () => ({
//         callback: function () { return; };
//         linkify: function () {
//             const links = document.querySelectorAll("a");
//             for (const a of links) {
//                 const targ = a.getAttribute('href');
//                 a.setAttribute('href','#');
//                 a.addEventListener('click', () => this.callback(targ)());
//             }
//         }
//     });

// exports.setLinkifier = function (linkifier,cb) {
//     return function() {
//         linkifier.callback = cb;
//     }
// }

// exports.runLinkifier = function (linkifier) {
//     return function() {
//         linkifier.linkify()
//     }
// }

// exports.moveDiv = function(x,y) {
//     return function () {
//         x.appendChild(y);
//     }
// }

// exports.linkify = function(callback) {
//     return function () {
//         window.linkify = gcap
//     }
// }
// // exports.linkify = function (callback) {
// //     return function () {
// //         const links = document.querySelectorAll("a");
// //         for (const a of links) {
// //             const targ = a.getAttribute('href');
// //             a.setAttribute('href','#');
// //             a.addEventListener('click', () => callback(targ));
// //         }
// //     }
// // }
