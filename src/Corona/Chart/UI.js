
"use strict";

exports.fullPagename = function () {
    return window.location.href.split("?")[0];
}

exports.execCopy = function () {
    document.execCommand("copy");
}


const linkifier = function (selector, addTitle, callback) {
    const links = document.querySelectorAll("a" + selector);
    for (const a of links) {
        const targ = a.getAttribute('href');
        if (targ) {
            const aNew = a.cloneNode(true);
            a.parentNode.replaceChild(aNew, a);
            aNew.setAttribute('title',addTitle);
            aNew.addEventListener('click', function (e) {
                e.preventDefault();
                callback(targ);
            });
        }
    }
}


exports.linkify = function (dataview, copylink, saveimage) {
    return function () {
        linkifier(".dataview", "Load me!", s => dataview(s.slice(1))());
        linkifier(".copylink", "Copy Permalink to Chart", () => copyLink());
        linkifier(".saveimage", "Save as Image", () => saveimage());
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


const scrollToTop = function () {
    var scrollAnimation;
    var position =
        document.body.scrollTop || document.documentElement.scrollTop;
    if (position) {
        window.scrollBy(0, -Math.max(1, Math.floor(position / 10)));
        scrollAnimation = setTimeout(scrollToTop, 15);
    } else if (scrollAnimation) { clearTimeout(scrollAnimation) };
}

exports.scrollToTop = scrollToTop;
