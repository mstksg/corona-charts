
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
            // const aNew = a.cloneNode(true);
            // a.parentNode.replaceChild(aNew, a);
            d3.select(a)
              .attr('title',addTitle)
              .on('click',null)
              .on('click', function () {
                 d3.event.preventDefault();
                 callback(targ);
               });
        }
    }
}


exports.linkify = function (dataview, copylink, saveimage) {
    return function () {
        linkifier(".dataview", "Load me!", s => dataview(s.slice(1))());
        linkifier(".copylink", "Copy Permalink to Chart", () => copylink());
        linkifier(".saveimage", "Save as Image", () => saveimage());
    }
}

exports.helpify = function (loader, unloader) {
    return function () {
        const links = document.querySelectorAll("a.helptip-link");
        for (const a of links) {
            const targ = a.getAttribute('href');
            if (targ) {
                d3.select(a)
                  .on('mouseenter',null)
                  .on('mouseenter', function() {
                    loader(targ)({x: d3.event.pageX, y: d3.event.pageY})();
                   })
                  .on('mouseleave',null)
                  .on('mouseleave', function(e) {
                     unloader();
                   })
                  .on('touchenter',null)
                  .on('touchenter', function() {
                    for (const t of d3.event.targetTouches) {
                        loader(targ)({x: t.pageX, y: t.pageY})();
                        return;
                    }
                   })
                  .on('touchleave',null)
                  .on('touchleave', function(e) {
                     unloader();
                   });

            }
        }
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


exports.scrollToTop = function(callback) {
    return function () {
        const scroller = function () {
          var scrollAnimation;
          var position =
              document.body.scrollTop || document.documentElement.scrollTop;
          if (position) {
              window.scrollBy(0, -Math.max(1, Math.floor(position / 5)));
              scrollAnimation = setTimeout(scroller, 15);
          } else {
              if (scrollAnimation) {
                clearTimeout(scrollAnimation);
              }
              callback();
          };
        }
        scroller();
    }
}

exports.setPos = function(el,pos) {
    return function () {
        if (screen.width > 800) {
          el.style.left = pos.x + "px";
        } else {
          el.style.removeProperty('left');
        }
        el.style.top = pos.y + "px";
    }
}
