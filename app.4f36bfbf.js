// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"xND8":[function(require,module,exports) {

// shim for using process in browser
var process = module.exports = {}; // cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
  throw new Error('setTimeout has not been defined');
}

function defaultClearTimeout() {
  throw new Error('clearTimeout has not been defined');
}

(function () {
  try {
    if (typeof setTimeout === 'function') {
      cachedSetTimeout = setTimeout;
    } else {
      cachedSetTimeout = defaultSetTimout;
    }
  } catch (e) {
    cachedSetTimeout = defaultSetTimout;
  }

  try {
    if (typeof clearTimeout === 'function') {
      cachedClearTimeout = clearTimeout;
    } else {
      cachedClearTimeout = defaultClearTimeout;
    }
  } catch (e) {
    cachedClearTimeout = defaultClearTimeout;
  }
})();

function runTimeout(fun) {
  if (cachedSetTimeout === setTimeout) {
    //normal enviroments in sane situations
    return setTimeout(fun, 0);
  } // if setTimeout wasn't available but was latter defined


  if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
    cachedSetTimeout = setTimeout;
    return setTimeout(fun, 0);
  }

  try {
    // when when somebody has screwed with setTimeout but no I.E. maddness
    return cachedSetTimeout(fun, 0);
  } catch (e) {
    try {
      // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
      return cachedSetTimeout.call(null, fun, 0);
    } catch (e) {
      // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
      return cachedSetTimeout.call(this, fun, 0);
    }
  }
}

function runClearTimeout(marker) {
  if (cachedClearTimeout === clearTimeout) {
    //normal enviroments in sane situations
    return clearTimeout(marker);
  } // if clearTimeout wasn't available but was latter defined


  if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
    cachedClearTimeout = clearTimeout;
    return clearTimeout(marker);
  }

  try {
    // when when somebody has screwed with setTimeout but no I.E. maddness
    return cachedClearTimeout(marker);
  } catch (e) {
    try {
      // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
      return cachedClearTimeout.call(null, marker);
    } catch (e) {
      // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
      // Some versions of I.E. have different rules for clearTimeout vs setTimeout
      return cachedClearTimeout.call(this, marker);
    }
  }
}

var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
  if (!draining || !currentQueue) {
    return;
  }

  draining = false;

  if (currentQueue.length) {
    queue = currentQueue.concat(queue);
  } else {
    queueIndex = -1;
  }

  if (queue.length) {
    drainQueue();
  }
}

function drainQueue() {
  if (draining) {
    return;
  }

  var timeout = runTimeout(cleanUpNextTick);
  draining = true;
  var len = queue.length;

  while (len) {
    currentQueue = queue;
    queue = [];

    while (++queueIndex < len) {
      if (currentQueue) {
        currentQueue[queueIndex].run();
      }
    }

    queueIndex = -1;
    len = queue.length;
  }

  currentQueue = null;
  draining = false;
  runClearTimeout(timeout);
}

process.nextTick = function (fun) {
  var args = new Array(arguments.length - 1);

  if (arguments.length > 1) {
    for (var i = 1; i < arguments.length; i++) {
      args[i - 1] = arguments[i];
    }
  }

  queue.push(new Item(fun, args));

  if (queue.length === 1 && !draining) {
    runTimeout(drainQueue);
  }
}; // v8 likes predictible objects


function Item(fun, array) {
  this.fun = fun;
  this.array = array;
}

Item.prototype.run = function () {
  this.fun.apply(null, this.array);
};

process.title = 'browser';
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues

process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) {
  return [];
};

process.binding = function (name) {
  throw new Error('process.binding is not supported');
};

process.cwd = function () {
  return '/';
};

process.chdir = function (dir) {
  throw new Error('process.chdir is not supported');
};

process.umask = function () {
  return 0;
};
},{}],"EcpK":[function(require,module,exports) {
var process = require("process");
var global = arguments[3];
function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

// Generated by purs bundle 0.13.6
var PS = {};

(function (exports) {
  /* global XMLHttpRequest */

  /* global process */
  "use strict";

  exports._ajax = function () {
    var platformSpecific = {};

    if (typeof module !== "undefined" && module.require && !(typeof process !== "undefined" && process.versions["electron"])) {
      // We are on node.js
      platformSpecific.newXHR = function () {
        var XHR = module.require("xhr2");

        return new XHR();
      };

      platformSpecific.fixupUrl = function (url, xhr) {
        if (xhr.nodejsBaseUrl === null) {
          var urllib = module.require("url");

          var u = urllib.parse(url);
          u.protocol = u.protocol || "http:";
          u.hostname = u.hostname || "localhost";
          return urllib.format(u);
        } else {
          return url || "/";
        }
      };

      platformSpecific.getResponse = function (xhr) {
        return xhr.response;
      };
    } else {
      // We are in the browser
      platformSpecific.newXHR = function () {
        return new XMLHttpRequest();
      };

      platformSpecific.fixupUrl = function (url) {
        return url || "/";
      };

      platformSpecific.getResponse = function (xhr) {
        return xhr.response;
      };
    }

    return function (mkHeader, options) {
      return function (errback, callback) {
        var xhr = platformSpecific.newXHR();
        var fixedUrl = platformSpecific.fixupUrl(options.url, xhr);
        xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);

        if (options.headers) {
          try {
            for (var i = 0, header; (header = options.headers[i]) != null; i++) {
              xhr.setRequestHeader(header.field, header.value);
            }
          } catch (e) {
            errback(e);
          }
        }

        var onerror = function onerror(msg) {
          return function () {
            errback(new Error(msg + ": " + options.method + " " + options.url));
          };
        };

        xhr.onerror = onerror("AJAX request failed");
        xhr.ontimeout = onerror("AJAX request timed out");

        xhr.onload = function () {
          callback({
            status: xhr.status,
            statusText: xhr.statusText,
            headers: xhr.getAllResponseHeaders().split("\r\n").filter(function (header) {
              return header.length > 0;
            }).map(function (header) {
              var i = header.indexOf(":");
              return mkHeader(header.substring(0, i))(header.substring(i + 2));
            }),
            body: platformSpecific.getResponse(xhr)
          });
        };

        xhr.responseType = options.responseType;
        xhr.withCredentials = options.withCredentials;
        xhr.send(options.content);
        return function (error, cancelErrback, cancelCallback) {
          try {
            xhr.abort();
          } catch (e) {
            return cancelErrback(e);
          }

          return cancelCallback();
        };
      };
    };
  }();
})(PS["Affjax"] = PS["Affjax"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Alt"] = $PS["Control.Alt"] || {};
  var exports = $PS["Control.Alt"];

  var Alt = function Alt(Functor0, alt) {
    this.Functor0 = Functor0;
    this.alt = alt;
  };

  exports["Alt"] = Alt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Alternative"] = $PS["Control.Alternative"] || {};
  var exports = $PS["Control.Alternative"];

  var Alternative = function Alternative(Applicative0, Plus1) {
    this.Applicative0 = Applicative0;
    this.Plus1 = Plus1;
  };

  exports["Alternative"] = Alternative;
})(PS);

(function (exports) {
  "use strict";

  exports.arrayApply = function (fs) {
    return function (xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;

      for (var i = 0; i < l; i++) {
        var f = fs[i];

        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }

      return result;
    };
  };
})(PS["Control.Apply"] = PS["Control.Apply"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];

  var Semigroupoid = function Semigroupoid(compose) {
    this.compose = compose;
  };

  var semigroupoidFn = new Semigroupoid(function (f) {
    return function (g) {
      return function (x) {
        return f(g(x));
      };
    };
  });

  var compose = function compose(dict) {
    return dict.compose;
  };

  exports["compose"] = compose;
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];

  var Category = function Category(Semigroupoid0, identity) {
    this.Semigroupoid0 = Semigroupoid0;
    this.identity = identity;
  };

  var identity = function identity(dict) {
    return dict.identity;
  };

  var categoryFn = new Category(function () {
    return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
    return x;
  });
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Function"] = $PS["Data.Function"] || {};
  var exports = $PS["Data.Function"];

  var on = function on(f) {
    return function (g) {
      return function (x) {
        return function (y) {
          return f(g(x))(g(y));
        };
      };
    };
  };

  var flip = function flip(f) {
    return function (b) {
      return function (a) {
        return f(a)(b);
      };
    };
  };

  var $$const = function $$const(a) {
    return function (v) {
      return a;
    };
  };

  var applyFlipped = function applyFlipped(x) {
    return function (f) {
      return f(x);
    };
  };

  var apply = function apply(f) {
    return function (x) {
      return f(x);
    };
  };

  exports["flip"] = flip;
  exports["const"] = $$const;
  exports["apply"] = apply;
  exports["applyFlipped"] = applyFlipped;
  exports["on"] = on;
})(PS);

(function (exports) {
  "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);

      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }

      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});

(function (exports) {
  "use strict";

  exports.unit = {};
})(PS["Data.Unit"] = PS["Data.Unit"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Unit"] = $PS["Data.Unit"] || {};
  var exports = $PS["Data.Unit"];
  var $foreign = $PS["Data.Unit"];
  exports["unit"] = $foreign.unit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var $foreign = $PS["Data.Functor"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];
  var Data_Function = $PS["Data.Function"];
  var Data_Unit = $PS["Data.Unit"];

  var Functor = function Functor(map) {
    this.map = map;
  };

  var map = function map(dict) {
    return dict.map;
  };

  var mapFlipped = function mapFlipped(dictFunctor) {
    return function (fa) {
      return function (f) {
        return map(dictFunctor)(f)(fa);
      };
    };
  };

  var $$void = function $$void(dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
  };

  var voidLeft = function voidLeft(dictFunctor) {
    return function (f) {
      return function (x) {
        return map(dictFunctor)(Data_Function["const"](x))(f);
      };
    };
  };

  var voidRight = function voidRight(dictFunctor) {
    return function (x) {
      return map(dictFunctor)(Data_Function["const"](x));
    };
  };

  var functorFn = new Functor(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn));
  var functorArray = new Functor($foreign.arrayMap);
  exports["Functor"] = Functor;
  exports["map"] = map;
  exports["mapFlipped"] = mapFlipped;
  exports["void"] = $$void;
  exports["voidRight"] = voidRight;
  exports["voidLeft"] = voidLeft;
  exports["functorFn"] = functorFn;
  exports["functorArray"] = functorArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];
  var $foreign = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];

  var Apply = function Apply(Functor0, apply) {
    this.Functor0 = Functor0;
    this.apply = apply;
  };

  var applyArray = new Apply(function () {
    return Data_Functor.functorArray;
  }, $foreign.arrayApply);

  var apply = function apply(dict) {
    return dict.apply;
  };

  var applySecond = function applySecond(dictApply) {
    return function (a) {
      return function (b) {
        return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Function["const"](Control_Category.identity(Control_Category.categoryFn)))(a))(b);
      };
    };
  };

  var lift2 = function lift2(dictApply) {
    return function (f) {
      return function (a) {
        return function (b) {
          return apply(dictApply)(Data_Functor.map(dictApply.Functor0())(f)(a))(b);
        };
      };
    };
  };

  exports["Apply"] = Apply;
  exports["apply"] = apply;
  exports["applySecond"] = applySecond;
  exports["lift2"] = lift2;
  exports["applyArray"] = applyArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Unit = $PS["Data.Unit"];

  var Applicative = function Applicative(Apply0, pure) {
    this.Apply0 = Apply0;
    this.pure = pure;
  };

  var pure = function pure(dict) {
    return dict.pure;
  };

  var unless = function unless(dictApplicative) {
    return function (v) {
      return function (v1) {
        if (!v) {
          return v1;
        }

        ;

        if (v) {
          return pure(dictApplicative)(Data_Unit.unit);
        }

        ;
        throw new Error("Failed pattern match at Control.Applicative (line 62, column 1 - line 62, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };

  var when = function when(dictApplicative) {
    return function (v) {
      return function (v1) {
        if (v) {
          return v1;
        }

        ;

        if (!v) {
          return pure(dictApplicative)(Data_Unit.unit);
        }

        ;
        throw new Error("Failed pattern match at Control.Applicative (line 57, column 1 - line 57, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };

  var liftA1 = function liftA1(dictApplicative) {
    return function (f) {
      return function (a) {
        return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
      };
    };
  };

  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
  exports["unless"] = unless;
  exports["when"] = when;
})(PS);

(function (exports) {
  "use strict";

  exports.arrayBind = function (arr) {
    return function (f) {
      var result = [];

      for (var i = 0, l = arr.length; i < l; i++) {
        Array.prototype.push.apply(result, f(arr[i]));
      }

      return result;
    };
  };
})(PS["Control.Bind"] = PS["Control.Bind"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var $foreign = $PS["Control.Bind"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Function = $PS["Data.Function"];

  var Discard = function Discard(discard) {
    this.discard = discard;
  };

  var Bind = function Bind(Apply0, bind) {
    this.Apply0 = Apply0;
    this.bind = bind;
  };

  var discard = function discard(dict) {
    return dict.discard;
  };

  var bindArray = new Bind(function () {
    return Control_Apply.applyArray;
  }, $foreign.arrayBind);

  var bind = function bind(dict) {
    return dict.bind;
  };

  var bindFlipped = function bindFlipped(dictBind) {
    return Data_Function.flip(bind(dictBind));
  };

  var composeKleisliFlipped = function composeKleisliFlipped(dictBind) {
    return function (f) {
      return function (g) {
        return function (a) {
          return bindFlipped(dictBind)(f)(g(a));
        };
      };
    };
  };

  var composeKleisli = function composeKleisli(dictBind) {
    return function (f) {
      return function (g) {
        return function (a) {
          return bind(dictBind)(f(a))(g);
        };
      };
    };
  };

  var discardUnit = new Discard(function (dictBind) {
    return bind(dictBind);
  });
  exports["Bind"] = Bind;
  exports["bind"] = bind;
  exports["bindFlipped"] = bindFlipped;
  exports["discard"] = discard;
  exports["composeKleisli"] = composeKleisli;
  exports["composeKleisliFlipped"] = composeKleisliFlipped;
  exports["bindArray"] = bindArray;
  exports["discardUnit"] = discardUnit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];

  var Monad = function Monad(Applicative0, Bind1) {
    this.Applicative0 = Applicative0;
    this.Bind1 = Bind1;
  };

  var unlessM = function unlessM(dictMonad) {
    return function (mb) {
      return function (m) {
        return Control_Bind.bind(dictMonad.Bind1())(mb)(function (b) {
          return Control_Applicative.unless(dictMonad.Applicative0())(b)(m);
        });
      };
    };
  };

  var liftM1 = function liftM1(dictMonad) {
    return function (f) {
      return function (a) {
        return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
          return Control_Applicative.pure(dictMonad.Applicative0())(f(a$prime));
        });
      };
    };
  };

  var ap = function ap(dictMonad) {
    return function (f) {
      return function (a) {
        return Control_Bind.bind(dictMonad.Bind1())(f)(function (f$prime) {
          return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
            return Control_Applicative.pure(dictMonad.Applicative0())(f$prime(a$prime));
          });
        });
      };
    };
  };

  exports["Monad"] = Monad;
  exports["liftM1"] = liftM1;
  exports["ap"] = ap;
  exports["unlessM"] = unlessM;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Plus"] = $PS["Control.Plus"] || {};
  var exports = $PS["Control.Plus"];

  var Plus = function Plus(Alt0, empty) {
    this.Alt0 = Alt0;
    this.empty = empty;
  };

  var empty = function empty(dict) {
    return dict.empty;
  };

  exports["Plus"] = Plus;
  exports["empty"] = empty;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.MonadZero"] = $PS["Control.MonadZero"] || {};
  var exports = $PS["Control.MonadZero"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Unit = $PS["Data.Unit"];

  var MonadZero = function MonadZero(Alternative1, Monad0) {
    this.Alternative1 = Alternative1;
    this.Monad0 = Monad0;
  };

  var guard = function guard(dictMonadZero) {
    return function (v) {
      if (v) {
        return Control_Applicative.pure(dictMonadZero.Alternative1().Applicative0())(Data_Unit.unit);
      }

      ;

      if (!v) {
        return Control_Plus.empty(dictMonadZero.Alternative1().Plus1());
      }

      ;
      throw new Error("Failed pattern match at Control.MonadZero (line 54, column 1 - line 54, column 52): " + [v.constructor.name]);
    };
  };

  exports["MonadZero"] = MonadZero;
  exports["guard"] = guard;
})(PS);

(function (exports) {
  "use strict";

  var refEq = function refEq(r1) {
    return function (r2) {
      return r1 === r2;
    };
  };

  exports.eqBooleanImpl = refEq;
  exports.eqIntImpl = refEq;
  exports.eqNumberImpl = refEq;
  exports.eqStringImpl = refEq;
})(PS["Data.Eq"] = PS["Data.Eq"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Symbol"] = $PS["Data.Symbol"] || {};
  var exports = $PS["Data.Symbol"];

  var SProxy = function () {
    function SProxy() {}

    ;
    SProxy.value = new SProxy();
    return SProxy;
  }();

  var IsSymbol = function IsSymbol(reflectSymbol) {
    this.reflectSymbol = reflectSymbol;
  };

  var reflectSymbol = function reflectSymbol(dict) {
    return dict.reflectSymbol;
  };

  exports["IsSymbol"] = IsSymbol;
  exports["reflectSymbol"] = reflectSymbol;
  exports["SProxy"] = SProxy;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeGet = function (label) {
    return function (rec) {
      return rec[label];
    };
  };
})(PS["Record.Unsafe"] = PS["Record.Unsafe"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Record.Unsafe"] = $PS["Record.Unsafe"] || {};
  var exports = $PS["Record.Unsafe"];
  var $foreign = $PS["Record.Unsafe"];
  exports["unsafeGet"] = $foreign.unsafeGet;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.Data.RowList"] = $PS["Type.Data.RowList"] || {};
  var exports = $PS["Type.Data.RowList"];

  var RLProxy = function () {
    function RLProxy() {}

    ;
    RLProxy.value = new RLProxy();
    return RLProxy;
  }();

  exports["RLProxy"] = RLProxy;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Eq"] = $PS["Data.Eq"] || {};
  var exports = $PS["Data.Eq"];
  var $foreign = $PS["Data.Eq"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var EqRecord = function EqRecord(eqRecord) {
    this.eqRecord = eqRecord;
  };

  var Eq = function Eq(eq) {
    this.eq = eq;
  };

  var eqUnit = new Eq(function (v) {
    return function (v1) {
      return true;
    };
  });
  var eqString = new Eq($foreign.eqStringImpl);
  var eqRowNil = new EqRecord(function (v) {
    return function (v1) {
      return function (v2) {
        return true;
      };
    };
  });

  var eqRecord = function eqRecord(dict) {
    return dict.eqRecord;
  };

  var eqRec = function eqRec(dictRowToList) {
    return function (dictEqRecord) {
      return new Eq(eqRecord(dictEqRecord)(Type_Data_RowList.RLProxy.value));
    };
  };

  var eqNumber = new Eq($foreign.eqNumberImpl);
  var eqInt = new Eq($foreign.eqIntImpl);
  var eqBoolean = new Eq($foreign.eqBooleanImpl);

  var eq = function eq(dict) {
    return dict.eq;
  };

  var eqRowCons = function eqRowCons(dictEqRecord) {
    return function (dictCons) {
      return function (dictIsSymbol) {
        return function (dictEq) {
          return new EqRecord(function (v) {
            return function (ra) {
              return function (rb) {
                var tail = eqRecord(dictEqRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                var get = Record_Unsafe.unsafeGet(key);
                return eq(dictEq)(get(ra))(get(rb)) && tail;
              };
            };
          });
        };
      };
    };
  };

  var notEq = function notEq(dictEq) {
    return function (x) {
      return function (y) {
        return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
      };
    };
  };

  exports["Eq"] = Eq;
  exports["eq"] = eq;
  exports["notEq"] = notEq;
  exports["eqBoolean"] = eqBoolean;
  exports["eqInt"] = eqInt;
  exports["eqNumber"] = eqNumber;
  exports["eqString"] = eqString;
  exports["eqUnit"] = eqUnit;
  exports["eqRec"] = eqRec;
  exports["eqRowNil"] = eqRowNil;
  exports["eqRowCons"] = eqRowCons;
})(PS);

(function (exports) {
  "use strict";

  exports.concatString = function (s1) {
    return function (s2) {
      return s1 + s2;
    };
  };

  exports.concatArray = function (xs) {
    return function (ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };
})(PS["Data.Semigroup"] = PS["Data.Semigroup"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Semigroup"] = $PS["Data.Semigroup"] || {};
  var exports = $PS["Data.Semigroup"];
  var $foreign = $PS["Data.Semigroup"];

  var Semigroup = function Semigroup(append) {
    this.append = append;
  };

  var semigroupString = new Semigroup($foreign.concatString);
  var semigroupArray = new Semigroup($foreign.concatArray);

  var append = function append(dict) {
    return dict.append;
  };

  exports["Semigroup"] = Semigroup;
  exports["append"] = append;
  exports["semigroupString"] = semigroupString;
  exports["semigroupArray"] = semigroupArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Monoid"] = $PS["Data.Monoid"] || {};
  var exports = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Monoid = function Monoid(Semigroup0, mempty) {
    this.Semigroup0 = Semigroup0;
    this.mempty = mempty;
  };

  var monoidString = new Monoid(function () {
    return Data_Semigroup.semigroupString;
  }, "");
  var monoidArray = new Monoid(function () {
    return Data_Semigroup.semigroupArray;
  }, []);

  var mempty = function mempty(dict) {
    return dict.mempty;
  };

  exports["Monoid"] = Monoid;
  exports["mempty"] = mempty;
  exports["monoidString"] = monoidString;
  exports["monoidArray"] = monoidArray;
})(PS);

(function (exports) {
  "use strict";

  var unsafeCompareImpl = function unsafeCompareImpl(lt) {
    return function (eq) {
      return function (gt) {
        return function (x) {
          return function (y) {
            return x < y ? lt : x === y ? eq : gt;
          };
        };
      };
    };
  };

  exports.ordIntImpl = unsafeCompareImpl;
  exports.ordNumberImpl = unsafeCompareImpl;
  exports.ordStringImpl = unsafeCompareImpl;
})(PS["Data.Ord"] = PS["Data.Ord"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Ordering"] = $PS["Data.Ordering"] || {};
  var exports = $PS["Data.Ordering"];
  var Data_Eq = $PS["Data.Eq"];

  var LT = function () {
    function LT() {}

    ;
    LT.value = new LT();
    return LT;
  }();

  var GT = function () {
    function GT() {}

    ;
    GT.value = new GT();
    return GT;
  }();

  var EQ = function () {
    function EQ() {}

    ;
    EQ.value = new EQ();
    return EQ;
  }();

  var eqOrdering = new Data_Eq.Eq(function (v) {
    return function (v1) {
      if (v instanceof LT && v1 instanceof LT) {
        return true;
      }

      ;

      if (v instanceof GT && v1 instanceof GT) {
        return true;
      }

      ;

      if (v instanceof EQ && v1 instanceof EQ) {
        return true;
      }

      ;
      return false;
    };
  });
  exports["LT"] = LT;
  exports["GT"] = GT;
  exports["EQ"] = EQ;
  exports["eqOrdering"] = eqOrdering;
})(PS);

(function (exports) {
  "use strict";

  exports.intSub = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x - y | 0;
    };
  };

  exports.numSub = function (n1) {
    return function (n2) {
      return n1 - n2;
    };
  };
})(PS["Data.Ring"] = PS["Data.Ring"] || {});

(function (exports) {
  "use strict";

  exports.intAdd = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x + y | 0;
    };
  };

  exports.intMul = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x * y | 0;
    };
  };

  exports.numAdd = function (n1) {
    return function (n2) {
      return n1 + n2;
    };
  };

  exports.numMul = function (n1) {
    return function (n2) {
      return n1 * n2;
    };
  };
})(PS["Data.Semiring"] = PS["Data.Semiring"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Semiring"] = $PS["Data.Semiring"] || {};
  var exports = $PS["Data.Semiring"];
  var $foreign = $PS["Data.Semiring"];

  var Semiring = function Semiring(add, mul, one, zero) {
    this.add = add;
    this.mul = mul;
    this.one = one;
    this.zero = zero;
  };

  var zero = function zero(dict) {
    return dict.zero;
  };

  var semiringNumber = new Semiring($foreign.numAdd, $foreign.numMul, 1.0, 0.0);
  var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);

  var add = function add(dict) {
    return dict.add;
  };

  exports["Semiring"] = Semiring;
  exports["add"] = add;
  exports["zero"] = zero;
  exports["semiringInt"] = semiringInt;
  exports["semiringNumber"] = semiringNumber;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Ring"] = $PS["Data.Ring"] || {};
  var exports = $PS["Data.Ring"];
  var $foreign = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];

  var Ring = function Ring(Semiring0, sub) {
    this.Semiring0 = Semiring0;
    this.sub = sub;
  };

  var sub = function sub(dict) {
    return dict.sub;
  };

  var ringNumber = new Ring(function () {
    return Data_Semiring.semiringNumber;
  }, $foreign.numSub);
  var ringInt = new Ring(function () {
    return Data_Semiring.semiringInt;
  }, $foreign.intSub);

  var negate = function negate(dictRing) {
    return function (a) {
      return sub(dictRing)(Data_Semiring.zero(dictRing.Semiring0()))(a);
    };
  };

  exports["Ring"] = Ring;
  exports["sub"] = sub;
  exports["negate"] = negate;
  exports["ringInt"] = ringInt;
  exports["ringNumber"] = ringNumber;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Ord"] = $PS["Data.Ord"] || {};
  var exports = $PS["Data.Ord"];
  var $foreign = $PS["Data.Ord"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var OrdRecord = function OrdRecord(EqRecord0, compareRecord) {
    this.EqRecord0 = EqRecord0;
    this.compareRecord = compareRecord;
  };

  var Ord = function Ord(Eq0, compare) {
    this.Eq0 = Eq0;
    this.compare = compare;
  };

  var ordUnit = new Ord(function () {
    return Data_Eq.eqUnit;
  }, function (v) {
    return function (v1) {
      return Data_Ordering.EQ.value;
    };
  });
  var ordString = new Ord(function () {
    return Data_Eq.eqString;
  }, $foreign.ordStringImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordRecordNil = new OrdRecord(function () {
    return Data_Eq.eqRowNil;
  }, function (v) {
    return function (v1) {
      return function (v2) {
        return Data_Ordering.EQ.value;
      };
    };
  });
  var ordNumber = new Ord(function () {
    return Data_Eq.eqNumber;
  }, $foreign.ordNumberImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));
  var ordInt = new Ord(function () {
    return Data_Eq.eqInt;
  }, $foreign.ordIntImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value));

  var compareRecord = function compareRecord(dict) {
    return dict.compareRecord;
  };

  var ordRecord = function ordRecord(dictRowToList) {
    return function (dictOrdRecord) {
      return new Ord(function () {
        return Data_Eq.eqRec()(dictOrdRecord.EqRecord0());
      }, compareRecord(dictOrdRecord)(Type_Data_RowList.RLProxy.value));
    };
  };

  var compare = function compare(dict) {
    return dict.compare;
  };

  var greaterThan = function greaterThan(dictOrd) {
    return function (a1) {
      return function (a2) {
        var v = compare(dictOrd)(a1)(a2);

        if (v instanceof Data_Ordering.GT) {
          return true;
        }

        ;
        return false;
      };
    };
  };

  var greaterThanOrEq = function greaterThanOrEq(dictOrd) {
    return function (a1) {
      return function (a2) {
        var v = compare(dictOrd)(a1)(a2);

        if (v instanceof Data_Ordering.LT) {
          return false;
        }

        ;
        return true;
      };
    };
  };

  var lessThan = function lessThan(dictOrd) {
    return function (a1) {
      return function (a2) {
        var v = compare(dictOrd)(a1)(a2);

        if (v instanceof Data_Ordering.LT) {
          return true;
        }

        ;
        return false;
      };
    };
  };

  var max = function max(dictOrd) {
    return function (x) {
      return function (y) {
        var v = compare(dictOrd)(x)(y);

        if (v instanceof Data_Ordering.LT) {
          return y;
        }

        ;

        if (v instanceof Data_Ordering.EQ) {
          return x;
        }

        ;

        if (v instanceof Data_Ordering.GT) {
          return x;
        }

        ;
        throw new Error("Failed pattern match at Data.Ord (line 167, column 3 - line 170, column 12): " + [v.constructor.name]);
      };
    };
  };

  var ordRecordCons = function ordRecordCons(dictOrdRecord) {
    return function (dictCons) {
      return function (dictIsSymbol) {
        return function (dictOrd) {
          return new OrdRecord(function () {
            return Data_Eq.eqRowCons(dictOrdRecord.EqRecord0())()(dictIsSymbol)(dictOrd.Eq0());
          }, function (v) {
            return function (ra) {
              return function (rb) {
                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
                var left = compare(dictOrd)(Record_Unsafe.unsafeGet(key)(ra))(Record_Unsafe.unsafeGet(key)(rb));
                var $49 = Data_Eq.notEq(Data_Ordering.eqOrdering)(left)(Data_Ordering.EQ.value);

                if ($49) {
                  return left;
                }

                ;
                return compareRecord(dictOrdRecord)(Type_Data_RowList.RLProxy.value)(ra)(rb);
              };
            };
          });
        };
      };
    };
  };

  var between = function between(dictOrd) {
    return function (low) {
      return function (hi) {
        return function (x) {
          if (lessThan(dictOrd)(x)(low)) {
            return false;
          }

          ;

          if (greaterThan(dictOrd)(x)(hi)) {
            return false;
          }

          ;
          return true;
        };
      };
    };
  };

  var abs = function abs(dictOrd) {
    return function (dictRing) {
      return function (x) {
        var $53 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));

        if ($53) {
          return x;
        }

        ;
        return Data_Ring.negate(dictRing)(x);
      };
    };
  };

  exports["Ord"] = Ord;
  exports["compare"] = compare;
  exports["greaterThan"] = greaterThan;
  exports["max"] = max;
  exports["between"] = between;
  exports["abs"] = abs;
  exports["ordInt"] = ordInt;
  exports["ordNumber"] = ordNumber;
  exports["ordString"] = ordString;
  exports["ordUnit"] = ordUnit;
  exports["ordRecordNil"] = ordRecordNil;
  exports["ordRecordCons"] = ordRecordCons;
  exports["ordRecord"] = ordRecord;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Alternative = $PS["Control.Alternative"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_MonadZero = $PS["Control.MonadZero"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Nothing = function () {
    function Nothing() {}

    ;
    Nothing.value = new Nothing();
    return Nothing;
  }();

  var Just = function () {
    function Just(value0) {
      this.value0 = value0;
    }

    ;

    Just.create = function (value0) {
      return new Just(value0);
    };

    return Just;
  }();

  var semigroupMaybe = function semigroupMaybe(dictSemigroup) {
    return new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
        if (v instanceof Nothing) {
          return v1;
        }

        ;

        if (v1 instanceof Nothing) {
          return v;
        }

        ;

        if (v instanceof Just && v1 instanceof Just) {
          return new Just(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 174, column 1 - line 177, column 43): " + [v.constructor.name, v1.constructor.name]);
      };
    });
  };

  var monoidMaybe = function monoidMaybe(dictSemigroup) {
    return new Data_Monoid.Monoid(function () {
      return semigroupMaybe(dictSemigroup);
    }, Nothing.value);
  };

  var maybe = function maybe(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Nothing) {
          return v;
        }

        ;

        if (v2 instanceof Just) {
          return v1(v2.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  var isNothing = maybe(true)(Data_Function["const"](false));
  var isJust = maybe(false)(Data_Function["const"](true));
  var functorMaybe = new Data_Functor.Functor(function (v) {
    return function (v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }

      ;
      return Nothing.value;
    };
  });

  var fromMaybe = function fromMaybe(a) {
    return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
  };

  var fromJust = function fromJust(dictPartial) {
    return function (v) {
      if (v instanceof Just) {
        return v.value0;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 268, column 1 - line 268, column 46): " + [v.constructor.name]);
    };
  };

  var eqMaybe = function eqMaybe(dictEq) {
    return new Data_Eq.Eq(function (x) {
      return function (y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return true;
        }

        ;

        if (x instanceof Just && y instanceof Just) {
          return Data_Eq.eq(dictEq)(x.value0)(y.value0);
        }

        ;
        return false;
      };
    });
  };

  var ordMaybe = function ordMaybe(dictOrd) {
    return new Data_Ord.Ord(function () {
      return eqMaybe(dictOrd.Eq0());
    }, function (x) {
      return function (y) {
        if (x instanceof Nothing && y instanceof Nothing) {
          return Data_Ordering.EQ.value;
        }

        ;

        if (x instanceof Nothing) {
          return Data_Ordering.LT.value;
        }

        ;

        if (y instanceof Nothing) {
          return Data_Ordering.GT.value;
        }

        ;

        if (x instanceof Just && y instanceof Just) {
          return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Maybe (line 194, column 1 - line 194, column 51): " + [x.constructor.name, y.constructor.name]);
      };
    });
  };

  var applyMaybe = new Control_Apply.Apply(function () {
    return functorMaybe;
  }, function (v) {
    return function (v1) {
      if (v instanceof Just) {
        return Data_Functor.map(functorMaybe)(v.value0)(v1);
      }

      ;

      if (v instanceof Nothing) {
        return Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var bindMaybe = new Control_Bind.Bind(function () {
    return applyMaybe;
  }, function (v) {
    return function (v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }

      ;

      if (v instanceof Nothing) {
        return Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var applicativeMaybe = new Control_Applicative.Applicative(function () {
    return applyMaybe;
  }, Just.create);
  var monadMaybe = new Control_Monad.Monad(function () {
    return applicativeMaybe;
  }, function () {
    return bindMaybe;
  });
  var altMaybe = new Control_Alt.Alt(function () {
    return functorMaybe;
  }, function (v) {
    return function (v1) {
      if (v instanceof Nothing) {
        return v1;
      }

      ;
      return v;
    };
  });
  var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
  }, Nothing.value);
  var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return applicativeMaybe;
  }, function () {
    return plusMaybe;
  });
  var monadZeroMaybe = new Control_MonadZero.MonadZero(function () {
    return alternativeMaybe;
  }, function () {
    return monadMaybe;
  });
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["maybe"] = maybe;
  exports["fromMaybe"] = fromMaybe;
  exports["isJust"] = isJust;
  exports["isNothing"] = isNothing;
  exports["fromJust"] = fromJust;
  exports["functorMaybe"] = functorMaybe;
  exports["applyMaybe"] = applyMaybe;
  exports["applicativeMaybe"] = applicativeMaybe;
  exports["bindMaybe"] = bindMaybe;
  exports["monadZeroMaybe"] = monadZeroMaybe;
  exports["monoidMaybe"] = monoidMaybe;
  exports["eqMaybe"] = eqMaybe;
  exports["ordMaybe"] = ordMaybe;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.MediaType.Common"] = $PS["Data.MediaType.Common"] || {};
  var exports = $PS["Data.MediaType.Common"];
  var applicationJSON = "application/json";
  var applicationFormURLEncoded = "application/x-www-form-urlencoded";
  exports["applicationFormURLEncoded"] = applicationFormURLEncoded;
  exports["applicationJSON"] = applicationJSON;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Affjax.RequestBody"] = $PS["Affjax.RequestBody"] || {};
  var exports = $PS["Affjax.RequestBody"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_MediaType_Common = $PS["Data.MediaType.Common"];

  var ArrayView = function () {
    function ArrayView(value0) {
      this.value0 = value0;
    }

    ;

    ArrayView.create = function (value0) {
      return new ArrayView(value0);
    };

    return ArrayView;
  }();

  var Blob = function () {
    function Blob(value0) {
      this.value0 = value0;
    }

    ;

    Blob.create = function (value0) {
      return new Blob(value0);
    };

    return Blob;
  }();

  var Document = function () {
    function Document(value0) {
      this.value0 = value0;
    }

    ;

    Document.create = function (value0) {
      return new Document(value0);
    };

    return Document;
  }();

  var $$String = function () {
    function $$String(value0) {
      this.value0 = value0;
    }

    ;

    $$String.create = function (value0) {
      return new $$String(value0);
    };

    return $$String;
  }();

  var FormData = function () {
    function FormData(value0) {
      this.value0 = value0;
    }

    ;

    FormData.create = function (value0) {
      return new FormData(value0);
    };

    return FormData;
  }();

  var FormURLEncoded = function () {
    function FormURLEncoded(value0) {
      this.value0 = value0;
    }

    ;

    FormURLEncoded.create = function (value0) {
      return new FormURLEncoded(value0);
    };

    return FormURLEncoded;
  }();

  var Json = function () {
    function Json(value0) {
      this.value0 = value0;
    }

    ;

    Json.create = function (value0) {
      return new Json(value0);
    };

    return Json;
  }();

  var toMediaType = function toMediaType(v) {
    if (v instanceof FormURLEncoded) {
      return new Data_Maybe.Just(Data_MediaType_Common.applicationFormURLEncoded);
    }

    ;

    if (v instanceof Json) {
      return new Data_Maybe.Just(Data_MediaType_Common.applicationJSON);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  exports["ArrayView"] = ArrayView;
  exports["Blob"] = Blob;
  exports["Document"] = Document;
  exports["String"] = $$String;
  exports["FormData"] = FormData;
  exports["FormURLEncoded"] = FormURLEncoded;
  exports["Json"] = Json;
  exports["toMediaType"] = toMediaType;
})(PS);

(function (exports) {
  "use strict";

  exports.boolConj = function (b1) {
    return function (b2) {
      return b1 && b2;
    };
  };

  exports.boolDisj = function (b1) {
    return function (b2) {
      return b1 || b2;
    };
  };

  exports.boolNot = function (b) {
    return !b;
  };
})(PS["Data.HeytingAlgebra"] = PS["Data.HeytingAlgebra"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.HeytingAlgebra"] = $PS["Data.HeytingAlgebra"] || {};
  var exports = $PS["Data.HeytingAlgebra"];
  var $foreign = $PS["Data.HeytingAlgebra"];

  var HeytingAlgebra = function HeytingAlgebra(conj, disj, ff, implies, not, tt) {
    this.conj = conj;
    this.disj = disj;
    this.ff = ff;
    this.implies = implies;
    this.not = not;
    this.tt = tt;
  };

  var tt = function tt(dict) {
    return dict.tt;
  };

  var not = function not(dict) {
    return dict.not;
  };

  var implies = function implies(dict) {
    return dict.implies;
  };

  var ff = function ff(dict) {
    return dict.ff;
  };

  var disj = function disj(dict) {
    return dict.disj;
  };

  var heytingAlgebraBoolean = new HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, function (a) {
    return function (b) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
  }, $foreign.boolNot, true);

  var conj = function conj(dict) {
    return dict.conj;
  };

  var heytingAlgebraFunction = function heytingAlgebraFunction(dictHeytingAlgebra) {
    return new HeytingAlgebra(function (f) {
      return function (g) {
        return function (a) {
          return conj(dictHeytingAlgebra)(f(a))(g(a));
        };
      };
    }, function (f) {
      return function (g) {
        return function (a) {
          return disj(dictHeytingAlgebra)(f(a))(g(a));
        };
      };
    }, function (v) {
      return ff(dictHeytingAlgebra);
    }, function (f) {
      return function (g) {
        return function (a) {
          return implies(dictHeytingAlgebra)(f(a))(g(a));
        };
      };
    }, function (f) {
      return function (a) {
        return not(dictHeytingAlgebra)(f(a));
      };
    }, function (v) {
      return tt(dictHeytingAlgebra);
    });
  };

  exports["ff"] = ff;
  exports["disj"] = disj;
  exports["not"] = not;
  exports["heytingAlgebraBoolean"] = heytingAlgebraBoolean;
  exports["heytingAlgebraFunction"] = heytingAlgebraFunction;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Monoid.Disj"] = $PS["Data.Monoid.Disj"] || {};
  var exports = $PS["Data.Monoid.Disj"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Disj = function Disj(x) {
    return x;
  };

  var semigroupDisj = function semigroupDisj(dictHeytingAlgebra) {
    return new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
        return Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v)(v1);
      };
    });
  };

  var monoidDisj = function monoidDisj(dictHeytingAlgebra) {
    return new Data_Monoid.Monoid(function () {
      return semigroupDisj(dictHeytingAlgebra);
    }, Data_HeytingAlgebra.ff(dictHeytingAlgebra));
  };

  exports["Disj"] = Disj;
  exports["monoidDisj"] = monoidDisj;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Newtype"] = $PS["Data.Newtype"] || {};
  var exports = $PS["Data.Newtype"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid_Disj = $PS["Data.Monoid.Disj"];

  var Newtype = function Newtype(unwrap, wrap) {
    this.unwrap = unwrap;
    this.wrap = wrap;
  };

  var wrap = function wrap(dict) {
    return dict.wrap;
  };

  var unwrap = function unwrap(dict) {
    return dict.unwrap;
  };

  var over = function over(dictNewtype) {
    return function (dictNewtype1) {
      return function (v) {
        return function (f) {
          var $90 = wrap(dictNewtype1);
          var $91 = unwrap(dictNewtype);
          return function ($92) {
            return $90(f($91($92)));
          };
        };
      };
    };
  };

  var newtypeDisj = new Newtype(function (v) {
    return v;
  }, Data_Monoid_Disj.Disj);

  var alaF = function alaF(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictNewtype) {
        return function (dictNewtype1) {
          return function (v) {
            return function (f) {
              var $96 = Data_Functor.map(dictFunctor1)(unwrap(dictNewtype1));
              var $97 = Data_Functor.map(dictFunctor)(wrap(dictNewtype));
              return function ($98) {
                return $96(f($97($98)));
              };
            };
          };
        };
      };
    };
  };

  exports["unwrap"] = unwrap;
  exports["Newtype"] = Newtype;
  exports["alaF"] = alaF;
  exports["over"] = over;
  exports["newtypeDisj"] = newtypeDisj;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.MediaType"] = $PS["Data.MediaType"] || {};
  var exports = $PS["Data.MediaType"];
  var Data_Newtype = $PS["Data.Newtype"];

  var MediaType = function MediaType(x) {
    return x;
  };

  var newtypeMediaType = new Data_Newtype.Newtype(function (n) {
    return n;
  }, MediaType);
  exports["newtypeMediaType"] = newtypeMediaType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Affjax.RequestHeader"] = $PS["Affjax.RequestHeader"] || {};
  var exports = $PS["Affjax.RequestHeader"];
  var Data_MediaType = $PS["Data.MediaType"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Accept = function () {
    function Accept(value0) {
      this.value0 = value0;
    }

    ;

    Accept.create = function (value0) {
      return new Accept(value0);
    };

    return Accept;
  }();

  var ContentType = function () {
    function ContentType(value0) {
      this.value0 = value0;
    }

    ;

    ContentType.create = function (value0) {
      return new ContentType(value0);
    };

    return ContentType;
  }();

  var RequestHeader = function () {
    function RequestHeader(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    RequestHeader.create = function (value0) {
      return function (value1) {
        return new RequestHeader(value0, value1);
      };
    };

    return RequestHeader;
  }();

  var value = function value(v) {
    if (v instanceof Accept) {
      return Data_Newtype.unwrap(Data_MediaType.newtypeMediaType)(v.value0);
    }

    ;

    if (v instanceof ContentType) {
      return Data_Newtype.unwrap(Data_MediaType.newtypeMediaType)(v.value0);
    }

    ;

    if (v instanceof RequestHeader) {
      return v.value1;
    }

    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };

  var name = function name(v) {
    if (v instanceof Accept) {
      return "Accept";
    }

    ;

    if (v instanceof ContentType) {
      return "Content-Type";
    }

    ;

    if (v instanceof RequestHeader) {
      return v.value0;
    }

    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
  };

  exports["Accept"] = Accept;
  exports["ContentType"] = ContentType;
  exports["name"] = name;
  exports["value"] = value;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Affjax.ResponseFormat"] = $PS["Affjax.ResponseFormat"] || {};
  var exports = $PS["Affjax.ResponseFormat"];
  var Control_Category = $PS["Control.Category"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_MediaType_Common = $PS["Data.MediaType.Common"];

  var $$ArrayBuffer = function () {
    function $$ArrayBuffer(value0) {
      this.value0 = value0;
    }

    ;

    $$ArrayBuffer.create = function (value0) {
      return new $$ArrayBuffer(value0);
    };

    return $$ArrayBuffer;
  }();

  var Blob = function () {
    function Blob(value0) {
      this.value0 = value0;
    }

    ;

    Blob.create = function (value0) {
      return new Blob(value0);
    };

    return Blob;
  }();

  var Document = function () {
    function Document(value0) {
      this.value0 = value0;
    }

    ;

    Document.create = function (value0) {
      return new Document(value0);
    };

    return Document;
  }();

  var Json = function () {
    function Json(value0) {
      this.value0 = value0;
    }

    ;

    Json.create = function (value0) {
      return new Json(value0);
    };

    return Json;
  }();

  var $$String = function () {
    function $$String(value0) {
      this.value0 = value0;
    }

    ;

    $$String.create = function (value0) {
      return new $$String(value0);
    };

    return $$String;
  }();

  var Ignore = function () {
    function Ignore(value0) {
      this.value0 = value0;
    }

    ;

    Ignore.create = function (value0) {
      return new Ignore(value0);
    };

    return Ignore;
  }();

  var toResponseType = function toResponseType(v) {
    if (v instanceof $$ArrayBuffer) {
      return "arraybuffer";
    }

    ;

    if (v instanceof Blob) {
      return "blob";
    }

    ;

    if (v instanceof Document) {
      return "document";
    }

    ;

    if (v instanceof Json) {
      return "text";
    }

    ;

    if (v instanceof $$String) {
      return "text";
    }

    ;

    if (v instanceof Ignore) {
      return "";
    }

    ;
    throw new Error("Failed pattern match at Affjax.ResponseFormat (line 46, column 3 - line 52, column 19): " + [v.constructor.name]);
  };

  var toMediaType = function toMediaType(v) {
    if (v instanceof Json) {
      return new Data_Maybe.Just(Data_MediaType_Common.applicationJSON);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var string = new $$String(Control_Category.identity(Control_Category.categoryFn));
  var ignore = new Ignore(Control_Category.identity(Control_Category.categoryFn));
  exports["ArrayBuffer"] = $$ArrayBuffer;
  exports["Blob"] = Blob;
  exports["Document"] = Document;
  exports["Json"] = Json;
  exports["String"] = $$String;
  exports["Ignore"] = Ignore;
  exports["string"] = string;
  exports["ignore"] = ignore;
  exports["toResponseType"] = toResponseType;
  exports["toMediaType"] = toMediaType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Affjax.ResponseHeader"] = $PS["Affjax.ResponseHeader"] || {};
  var exports = $PS["Affjax.ResponseHeader"];

  var ResponseHeader = function () {
    function ResponseHeader(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ResponseHeader.create = function (value0) {
      return function (value1) {
        return new ResponseHeader(value0, value1);
      };
    };

    return ResponseHeader;
  }();

  exports["ResponseHeader"] = ResponseHeader;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Bifunctor"] = $PS["Data.Bifunctor"] || {};
  var exports = $PS["Data.Bifunctor"];
  var Control_Category = $PS["Control.Category"];

  var Bifunctor = function Bifunctor(bimap) {
    this.bimap = bimap;
  };

  var bimap = function bimap(dict) {
    return dict.bimap;
  };

  var lmap = function lmap(dictBifunctor) {
    return function (f) {
      return bimap(dictBifunctor)(f)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  var rmap = function rmap(dictBifunctor) {
    return bimap(dictBifunctor)(Control_Category.identity(Control_Category.categoryFn));
  };

  exports["bimap"] = bimap;
  exports["Bifunctor"] = Bifunctor;
  exports["lmap"] = lmap;
  exports["rmap"] = rmap;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Either"] = $PS["Data.Either"] || {};
  var exports = $PS["Data.Either"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Left = function () {
    function Left(value0) {
      this.value0 = value0;
    }

    ;

    Left.create = function (value0) {
      return new Left(value0);
    };

    return Left;
  }();

  var Right = function () {
    function Right(value0) {
      this.value0 = value0;
    }

    ;

    Right.create = function (value0) {
      return new Right(value0);
    };

    return Right;
  }();

  var note = function note(a) {
    return Data_Maybe.maybe(new Left(a))(Right.create);
  };

  var functorEither = new Data_Functor.Functor(function (f) {
    return function (m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }

      ;

      if (m instanceof Right) {
        return new Right(f(m.value0));
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 38, column 1 - line 38, column 52): " + [m.constructor.name]);
    };
  });

  var either = function either(v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }

        ;

        if (v2 instanceof Right) {
          return v1(v2.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  var bifunctorEither = new Data_Bifunctor.Bifunctor(function (v) {
    return function (v1) {
      return function (v2) {
        if (v2 instanceof Left) {
          return new Left(v(v2.value0));
        }

        ;

        if (v2 instanceof Right) {
          return new Right(v1(v2.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Either (line 46, column 1 - line 48, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  });
  var applyEither = new Control_Apply.Apply(function () {
    return functorEither;
  }, function (v) {
    return function (v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }

      ;

      if (v instanceof Right) {
        return Data_Functor.map(functorEither)(v.value0)(v1);
      }

      ;
      throw new Error("Failed pattern match at Data.Either (line 82, column 1 - line 84, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var applicativeEither = new Control_Applicative.Applicative(function () {
    return applyEither;
  }, Right.create);
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["either"] = either;
  exports["note"] = note;
  exports["functorEither"] = functorEither;
  exports["bifunctorEither"] = bifunctorEither;
  exports["applyEither"] = applyEither;
  exports["applicativeEither"] = applicativeEither;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Error.Class"] = $PS["Control.Monad.Error.Class"] || {};
  var exports = $PS["Control.Monad.Error.Class"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];

  var MonadThrow = function MonadThrow(Monad0, throwError) {
    this.Monad0 = Monad0;
    this.throwError = throwError;
  };

  var MonadError = function MonadError(MonadThrow0, catchError) {
    this.MonadThrow0 = MonadThrow0;
    this.catchError = catchError;
  };

  var throwError = function throwError(dict) {
    return dict.throwError;
  };

  var catchError = function catchError(dict) {
    return dict.catchError;
  };

  var $$try = function $$try(dictMonadError) {
    return function (a) {
      return catchError(dictMonadError)(Data_Functor.map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Data_Either.Right.create)(a))(function () {
        var $17 = Control_Applicative.pure(dictMonadError.MonadThrow0().Monad0().Applicative0());
        return function ($18) {
          return $17(Data_Either.Left.create($18));
        };
      }());
    };
  };

  exports["throwError"] = throwError;
  exports["MonadThrow"] = MonadThrow;
  exports["MonadError"] = MonadError;
  exports["try"] = $$try;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Trans.Class"] = $PS["Control.Monad.Trans.Class"] || {};
  var exports = $PS["Control.Monad.Trans.Class"];

  var MonadTrans = function MonadTrans(lift) {
    this.lift = lift;
  };

  var lift = function lift(dict) {
    return dict.lift;
  };

  exports["lift"] = lift;
  exports["MonadTrans"] = MonadTrans;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Except.Trans"] = $PS["Control.Monad.Except.Trans"] || {};
  var exports = $PS["Control.Monad.Except.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];

  var ExceptT = function ExceptT(x) {
    return x;
  };

  var runExceptT = function runExceptT(v) {
    return v;
  };

  var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
      return Control_Bind.bind(dictMonad.Bind1())(m)(function (a) {
        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(a));
      });
    };
  });

  var mapExceptT = function mapExceptT(f) {
    return function (v) {
      return f(v);
    };
  };

  var functorExceptT = function functorExceptT(dictFunctor) {
    return new Data_Functor.Functor(function (f) {
      return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
    });
  };

  var monadExceptT = function monadExceptT(dictMonad) {
    return new Control_Monad.Monad(function () {
      return applicativeExceptT(dictMonad);
    }, function () {
      return bindExceptT(dictMonad);
    });
  };

  var bindExceptT = function bindExceptT(dictMonad) {
    return new Control_Bind.Bind(function () {
      return applyExceptT(dictMonad);
    }, function (v) {
      return function (k) {
        return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either(function () {
          var $90 = Control_Applicative.pure(dictMonad.Applicative0());
          return function ($91) {
            return $90(Data_Either.Left.create($91));
          };
        }())(function (a) {
          var v1 = k(a);
          return v1;
        }));
      };
    });
  };

  var applyExceptT = function applyExceptT(dictMonad) {
    return new Control_Apply.Apply(function () {
      return functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    }, Control_Monad.ap(monadExceptT(dictMonad)));
  };

  var applicativeExceptT = function applicativeExceptT(dictMonad) {
    return new Control_Applicative.Applicative(function () {
      return applyExceptT(dictMonad);
    }, function () {
      var $92 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($93) {
        return ExceptT($92(Data_Either.Right.create($93)));
      };
    }());
  };

  var monadThrowExceptT = function monadThrowExceptT(dictMonad) {
    return new Control_Monad_Error_Class.MonadThrow(function () {
      return monadExceptT(dictMonad);
    }, function () {
      var $102 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($103) {
        return ExceptT($102(Data_Either.Left.create($103)));
      };
    }());
  };

  exports["ExceptT"] = ExceptT;
  exports["runExceptT"] = runExceptT;
  exports["mapExceptT"] = mapExceptT;
  exports["functorExceptT"] = functorExceptT;
  exports["applicativeExceptT"] = applicativeExceptT;
  exports["bindExceptT"] = bindExceptT;
  exports["monadExceptT"] = monadExceptT;
  exports["monadTransExceptT"] = monadTransExceptT;
  exports["monadThrowExceptT"] = monadThrowExceptT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Identity"] = $PS["Data.Identity"] || {};
  var exports = $PS["Data.Identity"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Identity = function Identity(x) {
    return x;
  };

  var newtypeIdentity = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Identity);
  var functorIdentity = new Data_Functor.Functor(function (f) {
    return function (m) {
      return f(m);
    };
  });
  var applyIdentity = new Control_Apply.Apply(function () {
    return functorIdentity;
  }, function (v) {
    return function (v1) {
      return v(v1);
    };
  });
  var bindIdentity = new Control_Bind.Bind(function () {
    return applyIdentity;
  }, function (v) {
    return function (f) {
      return f(v);
    };
  });
  var applicativeIdentity = new Control_Applicative.Applicative(function () {
    return applyIdentity;
  }, Identity);
  var monadIdentity = new Control_Monad.Monad(function () {
    return applicativeIdentity;
  }, function () {
    return bindIdentity;
  });
  exports["Identity"] = Identity;
  exports["newtypeIdentity"] = newtypeIdentity;
  exports["functorIdentity"] = functorIdentity;
  exports["applicativeIdentity"] = applicativeIdentity;
  exports["monadIdentity"] = monadIdentity;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Except"] = $PS["Control.Monad.Except"] || {};
  var exports = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Newtype = $PS["Data.Newtype"];

  var runExcept = function () {
    var $0 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
    return function ($1) {
      return $0(Control_Monad_Except_Trans.runExceptT($1));
    };
  }();

  var mapExcept = function mapExcept(f) {
    return Control_Monad_Except_Trans.mapExceptT(function () {
      var $2 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
      return function ($3) {
        return Data_Identity.Identity(f($2($3)));
      };
    }());
  };

  exports["runExcept"] = runExcept;
  exports["mapExcept"] = mapExcept;
})(PS);

(function (exports) {
  "use strict";

  function id(x) {
    return x;
  }

  exports.fromObject = id;

  exports.stringify = function (j) {
    return JSON.stringify(j);
  };

  function isArray(a) {
    return Object.prototype.toString.call(a) === "[object Array]";
  }
})(PS["Data.Argonaut.Core"] = PS["Data.Argonaut.Core"] || {});

(function (exports) {
  "use strict";

  exports._copyST = function (m) {
    return function () {
      var r = {};

      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }

      return r;
    };
  };

  exports.empty = {};

  exports.runST = function (f) {
    return f();
  };

  exports._fmapObject = function (m0, f) {
    var m = {};

    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(m0[k]);
      }
    }

    return m;
  };

  exports._mapWithKey = function (m0, f) {
    var m = {};

    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(k)(m0[k]);
      }
    }

    return m;
  };

  exports._foldM = function (bind) {
    return function (f) {
      return function (mz) {
        return function (m) {
          var acc = mz;

          function g(k) {
            return function (z) {
              return f(z)(k)(m[k]);
            };
          }

          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind(acc)(g(k));
            }
          }

          return acc;
        };
      };
    };
  };

  exports._lookup = function (no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  };

  exports._lookupST = function (no, yes, k, m) {
    return function () {
      return k in m ? yes(m[k]) : no;
    };
  };

  function toArrayWithKey(f) {
    return function (m) {
      var r = [];

      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }

      return r;
    };
  }

  exports.toArrayWithKey = toArrayWithKey;
  exports.keys = Object.keys || toArrayWithKey(function (k) {
    return function () {
      return k;
    };
  });
})(PS["Foreign.Object"] = PS["Foreign.Object"] || {});

(function (exports) {
  "use strict";

  exports.map_ = function (f) {
    return function (a) {
      return function () {
        return f(a());
      };
    };
  };

  exports.pure_ = function (a) {
    return function () {
      return a;
    };
  };

  exports.bind_ = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Control.Monad.ST.Internal"] = PS["Control.Monad.ST.Internal"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.ST.Internal"] = $PS["Control.Monad.ST.Internal"] || {};
  var exports = $PS["Control.Monad.ST.Internal"];
  var $foreign = $PS["Control.Monad.ST.Internal"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];
  var functorST = new Data_Functor.Functor($foreign.map_);
  var monadST = new Control_Monad.Monad(function () {
    return applicativeST;
  }, function () {
    return bindST;
  });
  var bindST = new Control_Bind.Bind(function () {
    return applyST;
  }, $foreign.bind_);
  var applyST = new Control_Apply.Apply(function () {
    return functorST;
  }, Control_Monad.ap(monadST));
  var applicativeST = new Control_Applicative.Applicative(function () {
    return applyST;
  }, $foreign.pure_);
  exports["applicativeST"] = applicativeST;
})(PS);

(function (exports) {
  "use strict";

  exports.foldrArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;

        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }

        return acc;
      };
    };
  };

  exports.foldlArray = function (f) {
    return function (init) {
      return function (xs) {
        var acc = init;
        var len = xs.length;

        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }

        return acc;
      };
    };
  };
})(PS["Data.Foldable"] = PS["Data.Foldable"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Foldable"] = $PS["Data.Foldable"] || {};
  var exports = $PS["Data.Foldable"];
  var $foreign = $PS["Data.Foldable"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Monoid_Disj = $PS["Data.Monoid.Disj"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Unit = $PS["Data.Unit"];

  var Foldable = function Foldable(foldMap, foldl, foldr) {
    this.foldMap = foldMap;
    this.foldl = foldl;
    this.foldr = foldr;
  };

  var foldr = function foldr(dict) {
    return dict.foldr;
  };

  var $$null = function $$null(dictFoldable) {
    return foldr(dictFoldable)(function (v) {
      return function (v1) {
        return false;
      };
    })(true);
  };

  var traverse_ = function traverse_(dictApplicative) {
    return function (dictFoldable) {
      return function (f) {
        return foldr(dictFoldable)(function () {
          var $197 = Control_Apply.applySecond(dictApplicative.Apply0());
          return function ($198) {
            return $197(f($198));
          };
        }())(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
      };
    };
  };

  var for_ = function for_(dictApplicative) {
    return function (dictFoldable) {
      return Data_Function.flip(traverse_(dictApplicative)(dictFoldable));
    };
  };

  var foldl = function foldl(dict) {
    return dict.foldl;
  };

  var intercalate = function intercalate(dictFoldable) {
    return function (dictMonoid) {
      return function (sep) {
        return function (xs) {
          var go = function go(v) {
            return function (x) {
              if (v.init) {
                return {
                  init: false,
                  acc: x
                };
              }

              ;
              return {
                init: false,
                acc: Data_Semigroup.append(dictMonoid.Semigroup0())(v.acc)(Data_Semigroup.append(dictMonoid.Semigroup0())(sep)(x))
              };
            };
          };

          return foldl(dictFoldable)(go)({
            init: true,
            acc: Data_Monoid.mempty(dictMonoid)
          })(xs).acc;
        };
      };
    };
  };

  var sum = function sum(dictFoldable) {
    return function (dictSemiring) {
      return foldl(dictFoldable)(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
    };
  };

  var foldableMaybe = new Foldable(function (dictMonoid) {
    return function (f) {
      return function (v) {
        if (v instanceof Data_Maybe.Nothing) {
          return Data_Monoid.mempty(dictMonoid);
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return f(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [f.constructor.name, v.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return z;
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return v(z)(v1.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return z;
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return v(v1.value0)(z);
        }

        ;
        throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  });

  var foldMapDefaultR = function foldMapDefaultR(dictFoldable) {
    return function (dictMonoid) {
      return function (f) {
        return foldr(dictFoldable)(function (x) {
          return function (acc) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(Data_Monoid.mempty(dictMonoid));
      };
    };
  };

  var foldableArray = new Foldable(function (dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }, $foreign.foldlArray, $foreign.foldrArray);

  var foldMap = function foldMap(dict) {
    return dict.foldMap;
  };

  var fold = function fold(dictFoldable) {
    return function (dictMonoid) {
      return foldMap(dictFoldable)(dictMonoid)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  var any = function any(dictFoldable) {
    return function (dictHeytingAlgebra) {
      return Data_Newtype.alaF(Data_Functor.functorFn)(Data_Functor.functorFn)(Data_Newtype.newtypeDisj)(Data_Newtype.newtypeDisj)(Data_Monoid_Disj.Disj)(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
    };
  };

  exports["Foldable"] = Foldable;
  exports["foldr"] = foldr;
  exports["foldl"] = foldl;
  exports["foldMap"] = foldMap;
  exports["fold"] = fold;
  exports["traverse_"] = traverse_;
  exports["for_"] = for_;
  exports["intercalate"] = intercalate;
  exports["any"] = any;
  exports["sum"] = sum;
  exports["null"] = $$null;
  exports["foldableArray"] = foldableArray;
  exports["foldableMaybe"] = foldableMaybe;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.FoldableWithIndex"] = $PS["Data.FoldableWithIndex"] || {};
  var exports = $PS["Data.FoldableWithIndex"];

  var FoldableWithIndex = function FoldableWithIndex(Foldable0, foldMapWithIndex, foldlWithIndex, foldrWithIndex) {
    this.Foldable0 = Foldable0;
    this.foldMapWithIndex = foldMapWithIndex;
    this.foldlWithIndex = foldlWithIndex;
    this.foldrWithIndex = foldrWithIndex;
  };

  exports["FoldableWithIndex"] = FoldableWithIndex;
})(PS);

(function (exports) {
  "use strict";

  exports.runFn2 = function (fn) {
    return function (a) {
      return function (b) {
        return fn(a, b);
      };
    };
  };

  exports.runFn3 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return fn(a, b, c);
        };
      };
    };
  };

  exports.runFn4 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };

  exports.runFn6 = function (fn) {
    return function (a) {
      return function (b) {
        return function (c) {
          return function (d) {
            return function (e) {
              return function (f) {
                return fn(a, b, c, d, e, f);
              };
            };
          };
        };
      };
    };
  };
})(PS["Data.Function.Uncurried"] = PS["Data.Function.Uncurried"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Function.Uncurried"] = $PS["Data.Function.Uncurried"] || {};
  var exports = $PS["Data.Function.Uncurried"];
  var $foreign = $PS["Data.Function.Uncurried"];
  exports["runFn2"] = $foreign.runFn2;
  exports["runFn3"] = $foreign.runFn3;
  exports["runFn4"] = $foreign.runFn4;
  exports["runFn6"] = $foreign.runFn6;
})(PS);

(function (exports) {
  "use strict";

  exports.mapWithIndexArray = function (f) {
    return function (xs) {
      var l = xs.length;
      var result = Array(l);

      for (var i = 0; i < l; i++) {
        result[i] = f(i)(xs[i]);
      }

      return result;
    };
  };
})(PS["Data.FunctorWithIndex"] = PS["Data.FunctorWithIndex"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.FunctorWithIndex"] = $PS["Data.FunctorWithIndex"] || {};
  var exports = $PS["Data.FunctorWithIndex"];
  var $foreign = $PS["Data.FunctorWithIndex"];
  var Data_Functor = $PS["Data.Functor"];

  var FunctorWithIndex = function FunctorWithIndex(Functor0, mapWithIndex) {
    this.Functor0 = Functor0;
    this.mapWithIndex = mapWithIndex;
  };

  var mapWithIndex = function mapWithIndex(dict) {
    return dict.mapWithIndex;
  };

  var functorWithIndexArray = new FunctorWithIndex(function () {
    return Data_Functor.functorArray;
  }, $foreign.mapWithIndexArray);
  exports["FunctorWithIndex"] = FunctorWithIndex;
  exports["mapWithIndex"] = mapWithIndex;
  exports["functorWithIndexArray"] = functorWithIndexArray;
})(PS);

(function (exports) {
  "use strict"; // jshint maxparams: 3

  exports.traverseArrayImpl = function () {
    function array1(a) {
      return [a];
    }

    function array2(a) {
      return function (b) {
        return [a, b];
      };
    }

    function array3(a) {
      return function (b) {
        return function (c) {
          return [a, b, c];
        };
      };
    }

    function concat2(xs) {
      return function (ys) {
        return xs.concat(ys);
      };
    }

    return function (apply) {
      return function (map) {
        return function (pure) {
          return function (f) {
            return function (array) {
              function go(bot, top) {
                switch (top - bot) {
                  case 0:
                    return pure([]);

                  case 1:
                    return map(array1)(f(array[bot]));

                  case 2:
                    return apply(map(array2)(f(array[bot])))(f(array[bot + 1]));

                  case 3:
                    return apply(apply(map(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));

                  default:
                    // This slightly tricky pivot selection aims to produce two
                    // even-length partitions where possible.
                    var pivot = bot + Math.floor((top - bot) / 4) * 2;
                    return apply(map(concat2)(go(bot, pivot)))(go(pivot, top));
                }
              }

              return go(0, array.length);
            };
          };
        };
      };
    };
  }();
})(PS["Data.Traversable"] = PS["Data.Traversable"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Traversable.Accum.Internal"] = $PS["Data.Traversable.Accum.Internal"] || {};
  var exports = $PS["Data.Traversable.Accum.Internal"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Functor = $PS["Data.Functor"];

  var stateL = function stateL(v) {
    return v;
  };

  var functorStateL = new Data_Functor.Functor(function (f) {
    return function (k) {
      return function (s) {
        var v = stateL(k)(s);
        return {
          accum: v.accum,
          value: f(v.value)
        };
      };
    };
  });
  var applyStateL = new Control_Apply.Apply(function () {
    return functorStateL;
  }, function (f) {
    return function (x) {
      return function (s) {
        var v = stateL(f)(s);
        var v1 = stateL(x)(v.accum);
        return {
          accum: v1.accum,
          value: v.value(v1.value)
        };
      };
    };
  });
  var applicativeStateL = new Control_Applicative.Applicative(function () {
    return applyStateL;
  }, function (a) {
    return function (s) {
      return {
        accum: s,
        value: a
      };
    };
  });
  exports["stateL"] = stateL;
  exports["applicativeStateL"] = applicativeStateL;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Traversable"] = $PS["Data.Traversable"] || {};
  var exports = $PS["Data.Traversable"];
  var $foreign = $PS["Data.Traversable"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Traversable_Accum_Internal = $PS["Data.Traversable.Accum.Internal"];

  var Traversable = function Traversable(Foldable1, Functor0, sequence, traverse) {
    this.Foldable1 = Foldable1;
    this.Functor0 = Functor0;
    this.sequence = sequence;
    this.traverse = traverse;
  };

  var traverse = function traverse(dict) {
    return dict.traverse;
  };

  var traversableMaybe = new Traversable(function () {
    return Data_Foldable.foldableMaybe;
  }, function () {
    return Data_Maybe.functorMaybe;
  }, function (dictApplicative) {
    return function (v) {
      if (v instanceof Data_Maybe.Nothing) {
        return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
      }

      ;

      if (v instanceof Data_Maybe.Just) {
        return Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Maybe.Just.create)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Data.Traversable (line 86, column 1 - line 90, column 33): " + [v.constructor.name]);
    };
  }, function (dictApplicative) {
    return function (v) {
      return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
          return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
        }

        ;

        if (v1 instanceof Data_Maybe.Just) {
          return Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Maybe.Just.create)(v(v1.value0));
        }

        ;
        throw new Error("Failed pattern match at Data.Traversable (line 86, column 1 - line 90, column 33): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  });

  var sequenceDefault = function sequenceDefault(dictTraversable) {
    return function (dictApplicative) {
      return traverse(dictTraversable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  var traversableArray = new Traversable(function () {
    return Data_Foldable.foldableArray;
  }, function () {
    return Data_Functor.functorArray;
  }, function (dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
  }, function (dictApplicative) {
    return $foreign.traverseArrayImpl(Control_Apply.apply(dictApplicative.Apply0()))(Data_Functor.map(dictApplicative.Apply0().Functor0()))(Control_Applicative.pure(dictApplicative));
  });

  var mapAccumL = function mapAccumL(dictTraversable) {
    return function (f) {
      return function (s0) {
        return function (xs) {
          return Data_Traversable_Accum_Internal.stateL(traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateL)(function (a) {
            return function (s) {
              return f(s)(a);
            };
          })(xs))(s0);
        };
      };
    };
  };

  var scanl = function scanl(dictTraversable) {
    return function (f) {
      return function (b0) {
        return function (xs) {
          return mapAccumL(dictTraversable)(function (b) {
            return function (a) {
              var b$prime = f(b)(a);
              return {
                accum: b$prime,
                value: b$prime
              };
            };
          })(b0)(xs).value;
        };
      };
    };
  };

  var $$for = function $$for(dictApplicative) {
    return function (dictTraversable) {
      return function (x) {
        return function (f) {
          return traverse(dictTraversable)(dictApplicative)(f)(x);
        };
      };
    };
  };

  exports["Traversable"] = Traversable;
  exports["traverse"] = traverse;
  exports["for"] = $$for;
  exports["scanl"] = scanl;
  exports["traversableArray"] = traversableArray;
  exports["traversableMaybe"] = traversableMaybe;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.TraversableWithIndex"] = $PS["Data.TraversableWithIndex"] || {};
  var exports = $PS["Data.TraversableWithIndex"];
  var Data_Function = $PS["Data.Function"];

  var TraversableWithIndex = function TraversableWithIndex(FoldableWithIndex1, FunctorWithIndex0, Traversable2, traverseWithIndex) {
    this.FoldableWithIndex1 = FoldableWithIndex1;
    this.FunctorWithIndex0 = FunctorWithIndex0;
    this.Traversable2 = Traversable2;
    this.traverseWithIndex = traverseWithIndex;
  };

  var traverseWithIndex = function traverseWithIndex(dict) {
    return dict.traverseWithIndex;
  };

  var forWithIndex = function forWithIndex(dictApplicative) {
    return function (dictTraversableWithIndex) {
      return Data_Function.flip(traverseWithIndex(dictTraversableWithIndex)(dictApplicative));
    };
  };

  exports["TraversableWithIndex"] = TraversableWithIndex;
  exports["traverseWithIndex"] = traverseWithIndex;
  exports["forWithIndex"] = forWithIndex;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Tuple"] = $PS["Data.Tuple"] || {};
  var exports = $PS["Data.Tuple"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];

  var Tuple = function () {
    function Tuple(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Tuple.create = function (value0) {
      return function (value1) {
        return new Tuple(value0, value1);
      };
    };

    return Tuple;
  }();

  var uncurry = function uncurry(f) {
    return function (v) {
      return f(v.value0)(v.value1);
    };
  };

  var snd = function snd(v) {
    return v.value1;
  };

  var functorTuple = new Data_Functor.Functor(function (f) {
    return function (m) {
      return new Tuple(m.value0, f(m.value1));
    };
  });

  var fst = function fst(v) {
    return v.value0;
  };

  var eqTuple = function eqTuple(dictEq) {
    return function (dictEq1) {
      return new Data_Eq.Eq(function (x) {
        return function (y) {
          return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
        };
      });
    };
  };

  var ordTuple = function ordTuple(dictOrd) {
    return function (dictOrd1) {
      return new Data_Ord.Ord(function () {
        return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
      }, function (x) {
        return function (y) {
          var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);

          if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
          }

          ;

          if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
          }

          ;
          return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
        };
      });
    };
  };

  var bifunctorTuple = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        return new Tuple(f(v.value0), g(v.value1));
      };
    };
  });
  exports["Tuple"] = Tuple;
  exports["fst"] = fst;
  exports["snd"] = snd;
  exports["uncurry"] = uncurry;
  exports["ordTuple"] = ordTuple;
  exports["functorTuple"] = functorTuple;
  exports["bifunctorTuple"] = bifunctorTuple;
})(PS);

(function (exports) {
  "use strict";

  exports["new"] = function () {
    return {};
  };

  exports.poke = function (k) {
    return function (v) {
      return function (m) {
        return function () {
          m[k] = v;
          return m;
        };
      };
    };
  };
})(PS["Foreign.Object.ST"] = PS["Foreign.Object.ST"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Foreign.Object.ST"] = $PS["Foreign.Object.ST"] || {};
  var exports = $PS["Foreign.Object.ST"];
  var $foreign = $PS["Foreign.Object.ST"];
  exports["new"] = $foreign["new"];
  exports["poke"] = $foreign.poke;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Foreign.Object"] = $PS["Foreign.Object"] || {};
  var exports = $PS["Foreign.Object"];
  var $foreign = $PS["Foreign.Object"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_ST_Internal = $PS["Control.Monad.ST.Internal"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_FoldableWithIndex = $PS["Data.FoldableWithIndex"];
  var Data_Function = $PS["Data.Function"];
  var Data_Function_Uncurried = $PS["Data.Function.Uncurried"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_TraversableWithIndex = $PS["Data.TraversableWithIndex"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Foreign_Object_ST = $PS["Foreign.Object.ST"];
  var values = $foreign.toArrayWithKey(function (v) {
    return function (v1) {
      return v1;
    };
  });
  var thawST = $foreign["_copyST"];

  var mutate = function mutate(f) {
    return function (m) {
      return $foreign.runST(function __do() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };

  var mapWithKey = function mapWithKey(f) {
    return function (m) {
      return $foreign["_mapWithKey"](m, f);
    };
  };

  var lookup = Data_Function_Uncurried.runFn4($foreign["_lookup"])(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);

  var insert = function insert(k) {
    return function (v) {
      return mutate(Foreign_Object_ST.poke(k)(v));
    };
  };

  var functorObject = new Data_Functor.Functor(function (f) {
    return function (m) {
      return $foreign["_fmapObject"](m, f);
    };
  });
  var functorWithIndexObject = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorObject;
  }, mapWithKey);

  var fromFoldableWith = function fromFoldableWith(dictFoldable) {
    return function (f) {
      return function (l) {
        return $foreign.runST(function __do() {
          var s = Foreign_Object_ST["new"]();
          Data_Foldable.for_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(l)(function (v) {
            return function __do() {
              var v$prime = $foreign["_lookupST"](v.value1, f(v.value1), v.value0, s)();
              return Foreign_Object_ST.poke(v.value0)(v$prime)(s)();
            };
          })();
          return s;
        });
      };
    };
  };

  var fold = $foreign["_foldM"](Data_Function.applyFlipped);

  var foldMap = function foldMap(dictMonoid) {
    return function (f) {
      return fold(function (acc) {
        return function (k) {
          return function (v) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(k)(v));
          };
        };
      })(Data_Monoid.mempty(dictMonoid));
    };
  };

  var foldableObject = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return foldMap(dictMonoid)(Data_Function["const"](f));
    };
  }, function (f) {
    return fold(function (z) {
      return function (v) {
        return f(z);
      };
    });
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldr(Data_Foldable.foldableArray)(f)(z)(values(m));
      };
    };
  });
  var foldableWithIndexObject = new Data_FoldableWithIndex.FoldableWithIndex(function () {
    return foldableObject;
  }, function (dictMonoid) {
    return foldMap(dictMonoid);
  }, function (f) {
    return fold(Data_Function.flip(f));
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldr(Data_Foldable.foldableArray)(Data_Tuple.uncurry(f))(z)($foreign.toArrayWithKey(Data_Tuple.Tuple.create)(m));
      };
    };
  });
  var traversableWithIndexObject = new Data_TraversableWithIndex.TraversableWithIndex(function () {
    return foldableWithIndexObject;
  }, function () {
    return functorWithIndexObject;
  }, function () {
    return traversableObject;
  }, function (dictApplicative) {
    return function (f) {
      return function (ms) {
        return fold(function (acc) {
          return function (k) {
            return function (v) {
              return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Function.flip(insert(k)))(acc))(f(k)(v));
            };
          };
        })(Control_Applicative.pure(dictApplicative)($foreign.empty))(ms);
      };
    };
  });
  var traversableObject = new Data_Traversable.Traversable(function () {
    return foldableObject;
  }, function () {
    return functorObject;
  }, function (dictApplicative) {
    return Data_Traversable.traverse(traversableObject)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
  }, function (dictApplicative) {
    var $43 = Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexObject)(dictApplicative);
    return function ($44) {
      return $43(Data_Function["const"]($44));
    };
  });
  exports["lookup"] = lookup;
  exports["fromFoldableWith"] = fromFoldableWith;
  exports["traversableWithIndexObject"] = traversableWithIndexObject;
  exports["empty"] = $foreign.empty;
  exports["keys"] = $foreign.keys;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Argonaut.Core"] = $PS["Data.Argonaut.Core"] || {};
  var exports = $PS["Data.Argonaut.Core"];
  var $foreign = $PS["Data.Argonaut.Core"];
  var Foreign_Object = $PS["Foreign.Object"];
  var jsonEmptyObject = $foreign.fromObject(Foreign_Object.empty);
  exports["jsonEmptyObject"] = jsonEmptyObject;
  exports["stringify"] = $foreign.stringify;
})(PS);

(function (exports) {
  "use strict";

  exports._jsonParser = function (fail, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail(e.message);
    }
  };
})(PS["Data.Argonaut.Parser"] = PS["Data.Argonaut.Parser"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Argonaut.Parser"] = $PS["Data.Argonaut.Parser"] || {};
  var exports = $PS["Data.Argonaut.Parser"];
  var $foreign = $PS["Data.Argonaut.Parser"];
  var Data_Either = $PS["Data.Either"];

  var jsonParser = function jsonParser(j) {
    return $foreign["_jsonParser"](Data_Either.Left.create, Data_Either.Right.create, j);
  };

  exports["jsonParser"] = jsonParser;
})(PS);

(function (exports) {
  "use strict"; //------------------------------------------------------------------------------
  // Array creation --------------------------------------------------------------
  //------------------------------------------------------------------------------

  exports.range = function (start) {
    return function (end) {
      var step = start > end ? -1 : 1;
      var result = new Array(step * (end - start) + 1);
      var i = start,
          n = 0;

      while (i !== end) {
        result[n++] = i;
        i += step;
      }

      result[n] = i;
      return result;
    };
  };

  exports.fromFoldableImpl = function () {
    function Cons(head, tail) {
      this.head = head;
      this.tail = tail;
    }

    var emptyList = {};

    function curryCons(head) {
      return function (tail) {
        return new Cons(head, tail);
      };
    }

    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;

      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }

      return result;
    }

    return function (foldr) {
      return function (xs) {
        return listToArray(foldr(curryCons)(emptyList)(xs));
      };
    };
  }(); //------------------------------------------------------------------------------
  // Array size ------------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.length = function (xs) {
    return xs.length;
  }; //------------------------------------------------------------------------------
  // Extending arrays ------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.cons = function (e) {
    return function (l) {
      return [e].concat(l);
    };
  };

  exports.snoc = function (l) {
    return function (e) {
      var l1 = l.slice();
      l1.push(e);
      return l1;
    };
  }; //------------------------------------------------------------------------------
  // Non-indexed reads -----------------------------------------------------------
  //------------------------------------------------------------------------------


  exports["uncons'"] = function (empty) {
    return function (next) {
      return function (xs) {
        return xs.length === 0 ? empty({}) : next(xs[0])(xs.slice(1));
      };
    };
  }; //------------------------------------------------------------------------------
  // Indexed operations ----------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.indexImpl = function (just) {
    return function (nothing) {
      return function (xs) {
        return function (i) {
          return i < 0 || i >= xs.length ? nothing : just(xs[i]);
        };
      };
    };
  };

  exports.findIndexImpl = function (just) {
    return function (nothing) {
      return function (f) {
        return function (xs) {
          for (var i = 0, l = xs.length; i < l; i++) {
            if (f(xs[i])) return just(i);
          }

          return nothing;
        };
      };
    };
  };

  exports.findLastIndexImpl = function (just) {
    return function (nothing) {
      return function (f) {
        return function (xs) {
          for (var i = xs.length - 1; i >= 0; i--) {
            if (f(xs[i])) return just(i);
          }

          return nothing;
        };
      };
    };
  }; //------------------------------------------------------------------------------
  // Subarrays -------------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.slice = function (s) {
    return function (e) {
      return function (l) {
        return l.slice(s, e);
      };
    };
  };

  exports.take = function (n) {
    return function (l) {
      return n < 1 ? [] : l.slice(0, n);
    };
  };

  exports.drop = function (n) {
    return function (l) {
      return n < 1 ? l : l.slice(n);
    };
  }; //------------------------------------------------------------------------------
  // Zipping ---------------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.zipWith = function (f) {
    return function (xs) {
      return function (ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);

        for (var i = 0; i < l; i++) {
          result[i] = f(xs[i])(ys[i]);
        }

        return result;
      };
    };
  }; //------------------------------------------------------------------------------
  // Partial ---------------------------------------------------------------------
  //------------------------------------------------------------------------------


  exports.unsafeIndexImpl = function (xs) {
    return function (n) {
      return xs[n];
    };
  };
})(PS["Data.Array"] = PS["Data.Array"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Array"] = $PS["Data.Array"] || {};
  var exports = $PS["Data.Array"];
  var $foreign = $PS["Data.Array"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Maybe = $PS["Data.Maybe"];

  var unsafeIndex = function unsafeIndex(dictPartial) {
    return $foreign.unsafeIndexImpl;
  };

  var uncons = $foreign["uncons'"](Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
      return new Data_Maybe.Just({
        head: x,
        tail: xs
      });
    };
  });

  var takeEnd = function takeEnd(n) {
    return function (xs) {
      return $foreign.drop($foreign.length(xs) - n | 0)(xs);
    };
  };

  var singleton = function singleton(a) {
    return [a];
  };

  var mapWithIndex = function mapWithIndex(f) {
    return function (xs) {
      return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1 | 0))(xs);
    };
  };

  var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

  var last = function last(xs) {
    return index(xs)($foreign.length(xs) - 1 | 0);
  };

  var head = function head(xs) {
    return index(xs)(0);
  };

  var fromFoldable = function fromFoldable(dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
  };

  var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));

  var mapMaybe = function mapMaybe(f) {
    return concatMap(function () {
      var $94 = Data_Maybe.maybe([])(singleton);
      return function ($95) {
        return $94(f($95));
      };
    }());
  };

  var catMaybes = mapMaybe(Control_Category.identity(Control_Category.categoryFn));
  exports["fromFoldable"] = fromFoldable;
  exports["singleton"] = singleton;
  exports["head"] = head;
  exports["last"] = last;
  exports["uncons"] = uncons;
  exports["index"] = index;
  exports["findIndex"] = findIndex;
  exports["findLastIndex"] = findLastIndex;
  exports["mapMaybe"] = mapMaybe;
  exports["catMaybes"] = catMaybes;
  exports["mapWithIndex"] = mapWithIndex;
  exports["takeEnd"] = takeEnd;
  exports["unsafeIndex"] = unsafeIndex;
  exports["length"] = $foreign.length;
  exports["cons"] = $foreign.cons;
  exports["snoc"] = $foreign.snoc;
  exports["slice"] = $foreign.slice;
  exports["take"] = $foreign.take;
  exports["drop"] = $foreign.drop;
  exports["zipWith"] = $foreign.zipWith;
})(PS);

(function (exports) {
  "use strict";

  exports.toLower = function (s) {
    return s.toLowerCase();
  };

  exports.joinWith = function (s) {
    return function (xs) {
      return xs.join(s);
    };
  };
})(PS["Data.String.Common"] = PS["Data.String.Common"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.String.Common"] = $PS["Data.String.Common"] || {};
  var exports = $PS["Data.String.Common"];
  var $foreign = $PS["Data.String.Common"];

  var $$null = function $$null(s) {
    return s === "";
  };

  exports["null"] = $$null;
  exports["toLower"] = $foreign.toLower;
  exports["joinWith"] = $foreign.joinWith;
})(PS);

(function (exports) {
  /* globals exports */
  "use strict";

  exports.nan = NaN;
  exports.isNaN = isNaN;
  exports.infinity = Infinity;
  exports.isFinite = isFinite;
  exports.readFloat = parseFloat;

  var encdecURI = function encdecURI(encdec) {
    return function (fail, succ, s) {
      try {
        return succ(encdec(s));
      } catch (e) {
        return fail(e.message);
      }
    };
  };

  exports._encodeURIComponent = encdecURI(encodeURIComponent);
})(PS["Global"] = PS["Global"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Global"] = $PS["Global"] || {};
  var exports = $PS["Global"];
  var $foreign = $PS["Global"];
  var Data_Function = $PS["Data.Function"];
  var Data_Maybe = $PS["Data.Maybe"];

  var $$encodeURIComponent = function $$encodeURIComponent(s) {
    return $foreign["_encodeURIComponent"](Data_Function["const"](Data_Maybe.Nothing.value), Data_Maybe.Just.create, s);
  };

  exports["encodeURIComponent"] = $$encodeURIComponent;
  exports["nan"] = $foreign.nan;
  exports["isNaN"] = $foreign["isNaN"];
  exports["infinity"] = $foreign.infinity;
  exports["isFinite"] = $foreign["isFinite"];
  exports["readFloat"] = $foreign.readFloat;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.FormURLEncoded"] = $PS["Data.FormURLEncoded"] || {};
  var exports = $PS["Data.FormURLEncoded"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Global = $PS["Global"];

  var toArray = function toArray(v) {
    return v;
  };

  var encode = function () {
    var encodePart = function encodePart(v) {
      if (v.value1 instanceof Data_Maybe.Nothing) {
        return Global["encodeURIComponent"](v.value0);
      }

      ;

      if (v.value1 instanceof Data_Maybe.Just) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (key) {
          return function (val) {
            return key + ("=" + val);
          };
        })(Global["encodeURIComponent"](v.value0)))(Global["encodeURIComponent"](v.value1.value0));
      }

      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 18 - line 39, column 108): " + [v.constructor.name]);
    };

    var $19 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_String_Common.joinWith("&"));
    var $20 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(encodePart);
    return function ($21) {
      return $19($20(toArray($21)));
    };
  }();

  exports["encode"] = encode;
})(PS);

(function (exports) {
  "use strict";

  exports.showIntImpl = function (n) {
    return n.toString();
  };

  exports.showNumberImpl = function (n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(/[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
    function (c, i) {
      switch (c) {
        case "\"":
        case "\\":
          return "\\" + c;

        case "\x07":
          return "\\a";

        case "\b":
          return "\\b";

        case "\f":
          return "\\f";

        case "\n":
          return "\\n";

        case "\r":
          return "\\r";

        case "\t":
          return "\\t";

        case "\v":
          return "\\v";
      }

      var k = i + 1;
      var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty;
    }) + "\"";
  };

  exports.cons = function (head) {
    return function (tail) {
      return [head].concat(tail);
    };
  };

  exports.join = function (separator) {
    return function (xs) {
      return xs.join(separator);
    };
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var Type_Data_RowList = $PS["Type.Data.RowList"];

  var ShowRecordFields = function ShowRecordFields(showRecordFields) {
    this.showRecordFields = showRecordFields;
  };

  var Show = function Show(show) {
    this.show = show;
  };

  var showString = new Show($foreign.showStringImpl);
  var showRecordFieldsNil = new ShowRecordFields(function (v) {
    return function (v1) {
      return [];
    };
  });

  var showRecordFields = function showRecordFields(dict) {
    return dict.showRecordFields;
  };

  var showRecord = function showRecord(dictRowToList) {
    return function (dictShowRecordFields) {
      return new Show(function (record) {
        var v = showRecordFields(dictShowRecordFields)(Type_Data_RowList.RLProxy.value)(record);

        if (v.length === 0) {
          return "{}";
        }

        ;
        return $foreign.join(" ")(["{", $foreign.join(", ")(v), "}"]);
      });
    };
  };

  var showNumber = new Show($foreign.showNumberImpl);
  var showInt = new Show($foreign.showIntImpl);

  var show = function show(dict) {
    return dict.show;
  };

  var showRecordFieldsCons = function showRecordFieldsCons(dictIsSymbol) {
    return function (dictShowRecordFields) {
      return function (dictShow) {
        return new ShowRecordFields(function (v) {
          return function (record) {
            var tail = showRecordFields(dictShowRecordFields)(Type_Data_RowList.RLProxy.value)(record);
            var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value);
            var focus = Record_Unsafe.unsafeGet(key)(record);
            return $foreign.cons($foreign.join(": ")([key, show(dictShow)(focus)]))(tail);
          };
        });
      };
    };
  };

  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
  exports["showNumber"] = showNumber;
  exports["showString"] = showString;
  exports["showRecord"] = showRecord;
  exports["showRecordFieldsNil"] = showRecordFieldsNil;
  exports["showRecordFieldsCons"] = showRecordFieldsCons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.HTTP.Method"] = $PS["Data.HTTP.Method"] || {};
  var exports = $PS["Data.HTTP.Method"];
  var Data_Either = $PS["Data.Either"];
  var Data_Show = $PS["Data.Show"];

  var OPTIONS = function () {
    function OPTIONS() {}

    ;
    OPTIONS.value = new OPTIONS();
    return OPTIONS;
  }();

  var GET = function () {
    function GET() {}

    ;
    GET.value = new GET();
    return GET;
  }();

  var HEAD = function () {
    function HEAD() {}

    ;
    HEAD.value = new HEAD();
    return HEAD;
  }();

  var POST = function () {
    function POST() {}

    ;
    POST.value = new POST();
    return POST;
  }();

  var PUT = function () {
    function PUT() {}

    ;
    PUT.value = new PUT();
    return PUT;
  }();

  var DELETE = function () {
    function DELETE() {}

    ;
    DELETE.value = new DELETE();
    return DELETE;
  }();

  var TRACE = function () {
    function TRACE() {}

    ;
    TRACE.value = new TRACE();
    return TRACE;
  }();

  var CONNECT = function () {
    function CONNECT() {}

    ;
    CONNECT.value = new CONNECT();
    return CONNECT;
  }();

  var PROPFIND = function () {
    function PROPFIND() {}

    ;
    PROPFIND.value = new PROPFIND();
    return PROPFIND;
  }();

  var PROPPATCH = function () {
    function PROPPATCH() {}

    ;
    PROPPATCH.value = new PROPPATCH();
    return PROPPATCH;
  }();

  var MKCOL = function () {
    function MKCOL() {}

    ;
    MKCOL.value = new MKCOL();
    return MKCOL;
  }();

  var COPY = function () {
    function COPY() {}

    ;
    COPY.value = new COPY();
    return COPY;
  }();

  var MOVE = function () {
    function MOVE() {}

    ;
    MOVE.value = new MOVE();
    return MOVE;
  }();

  var LOCK = function () {
    function LOCK() {}

    ;
    LOCK.value = new LOCK();
    return LOCK;
  }();

  var UNLOCK = function () {
    function UNLOCK() {}

    ;
    UNLOCK.value = new UNLOCK();
    return UNLOCK;
  }();

  var PATCH = function () {
    function PATCH() {}

    ;
    PATCH.value = new PATCH();
    return PATCH;
  }();

  var unCustomMethod = function unCustomMethod(v) {
    return v;
  };

  var showMethod = new Data_Show.Show(function (v) {
    if (v instanceof OPTIONS) {
      return "OPTIONS";
    }

    ;

    if (v instanceof GET) {
      return "GET";
    }

    ;

    if (v instanceof HEAD) {
      return "HEAD";
    }

    ;

    if (v instanceof POST) {
      return "POST";
    }

    ;

    if (v instanceof PUT) {
      return "PUT";
    }

    ;

    if (v instanceof DELETE) {
      return "DELETE";
    }

    ;

    if (v instanceof TRACE) {
      return "TRACE";
    }

    ;

    if (v instanceof CONNECT) {
      return "CONNECT";
    }

    ;

    if (v instanceof PROPFIND) {
      return "PROPFIND";
    }

    ;

    if (v instanceof PROPPATCH) {
      return "PROPPATCH";
    }

    ;

    if (v instanceof MKCOL) {
      return "MKCOL";
    }

    ;

    if (v instanceof COPY) {
      return "COPY";
    }

    ;

    if (v instanceof MOVE) {
      return "MOVE";
    }

    ;

    if (v instanceof LOCK) {
      return "LOCK";
    }

    ;

    if (v instanceof UNLOCK) {
      return "UNLOCK";
    }

    ;

    if (v instanceof PATCH) {
      return "PATCH";
    }

    ;
    throw new Error("Failed pattern match at Data.HTTP.Method (line 40, column 1 - line 56, column 23): " + [v.constructor.name]);
  });
  var print = Data_Either.either(Data_Show.show(showMethod))(unCustomMethod);
  exports["GET"] = GET;
  exports["print"] = print;
})(PS);

(function (exports) {
  "use strict";

  exports.unfoldrArrayImpl = function (isNothing) {
    return function (fromJust) {
      return function (fst) {
        return function (snd) {
          return function (f) {
            return function (b) {
              var result = [];
              var value = b;

              while (true) {
                // eslint-disable-line no-constant-condition
                var maybe = f(value);
                if (isNothing(maybe)) return result;
                var tuple = fromJust(maybe);
                result.push(fst(tuple));
                value = snd(tuple);
              }
            };
          };
        };
      };
    };
  };
})(PS["Data.Unfoldable"] = PS["Data.Unfoldable"] || {});

(function (exports) {
  "use strict";

  exports.unfoldr1ArrayImpl = function (isNothing) {
    return function (fromJust) {
      return function (fst) {
        return function (snd) {
          return function (f) {
            return function (b) {
              var result = [];
              var value = b;

              while (true) {
                // eslint-disable-line no-constant-condition
                var tuple = f(value);
                result.push(fst(tuple));
                var maybe = snd(tuple);
                if (isNothing(maybe)) return result;
                value = fromJust(maybe);
              }
            };
          };
        };
      };
    };
  };
})(PS["Data.Unfoldable1"] = PS["Data.Unfoldable1"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Unfoldable1"] = $PS["Data.Unfoldable1"] || {};
  var exports = $PS["Data.Unfoldable1"];
  var $foreign = $PS["Data.Unfoldable1"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];

  var Unfoldable1 = function Unfoldable1(unfoldr1) {
    this.unfoldr1 = unfoldr1;
  };

  var unfoldable1Array = new Unfoldable1($foreign.unfoldr1ArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
  exports["Unfoldable1"] = Unfoldable1;
  exports["unfoldable1Array"] = unfoldable1Array;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Unfoldable"] = $PS["Data.Unfoldable"] || {};
  var exports = $PS["Data.Unfoldable"];
  var $foreign = $PS["Data.Unfoldable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable1 = $PS["Data.Unfoldable1"];

  var Unfoldable = function Unfoldable(Unfoldable10, unfoldr) {
    this.Unfoldable10 = Unfoldable10;
    this.unfoldr = unfoldr;
  };

  var unfoldr = function unfoldr(dict) {
    return dict.unfoldr;
  };

  var unfoldableArray = new Unfoldable(function () {
    return Data_Unfoldable1.unfoldable1Array;
  }, $foreign.unfoldrArrayImpl(Data_Maybe.isNothing)(Data_Maybe.fromJust())(Data_Tuple.fst)(Data_Tuple.snd));
  exports["Unfoldable"] = Unfoldable;
  exports["unfoldr"] = unfoldr;
  exports["unfoldableArray"] = unfoldableArray;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.List.Types"] = $PS["Data.List.Types"] || {};
  var exports = $PS["Data.List.Types"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Show = $PS["Data.Show"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];
  var Data_Unfoldable1 = $PS["Data.Unfoldable1"];

  var Nil = function () {
    function Nil() {}

    ;
    Nil.value = new Nil();
    return Nil;
  }();

  var Cons = function () {
    function Cons(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Cons.create = function (value0) {
      return function (value1) {
        return new Cons(value0, value1);
      };
    };

    return Cons;
  }();

  var NonEmptyList = function NonEmptyList(x) {
    return x;
  };

  var listMap = function listMap(f) {
    var chunkedRevMap = function chunkedRevMap($copy_chunksAcc) {
      return function ($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && v.value1 instanceof Cons && v.value1.value1 instanceof Cons) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }

          ;

          var unrolledMap = function unrolledMap(v1) {
            if (v1 instanceof Cons && v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }

            ;

            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }

            ;
            return Nil.value;
          };

          var reverseUnrolledMap = function reverseUnrolledMap($copy_v1) {
            return function ($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(v1, acc) {
                if (v1 instanceof Cons && v1.value0 instanceof Cons && v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }

                ;
                $tco_done = true;
                return acc;
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v1, $copy_acc);
              }

              ;
              return $tco_result;
            };
          };

          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return chunkedRevMap(Nil.value);
  };

  var functorList = new Data_Functor.Functor(listMap);
  var foldableList = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return Data_Foldable.foldl(foldableList)(function (acc) {
        var $202 = Data_Semigroup.append(dictMonoid.Semigroup0())(acc);
        return function ($203) {
          return $202(f($203));
        };
      })(Data_Monoid.mempty(dictMonoid));
    };
  }, function (f) {
    var go = function go($copy_b) {
      return function ($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return b;
          }

          ;

          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.List.Types (line 109, column 12 - line 111, column 30): " + [v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return go;
  }, function (f) {
    return function (b) {
      var rev = Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value);
      var $204 = Data_Foldable.foldl(foldableList)(Data_Function.flip(f))(b);
      return function ($205) {
        return $204(rev($205));
      };
    };
  });
  var semigroupList = new Data_Semigroup.Semigroup(function (xs) {
    return function (ys) {
      return Data_Foldable.foldr(foldableList)(Cons.create)(ys)(xs);
    };
  });

  var showList = function showList(dictShow) {
    return new Data_Show.Show(function (v) {
      if (v instanceof Nil) {
        return "Nil";
      }

      ;
      return "(" + (Data_Foldable.intercalate(foldableList)(Data_Monoid.monoidString)(" : ")(Data_Functor.map(functorList)(Data_Show.show(dictShow))(v)) + " : Nil)");
    });
  };

  var traversableList = new Data_Traversable.Traversable(function () {
    return foldableList;
  }, function () {
    return functorList;
  }, function (dictApplicative) {
    return Data_Traversable.traverse(traversableList)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
  }, function (dictApplicative) {
    return function (f) {
      var $219 = Data_Functor.map(dictApplicative.Apply0().Functor0())(Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value));
      var $220 = Data_Foldable.foldl(foldableList)(function (acc) {
        var $222 = Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc);
        return function ($223) {
          return $222(f($223));
        };
      })(Control_Applicative.pure(dictApplicative)(Nil.value));
      return function ($221) {
        return $219($220($221));
      };
    };
  });
  var unfoldable1List = new Data_Unfoldable1.Unfoldable1(function (f) {
    return function (b) {
      var go = function go($copy_source) {
        return function ($copy_memo) {
          var $tco_var_source = $copy_source;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(source, memo) {
            var v = f(source);

            if (v.value1 instanceof Data_Maybe.Just) {
              $tco_var_source = v.value1.value0;
              $copy_memo = new Cons(v.value0, memo);
              return;
            }

            ;

            if (v.value1 instanceof Data_Maybe.Nothing) {
              $tco_done = true;
              return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
            }

            ;
            throw new Error("Failed pattern match at Data.List.Types (line 133, column 22 - line 135, column 61): " + [v.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_source, $copy_memo);
          }

          ;
          return $tco_result;
        };
      };

      return go(b)(Nil.value);
    };
  });
  var unfoldableList = new Data_Unfoldable.Unfoldable(function () {
    return unfoldable1List;
  }, function (f) {
    return function (b) {
      var go = function go($copy_source) {
        return function ($copy_memo) {
          var $tco_var_source = $copy_source;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(source, memo) {
            var v = f(source);

            if (v instanceof Data_Maybe.Nothing) {
              $tco_done = true;
              return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(memo);
            }

            ;

            if (v instanceof Data_Maybe.Just) {
              $tco_var_source = v.value0.value1;
              $copy_memo = new Cons(v.value0.value0, memo);
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.List.Types (line 140, column 22 - line 142, column 52): " + [v.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_source, $copy_memo);
          }

          ;
          return $tco_result;
        };
      };

      return go(b)(Nil.value);
    };
  });
  var applyList = new Control_Apply.Apply(function () {
    return functorList;
  }, function (v) {
    return function (v1) {
      if (v instanceof Nil) {
        return Nil.value;
      }

      ;

      if (v instanceof Cons) {
        return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
      }

      ;
      throw new Error("Failed pattern match at Data.List.Types (line 155, column 1 - line 157, column 48): " + [v.constructor.name, v1.constructor.name]);
    };
  });
  var applicativeList = new Control_Applicative.Applicative(function () {
    return applyList;
  }, function (a) {
    return new Cons(a, Nil.value);
  });
  var altList = new Control_Alt.Alt(function () {
    return functorList;
  }, Data_Semigroup.append(semigroupList));
  var plusList = new Control_Plus.Plus(function () {
    return altList;
  }, Nil.value);
  exports["Nil"] = Nil;
  exports["Cons"] = Cons;
  exports["NonEmptyList"] = NonEmptyList;
  exports["showList"] = showList;
  exports["semigroupList"] = semigroupList;
  exports["functorList"] = functorList;
  exports["foldableList"] = foldableList;
  exports["unfoldableList"] = unfoldableList;
  exports["traversableList"] = traversableList;
  exports["applicativeList"] = applicativeList;
  exports["plusList"] = plusList;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Semigroup.Foldable"] = $PS["Data.Semigroup.Foldable"] || {};
  var exports = $PS["Data.Semigroup.Foldable"];

  var Foldable1 = function Foldable1(Foldable0, fold1, foldMap1) {
    this.Foldable0 = Foldable0;
    this.fold1 = fold1;
    this.foldMap1 = foldMap1;
  };

  var foldMap1 = function foldMap1(dict) {
    return dict.foldMap1;
  };

  exports["Foldable1"] = Foldable1;
  exports["foldMap1"] = foldMap1;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.NonEmpty"] = $PS["Data.NonEmpty"] || {};
  var exports = $PS["Data.NonEmpty"];
  var Control_Category = $PS["Control.Category"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semigroup_Foldable = $PS["Data.Semigroup.Foldable"];

  var NonEmpty = function () {
    function NonEmpty(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    NonEmpty.create = function (value0) {
      return function (value1) {
        return new NonEmpty(value0, value1);
      };
    };

    return NonEmpty;
  }();

  var singleton = function singleton(dictPlus) {
    return function (a) {
      return new NonEmpty(a, Control_Plus.empty(dictPlus));
    };
  };

  var foldableNonEmpty = function foldableNonEmpty(dictFoldable) {
    return new Data_Foldable.Foldable(function (dictMonoid) {
      return function (f) {
        return function (v) {
          return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f)(v.value1));
        };
      };
    }, function (f) {
      return function (b) {
        return function (v) {
          return Data_Foldable.foldl(dictFoldable)(f)(f(b)(v.value0))(v.value1);
        };
      };
    }, function (f) {
      return function (b) {
        return function (v) {
          return f(v.value0)(Data_Foldable.foldr(dictFoldable)(f)(b)(v.value1));
        };
      };
    });
  };

  var foldable1NonEmpty = function foldable1NonEmpty(dictFoldable) {
    return new Data_Semigroup_Foldable.Foldable1(function () {
      return foldableNonEmpty(dictFoldable);
    }, function (dictSemigroup) {
      return Data_Semigroup_Foldable.foldMap1(foldable1NonEmpty(dictFoldable))(dictSemigroup)(Control_Category.identity(Control_Category.categoryFn));
    }, function (dictSemigroup) {
      return function (f) {
        return function (v) {
          return Data_Foldable.foldl(dictFoldable)(function (s) {
            return function (a1) {
              return Data_Semigroup.append(dictSemigroup)(s)(f(a1));
            };
          })(f(v.value0))(v.value1);
        };
      };
    });
  };

  exports["NonEmpty"] = NonEmpty;
  exports["singleton"] = singleton;
  exports["foldable1NonEmpty"] = foldable1NonEmpty;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.List.NonEmpty"] = $PS["Data.List.NonEmpty"] || {};
  var exports = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];

  var singleton = function () {
    var $168 = Data_NonEmpty.singleton(Data_List_Types.plusList);
    return function ($169) {
      return Data_List_Types.NonEmptyList($168($169));
    };
  }();

  var head = function head(v) {
    return v.value0;
  };

  var cons = function cons(y) {
    return function (v) {
      return new Data_NonEmpty.NonEmpty(y, new Data_List_Types.Cons(v.value0, v.value1));
    };
  };

  exports["singleton"] = singleton;
  exports["cons"] = cons;
  exports["head"] = head;
})(PS);

(function (exports) {
  "use strict";

  exports["null"] = null;

  exports.nullable = function (a, r, f) {
    return a == null ? r : f(a);
  };

  exports.notNull = function (x) {
    return x;
  };
})(PS["Data.Nullable"] = PS["Data.Nullable"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Nullable"] = $PS["Data.Nullable"] || {};
  var exports = $PS["Data.Nullable"];
  var $foreign = $PS["Data.Nullable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var toNullable = Data_Maybe.maybe($foreign["null"])($foreign.notNull);

  var toMaybe = function toMaybe(n) {
    return $foreign.nullable(n, Data_Maybe.Nothing.value, Data_Maybe.Just.create);
  };

  exports["toMaybe"] = toMaybe;
  exports["toNullable"] = toNullable;
  exports["null"] = $foreign["null"];
})(PS);

(function (exports) {
  /* globals setImmediate, clearImmediate, setTimeout, clearTimeout */

  /* jshint -W083, -W098, -W003 */
  "use strict";

  var Aff = function () {
    // A unique value for empty.
    var EMPTY = {};
    /*
    An awkward approximation. We elide evidence we would otherwise need in PS for
    efficiency sake.
    data Aff eff a
    = Pure a
    | Throw Error
    | Catch (Aff eff a) (Error -> Aff eff a)
    | Sync (Eff eff a)
    | Async ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff))
    | forall b. Bind (Aff eff b) (b -> Aff eff a)
    | forall b. Bracket (Aff eff b) (BracketConditions eff b) (b -> Aff eff a)
    | forall b. Fork Boolean (Aff eff b) ?(Fiber eff b -> a)
    | Sequential (ParAff aff a)
    */

    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    /*
    data ParAff eff a
    = forall b. Map (b -> a) (ParAff eff b)
    | forall b. Apply (ParAff eff (b -> a)) (ParAff eff b)
    | Alt (ParAff eff a) (ParAff eff a)
    | ?Par (Aff eff a)
    */

    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt"; // Various constructors used in interpretation

    var CONS = "Cons"; // Cons-list, for stacks

    var RESUME = "Resume"; // Continue indiscriminately

    var RELEASE = "Release"; // Continue with bracket finalizers

    var FINALIZER = "Finalizer"; // A non-interruptible effect

    var FINALIZED = "Finalized"; // Marker for finalization

    var FORKED = "Forked"; // Reference to a forked fiber, with resumption stack

    var FIBER = "Fiber"; // Actual fiber reference

    var THUNK = "Thunk"; // Primed effect, ready to invoke

    function Aff(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }

    function AffCtr(tag) {
      var fn = function fn(_1, _2, _3) {
        return new Aff(tag, _1, _2, _3);
      };

      fn.tag = tag;
      return fn;
    }

    function nonCanceler(error) {
      return new Aff(PURE, void 0);
    }

    function runEff(eff) {
      try {
        eff();
      } catch (error) {
        setTimeout(function () {
          throw error;
        }, 0);
      }
    }

    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error) {
        return left(error);
      }
    }

    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error) {
        k(left(error))();
        return nonCanceler;
      }
    }

    var Scheduler = function () {
      var limit = 1024;
      var size = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;

      function drain() {
        var thunk;
        draining = true;

        while (size !== 0) {
          size--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }

        draining = false;
      }

      return {
        isDraining: function isDraining() {
          return draining;
        },
        enqueue: function enqueue(cb) {
          var i, tmp;

          if (size === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }

          queue[(ix + size) % limit] = cb;
          size++;

          if (!draining) {
            drain();
          }
        }
      };
    }();

    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function register(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function handler(result) {
              return function () {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function isEmpty() {
          return count === 0;
        },
        killAll: function killAll(killError, cb) {
          return function () {
            if (count === 0) {
              return cb();
            }

            var killCount = 0;
            var kills = {};

            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function (result) {
                return function () {
                  delete kills[fid];
                  killCount--;

                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function () {
                      throw util.fromLeft(result);
                    }, 0);
                  }

                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }

            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }

            fibers = {};
            fiberId = 0;
            count = 0;
            return function (error) {
              return new Aff(SYNC, function () {
                for (var k in kills) {
                  if (kills.hasOwnProperty(k)) {
                    kills[k]();
                  }
                }
              });
            };
          };
        }
      };
    } // Fiber state machine


    var SUSPENDED = 0; // Suspended, pending a join.

    var CONTINUE = 1; // Interpret the next instruction.

    var STEP_BIND = 2; // Apply the next bind.

    var STEP_RESULT = 3; // Handle potential failure from a result.

    var PENDING = 4; // An async effect is running.

    var RETURN = 5; // The current stack has returned.

    var COMPLETED = 6; // The entire fiber has completed.

    function Fiber(util, supervisor, aff) {
      // Monotonically increasing tick, increased on each asynchronous turn.
      var runTick = 0; // The current branch of the state machine.

      var status = SUSPENDED; // The current point of interest for the state machine branch.

      var step = aff; // Successful step

      var fail = null; // Failure step

      var interrupt = null; // Asynchronous interrupt
      // Stack of continuations for the current fiber.

      var bhead = null;
      var btail = null; // Stack of attempts and finalizers for error recovery. Every `Cons` is also
      // tagged with current `interrupt` state. We use this to track which items
      // should be ignored or evaluated as a result of a kill.

      var attempts = null; // A special state is needed for Bracket, because it cannot be killed. When
      // we enter a bracket acquisition or finalizer, we increment the counter,
      // and then decrement once complete.

      var bracketCount = 0; // Each join gets a new id so they can be revoked.

      var joinId = 0;
      var joins = null;
      var rethrow = true; // Each invocation of `run` requires a tick. When an asynchronous effect is
      // resolved, we must check that the local tick coincides with the fiber
      // tick before resuming. This prevents multiple async continuations from
      // accidentally resuming the same fiber. A common example may be invoking
      // the provided callback in `makeAff` more than once, but it may also be an
      // async effect resuming after the fiber was already cancelled.

      function _run(localRunTick) {
        var tmp, result, attempt;

        while (true) {
          tmp = null;
          result = null;
          attempt = null;

          switch (status) {
            case STEP_BIND:
              status = CONTINUE;

              try {
                step = bhead(step);

                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail = util.left(e);
                step = null;
              }

              break;

            case STEP_RESULT:
              if (util.isLeft(step)) {
                status = RETURN;
                fail = step;
                step = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step = util.fromRight(step);
              }

              break;

            case CONTINUE:
              switch (step.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff(CONS, bhead, btail);
                  }

                  bhead = step._2;
                  status = CONTINUE;
                  step = step._1;
                  break;

                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step = util.right(step._1);
                  } else {
                    status = STEP_BIND;
                    step = step._1;
                  }

                  break;

                case SYNC:
                  status = STEP_RESULT;
                  step = runSync(util.left, util.right, step._1);
                  break;

                case ASYNC:
                  status = PENDING;
                  step = runAsync(util.left, step._1, function (result) {
                    return function () {
                      if (runTick !== localRunTick) {
                        return;
                      }

                      runTick++;
                      Scheduler.enqueue(function () {
                        // It's possible to interrupt the fiber between enqueuing and
                        // resuming, so we need to check that the runTick is still
                        // valid.
                        if (runTick !== localRunTick + 1) {
                          return;
                        }

                        status = STEP_RESULT;
                        step = result;

                        _run(runTick);
                      });
                    };
                  });
                  return;

                case THROW:
                  status = RETURN;
                  fail = util.left(step._1);
                  step = null;
                  break;
                // Enqueue the Catch so that we can call the error handler later on
                // in case of an exception.

                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }

                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;
                // Enqueue the Bracket so that we can call the appropriate handlers
                // after resource acquisition.

                case BRACKET:
                  bracketCount++;

                  if (bhead === null) {
                    attempts = new Aff(CONS, step, attempts, interrupt);
                  } else {
                    attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }

                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step = step._1;
                  break;

                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step._2);

                  if (supervisor) {
                    supervisor.register(tmp);
                  }

                  if (step._1) {
                    tmp.run();
                  }

                  step = util.right(tmp);
                  break;

                case SEQ:
                  status = CONTINUE;
                  step = sequential(util, supervisor, step._1);
                  break;
              }

              break;

            case RETURN:
              bhead = null;
              btail = null; // If the current stack has returned, and we have no other stacks to
              // resume or finalizers to run, the fiber has halted and we can
              // invoke all join callbacks. Otherwise we need to resume.

              if (attempts === null) {
                status = COMPLETED;
                step = interrupt || fail || step;
              } else {
                // The interrupt status for the enqueued item.
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;

                switch (attempt.tag) {
                  // We cannot recover from an unmasked interrupt. Otherwise we should
                  // continue stepping, or run the exception handler if an exception
                  // was raised.
                  case CATCH:
                    // We should compare the interrupt status as well because we
                    // only want it to apply if there has been an interrupt since
                    // enqueuing the catch.
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail) {
                      status = CONTINUE;
                      step = attempt._2(util.fromLeft(fail));
                      fail = null;
                    }

                    break;
                  // We cannot resume from an unmasked interrupt or exception.

                  case RESUME:
                    // As with Catch, we only want to ignore in the case of an
                    // interrupt since enqueing the item.
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step = util.fromRight(step);
                    }

                    break;
                  // If we have a bracket, we should enqueue the handlers,
                  // and continue with the success branch only if the fiber has
                  // not been interrupted. If the bracket acquisition failed, we
                  // should not run either.

                  case BRACKET:
                    bracketCount--;

                    if (fail === null) {
                      result = util.fromRight(step); // We need to enqueue the Release with the same interrupt
                      // status as the Bracket that is initiating it.

                      attempts = new Aff(CONS, new Aff(RELEASE, attempt._2, result), attempts, tmp); // We should only coninue as long as the interrupt status has not changed or
                      // we are currently within a non-interruptable finalizer.

                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step = attempt._3(result);
                      }
                    }

                    break;
                  // Enqueue the appropriate handler. We increase the bracket count
                  // because it should not be cancelled.

                  case RELEASE:
                    attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE; // It has only been killed if the interrupt status has changed
                    // since we enqueued the item, and the bracket count is 0. If the
                    // bracket count is non-zero then we are in a masked state so it's
                    // impossible to be killed.

                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail) {
                      step = attempt._1.failed(util.fromLeft(fail))(attempt._2);
                    } else {
                      step = attempt._1.completed(util.fromRight(step))(attempt._2);
                    }

                    fail = null;
                    bracketCount++;
                    break;

                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff(CONS, new Aff(FINALIZED, step, fail), attempts, interrupt);
                    status = CONTINUE;
                    step = attempt._1;
                    break;

                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step = attempt._1;
                    fail = attempt._2;
                    break;
                }
              }

              break;

            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step));
                }
              }

              joins = null; // If we have an interrupt and a fail, then the thread threw while
              // running finalizers. This should always rethrow in a fresh stack.

              if (interrupt && fail) {
                setTimeout(function () {
                  throw util.fromLeft(fail);
                }, 0); // If we have an unhandled exception, and no other fiber has joined
                // then we need to throw the exception in a fresh stack.
              } else if (util.isLeft(step) && rethrow) {
                setTimeout(function () {
                  // Guard on reathrow because a completely synchronous fiber can
                  // still have an observer which was added after-the-fact.
                  if (rethrow) {
                    throw util.fromLeft(step);
                  }
                }, 0);
              }

              return;

            case SUSPENDED:
              status = CONTINUE;
              break;

            case PENDING:
              return;
          }
        }
      }

      function onComplete(join) {
        return function () {
          if (status === COMPLETED) {
            rethrow = rethrow && join.rethrow;
            join.handler(step)();
            return function () {};
          }

          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join;
          return function () {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }

      function kill(error, cb) {
        return function () {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function () {};
          }

          var canceler = onComplete({
            rethrow: false,
            handler: function handler()
            /* unused */
            {
              return cb(util.right(void 0));
            }
          })();

          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error);
              status = COMPLETED;
              step = interrupt;

              _run(runTick);

              break;

            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error);
              }

              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff(CONS, new Aff(FINALIZER, step(error)), attempts, interrupt);
                }

                status = RETURN;
                step = null;
                fail = null;

                _run(++runTick);
              }

              break;

            default:
              if (interrupt === null) {
                interrupt = util.left(error);
              }

              if (bracketCount === 0) {
                status = RETURN;
                step = null;
                fail = null;
              }

          }

          return canceler;
        };
      }

      function join(cb) {
        return function () {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();

          if (status === SUSPENDED) {
            _run(runTick);
          }

          return canceler;
        };
      }

      return {
        kill: kill,
        join: join,
        onComplete: onComplete,
        isSuspended: function isSuspended() {
          return status === SUSPENDED;
        },
        run: function run() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function () {
                _run(runTick);
              });
            } else {
              _run(runTick);
            }
          }
        }
      };
    }

    function runPar(util, supervisor, par, cb) {
      // Table of all forked fibers.
      var fiberId = 0;
      var fibers = {}; // Table of currently running cancelers, as a product of `Alt` behavior.

      var killId = 0;
      var kills = {}; // Error used for early cancelation on Alt branches.

      var early = new Error("[ParAff] Early exit"); // Error used to kill the entire tree.

      var interrupt = null; // The root pointer of the tree.

      var root = EMPTY; // Walks a tree, invoking all the cancelers. Returns the table of pending
      // cancellation fibers.

      function kill(error, par, cb) {
        var step = par;
        var head = null;
        var tail = null;
        var count = 0;
        var kills = {};
        var tmp, kid;

        loop: while (true) {
          tmp = null;

          switch (step.tag) {
            case FORKED:
              if (step._3 === EMPTY) {
                tmp = fibers[step._1];
                kills[count++] = tmp.kill(error, function (result) {
                  return function () {
                    count--;

                    if (count === 0) {
                      cb(result)();
                    }
                  };
                });
              } // Terminal case.


              if (head === null) {
                break loop;
              } // Go down the right side of the tree.


              step = head._2;

              if (tail === null) {
                head = null;
              } else {
                head = tail._1;
                tail = tail._2;
              }

              break;

            case MAP:
              step = step._2;
              break;

            case APPLY:
            case ALT:
              if (head) {
                tail = new Aff(CONS, head, tail);
              }

              head = step;
              step = step._1;
              break;
          }
        }

        if (count === 0) {
          cb(util.right(void 0))();
        } else {
          // Run the cancelation effects. We alias `count` because it's mutable.
          kid = 0;
          tmp = count;

          for (; kid < tmp; kid++) {
            kills[kid] = kills[kid]();
          }
        }

        return kills;
      } // When a fiber resolves, we need to bubble back up the tree with the
      // result, computing the applicative nodes.


      function join(result, head, tail) {
        var fail, step, lhs, rhs, tmp, kid;

        if (util.isLeft(result)) {
          fail = result;
          step = null;
        } else {
          step = result;
          fail = null;
        }

        loop: while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null; // We should never continue if the entire tree has been interrupted.

          if (interrupt !== null) {
            return;
          } // We've made it all the way to the root of the tree, which means
          // the tree has fully evaluated.


          if (head === null) {
            cb(fail || step)();
            return;
          } // The tree has already been computed, so we shouldn't try to do it
          // again. This should never happen.
          // TODO: Remove this?


          if (head._3 !== EMPTY) {
            return;
          }

          switch (head.tag) {
            case MAP:
              if (fail === null) {
                head._3 = util.right(head._1(util.fromRight(step)));
                step = head._3;
              } else {
                head._3 = fail;
              }

              break;

            case APPLY:
              lhs = head._1._3;
              rhs = head._2._3; // If we have a failure we should kill the other side because we
              // can't possible yield a result anymore.

              if (fail) {
                head._3 = fail;
                tmp = true;
                kid = killId++;
                kills[kid] = kill(early, fail === lhs ? head._2 : head._1, function ()
                /* unused */
                {
                  return function () {
                    delete kills[kid];

                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join(fail, null, null);
                    } else {
                      join(fail, tail._1, tail._2);
                    }
                  };
                });

                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                // We can only proceed if both sides have resolved.
                return;
              } else {
                step = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head._3 = step;
              }

              break;

            case ALT:
              lhs = head._1._3;
              rhs = head._2._3; // We can only proceed if both have resolved or we have a success

              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              } // If both sides resolve with an error, we should continue with the
              // first error


              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail = step === lhs ? rhs : lhs;
                step = null;
                head._3 = fail;
              } else {
                head._3 = step;
                tmp = true;
                kid = killId++; // Once a side has resolved, we need to cancel the side that is still
                // pending before we can continue.

                kills[kid] = kill(early, step === lhs ? head._2 : head._1, function ()
                /* unused */
                {
                  return function () {
                    delete kills[kid];

                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join(step, null, null);
                    } else {
                      join(step, tail._1, tail._2);
                    }
                  };
                });

                if (tmp) {
                  tmp = false;
                  return;
                }
              }

              break;
          }

          if (tail === null) {
            head = null;
          } else {
            head = tail._1;
            tail = tail._2;
          }
        }
      }

      function resolve(fiber) {
        return function (result) {
          return function () {
            delete fibers[fiber._1];
            fiber._3 = result;
            join(result, fiber._2._1, fiber._2._2);
          };
        };
      } // Walks the applicative tree, substituting non-applicative nodes with
      // `FORKED` nodes. In this tree, all applicative nodes use the `_3` slot
      // as a mutable slot for memoization. In an unresolved state, the `_3`
      // slot is `EMPTY`. In the cases of `ALT` and `APPLY`, we always walk
      // the left side first, because both operations are left-associative. As
      // we `RETURN` from those branches, we then walk the right side.


      function run() {
        var status = CONTINUE;
        var step = par;
        var head = null;
        var tail = null;
        var tmp, fid;

        loop: while (true) {
          tmp = null;
          fid = null;

          switch (status) {
            case CONTINUE:
              switch (step.tag) {
                case MAP:
                  if (head) {
                    tail = new Aff(CONS, head, tail);
                  }

                  head = new Aff(MAP, step._1, EMPTY, EMPTY);
                  step = step._2;
                  break;

                case APPLY:
                  if (head) {
                    tail = new Aff(CONS, head, tail);
                  }

                  head = new Aff(APPLY, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;

                case ALT:
                  if (head) {
                    tail = new Aff(CONS, head, tail);
                  }

                  head = new Aff(ALT, EMPTY, step._2, EMPTY);
                  step = step._1;
                  break;

                default:
                  // When we hit a leaf value, we suspend the stack in the `FORKED`.
                  // When the fiber resolves, it can bubble back up the tree.
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step;
                  step = new Aff(FORKED, fid, new Aff(CONS, head, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step)
                  })();
                  fibers[fid] = tmp;

                  if (supervisor) {
                    supervisor.register(tmp);
                  }

              }

              break;

            case RETURN:
              // Terminal case, we are back at the root.
              if (head === null) {
                break loop;
              } // If we are done with the right side, we need to continue down the
              // left. Otherwise we should continue up the stack.


              if (head._1 === EMPTY) {
                head._1 = step;
                status = CONTINUE;
                step = head._2;
                head._2 = EMPTY;
              } else {
                head._2 = step;
                step = head;

                if (tail === null) {
                  head = null;
                } else {
                  head = tail._1;
                  tail = tail._2;
                }
              }

          }
        } // Keep a reference to the tree root so it can be cancelled.


        root = step;

        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      } // Cancels the entire tree. If there are already subtrees being canceled,
      // we need to first cancel those joins. We will then add fresh joins for
      // all pending branches including those that were in the process of being
      // canceled.


      function cancel(error, cb) {
        interrupt = util.left(error);
        var innerKills;

        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];

            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }

        kills = null;
        var newKills = kill(error, root, cb);
        return function (killError) {
          return new Aff(ASYNC, function (killCb) {
            return function () {
              for (var kid in newKills) {
                if (newKills.hasOwnProperty(kid)) {
                  newKills[kid]();
                }
              }

              return nonCanceler;
            };
          });
        };
      }

      run();
      return function (killError) {
        return new Aff(ASYNC, function (killCb) {
          return function () {
            return cancel(killError, killCb);
          };
        });
      };
    }

    function sequential(util, supervisor, par) {
      return new Aff(ASYNC, function (cb) {
        return function () {
          return runPar(util, supervisor, par, cb);
        };
      });
    }

    Aff.EMPTY = EMPTY;
    Aff.Pure = AffCtr(PURE);
    Aff.Throw = AffCtr(THROW);
    Aff.Catch = AffCtr(CATCH);
    Aff.Sync = AffCtr(SYNC);
    Aff.Async = AffCtr(ASYNC);
    Aff.Bind = AffCtr(BIND);
    Aff.Bracket = AffCtr(BRACKET);
    Aff.Fork = AffCtr(FORK);
    Aff.Seq = AffCtr(SEQ);
    Aff.ParMap = AffCtr(MAP);
    Aff.ParApply = AffCtr(APPLY);
    Aff.ParAlt = AffCtr(ALT);
    Aff.Fiber = Fiber;
    Aff.Supervisor = Supervisor;
    Aff.Scheduler = Scheduler;
    Aff.nonCanceler = nonCanceler;
    return Aff;
  }();

  exports._pure = Aff.Pure;
  exports._throwError = Aff.Throw;

  exports._catchError = function (aff) {
    return function (k) {
      return Aff.Catch(aff, k);
    };
  };

  exports._map = function (f) {
    return function (aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function (value) {
          return Aff.Pure(f(value));
        });
      }
    };
  };

  exports._bind = function (aff) {
    return function (k) {
      return Aff.Bind(aff, k);
    };
  };

  exports._fork = function (immediate) {
    return function (aff) {
      return Aff.Fork(immediate, aff);
    };
  };

  exports._liftEffect = Aff.Sync;

  exports._parAffMap = function (f) {
    return function (aff) {
      return Aff.ParMap(f, aff);
    };
  };

  exports._parAffApply = function (aff1) {
    return function (aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  };

  exports.makeAff = Aff.Async;

  exports.generalBracket = function (acquire) {
    return function (options) {
      return function (k) {
        return Aff.Bracket(acquire, options, k);
      };
    };
  };

  exports._makeFiber = function (util, aff) {
    return function () {
      return Aff.Fiber(util, null, aff);
    };
  };

  exports._sequential = Aff.Seq;
})(PS["Effect.Aff"] = PS["Effect.Aff"] || {});

(function (exports) {
  "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect"] = $PS["Effect"] || {};
  var exports = $PS["Effect"];
  var $foreign = $PS["Effect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];
  var monadEffect = new Control_Monad.Monad(function () {
    return applicativeEffect;
  }, function () {
    return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
    return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
    return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
    return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["applicativeEffect"] = applicativeEffect;
  exports["bindEffect"] = bindEffect;
  exports["monadEffect"] = monadEffect;
})(PS);

(function (exports) {
  "use strict";

  exports.new = function (val) {
    return function () {
      return {
        value: val
      };
    };
  };

  exports.read = function (ref) {
    return function () {
      return ref.value;
    };
  };

  exports["modify'"] = function (f) {
    return function (ref) {
      return function () {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };

  exports.write = function (val) {
    return function (ref) {
      return function () {
        ref.value = val;
        return {};
      };
    };
  };
})(PS["Effect.Ref"] = PS["Effect.Ref"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Ref"] = $PS["Effect.Ref"] || {};
  var exports = $PS["Effect.Ref"];
  var $foreign = $PS["Effect.Ref"];
  var Data_Functor = $PS["Data.Functor"];
  var Effect = $PS["Effect"];

  var modify = function modify(f) {
    return $foreign["modify'"](function (s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };

  var modify_ = function modify_(f) {
    return function (s) {
      return Data_Functor["void"](Effect.functorEffect)(modify(f)(s));
    };
  };

  exports["modify_"] = modify_;
  exports["new"] = $foreign["new"];
  exports["read"] = $foreign.read;
  exports["modify'"] = $foreign["modify'"];
  exports["write"] = $foreign.write;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Rec.Class"] = $PS["Control.Monad.Rec.Class"] || {};
  var exports = $PS["Control.Monad.Rec.Class"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Functor = $PS["Data.Functor"];
  var Effect = $PS["Effect"];
  var Effect_Ref = $PS["Effect.Ref"];

  var Loop = function () {
    function Loop(value0) {
      this.value0 = value0;
    }

    ;

    Loop.create = function (value0) {
      return new Loop(value0);
    };

    return Loop;
  }();

  var Done = function () {
    function Done(value0) {
      this.value0 = value0;
    }

    ;

    Done.create = function (value0) {
      return new Done(value0);
    };

    return Done;
  }();

  var MonadRec = function MonadRec(Monad0, tailRecM) {
    this.Monad0 = Monad0;
    this.tailRecM = tailRecM;
  };

  var tailRecM = function tailRecM(dict) {
    return dict.tailRecM;
  };

  var monadRecEffect = new MonadRec(function () {
    return Effect.monadEffect;
  }, function (f) {
    return function (a) {
      var fromDone = function fromDone(v) {
        if (v instanceof Done) {
          return v.value0;
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): " + [v.constructor.name]);
      };

      return function __do() {
        var r = Control_Bind.bindFlipped(Effect.bindEffect)(Effect_Ref["new"])(f(a))();

        (function () {
          while (!function __do() {
            var v = Effect_Ref.read(r)();

            if (v instanceof Loop) {
              var e = f(v.value0)();
              Effect_Ref.write(e)(r)();
              return false;
            }

            ;

            if (v instanceof Done) {
              return true;
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): " + [v.constructor.name]);
          }()) {}

          ;
          return {};
        })();

        return Data_Functor.map(Effect.functorEffect)(fromDone)(Effect_Ref.read(r))();
      };
    };
  });
  exports["Loop"] = Loop;
  exports["Done"] = Done;
  exports["MonadRec"] = MonadRec;
  exports["tailRecM"] = tailRecM;
  exports["monadRecEffect"] = monadRecEffect;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Parallel.Class"] = $PS["Control.Parallel.Class"] || {};
  var exports = $PS["Control.Parallel.Class"];

  var Parallel = function Parallel(Applicative1, Monad0, parallel, sequential) {
    this.Applicative1 = Applicative1;
    this.Monad0 = Monad0;
    this.parallel = parallel;
    this.sequential = sequential;
  };

  var sequential = function sequential(dict) {
    return dict.sequential;
  };

  var parallel = function parallel(dict) {
    return dict.parallel;
  };

  exports["parallel"] = parallel;
  exports["sequential"] = sequential;
  exports["Parallel"] = Parallel;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Class"] = $PS["Effect.Class"] || {};
  var exports = $PS["Effect.Class"];

  var MonadEffect = function MonadEffect(Monad0, liftEffect) {
    this.Monad0 = Monad0;
    this.liftEffect = liftEffect;
  };

  var liftEffect = function liftEffect(dict) {
    return dict.liftEffect;
  };

  exports["liftEffect"] = liftEffect;
  exports["MonadEffect"] = MonadEffect;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafePerformEffect = function (f) {
    return f();
  };
})(PS["Effect.Unsafe"] = PS["Effect.Unsafe"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Unsafe"] = $PS["Effect.Unsafe"] || {};
  var exports = $PS["Effect.Unsafe"];
  var $foreign = $PS["Effect.Unsafe"];
  exports["unsafePerformEffect"] = $foreign.unsafePerformEffect;
})(PS);

(function (exports) {
  "use strict"; // module Partial.Unsafe

  exports.unsafePartial = function (f) {
    return f();
  };
})(PS["Partial.Unsafe"] = PS["Partial.Unsafe"] || {});

(function (exports) {
  "use strict"; // module Partial

  exports.crashWith = function () {
    return function (msg) {
      throw new Error(msg);
    };
  };
})(PS["Partial"] = PS["Partial"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Partial"] = $PS["Partial"] || {};
  var exports = $PS["Partial"];
  var $foreign = $PS["Partial"];
  exports["crashWith"] = $foreign.crashWith;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Partial.Unsafe"] = $PS["Partial.Unsafe"] || {};
  var exports = $PS["Partial.Unsafe"];
  var $foreign = $PS["Partial.Unsafe"];
  var Partial = $PS["Partial"];

  var unsafeCrashWith = function unsafeCrashWith(msg) {
    return $foreign.unsafePartial(function (dictPartial) {
      return Partial.crashWith()(msg);
    });
  };

  exports["unsafeCrashWith"] = unsafeCrashWith;
})(PS);

(function (exports) {
  "use strict"; // module Unsafe.Coerce

  exports.unsafeCoerce = function (x) {
    return x;
  };
})(PS["Unsafe.Coerce"] = PS["Unsafe.Coerce"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Unsafe.Coerce"] = $PS["Unsafe.Coerce"] || {};
  var exports = $PS["Unsafe.Coerce"];
  var $foreign = $PS["Unsafe.Coerce"];
  exports["unsafeCoerce"] = $foreign.unsafeCoerce;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Aff"] = $PS["Effect.Aff"] || {};
  var exports = $PS["Effect.Aff"];
  var $foreign = $PS["Effect.Aff"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Unsafe = $PS["Effect.Unsafe"];
  var Partial_Unsafe = $PS["Partial.Unsafe"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Canceler = function Canceler(x) {
    return x;
  };

  var suspendAff = $foreign["_fork"](false);
  var functorParAff = new Data_Functor.Functor($foreign["_parAffMap"]);
  var functorAff = new Data_Functor.Functor($foreign["_map"]);
  var forkAff = $foreign["_fork"](true);

  var ffiUtil = function () {
    var unsafeFromRight = function unsafeFromRight(v) {
      if (v instanceof Data_Either.Right) {
        return v.value0;
      }

      ;

      if (v instanceof Data_Either.Left) {
        return Partial_Unsafe.unsafeCrashWith("unsafeFromRight: Left");
      }

      ;
      throw new Error("Failed pattern match at Effect.Aff (line 400, column 21 - line 402, column 54): " + [v.constructor.name]);
    };

    var unsafeFromLeft = function unsafeFromLeft(v) {
      if (v instanceof Data_Either.Left) {
        return v.value0;
      }

      ;

      if (v instanceof Data_Either.Right) {
        return Partial_Unsafe.unsafeCrashWith("unsafeFromLeft: Right");
      }

      ;
      throw new Error("Failed pattern match at Effect.Aff (line 395, column 20 - line 397, column 54): " + [v.constructor.name]);
    };

    var isLeft = function isLeft(v) {
      if (v instanceof Data_Either.Left) {
        return true;
      }

      ;

      if (v instanceof Data_Either.Right) {
        return false;
      }

      ;
      throw new Error("Failed pattern match at Effect.Aff (line 390, column 12 - line 392, column 20): " + [v.constructor.name]);
    };

    return {
      isLeft: isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Data_Either.Left.create,
      right: Data_Either.Right.create
    };
  }();

  var makeFiber = function makeFiber(aff) {
    return $foreign["_makeFiber"](ffiUtil, aff);
  };

  var launchAff = function launchAff(aff) {
    return function __do() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };

  var launchAff_ = function () {
    var $43 = Data_Functor["void"](Effect.functorEffect);
    return function ($44) {
      return $43(launchAff($44));
    };
  }();

  var bracket = function bracket(acquire) {
    return function (completed) {
      return $foreign.generalBracket(acquire)({
        killed: Data_Function["const"](completed),
        failed: Data_Function["const"](completed),
        completed: Data_Function["const"](completed)
      });
    };
  };

  var applyParAff = new Control_Apply.Apply(function () {
    return functorParAff;
  }, $foreign["_parAffApply"]);
  var monadAff = new Control_Monad.Monad(function () {
    return applicativeAff;
  }, function () {
    return bindAff;
  });
  var bindAff = new Control_Bind.Bind(function () {
    return applyAff;
  }, $foreign["_bind"]);
  var applyAff = new Control_Apply.Apply(function () {
    return functorAff;
  }, Control_Monad.ap(monadAff));
  var applicativeAff = new Control_Applicative.Applicative(function () {
    return applyAff;
  }, $foreign["_pure"]);

  var $$finally = function $$finally(fin) {
    return function (a) {
      return bracket(Control_Applicative.pure(applicativeAff)(Data_Unit.unit))(Data_Function["const"](fin))(Data_Function["const"](a));
    };
  };

  var monadEffectAff = new Effect_Class.MonadEffect(function () {
    return monadAff;
  }, $foreign["_liftEffect"]);

  var effectCanceler = function () {
    var $45 = Effect_Class.liftEffect(monadEffectAff);
    return function ($46) {
      return Canceler(Data_Function["const"]($45($46)));
    };
  }();

  var joinFiber = function joinFiber(v) {
    return $foreign.makeAff(function (k) {
      return Data_Functor.map(Effect.functorEffect)(effectCanceler)(v.join(k));
    });
  };

  var functorFiber = new Data_Functor.Functor(function (f) {
    return function (t) {
      return Effect_Unsafe.unsafePerformEffect(makeFiber(Data_Functor.map(functorAff)(f)(joinFiber(t))));
    };
  });

  var killFiber = function killFiber(e) {
    return function (v) {
      return Control_Bind.bind(bindAff)(Effect_Class.liftEffect(monadEffectAff)(v.isSuspended))(function (v1) {
        if (v1) {
          return Effect_Class.liftEffect(monadEffectAff)(Data_Functor["void"](Effect.functorEffect)(v.kill(e, Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit)))));
        }

        ;
        return $foreign.makeAff(function (k) {
          return Data_Functor.map(Effect.functorEffect)(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };

  var monadThrowAff = new Control_Monad_Error_Class.MonadThrow(function () {
    return monadAff;
  }, $foreign["_throwError"]);
  var monadErrorAff = new Control_Monad_Error_Class.MonadError(function () {
    return monadThrowAff;
  }, $foreign["_catchError"]);

  var runAff = function runAff(k) {
    return function (aff) {
      return launchAff(Control_Bind.bindFlipped(bindAff)(function () {
        var $49 = Effect_Class.liftEffect(monadEffectAff);
        return function ($50) {
          return $49(k($50));
        };
      }())(Control_Monad_Error_Class["try"](monadErrorAff)(aff)));
    };
  };

  var runAff_ = function runAff_(k) {
    return function (aff) {
      return Data_Functor["void"](Effect.functorEffect)(runAff(k)(aff));
    };
  };

  var parallelAff = new Control_Parallel_Class.Parallel(function () {
    return applicativeParAff;
  }, function () {
    return monadAff;
  }, Unsafe_Coerce.unsafeCoerce, $foreign["_sequential"]);
  var applicativeParAff = new Control_Applicative.Applicative(function () {
    return applyParAff;
  }, function () {
    var $53 = Control_Parallel_Class.parallel(parallelAff);
    var $54 = Control_Applicative.pure(applicativeAff);
    return function ($55) {
      return $53($54($55));
    };
  }());
  var monadRecAff = new Control_Monad_Rec_Class.MonadRec(function () {
    return monadAff;
  }, function (k) {
    var go = function go(a) {
      return Control_Bind.bind(bindAff)(k(a))(function (res) {
        if (res instanceof Control_Monad_Rec_Class.Done) {
          return Control_Applicative.pure(applicativeAff)(res.value0);
        }

        ;

        if (res instanceof Control_Monad_Rec_Class.Loop) {
          return go(res.value0);
        }

        ;
        throw new Error("Failed pattern match at Effect.Aff (line 100, column 7 - line 102, column 22): " + [res.constructor.name]);
      });
    };

    return go;
  });
  var nonCanceler = Data_Function["const"](Control_Applicative.pure(applicativeAff)(Data_Unit.unit));
  exports["launchAff_"] = launchAff_;
  exports["runAff_"] = runAff_;
  exports["forkAff"] = forkAff;
  exports["suspendAff"] = suspendAff;
  exports["finally"] = $$finally;
  exports["killFiber"] = killFiber;
  exports["joinFiber"] = joinFiber;
  exports["nonCanceler"] = nonCanceler;
  exports["effectCanceler"] = effectCanceler;
  exports["functorAff"] = functorAff;
  exports["applicativeAff"] = applicativeAff;
  exports["bindAff"] = bindAff;
  exports["monadAff"] = monadAff;
  exports["monadRecAff"] = monadRecAff;
  exports["monadThrowAff"] = monadThrowAff;
  exports["monadErrorAff"] = monadErrorAff;
  exports["monadEffectAff"] = monadEffectAff;
  exports["applicativeParAff"] = applicativeParAff;
  exports["parallelAff"] = parallelAff;
  exports["functorFiber"] = functorFiber;
  exports["makeAff"] = $foreign.makeAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Aff.Compat"] = $PS["Effect.Aff.Compat"] || {};
  var exports = $PS["Effect.Aff.Compat"];
  var Data_Either = $PS["Data.Either"];
  var Effect_Aff = $PS["Effect.Aff"];

  var fromEffectFnAff = function fromEffectFnAff(v) {
    return Effect_Aff.makeAff(function (k) {
      return function __do() {
        var v1 = v(function ($4) {
          return k(Data_Either.Left.create($4))();
        }, function ($5) {
          return k(Data_Either.Right.create($5))();
        });
        return function (e) {
          return Effect_Aff.makeAff(function (k2) {
            return function __do() {
              v1(e, function ($6) {
                return k2(Data_Either.Left.create($6))();
              }, function ($7) {
                return k2(Data_Either.Right.create($7))();
              });
              return Effect_Aff.nonCanceler;
            };
          });
        };
      };
    });
  };

  exports["fromEffectFnAff"] = fromEffectFnAff;
})(PS);

(function (exports) {
  "use strict";

  exports.error = function (msg) {
    return new Error(msg);
  };

  exports.message = function (e) {
    return e.message;
  };

  exports.throwException = function (e) {
    return function () {
      throw e;
    };
  };
})(PS["Effect.Exception"] = PS["Effect.Exception"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Exception"] = $PS["Effect.Exception"] || {};
  var exports = $PS["Effect.Exception"];
  var $foreign = $PS["Effect.Exception"];

  var $$throw = function $$throw($2) {
    return $foreign.throwException($foreign.error($2));
  };

  exports["throw"] = $$throw;
  exports["error"] = $foreign.error;
  exports["message"] = $foreign.message;
  exports["throwException"] = $foreign.throwException;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeToForeign = function (value) {
    return value;
  };

  exports.unsafeFromForeign = function (value) {
    return value;
  };

  exports.typeOf = function (value) {
    return _typeof(value);
  };

  exports.tagOf = function (value) {
    return Object.prototype.toString.call(value).slice(8, -1);
  };
})(PS["Foreign"] = PS["Foreign"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Boolean"] = $PS["Data.Boolean"] || {};
  var exports = $PS["Data.Boolean"];
  var otherwise = true;
  exports["otherwise"] = otherwise;
})(PS);

(function (exports) {
  "use strict";

  exports.fromNumberImpl = function (just) {
    return function (nothing) {
      return function (n) {
        /* jshint bitwise: false */
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };

  exports.toNumber = function (n) {
    return n;
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});

(function (exports) {
  "use strict";

  exports.topInt = 2147483647;
  exports.bottomInt = -2147483648;
})(PS["Data.Bounded"] = PS["Data.Bounded"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Bounded"] = $PS["Data.Bounded"] || {};
  var exports = $PS["Data.Bounded"];
  var $foreign = $PS["Data.Bounded"];
  var Data_Ord = $PS["Data.Ord"];

  var Bounded = function Bounded(Ord0, bottom, top) {
    this.Ord0 = Ord0;
    this.bottom = bottom;
    this.top = top;
  };

  var top = function top(dict) {
    return dict.top;
  };

  var boundedInt = new Bounded(function () {
    return Data_Ord.ordInt;
  }, $foreign.bottomInt, $foreign.topInt);

  var bottom = function bottom(dict) {
    return dict.bottom;
  };

  exports["Bounded"] = Bounded;
  exports["bottom"] = bottom;
  exports["top"] = top;
  exports["boundedInt"] = boundedInt;
})(PS);

(function (exports) {
  "use strict";

  exports.round = Math.round;
})(PS["Math"] = PS["Math"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Math"] = $PS["Math"] || {};
  var exports = $PS["Math"];
  var $foreign = $PS["Math"];
  exports["round"] = $foreign.round;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Int"] = $PS["Data.Int"] || {};
  var exports = $PS["Data.Int"];
  var $foreign = $PS["Data.Int"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Global = $PS["Global"];
  var $$Math = $PS["Math"];
  var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

  var unsafeClamp = function unsafeClamp(x) {
    if (x === Global.infinity) {
      return 0;
    }

    ;

    if (x === -Global.infinity) {
      return 0;
    }

    ;

    if (x >= $foreign.toNumber(Data_Bounded.top(Data_Bounded.boundedInt))) {
      return Data_Bounded.top(Data_Bounded.boundedInt);
    }

    ;

    if (x <= $foreign.toNumber(Data_Bounded.bottom(Data_Bounded.boundedInt))) {
      return Data_Bounded.bottom(Data_Bounded.boundedInt);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.fromMaybe(0)(fromNumber(x));
    }

    ;
    throw new Error("Failed pattern match at Data.Int (line 66, column 1 - line 66, column 29): " + [x.constructor.name]);
  };

  var round = function round($23) {
    return unsafeClamp($$Math.round($23));
  };

  exports["fromNumber"] = fromNumber;
  exports["round"] = round;
  exports["toNumber"] = $foreign.toNumber;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Foreign"] = $PS["Foreign"] || {};
  var exports = $PS["Foreign"];
  var $foreign = $PS["Foreign"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Int = $PS["Data.Int"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];

  var ForeignError = function () {
    function ForeignError(value0) {
      this.value0 = value0;
    }

    ;

    ForeignError.create = function (value0) {
      return new ForeignError(value0);
    };

    return ForeignError;
  }();

  var TypeMismatch = function () {
    function TypeMismatch(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    TypeMismatch.create = function (value0) {
      return function (value1) {
        return new TypeMismatch(value0, value1);
      };
    };

    return TypeMismatch;
  }();

  var ErrorAtIndex = function () {
    function ErrorAtIndex(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ErrorAtIndex.create = function (value0) {
      return function (value1) {
        return new ErrorAtIndex(value0, value1);
      };
    };

    return ErrorAtIndex;
  }();

  var ErrorAtProperty = function () {
    function ErrorAtProperty(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ErrorAtProperty.create = function (value0) {
      return function (value1) {
        return new ErrorAtProperty(value0, value1);
      };
    };

    return ErrorAtProperty;
  }();

  var renderForeignError = function renderForeignError(v) {
    if (v instanceof ForeignError) {
      return v.value0;
    }

    ;

    if (v instanceof ErrorAtIndex) {
      return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + renderForeignError(v.value1)));
    }

    ;

    if (v instanceof ErrorAtProperty) {
      return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + renderForeignError(v.value1)));
    }

    ;

    if (v instanceof TypeMismatch) {
      return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    }

    ;
    throw new Error("Failed pattern match at Foreign (line 72, column 1 - line 72, column 45): " + [v.constructor.name]);
  };

  var fail = function () {
    var $107 = Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Data_Identity.monadIdentity));
    return function ($108) {
      return $107(Data_List_NonEmpty.singleton($108));
    };
  }();

  var unsafeReadTagged = function unsafeReadTagged(tag) {
    return function (value) {
      if ($foreign.tagOf(value) === tag) {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))($foreign.unsafeFromForeign(value));
      }

      ;

      if (Data_Boolean.otherwise) {
        return fail(new TypeMismatch(tag, $foreign.tagOf(value)));
      }

      ;
      throw new Error("Failed pattern match at Foreign (line 106, column 1 - line 106, column 55): " + [tag.constructor.name, value.constructor.name]);
    };
  };

  var readNumber = unsafeReadTagged("Number");

  var readInt = function readInt(value) {
    var error = Data_Either.Left.create(Data_List_NonEmpty.singleton(new TypeMismatch("Int", $foreign.tagOf(value))));

    var fromNumber = function () {
      var $109 = Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither));
      return function ($110) {
        return $109(Data_Int.fromNumber($110));
      };
    }();

    return Control_Monad_Except.mapExcept(Data_Either.either(Data_Function["const"](error))(fromNumber))(readNumber(value));
  };

  var readString = unsafeReadTagged("String");
  exports["ForeignError"] = ForeignError;
  exports["TypeMismatch"] = TypeMismatch;
  exports["renderForeignError"] = renderForeignError;
  exports["unsafeReadTagged"] = unsafeReadTagged;
  exports["readString"] = readString;
  exports["readInt"] = readInt;
  exports["fail"] = fail;
  exports["unsafeToForeign"] = $foreign.unsafeToForeign;
  exports["typeOf"] = $foreign.typeOf;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Affjax"] = $PS["Affjax"] || {};
  var exports = $PS["Affjax"];
  var $foreign = $PS["Affjax"];
  var Affjax_RequestBody = $PS["Affjax.RequestBody"];
  var Affjax_RequestHeader = $PS["Affjax.RequestHeader"];
  var Affjax_ResponseFormat = $PS["Affjax.ResponseFormat"];
  var Affjax_ResponseHeader = $PS["Affjax.ResponseHeader"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Argonaut_Core = $PS["Data.Argonaut.Core"];
  var Data_Argonaut_Parser = $PS["Data.Argonaut.Parser"];
  var Data_Array = $PS["Data.Array"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_FormURLEncoded = $PS["Data.FormURLEncoded"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HTTP_Method = $PS["Data.HTTP.Method"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_Compat = $PS["Effect.Aff.Compat"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Foreign = $PS["Foreign"];

  var RequestContentError = function () {
    function RequestContentError(value0) {
      this.value0 = value0;
    }

    ;

    RequestContentError.create = function (value0) {
      return new RequestContentError(value0);
    };

    return RequestContentError;
  }();

  var ResponseBodyError = function () {
    function ResponseBodyError(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ResponseBodyError.create = function (value0) {
      return function (value1) {
        return new ResponseBodyError(value0, value1);
      };
    };

    return ResponseBodyError;
  }();

  var XHRError = function () {
    function XHRError(value0) {
      this.value0 = value0;
    }

    ;

    XHRError.create = function (value0) {
      return new XHRError(value0);
    };

    return XHRError;
  }();

  var request = function request(req) {
    var parseJSON = function parseJSON(v) {
      if (v === "") {
        return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Argonaut_Core.jsonEmptyObject);
      }

      ;
      return Data_Either.either(function ($47) {
        return Foreign.fail(Foreign.ForeignError.create($47));
      })(Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity)))(Data_Argonaut_Parser.jsonParser(v));
    };

    var fromResponse = function () {
      if (req.responseFormat instanceof Affjax_ResponseFormat["ArrayBuffer"]) {
        return Foreign.unsafeReadTagged("ArrayBuffer");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Blob) {
        return Foreign.unsafeReadTagged("Blob");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Document) {
        return Foreign.unsafeReadTagged("Document");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Json) {
        return Control_Bind.composeKleisliFlipped(Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity))(function ($48) {
          return req.responseFormat.value0(parseJSON($48));
        })(Foreign.unsafeReadTagged("String"));
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat["String"]) {
        return Foreign.unsafeReadTagged("String");
      }

      ;

      if (req.responseFormat instanceof Affjax_ResponseFormat.Ignore) {
        return Data_Function["const"](req.responseFormat.value0(Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity))(Data_Unit.unit)));
      }

      ;
      throw new Error("Failed pattern match at Affjax (line 237, column 18 - line 243, column 57): " + [req.responseFormat.constructor.name]);
    }();

    var extractContent = function extractContent(v) {
      if (v instanceof Affjax_RequestBody.ArrayView) {
        return Data_Either.Right.create(v.value0(Foreign.unsafeToForeign));
      }

      ;

      if (v instanceof Affjax_RequestBody.Blob) {
        return Data_Either.Right.create(Foreign.unsafeToForeign(v.value0));
      }

      ;

      if (v instanceof Affjax_RequestBody.Document) {
        return Data_Either.Right.create(Foreign.unsafeToForeign(v.value0));
      }

      ;

      if (v instanceof Affjax_RequestBody["String"]) {
        return Data_Either.Right.create(Foreign.unsafeToForeign(v.value0));
      }

      ;

      if (v instanceof Affjax_RequestBody.FormData) {
        return Data_Either.Right.create(Foreign.unsafeToForeign(v.value0));
      }

      ;

      if (v instanceof Affjax_RequestBody.FormURLEncoded) {
        return Data_Either.note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(Data_Functor.map(Data_Maybe.functorMaybe)(Foreign.unsafeToForeign)(Data_FormURLEncoded.encode(v.value0)));
      }

      ;

      if (v instanceof Affjax_RequestBody.Json) {
        return Data_Either.Right.create(Foreign.unsafeToForeign(Data_Argonaut_Core.stringify(v.value0)));
      }

      ;
      throw new Error("Failed pattern match at Affjax (line 203, column 20 - line 218, column 69): " + [v.constructor.name]);
    };

    var addHeader = function addHeader(mh) {
      return function (hs) {
        if (mh instanceof Data_Maybe.Just && !Data_Foldable.any(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Function.on(Data_Eq.eq(Data_Eq.eqString))(Affjax_RequestHeader.name)(mh.value0))(hs)) {
          return Data_Array.snoc(hs)(mh.value0);
        }

        ;
        return hs;
      };
    };

    var headers = function headers(reqContent) {
      return addHeader(Data_Functor.map(Data_Maybe.functorMaybe)(Affjax_RequestHeader.ContentType.create)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Affjax_RequestBody.toMediaType)(reqContent)))(addHeader(Data_Functor.map(Data_Maybe.functorMaybe)(Affjax_RequestHeader.Accept.create)(Affjax_ResponseFormat.toMediaType(req.responseFormat)))(req.headers));
    };

    var ajaxRequest = function ajaxRequest(v) {
      return {
        method: Data_HTTP_Method.print(req.method),
        url: req.url,
        headers: Data_Functor.map(Data_Functor.functorArray)(function (h) {
          return {
            field: Affjax_RequestHeader.name(h),
            value: Affjax_RequestHeader.value(h)
          };
        })(headers(req.content)),
        content: v,
        responseType: Affjax_ResponseFormat.toResponseType(req.responseFormat),
        username: Data_Nullable.toNullable(req.username),
        password: Data_Nullable.toNullable(req.password),
        withCredentials: req.withCredentials
      };
    };

    var send = function send(content) {
      return Data_Functor.mapFlipped(Effect_Aff.functorAff)(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(Effect_Aff_Compat.fromEffectFnAff($foreign["_ajax"](Affjax_ResponseHeader.ResponseHeader.create, ajaxRequest(content)))))(function (v) {
        if (v instanceof Data_Either.Right) {
          var v1 = Control_Monad_Except.runExcept(fromResponse(v.value0.body));

          if (v1 instanceof Data_Either.Left) {
            return new Data_Either.Left(new ResponseBodyError(Data_List_NonEmpty.head(v1.value0), v.value0));
          }

          ;

          if (v1 instanceof Data_Either.Right) {
            return new Data_Either.Right({
              body: v1.value0,
              headers: v.value0.headers,
              status: v.value0.status,
              statusText: v.value0.statusText
            });
          }

          ;
          throw new Error("Failed pattern match at Affjax (line 184, column 9 - line 186, column 52): " + [v1.constructor.name]);
        }

        ;

        if (v instanceof Data_Either.Left) {
          return new Data_Either.Left(new XHRError(v.value0));
        }

        ;
        throw new Error("Failed pattern match at Affjax (line 182, column 86 - line 188, column 28): " + [v.constructor.name]);
      });
    };

    if (req.content instanceof Data_Maybe.Nothing) {
      return send(Data_Nullable.toNullable(Data_Maybe.Nothing.value));
    }

    ;

    if (req.content instanceof Data_Maybe.Just) {
      var v = extractContent(req.content.value0);

      if (v instanceof Data_Either.Right) {
        return send(Data_Nullable.toNullable(new Data_Maybe.Just(v.value0)));
      }

      ;

      if (v instanceof Data_Either.Left) {
        return Control_Applicative.pure(Effect_Aff.applicativeAff)(new Data_Either.Left(new RequestContentError(v.value0)));
      }

      ;
      throw new Error("Failed pattern match at Affjax (line 173, column 7 - line 177, column 48): " + [v.constructor.name]);
    }

    ;
    throw new Error("Failed pattern match at Affjax (line 169, column 3 - line 177, column 48): " + [req.content.constructor.name]);
  };

  var printError = function printError(v) {
    if (v instanceof RequestContentError) {
      return "There was a problem with the request content: " + v.value0;
    }

    ;

    if (v instanceof ResponseBodyError) {
      return "There was a problem with the response body: " + Foreign.renderForeignError(v.value0);
    }

    ;

    if (v instanceof XHRError) {
      return "There was a problem making the request: " + Effect_Exception.message(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Affjax (line 91, column 14 - line 97, column 66): " + [v.constructor.name]);
  };

  var defaultRequest = {
    method: new Data_Either.Left(Data_HTTP_Method.GET.value),
    url: "/",
    headers: [],
    content: Data_Maybe.Nothing.value,
    username: Data_Maybe.Nothing.value,
    password: Data_Maybe.Nothing.value,
    withCredentials: false,
    responseFormat: Affjax_ResponseFormat.ignore
  };
  exports["defaultRequest"] = defaultRequest;
  exports["printError"] = printError;
  exports["request"] = request;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Applicative.Free"] = $PS["Control.Applicative.Free"] || {};
  var exports = $PS["Control.Applicative.Free"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Category = $PS["Control.Category"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_NonEmpty = $PS["Data.List.NonEmpty"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Tuple = $PS["Data.Tuple"];

  var Pure = function () {
    function Pure(value0) {
      this.value0 = value0;
    }

    ;

    Pure.create = function (value0) {
      return new Pure(value0);
    };

    return Pure;
  }();

  var Lift = function () {
    function Lift(value0) {
      this.value0 = value0;
    }

    ;

    Lift.create = function (value0) {
      return new Lift(value0);
    };

    return Lift;
  }();

  var Ap = function () {
    function Ap(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Ap.create = function (value0) {
      return function (value1) {
        return new Ap(value0, value1);
      };
    };

    return Ap;
  }();

  var mkAp = function mkAp(fba) {
    return function (fb) {
      return new Ap(fba, fb);
    };
  };

  var liftFreeAp = Lift.create;

  var goLeft = function goLeft($copy_dictApplicative) {
    return function ($copy_fStack) {
      return function ($copy_valStack) {
        return function ($copy_nat) {
          return function ($copy_func) {
            return function ($copy_count) {
              var $tco_var_dictApplicative = $copy_dictApplicative;
              var $tco_var_fStack = $copy_fStack;
              var $tco_var_valStack = $copy_valStack;
              var $tco_var_nat = $copy_nat;
              var $tco_var_func = $copy_func;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(dictApplicative, fStack, valStack, nat, func, count) {
                if (func instanceof Pure) {
                  $tco_done = true;
                  return new Data_Tuple.Tuple(new Data_List_Types.Cons({
                    func: Control_Applicative.pure(dictApplicative)(func.value0),
                    count: count
                  }, fStack), valStack);
                }

                ;

                if (func instanceof Lift) {
                  $tco_done = true;
                  return new Data_Tuple.Tuple(new Data_List_Types.Cons({
                    func: nat(func.value0),
                    count: count
                  }, fStack), valStack);
                }

                ;

                if (func instanceof Ap) {
                  $tco_var_dictApplicative = dictApplicative;
                  $tco_var_fStack = fStack;
                  $tco_var_valStack = Data_List_NonEmpty.cons(func.value1)(valStack);
                  $tco_var_nat = nat;
                  $tco_var_func = func.value0;
                  $copy_count = count + 1 | 0;
                  return;
                }

                ;
                throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_valStack, $tco_var_nat, $tco_var_func, $copy_count);
              }

              ;
              return $tco_result;
            };
          };
        };
      };
    };
  };

  var goApply = function goApply($copy_dictApplicative) {
    return function ($copy_fStack) {
      return function ($copy_vals) {
        return function ($copy_gVal) {
          var $tco_var_dictApplicative = $copy_dictApplicative;
          var $tco_var_fStack = $copy_fStack;
          var $tco_var_vals = $copy_vals;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(dictApplicative, fStack, vals, gVal) {
            if (fStack instanceof Data_List_Types.Nil) {
              $tco_done = true;
              return new Data_Either.Left(gVal);
            }

            ;

            if (fStack instanceof Data_List_Types.Cons) {
              var gRes = Control_Apply.apply(dictApplicative.Apply0())(fStack.value0.func)(gVal);
              var $14 = fStack.value0.count === 1;

              if ($14) {
                if (fStack.value1 instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return new Data_Either.Left(gRes);
                }

                ;
                $tco_var_dictApplicative = dictApplicative;
                $tco_var_fStack = fStack.value1;
                $tco_var_vals = vals;
                $copy_gVal = gRes;
                return;
              }

              ;

              if (vals instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return new Data_Either.Left(gRes);
              }

              ;

              if (vals instanceof Data_List_Types.Cons) {
                $tco_done = true;
                return Data_Either.Right.create(new Data_Tuple.Tuple(new Data_List_Types.Cons({
                  func: gRes,
                  count: fStack.value0.count - 1 | 0
                }, fStack.value1), new Data_NonEmpty.NonEmpty(vals.value0, vals.value1)));
              }

              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
            }

            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_vals, $copy_gVal);
          }

          ;
          return $tco_result;
        };
      };
    };
  };

  var functorFreeAp = new Data_Functor.Functor(function (f) {
    return function (x) {
      return mkAp(new Pure(f))(x);
    };
  });

  var foldFreeAp = function foldFreeAp(dictApplicative) {
    return function (nat) {
      return function (z) {
        var go = function go($copy_v) {
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(Control_Applicative.pure(dictApplicative)(v.value1.value0.value0));

              if (v1 instanceof Data_Either.Left) {
                $tco_done = true;
                return v1.value0;
              }

              ;

              if (v1 instanceof Data_Either.Right) {
                $copy_v = v1.value0;
                return;
              }

              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }

            ;

            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));

              if (v1 instanceof Data_Either.Left) {
                $tco_done = true;
                return v1.value0;
              }

              ;

              if (v1 instanceof Data_Either.Right) {
                $copy_v = v1.value0;
                return;
              }

              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }

            ;

            if (v.value1.value0 instanceof Ap) {
              var nextVals = new Data_NonEmpty.NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft(dictApplicative)(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }

            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }

          ;
          return $tco_result;
        };

        return go(new Data_Tuple.Tuple(Data_List_Types.Nil.value, Data_List_NonEmpty.singleton(z)));
      };
    };
  };

  var retractFreeAp = function retractFreeAp(dictApplicative) {
    return foldFreeAp(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
  };

  var applyFreeAp = new Control_Apply.Apply(function () {
    return functorFreeAp;
  }, function (fba) {
    return function (fb) {
      return mkAp(fba)(fb);
    };
  });
  var applicativeFreeAp = new Control_Applicative.Applicative(function () {
    return applyFreeAp;
  }, Pure.create);

  var hoistFreeAp = function hoistFreeAp(f) {
    return foldFreeAp(applicativeFreeAp)(function ($37) {
      return liftFreeAp(f($37));
    });
  };

  exports["retractFreeAp"] = retractFreeAp;
  exports["hoistFreeAp"] = hoistFreeAp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Exists"] = $PS["Data.Exists"] || {};
  var exports = $PS["Data.Exists"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var runExists = Unsafe_Coerce.unsafeCoerce;
  var mkExists = Unsafe_Coerce.unsafeCoerce;
  exports["mkExists"] = mkExists;
  exports["runExists"] = runExists;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Free.Trans"] = $PS["Control.Monad.Free.Trans"] || {};
  var exports = $PS["Control.Monad.Free.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Unit = $PS["Data.Unit"];

  var Bound = function () {
    function Bound(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Bound.create = function (value0) {
      return function (value1) {
        return new Bound(value0, value1);
      };
    };

    return Bound;
  }();

  var FreeT = function () {
    function FreeT(value0) {
      this.value0 = value0;
    }

    ;

    FreeT.create = function (value0) {
      return new FreeT(value0);
    };

    return FreeT;
  }();

  var Bind = function () {
    function Bind(value0) {
      this.value0 = value0;
    }

    ;

    Bind.create = function (value0) {
      return new Bind(value0);
    };

    return Bind;
  }();

  var monadTransFreeT = function monadTransFreeT(dictFunctor) {
    return new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
      return function (ma) {
        return new FreeT(function (v) {
          return Data_Functor.map(dictMonad.Bind1().Apply0().Functor0())(Data_Either.Left.create)(ma);
        });
      };
    });
  };

  var freeT = FreeT.create;

  var bound = function bound(m) {
    return function (f) {
      return new Bind(Data_Exists.mkExists(new Bound(m, f)));
    };
  };

  var functorFreeT = function functorFreeT(dictFunctor) {
    return function (dictFunctor1) {
      return new Data_Functor.Functor(function (f) {
        return function (v) {
          if (v instanceof FreeT) {
            return new FreeT(function (v1) {
              return Data_Functor.map(dictFunctor1)(Data_Bifunctor.bimap(Data_Either.bifunctorEither)(f)(Data_Functor.map(dictFunctor)(Data_Functor.map(functorFreeT(dictFunctor)(dictFunctor1))(f))))(v.value0(Data_Unit.unit));
            });
          }

          ;

          if (v instanceof Bind) {
            return Data_Exists.runExists(function (v1) {
              return bound(v1.value0)(function () {
                var $123 = Data_Functor.map(functorFreeT(dictFunctor)(dictFunctor1))(f);
                return function ($124) {
                  return $123(v1.value1($124));
                };
              }());
            })(v.value0);
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 59, column 1 - line 61, column 71): " + [f.constructor.name, v.constructor.name]);
        };
      });
    };
  };

  var monadFreeT = function monadFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Monad.Monad(function () {
        return applicativeFreeT(dictFunctor)(dictMonad);
      }, function () {
        return bindFreeT(dictFunctor)(dictMonad);
      });
    };
  };

  var bindFreeT = function bindFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Bind.Bind(function () {
        return applyFreeT(dictFunctor)(dictMonad);
      }, function (v) {
        return function (f) {
          if (v instanceof Bind) {
            return Data_Exists.runExists(function (v1) {
              return bound(v1.value0)(function (x) {
                return bound(function (v2) {
                  return v1.value1(x);
                })(f);
              });
            })(v.value0);
          }

          ;
          return bound(function (v1) {
            return v;
          })(f);
        };
      });
    };
  };

  var applyFreeT = function applyFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Apply.Apply(function () {
        return functorFreeT(dictFunctor)(dictMonad.Bind1().Apply0().Functor0());
      }, Control_Monad.ap(monadFreeT(dictFunctor)(dictMonad)));
    };
  };

  var applicativeFreeT = function applicativeFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Applicative.Applicative(function () {
        return applyFreeT(dictFunctor)(dictMonad);
      }, function (a) {
        return new FreeT(function (v) {
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Left(a));
        });
      });
    };
  };

  var liftFreeT = function liftFreeT(dictFunctor) {
    return function (dictMonad) {
      return function (fa) {
        return new FreeT(function (v) {
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(Data_Functor.map(dictFunctor)(Control_Applicative.pure(applicativeFreeT(dictFunctor)(dictMonad)))(fa)));
        });
      };
    };
  };

  var resume = function resume(dictFunctor) {
    return function (dictMonadRec) {
      var go = function go(v) {
        if (v instanceof FreeT) {
          return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Control_Monad_Rec_Class.Done.create)(v.value0(Data_Unit.unit));
        }

        ;

        if (v instanceof Bind) {
          return Data_Exists.runExists(function (v1) {
            var v2 = v1.value0(Data_Unit.unit);

            if (v2 instanceof FreeT) {
              return Control_Bind.bind(dictMonadRec.Monad0().Bind1())(v2.value0(Data_Unit.unit))(function (v3) {
                if (v3 instanceof Data_Either.Left) {
                  return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Loop(v1.value1(v3.value0)));
                }

                ;

                if (v3 instanceof Data_Either.Right) {
                  return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Done(new Data_Either.Right(Data_Functor.map(dictFunctor)(function (h) {
                    return Control_Bind.bind(bindFreeT(dictFunctor)(dictMonadRec.Monad0()))(h)(v1.value1);
                  })(v3.value0))));
                }

                ;
                throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 54, column 20 - line 56, column 67): " + [v3.constructor.name]);
              });
            }

            ;

            if (v2 instanceof Bind) {
              return Data_Exists.runExists(function (v3) {
                return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Loop(Control_Bind.bind(bindFreeT(dictFunctor)(dictMonadRec.Monad0()))(v3.value0(Data_Unit.unit))(function (z) {
                  return Control_Bind.bind(bindFreeT(dictFunctor)(dictMonadRec.Monad0()))(v3.value1(z))(v1.value1);
                })));
              })(v2.value0);
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 52, column 5 - line 57, column 98): " + [v2.constructor.name]);
          })(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 49, column 3 - line 49, column 75): " + [v.constructor.name]);
      };

      return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
  };

  var runFreeT = function runFreeT(dictFunctor) {
    return function (dictMonadRec) {
      return function (interp) {
        var go = function go(v) {
          if (v instanceof Data_Either.Left) {
            return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Control_Monad_Rec_Class.Done(v.value0));
          }

          ;

          if (v instanceof Data_Either.Right) {
            return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Control_Monad_Rec_Class.Loop.create)(interp(v.value0));
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 141, column 3 - line 141, column 63): " + [v.constructor.name]);
        };

        return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(Control_Bind.composeKleisliFlipped(dictMonadRec.Monad0().Bind1())(go)(resume(dictFunctor)(dictMonadRec)));
      };
    };
  };

  var monadRecFreeT = function monadRecFreeT(dictFunctor) {
    return function (dictMonad) {
      return new Control_Monad_Rec_Class.MonadRec(function () {
        return monadFreeT(dictFunctor)(dictMonad);
      }, function (f) {
        var go = function go(s) {
          return Control_Bind.bind(bindFreeT(dictFunctor)(dictMonad))(f(s))(function (v) {
            if (v instanceof Control_Monad_Rec_Class.Loop) {
              return go(v.value0);
            }

            ;

            if (v instanceof Control_Monad_Rec_Class.Done) {
              return Control_Applicative.pure(applicativeFreeT(dictFunctor)(dictMonad))(v.value0);
            }

            ;
            throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 82, column 15 - line 84, column 25): " + [v.constructor.name]);
          });
        };

        return go;
      });
    };
  };

  exports["freeT"] = freeT;
  exports["liftFreeT"] = liftFreeT;
  exports["resume"] = resume;
  exports["runFreeT"] = runFreeT;
  exports["functorFreeT"] = functorFreeT;
  exports["applicativeFreeT"] = applicativeFreeT;
  exports["bindFreeT"] = bindFreeT;
  exports["monadTransFreeT"] = monadTransFreeT;
  exports["monadRecFreeT"] = monadRecFreeT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Profunctor"] = $PS["Data.Profunctor"] || {};
  var exports = $PS["Data.Profunctor"];
  var Control_Category = $PS["Control.Category"];

  var Profunctor = function Profunctor(dimap) {
    this.dimap = dimap;
  };

  var profunctorFn = new Profunctor(function (a2b) {
    return function (c2d) {
      return function (b2c) {
        return function ($9) {
          return c2d(b2c(a2b($9)));
        };
      };
    };
  });

  var dimap = function dimap(dict) {
    return dict.dimap;
  };

  var rmap = function rmap(dictProfunctor) {
    return function (b2c) {
      return dimap(dictProfunctor)(Control_Category.identity(Control_Category.categoryFn))(b2c);
    };
  };

  exports["dimap"] = dimap;
  exports["Profunctor"] = Profunctor;
  exports["rmap"] = rmap;
  exports["profunctorFn"] = profunctorFn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Coroutine"] = $PS["Control.Coroutine"] || {};
  var exports = $PS["Control.Coroutine"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_Free_Trans = $PS["Control.Monad.Free.Trans"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];

  var Emit = function () {
    function Emit(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Emit.create = function (value0) {
      return function (value1) {
        return new Emit(value0, value1);
      };
    };

    return Emit;
  }();

  var runProcess = function runProcess(dictMonadRec) {
    return Control_Monad_Free_Trans.runFreeT(Data_Identity.functorIdentity)(dictMonadRec)(function () {
      var $176 = Control_Applicative.pure(dictMonadRec.Monad0().Applicative0());
      var $177 = Data_Newtype.unwrap(Data_Identity.newtypeIdentity);
      return function ($178) {
        return $176($177($178));
      };
    }());
  };

  var profunctorAwait = new Data_Profunctor.Profunctor(function (f) {
    return function (g) {
      return function (v) {
        return Data_Profunctor.dimap(Data_Profunctor.profunctorFn)(f)(g)(v);
      };
    };
  });

  var loop = function loop(dictFunctor) {
    return function (dictMonad) {
      return function (me) {
        return Control_Monad_Rec_Class.tailRecM(Control_Monad_Free_Trans.monadRecFreeT(dictFunctor)(dictMonad))(function (v) {
          return Data_Functor.map(Control_Monad_Free_Trans.functorFreeT(dictFunctor)(dictMonad.Bind1().Apply0().Functor0()))(Data_Maybe.maybe(new Control_Monad_Rec_Class.Loop(Data_Unit.unit))(Control_Monad_Rec_Class.Done.create))(me);
        })(Data_Unit.unit);
      };
    };
  };

  var fuseWithL = function fuseWithL(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictFunctor2) {
        return function (dictMonadRec) {
          return function (zap) {
            return function (fs) {
              return function (gs) {
                var go = function go(v) {
                  return Control_Monad_Except_Trans.runExceptT(Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(dictMonadRec.Monad0()))(Control_Monad_Except_Trans.ExceptT(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(v.value0)))(function (l) {
                    return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(dictMonadRec.Monad0()))(Control_Monad_Except_Trans.ExceptT(Control_Monad_Free_Trans.resume(dictFunctor1)(dictMonadRec)(v.value1)))(function (r) {
                      return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonadRec.Monad0()))(Data_Functor.map(dictFunctor2)(function (t) {
                        return Control_Monad_Free_Trans.freeT(function (v1) {
                          return go(t);
                        });
                      })(zap(Data_Tuple.Tuple.create)(l)(r)));
                    });
                  }));
                };

                return Control_Monad_Free_Trans.freeT(function (v) {
                  return go(new Data_Tuple.Tuple(fs, gs));
                });
              };
            };
          };
        };
      };
    };
  };

  var fuseWith = function fuseWith(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictFunctor2) {
        return function (dictMonadRec) {
          return function (dictParallel) {
            return function (zap) {
              return function (fs) {
                return function (gs) {
                  var go = function go(v) {
                    return Control_Bind.bind(dictMonadRec.Monad0().Bind1())(Control_Parallel_Class.sequential(dictParallel)(Control_Apply.apply(dictParallel.Applicative1().Apply0())(Data_Functor.map(dictParallel.Applicative1().Apply0().Functor0())(Control_Apply.lift2(Data_Either.applyEither)(zap(Data_Tuple.Tuple.create)))(Control_Parallel_Class.parallel(dictParallel)(Control_Monad_Free_Trans.resume(dictFunctor)(dictMonadRec)(v.value0))))(Control_Parallel_Class.parallel(dictParallel)(Control_Monad_Free_Trans.resume(dictFunctor1)(dictMonadRec)(v.value1)))))(function (next) {
                      if (next instanceof Data_Either.Left) {
                        return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Data_Either.Left(next.value0));
                      }

                      ;

                      if (next instanceof Data_Either.Right) {
                        return Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(new Data_Either.Right(Data_Functor.map(dictFunctor2)(function (t) {
                          return Control_Monad_Free_Trans.freeT(function (v1) {
                            return go(t);
                          });
                        })(next.value0)));
                      }

                      ;
                      throw new Error("Failed pattern match at Control.Coroutine (line 79, column 5 - line 81, column 63): " + [next.constructor.name]);
                    });
                  };

                  return Control_Monad_Free_Trans.freeT(function (v) {
                    return go(new Data_Tuple.Tuple(fs, gs));
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  var functorAwait = new Data_Functor.Functor(Data_Profunctor.rmap(profunctorAwait));
  var bifunctorEmit = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        return new Emit(f(v.value0), g(v.value1));
      };
    };
  });
  var functorEmit = new Data_Functor.Functor(Data_Bifunctor.rmap(bifunctorEmit));

  var connect = function connect(dictMonadRec) {
    return function (dictParallel) {
      return fuseWith(functorEmit)(functorAwait)(Data_Identity.functorIdentity)(dictMonadRec)(dictParallel)(function (f) {
        return function (v) {
          return function (v1) {
            return f(v.value1)(v1(v.value0));
          };
        };
      });
    };
  };

  var emit = function emit(dictMonad) {
    return function (o) {
      return Control_Monad_Free_Trans.liftFreeT(functorEmit)(dictMonad)(new Emit(o, Data_Unit.unit));
    };
  };

  var producer = function producer(dictMonad) {
    return function (recv) {
      return loop(functorEmit)(dictMonad)(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(functorEmit)(dictMonad))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(functorEmit))(dictMonad)(recv))(function (e) {
        if (e instanceof Data_Either.Left) {
          return Data_Functor.voidLeft(Control_Monad_Free_Trans.functorFreeT(functorEmit)(dictMonad.Bind1().Apply0().Functor0()))(emit(dictMonad)(e.value0))(Data_Maybe.Nothing.value);
        }

        ;

        if (e instanceof Data_Either.Right) {
          return Control_Applicative.pure(Control_Monad_Free_Trans.applicativeFreeT(functorEmit)(dictMonad))(new Data_Maybe.Just(e.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Coroutine (line 125, column 3 - line 127, column 29): " + [e.constructor.name]);
      }));
    };
  };

  var pullFrom = function pullFrom(dictMonadRec) {
    return fuseWithL(functorAwait)(functorEmit)(Data_Identity.functorIdentity)(dictMonadRec)(function (f) {
      return function (v) {
        return function (v1) {
          return Control_Applicative.pure(Data_Identity.applicativeIdentity)(f(v(v1.value0))(v1.value1));
        };
      };
    });
  };

  var $$await = function $$await(dictMonad) {
    return Control_Monad_Free_Trans.liftFreeT(functorAwait)(dictMonad)(Control_Category.identity(Control_Category.categoryFn));
  };

  exports["runProcess"] = runProcess;
  exports["producer"] = producer;
  exports["await"] = $$await;
  exports["connect"] = connect;
  exports["pullFrom"] = pullFrom;
  exports["functorAwait"] = functorAwait;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Fork.Class"] = $PS["Control.Monad.Fork.Class"] || {};
  var exports = $PS["Control.Monad.Fork.Class"];
  var Effect_Aff = $PS["Effect.Aff"];

  var MonadFork = function MonadFork(Functor1, Monad0, fork, join, suspend) {
    this.Functor1 = Functor1;
    this.Monad0 = Monad0;
    this.fork = fork;
    this.join = join;
    this.suspend = suspend;
  };

  var monadForkAff = new MonadFork(function () {
    return Effect_Aff.functorFiber;
  }, function () {
    return Effect_Aff.monadAff;
  }, Effect_Aff.forkAff, Effect_Aff.joinFiber, Effect_Aff.suspendAff);

  var fork = function fork(dict) {
    return dict.fork;
  };

  exports["fork"] = fork;
  exports["monadForkAff"] = monadForkAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.List"] = $PS["Data.List"] || {};
  var exports = $PS["Data.List"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];

  var uncons = function uncons(v) {
    if (v instanceof Data_List_Types.Nil) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v instanceof Data_List_Types.Cons) {
      return new Data_Maybe.Just({
        head: v.value0,
        tail: v.value1
      });
    }

    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };

  var toUnfoldable = function toUnfoldable(dictUnfoldable) {
    return Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
      return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
        return new Data_Tuple.Tuple(rec.head, rec.tail);
      })(uncons(xs));
    });
  };

  var tail = function tail(v) {
    if (v instanceof Data_List_Types.Nil) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v instanceof Data_List_Types.Cons) {
      return new Data_Maybe.Just(v.value1);
    }

    ;
    throw new Error("Failed pattern match at Data.List (line 245, column 1 - line 245, column 43): " + [v.constructor.name]);
  };

  var reverse = function () {
    var go = function go($copy_acc) {
      return function ($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(acc, v) {
          if (v instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return acc;
          }

          ;

          if (v instanceof Data_List_Types.Cons) {
            $tco_var_acc = new Data_List_Types.Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return go(Data_List_Types.Nil.value);
  }();

  var take = function () {
    var go = function go($copy_acc) {
      return function ($copy_v) {
        return function ($copy_v1) {
          var $tco_var_acc = $copy_acc;
          var $tco_var_v = $copy_v;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(acc, v, v1) {
            if (v < 1) {
              $tco_done = true;
              return reverse(acc);
            }

            ;

            if (v1 instanceof Data_List_Types.Nil) {
              $tco_done = true;
              return reverse(acc);
            }

            ;

            if (v1 instanceof Data_List_Types.Cons) {
              $tco_var_acc = new Data_List_Types.Cons(v1.value0, acc);
              $tco_var_v = v - 1 | 0;
              $copy_v1 = v1.value1;
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.List (line 520, column 3 - line 520, column 35): " + [acc.constructor.name, v.constructor.name, v1.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_acc, $tco_var_v, $copy_v1);
          }

          ;
          return $tco_result;
        };
      };
    };

    return go(Data_List_Types.Nil.value);
  }();

  var zipWith = function zipWith(f) {
    return function (xs) {
      return function (ys) {
        var go = function go($copy_v) {
          return function ($copy_v1) {
            return function ($copy_acc) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(v, v1, acc) {
                if (v instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return acc;
                }

                ;

                if (v1 instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return acc;
                }

                ;

                if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                  $tco_var_v = v.value1;
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Data_List_Types.Cons(f(v.value0)(v1.value0), acc);
                  return;
                }

                ;
                throw new Error("Failed pattern match at Data.List (line 718, column 3 - line 718, column 21): " + [v.constructor.name, v1.constructor.name, acc.constructor.name]);
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_acc);
              }

              ;
              return $tco_result;
            };
          };
        };

        return reverse(go(xs)(ys)(Data_List_Types.Nil.value));
      };
    };
  };

  var $$null = function $$null(v) {
    if (v instanceof Data_List_Types.Nil) {
      return true;
    }

    ;
    return false;
  };

  var mapMaybe = function mapMaybe(f) {
    var go = function go($copy_acc) {
      return function ($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(acc, v) {
          if (v instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return reverse(acc);
          }

          ;

          if (v instanceof Data_List_Types.Cons) {
            var v1 = f(v.value0);

            if (v1 instanceof Data_Maybe.Nothing) {
              $tco_var_acc = acc;
              $copy_v = v.value1;
              return;
            }

            ;

            if (v1 instanceof Data_Maybe.Just) {
              $tco_var_acc = new Data_List_Types.Cons(v1.value0, acc);
              $copy_v = v.value1;
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.List (line 419, column 5 - line 421, column 32): " + [v1.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Data.List (line 417, column 3 - line 417, column 27): " + [acc.constructor.name, v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }

        ;
        return $tco_result;
      };
    };

    return go(Data_List_Types.Nil.value);
  };

  var head = function head(v) {
    if (v instanceof Data_List_Types.Nil) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v instanceof Data_List_Types.Cons) {
      return new Data_Maybe.Just(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Data.List (line 230, column 1 - line 230, column 22): " + [v.constructor.name]);
  };

  var fromFoldable = function fromFoldable(dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(Data_List_Types.Cons.create)(Data_List_Types.Nil.value);
  };

  var drop = function drop($copy_v) {
    return function ($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;

      function $tco_loop(v, v1) {
        if (v < 1) {
          $tco_done = true;
          return v1;
        }

        ;

        if (v1 instanceof Data_List_Types.Nil) {
          $tco_done = true;
          return Data_List_Types.Nil.value;
        }

        ;

        if (v1 instanceof Data_List_Types.Cons) {
          $tco_var_v = v - 1 | 0;
          $copy_v1 = v1.value1;
          return;
        }

        ;
        throw new Error("Failed pattern match at Data.List (line 543, column 1 - line 543, column 42): " + [v.constructor.name, v1.constructor.name]);
      }

      ;

      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }

      ;
      return $tco_result;
    };
  };

  exports["toUnfoldable"] = toUnfoldable;
  exports["fromFoldable"] = fromFoldable;
  exports["null"] = $$null;
  exports["reverse"] = reverse;
  exports["mapMaybe"] = mapMaybe;
  exports["take"] = take;
  exports["drop"] = drop;
  exports["zipWith"] = zipWith;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.CatQueue"] = $PS["Data.CatQueue"] || {};
  var exports = $PS["Data.CatQueue"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];

  var CatQueue = function () {
    function CatQueue(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    CatQueue.create = function (value0) {
      return function (value1) {
        return new CatQueue(value0, value1);
      };
    };

    return CatQueue;
  }();

  var uncons = function uncons($copy_v) {
    var $tco_done = false;
    var $tco_result;

    function $tco_loop(v) {
      if (v.value0 instanceof Data_List_Types.Nil && v.value1 instanceof Data_List_Types.Nil) {
        $tco_done = true;
        return Data_Maybe.Nothing.value;
      }

      ;

      if (v.value0 instanceof Data_List_Types.Nil) {
        $copy_v = new CatQueue(Data_List.reverse(v.value1), Data_List_Types.Nil.value);
        return;
      }

      ;

      if (v.value0 instanceof Data_List_Types.Cons) {
        $tco_done = true;
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }

      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 83, column 1 - line 83, column 63): " + [v.constructor.name]);
    }

    ;

    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }

    ;
    return $tco_result;
  };

  var snoc = function snoc(v) {
    return function (a) {
      return new CatQueue(v.value0, new Data_List_Types.Cons(a, v.value1));
    };
  };

  var $$null = function $$null(v) {
    if (v.value0 instanceof Data_List_Types.Nil && v.value1 instanceof Data_List_Types.Nil) {
      return true;
    }

    ;
    return false;
  };

  var empty = new CatQueue(Data_List_Types.Nil.value, Data_List_Types.Nil.value);
  exports["empty"] = empty;
  exports["null"] = $$null;
  exports["snoc"] = snoc;
  exports["uncons"] = uncons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.CatList"] = $PS["Data.CatList"] || {};
  var exports = $PS["Data.CatList"];
  var Data_CatQueue = $PS["Data.CatQueue"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Tuple = $PS["Data.Tuple"];

  var CatNil = function () {
    function CatNil() {}

    ;
    CatNil.value = new CatNil();
    return CatNil;
  }();

  var CatCons = function () {
    function CatCons(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    CatCons.create = function (value0) {
      return function (value1) {
        return new CatCons(value0, value1);
      };
    };

    return CatCons;
  }();

  var link = function link(v) {
    return function (v1) {
      if (v instanceof CatNil) {
        return v1;
      }

      ;

      if (v1 instanceof CatNil) {
        return v;
      }

      ;

      if (v instanceof CatCons) {
        return new CatCons(v.value0, Data_CatQueue.snoc(v.value1)(v1));
      }

      ;
      throw new Error("Failed pattern match at Data.CatList (line 109, column 1 - line 109, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };

  var foldr = function foldr(k) {
    return function (b) {
      return function (q) {
        var foldl = function foldl($copy_v) {
          return function ($copy_c) {
            return function ($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_var_c = $copy_c;
              var $tco_done = false;
              var $tco_result;

              function $tco_loop(v, c, v1) {
                if (v1 instanceof Data_List_Types.Nil) {
                  $tco_done = true;
                  return c;
                }

                ;

                if (v1 instanceof Data_List_Types.Cons) {
                  $tco_var_v = v;
                  $tco_var_c = v(c)(v1.value0);
                  $copy_v1 = v1.value1;
                  return;
                }

                ;
                throw new Error("Failed pattern match at Data.CatList (line 125, column 3 - line 125, column 59): " + [v.constructor.name, c.constructor.name, v1.constructor.name]);
              }

              ;

              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_c, $copy_v1);
              }

              ;
              return $tco_result;
            };
          };
        };

        var go = function go($copy_xs) {
          return function ($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done = false;
            var $tco_result;

            function $tco_loop(xs, ys) {
              var v = Data_CatQueue.uncons(xs);

              if (v instanceof Data_Maybe.Nothing) {
                $tco_done = true;
                return foldl(function (x) {
                  return function (i) {
                    return i(x);
                  };
                })(b)(ys);
              }

              ;

              if (v instanceof Data_Maybe.Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Data_List_Types.Cons(k(v.value0.value0), ys);
                return;
              }

              ;
              throw new Error("Failed pattern match at Data.CatList (line 121, column 14 - line 123, column 67): " + [v.constructor.name]);
            }

            ;

            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }

            ;
            return $tco_result;
          };
        };

        return go(q)(Data_List_Types.Nil.value);
      };
    };
  };

  var uncons = function uncons(v) {
    if (v instanceof CatNil) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v instanceof CatCons) {
      return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, function () {
        var $44 = Data_CatQueue["null"](v.value1);

        if ($44) {
          return CatNil.value;
        }

        ;
        return foldr(link)(CatNil.value)(v.value1);
      }()));
    }

    ;
    throw new Error("Failed pattern match at Data.CatList (line 100, column 1 - line 100, column 61): " + [v.constructor.name]);
  };

  var empty = CatNil.value;
  var append = link;
  var semigroupCatList = new Data_Semigroup.Semigroup(append);

  var snoc = function snoc(cat) {
    return function (a) {
      return append(cat)(new CatCons(a, Data_CatQueue.empty));
    };
  };

  exports["empty"] = empty;
  exports["snoc"] = snoc;
  exports["uncons"] = uncons;
  exports["semigroupCatList"] = semigroupCatList;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Free"] = $PS["Control.Monad.Free"] || {};
  var exports = $PS["Control.Monad.Free"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Data_CatList = $PS["Data.CatList"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Free = function () {
    function Free(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Free.create = function (value0) {
      return function (value1) {
        return new Free(value0, value1);
      };
    };

    return Free;
  }();

  var Return = function () {
    function Return(value0) {
      this.value0 = value0;
    }

    ;

    Return.create = function (value0) {
      return new Return(value0);
    };

    return Return;
  }();

  var Bind = function () {
    function Bind(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Bind.create = function (value0) {
      return function (value1) {
        return new Bind(value0, value1);
      };
    };

    return Bind;
  }();

  var toView = function toView($copy_v) {
    var $tco_done = false;
    var $tco_result;

    function $tco_loop(v) {
      var runExpF = function runExpF(v2) {
        return v2;
      };

      var concatF = function concatF(v2) {
        return function (r) {
          return new Free(v2.value0, Data_Semigroup.append(Data_CatList.semigroupCatList)(v2.value1)(r));
        };
      };

      if (v.value0 instanceof Return) {
        var v2 = Data_CatList.uncons(v.value1);

        if (v2 instanceof Data_Maybe.Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }

        ;

        if (v2 instanceof Data_Maybe.Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }

      ;

      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function (a) {
          return concatF(v.value0.value1(a))(v.value1);
        });
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }

    ;

    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }

    ;
    return $tco_result;
  };

  var fromView = function fromView(f) {
    return new Free(f, Data_CatList.empty);
  };

  var freeMonad = new Control_Monad.Monad(function () {
    return freeApplicative;
  }, function () {
    return freeBind;
  });
  var freeFunctor = new Data_Functor.Functor(function (k) {
    return function (f) {
      return Control_Bind.bindFlipped(freeBind)(function () {
        var $120 = Control_Applicative.pure(freeApplicative);
        return function ($121) {
          return $120(k($121));
        };
      }())(f);
    };
  });
  var freeBind = new Control_Bind.Bind(function () {
    return freeApply;
  }, function (v) {
    return function (k) {
      return new Free(v.value0, Data_CatList.snoc(v.value1)(k));
    };
  });
  var freeApply = new Control_Apply.Apply(function () {
    return freeFunctor;
  }, Control_Monad.ap(freeMonad));
  var freeApplicative = new Control_Applicative.Applicative(function () {
    return freeApply;
  }, function ($122) {
    return fromView(Return.create($122));
  });

  var liftF = function liftF(f) {
    return fromView(new Bind(f, function () {
      var $123 = Control_Applicative.pure(freeApplicative);
      return function ($124) {
        return $123($124);
      };
    }()));
  };

  var substFree = function substFree(k) {
    var go = function go(f) {
      var v = toView(f);

      if (v instanceof Return) {
        return Control_Applicative.pure(freeApplicative)(v.value0);
      }

      ;

      if (v instanceof Bind) {
        return Control_Bind.bind(freeBind)(k(v.value0))(Data_Functor.map(Data_Functor.functorFn)(go)(v.value1));
      }

      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 168, column 10 - line 170, column 33): " + [v.constructor.name]);
    };

    return go;
  };

  var hoistFree = function hoistFree(k) {
    return substFree(function ($125) {
      return liftF(k($125));
    });
  };

  var foldFree = function foldFree(dictMonadRec) {
    return function (k) {
      var go = function go(f) {
        var v = toView(f);

        if (v instanceof Return) {
          return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Control_Monad_Rec_Class.Done.create)(Control_Applicative.pure(dictMonadRec.Monad0().Applicative0())(v.value0));
        }

        ;

        if (v instanceof Bind) {
          return Data_Functor.map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(function ($136) {
            return Control_Monad_Rec_Class.Loop.create(v.value1($136));
          })(k(v.value0));
        }

        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };

      return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
    };
  };

  exports["liftF"] = liftF;
  exports["hoistFree"] = hoistFree;
  exports["foldFree"] = foldFree;
  exports["freeFunctor"] = freeFunctor;
  exports["freeBind"] = freeBind;
  exports["freeApplicative"] = freeApplicative;
  exports["freeMonad"] = freeMonad;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.State.Class"] = $PS["Control.Monad.State.Class"] || {};
  var exports = $PS["Control.Monad.State.Class"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];

  var MonadState = function MonadState(Monad0, state) {
    this.Monad0 = Monad0;
    this.state = state;
  };

  var state = function state(dict) {
    return dict.state;
  };

  var put = function put(dictMonadState) {
    return function (s) {
      return state(dictMonadState)(function (v) {
        return new Data_Tuple.Tuple(Data_Unit.unit, s);
      });
    };
  };

  var modify_ = function modify_(dictMonadState) {
    return function (f) {
      return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(Data_Unit.unit, f(s));
      });
    };
  };

  var gets = function gets(dictMonadState) {
    return function (f) {
      return state(dictMonadState)(function (s) {
        return new Data_Tuple.Tuple(f(s), s);
      });
    };
  };

  var get = function get(dictMonadState) {
    return state(dictMonadState)(function (s) {
      return new Data_Tuple.Tuple(s, s);
    });
  };

  exports["state"] = state;
  exports["MonadState"] = MonadState;
  exports["get"] = get;
  exports["gets"] = gets;
  exports["put"] = put;
  exports["modify_"] = modify_;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.Maybe.Trans"] = $PS["Control.Monad.Maybe.Trans"] || {};
  var exports = $PS["Control.Monad.Maybe.Trans"];
  var Control_Alt = $PS["Control.Alt"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect_Class = $PS["Effect.Class"];

  var MaybeT = function MaybeT(x) {
    return x;
  };

  var runMaybeT = function runMaybeT(v) {
    return v;
  };

  var monadTransMaybeT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    var $71 = Control_Monad.liftM1(dictMonad)(Data_Maybe.Just.create);
    return function ($72) {
      return MaybeT($71($72));
    };
  });

  var functorMaybeT = function functorMaybeT(dictFunctor) {
    return new Data_Functor.Functor(function (f) {
      return function (v) {
        return Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(v);
      };
    });
  };

  var monadMaybeT = function monadMaybeT(dictMonad) {
    return new Control_Monad.Monad(function () {
      return applicativeMaybeT(dictMonad);
    }, function () {
      return bindMaybeT(dictMonad);
    });
  };

  var bindMaybeT = function bindMaybeT(dictMonad) {
    return new Control_Bind.Bind(function () {
      return applyMaybeT(dictMonad);
    }, function (v) {
      return function (f) {
        return Control_Bind.bind(dictMonad.Bind1())(v)(function (v1) {
          if (v1 instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value);
          }

          ;

          if (v1 instanceof Data_Maybe.Just) {
            var v2 = f(v1.value0);
            return v2;
          }

          ;
          throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 54, column 11 - line 56, column 42): " + [v1.constructor.name]);
        });
      };
    });
  };

  var applyMaybeT = function applyMaybeT(dictMonad) {
    return new Control_Apply.Apply(function () {
      return functorMaybeT(dictMonad.Bind1().Apply0().Functor0());
    }, Control_Monad.ap(monadMaybeT(dictMonad)));
  };

  var applicativeMaybeT = function applicativeMaybeT(dictMonad) {
    return new Control_Applicative.Applicative(function () {
      return applyMaybeT(dictMonad);
    }, function () {
      var $73 = Control_Applicative.pure(dictMonad.Applicative0());
      return function ($74) {
        return MaybeT($73(Data_Maybe.Just.create($74)));
      };
    }());
  };

  var monadEffectMaybe = function monadEffectMaybe(dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
      return monadMaybeT(dictMonadEffect.Monad0());
    }, function () {
      var $75 = Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadEffect.Monad0());
      var $76 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($77) {
        return $75($76($77));
      };
    }());
  };

  var monadStateMaybeT = function monadStateMaybeT(dictMonadState) {
    return new Control_Monad_State_Class.MonadState(function () {
      return monadMaybeT(dictMonadState.Monad0());
    }, function (f) {
      return Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
    });
  };

  var altMaybeT = function altMaybeT(dictMonad) {
    return new Control_Alt.Alt(function () {
      return functorMaybeT(dictMonad.Bind1().Apply0().Functor0());
    }, function (v) {
      return function (v1) {
        return Control_Bind.bind(dictMonad.Bind1())(v)(function (m) {
          if (m instanceof Data_Maybe.Nothing) {
            return v1;
          }

          ;
          return Control_Applicative.pure(dictMonad.Applicative0())(m);
        });
      };
    });
  };

  var plusMaybeT = function plusMaybeT(dictMonad) {
    return new Control_Plus.Plus(function () {
      return altMaybeT(dictMonad);
    }, Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value));
  };

  exports["MaybeT"] = MaybeT;
  exports["runMaybeT"] = runMaybeT;
  exports["applicativeMaybeT"] = applicativeMaybeT;
  exports["bindMaybeT"] = bindMaybeT;
  exports["monadTransMaybeT"] = monadTransMaybeT;
  exports["plusMaybeT"] = plusMaybeT;
  exports["monadEffectMaybe"] = monadEffectMaybe;
  exports["monadStateMaybeT"] = monadStateMaybeT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Monad.State.Trans"] = $PS["Control.Monad.State.Trans"] || {};
  var exports = $PS["Control.Monad.State.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Tuple = $PS["Data.Tuple"];

  var StateT = function StateT(x) {
    return x;
  };

  var monadTransStateT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
    return function (m) {
      return function (s) {
        return Control_Bind.bind(dictMonad.Bind1())(m)(function (x) {
          return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(x, s));
        });
      };
    };
  });

  var functorStateT = function functorStateT(dictFunctor) {
    return new Data_Functor.Functor(function (f) {
      return function (v) {
        return function (s) {
          return Data_Functor.map(dictFunctor)(function (v1) {
            return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
          })(v(s));
        };
      };
    });
  };

  var evalStateT = function evalStateT(dictFunctor) {
    return function (v) {
      return function (s) {
        return Data_Functor.map(dictFunctor)(Data_Tuple.fst)(v(s));
      };
    };
  };

  var monadStateT = function monadStateT(dictMonad) {
    return new Control_Monad.Monad(function () {
      return applicativeStateT(dictMonad);
    }, function () {
      return bindStateT(dictMonad);
    });
  };

  var bindStateT = function bindStateT(dictMonad) {
    return new Control_Bind.Bind(function () {
      return applyStateT(dictMonad);
    }, function (v) {
      return function (f) {
        return function (s) {
          return Control_Bind.bind(dictMonad.Bind1())(v(s))(function (v1) {
            var v3 = f(v1.value0);
            return v3(v1.value1);
          });
        };
      };
    });
  };

  var applyStateT = function applyStateT(dictMonad) {
    return new Control_Apply.Apply(function () {
      return functorStateT(dictMonad.Bind1().Apply0().Functor0());
    }, Control_Monad.ap(monadStateT(dictMonad)));
  };

  var applicativeStateT = function applicativeStateT(dictMonad) {
    return new Control_Applicative.Applicative(function () {
      return applyStateT(dictMonad);
    }, function (a) {
      return function (s) {
        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(a, s));
      };
    });
  };

  var monadStateStateT = function monadStateStateT(dictMonad) {
    return new Control_Monad_State_Class.MonadState(function () {
      return monadStateT(dictMonad);
    }, function (f) {
      return StateT(function () {
        var $112 = Control_Applicative.pure(dictMonad.Applicative0());
        return function ($113) {
          return $112(f($113));
        };
      }());
    });
  };

  var monadThrowStateT = function monadThrowStateT(dictMonadThrow) {
    return new Control_Monad_Error_Class.MonadThrow(function () {
      return monadStateT(dictMonadThrow.Monad0());
    }, function (e) {
      return Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
    });
  };

  exports["evalStateT"] = evalStateT;
  exports["functorStateT"] = functorStateT;
  exports["applicativeStateT"] = applicativeStateT;
  exports["bindStateT"] = bindStateT;
  exports["monadTransStateT"] = monadTransStateT;
  exports["monadThrowStateT"] = monadThrowStateT;
  exports["monadStateStateT"] = monadStateStateT;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Control.Parallel"] = $PS["Control.Parallel"] || {};
  var exports = $PS["Control.Parallel"];
  var Control_Category = $PS["Control.Category"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Foldable = $PS["Data.Foldable"];

  var parTraverse_ = function parTraverse_(dictParallel) {
    return function (dictFoldable) {
      return function (f) {
        var $17 = Control_Parallel_Class.sequential(dictParallel);
        var $18 = Data_Foldable.traverse_(dictParallel.Applicative1())(dictFoldable)(function () {
          var $20 = Control_Parallel_Class.parallel(dictParallel);
          return function ($21) {
            return $20(f($21));
          };
        }());
        return function ($19) {
          return $17($18($19));
        };
      };
    };
  };

  var parSequence_ = function parSequence_(dictParallel) {
    return function (dictFoldable) {
      return parTraverse_(dictParallel)(dictFoldable)(Control_Category.identity(Control_Category.categoryFn));
    };
  };

  exports["parSequence_"] = parSequence_;
})(PS);

(function (exports) {
  "use strict"; // Alias require to prevent webpack or browserify from actually requiring.

  var req = typeof module === "undefined" ? undefined : module.require;
  var util = req === undefined ? undefined : req("util"); // exports.spy = function () {
  //   return function (tag) {
  //     return function (x) {
  //       if (util !== undefined) {
  //         console.log(tag + ":", util.inspect(x, { depth: null, colors: true }));
  //       } else {
  //         console.log(tag + ":", x);
  //       }
  //       return x;
  //     };
  //   };
  // };

  exports.spy = function (tag) {
    return function (x) {
      if (util !== undefined) {
        console.log(tag + ":", util.inspect(x, {
          depth: null,
          colors: true
        }));
      } else {
        console.log(tag + ":", x);
      }

      return x;
    };
  };
})(PS["Corona.Chart"] = PS["Corona.Chart"] || {});

(function (exports) {
  "use strict";

  exports._parseIso = function (just, nothing, v) {
    var d = new Date(v);

    if (!isNaN(d)) {
      return just(d);
    } else {
      return nothing();
    }
  };

  exports._toIso = function (d) {
    return d.toISOString().slice(0, 10);
  };
})(PS["Data.ModifiedJulianDay"] = PS["Data.ModifiedJulianDay"] || {});

(function (exports) {
  "use strict";

  var createDate = function createDate(y, m, d) {
    var date = new Date(Date.UTC(y, m, d));

    if (y >= 0 && y < 100) {
      date.setUTCFullYear(y);
    }

    return date;
  };

  exports.canonicalDateImpl = function (ctor, y, m, d) {
    var date = createDate(y, m - 1, d);
    return ctor(date.getUTCFullYear())(date.getUTCMonth() + 1)(date.getUTCDate());
  };

  exports.calcDiff = function (y1, m1, d1, y2, m2, d2) {
    var dt1 = createDate(y1, m1 - 1, d1);
    var dt2 = createDate(y2, m2 - 1, d2);
    return dt1.getTime() - dt2.getTime();
  };
})(PS["Data.Date"] = PS["Data.Date"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Enum"] = $PS["Data.Enum"] || {};
  var exports = $PS["Data.Enum"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Enum = function Enum(Ord0, pred, succ) {
    this.Ord0 = Ord0;
    this.pred = pred;
    this.succ = succ;
  };

  var BoundedEnum = function BoundedEnum(Bounded0, Enum1, cardinality, fromEnum, toEnum) {
    this.Bounded0 = Bounded0;
    this.Enum1 = Enum1;
    this.cardinality = cardinality;
    this.fromEnum = fromEnum;
    this.toEnum = toEnum;
  };

  var toEnum = function toEnum(dict) {
    return dict.toEnum;
  };

  var succ = function succ(dict) {
    return dict.succ;
  };

  var pred = function pred(dict) {
    return dict.pred;
  };

  var fromEnum = function fromEnum(dict) {
    return dict.fromEnum;
  };

  var toEnumWithDefaults = function toEnumWithDefaults(dictBoundedEnum) {
    return function (low) {
      return function (high) {
        return function (x) {
          var v = toEnum(dictBoundedEnum)(x);

          if (v instanceof Data_Maybe.Just) {
            return v.value0;
          }

          ;

          if (v instanceof Data_Maybe.Nothing) {
            var $54 = x < fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));

            if ($54) {
              return low;
            }

            ;
            return high;
          }

          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };

  exports["Enum"] = Enum;
  exports["succ"] = succ;
  exports["pred"] = pred;
  exports["BoundedEnum"] = BoundedEnum;
  exports["toEnum"] = toEnum;
  exports["fromEnum"] = fromEnum;
  exports["toEnumWithDefaults"] = toEnumWithDefaults;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Date.Component"] = $PS["Data.Date.Component"] || {};
  var exports = $PS["Data.Date.Component"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];

  var January = function () {
    function January() {}

    ;
    January.value = new January();
    return January;
  }();

  var February = function () {
    function February() {}

    ;
    February.value = new February();
    return February;
  }();

  var March = function () {
    function March() {}

    ;
    March.value = new March();
    return March;
  }();

  var April = function () {
    function April() {}

    ;
    April.value = new April();
    return April;
  }();

  var May = function () {
    function May() {}

    ;
    May.value = new May();
    return May;
  }();

  var June = function () {
    function June() {}

    ;
    June.value = new June();
    return June;
  }();

  var July = function () {
    function July() {}

    ;
    July.value = new July();
    return July;
  }();

  var August = function () {
    function August() {}

    ;
    August.value = new August();
    return August;
  }();

  var September = function () {
    function September() {}

    ;
    September.value = new September();
    return September;
  }();

  var October = function () {
    function October() {}

    ;
    October.value = new October();
    return October;
  }();

  var November = function () {
    function November() {}

    ;
    November.value = new November();
    return November;
  }();

  var December = function () {
    function December() {}

    ;
    December.value = new December();
    return December;
  }();

  var ordYear = Data_Ord.ordInt;
  var ordDay = Data_Ord.ordInt;
  var eqYear = Data_Eq.eqInt;
  var eqMonth = new Data_Eq.Eq(function (x) {
    return function (y) {
      if (x instanceof January && y instanceof January) {
        return true;
      }

      ;

      if (x instanceof February && y instanceof February) {
        return true;
      }

      ;

      if (x instanceof March && y instanceof March) {
        return true;
      }

      ;

      if (x instanceof April && y instanceof April) {
        return true;
      }

      ;

      if (x instanceof May && y instanceof May) {
        return true;
      }

      ;

      if (x instanceof June && y instanceof June) {
        return true;
      }

      ;

      if (x instanceof July && y instanceof July) {
        return true;
      }

      ;

      if (x instanceof August && y instanceof August) {
        return true;
      }

      ;

      if (x instanceof September && y instanceof September) {
        return true;
      }

      ;

      if (x instanceof October && y instanceof October) {
        return true;
      }

      ;

      if (x instanceof November && y instanceof November) {
        return true;
      }

      ;

      if (x instanceof December && y instanceof December) {
        return true;
      }

      ;
      return false;
    };
  });
  var ordMonth = new Data_Ord.Ord(function () {
    return eqMonth;
  }, function (x) {
    return function (y) {
      if (x instanceof January && y instanceof January) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof January) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof January) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof February && y instanceof February) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof February) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof February) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof March && y instanceof March) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof March) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof March) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof April && y instanceof April) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof April) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof April) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof May && y instanceof May) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof May) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof May) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof June && y instanceof June) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof June) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof June) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof July && y instanceof July) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof July) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof July) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof August && y instanceof August) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof August) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof August) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof September && y instanceof September) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof September) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof September) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof October && y instanceof October) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof October) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof October) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof November && y instanceof November) {
        return Data_Ordering.EQ.value;
      }

      ;

      if (x instanceof November) {
        return Data_Ordering.LT.value;
      }

      ;

      if (y instanceof November) {
        return Data_Ordering.GT.value;
      }

      ;

      if (x instanceof December && y instanceof December) {
        return Data_Ordering.EQ.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 61, column 1 - line 61, column 38): " + [x.constructor.name, y.constructor.name]);
    };
  });
  var eqDay = Data_Eq.eqInt;
  var boundedYear = new Data_Bounded.Bounded(function () {
    return ordYear;
  }, -271820 | 0, 275759);
  var boundedMonth = new Data_Bounded.Bounded(function () {
    return ordMonth;
  }, January.value, December.value);
  var boundedEnumYear = new Data_Enum.BoundedEnum(function () {
    return boundedYear;
  }, function () {
    return enumYear;
  }, 547580, function (v) {
    return v;
  }, function (n) {
    if (n >= (-271820 | 0) && n <= 275759) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [n.constructor.name]);
  });
  var enumYear = new Data_Enum.Enum(function () {
    return ordYear;
  }, function () {
    var $46 = Data_Enum.toEnum(boundedEnumYear);
    var $47 = Data_Enum.fromEnum(boundedEnumYear);
    return function ($48) {
      return $46(function (v) {
        return v - 1 | 0;
      }($47($48)));
    };
  }(), function () {
    var $49 = Data_Enum.toEnum(boundedEnumYear);
    var $50 = Data_Enum.fromEnum(boundedEnumYear);
    return function ($51) {
      return $49(function (v) {
        return v + 1 | 0;
      }($50($51)));
    };
  }());
  var boundedEnumMonth = new Data_Enum.BoundedEnum(function () {
    return boundedMonth;
  }, function () {
    return enumMonth;
  }, 12, function (v) {
    if (v instanceof January) {
      return 1;
    }

    ;

    if (v instanceof February) {
      return 2;
    }

    ;

    if (v instanceof March) {
      return 3;
    }

    ;

    if (v instanceof April) {
      return 4;
    }

    ;

    if (v instanceof May) {
      return 5;
    }

    ;

    if (v instanceof June) {
      return 6;
    }

    ;

    if (v instanceof July) {
      return 7;
    }

    ;

    if (v instanceof August) {
      return 8;
    }

    ;

    if (v instanceof September) {
      return 9;
    }

    ;

    if (v instanceof October) {
      return 10;
    }

    ;

    if (v instanceof November) {
      return 11;
    }

    ;

    if (v instanceof December) {
      return 12;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [v.constructor.name]);
  }, function (v) {
    if (v === 1) {
      return new Data_Maybe.Just(January.value);
    }

    ;

    if (v === 2) {
      return new Data_Maybe.Just(February.value);
    }

    ;

    if (v === 3) {
      return new Data_Maybe.Just(March.value);
    }

    ;

    if (v === 4) {
      return new Data_Maybe.Just(April.value);
    }

    ;

    if (v === 5) {
      return new Data_Maybe.Just(May.value);
    }

    ;

    if (v === 6) {
      return new Data_Maybe.Just(June.value);
    }

    ;

    if (v === 7) {
      return new Data_Maybe.Just(July.value);
    }

    ;

    if (v === 8) {
      return new Data_Maybe.Just(August.value);
    }

    ;

    if (v === 9) {
      return new Data_Maybe.Just(September.value);
    }

    ;

    if (v === 10) {
      return new Data_Maybe.Just(October.value);
    }

    ;

    if (v === 11) {
      return new Data_Maybe.Just(November.value);
    }

    ;

    if (v === 12) {
      return new Data_Maybe.Just(December.value);
    }

    ;
    return Data_Maybe.Nothing.value;
  });
  var enumMonth = new Data_Enum.Enum(function () {
    return ordMonth;
  }, function () {
    var $58 = Data_Enum.toEnum(boundedEnumMonth);
    var $59 = Data_Enum.fromEnum(boundedEnumMonth);
    return function ($60) {
      return $58(function (v) {
        return v - 1 | 0;
      }($59($60)));
    };
  }(), function () {
    var $61 = Data_Enum.toEnum(boundedEnumMonth);
    var $62 = Data_Enum.fromEnum(boundedEnumMonth);
    return function ($63) {
      return $61(function (v) {
        return v + 1 | 0;
      }($62($63)));
    };
  }());
  var boundedDay = new Data_Bounded.Bounded(function () {
    return ordDay;
  }, 1, 31);
  var boundedEnumDay = new Data_Enum.BoundedEnum(function () {
    return boundedDay;
  }, function () {
    return enumDay;
  }, 31, function (v) {
    return v;
  }, function (n) {
    if (n >= 1 && n <= 31) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [n.constructor.name]);
  });
  var enumDay = new Data_Enum.Enum(function () {
    return ordDay;
  }, function () {
    var $64 = Data_Enum.toEnum(boundedEnumDay);
    var $65 = Data_Enum.fromEnum(boundedEnumDay);
    return function ($66) {
      return $64(function (v) {
        return v - 1 | 0;
      }($65($66)));
    };
  }(), function () {
    var $67 = Data_Enum.toEnum(boundedEnumDay);
    var $68 = Data_Enum.fromEnum(boundedEnumDay);
    return function ($69) {
      return $67(function (v) {
        return v + 1 | 0;
      }($68($69)));
    };
  }());
  exports["January"] = January;
  exports["February"] = February;
  exports["March"] = March;
  exports["April"] = April;
  exports["May"] = May;
  exports["June"] = June;
  exports["July"] = July;
  exports["August"] = August;
  exports["September"] = September;
  exports["October"] = October;
  exports["November"] = November;
  exports["December"] = December;
  exports["eqYear"] = eqYear;
  exports["ordYear"] = ordYear;
  exports["boundedYear"] = boundedYear;
  exports["enumYear"] = enumYear;
  exports["boundedEnumYear"] = boundedEnumYear;
  exports["eqMonth"] = eqMonth;
  exports["ordMonth"] = ordMonth;
  exports["enumMonth"] = enumMonth;
  exports["boundedEnumMonth"] = boundedEnumMonth;
  exports["eqDay"] = eqDay;
  exports["ordDay"] = ordDay;
  exports["boundedDay"] = boundedDay;
  exports["enumDay"] = enumDay;
  exports["boundedEnumDay"] = boundedEnumDay;
})(PS);

(function (exports) {
  "use strict";

  exports.intDegree = function (x) {
    return Math.min(Math.abs(x), 2147483647);
  }; // See the Euclidean definition in
  // https://en.m.wikipedia.org/wiki/Modulo_operation.


  exports.intDiv = function (x) {
    return function (y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };

  exports.intMod = function (x) {
    return function (y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };
})(PS["Data.EuclideanRing"] = PS["Data.EuclideanRing"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.CommutativeRing"] = $PS["Data.CommutativeRing"] || {};
  var exports = $PS["Data.CommutativeRing"];
  var Data_Ring = $PS["Data.Ring"];

  var CommutativeRing = function CommutativeRing(Ring0) {
    this.Ring0 = Ring0;
  };

  var commutativeRingInt = new CommutativeRing(function () {
    return Data_Ring.ringInt;
  });
  exports["commutativeRingInt"] = commutativeRingInt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.EuclideanRing"] = $PS["Data.EuclideanRing"] || {};
  var exports = $PS["Data.EuclideanRing"];
  var $foreign = $PS["Data.EuclideanRing"];
  var Data_CommutativeRing = $PS["Data.CommutativeRing"];

  var EuclideanRing = function EuclideanRing(CommutativeRing0, degree, div, mod) {
    this.CommutativeRing0 = CommutativeRing0;
    this.degree = degree;
    this.div = div;
    this.mod = mod;
  };

  var mod = function mod(dict) {
    return dict.mod;
  };

  var euclideanRingInt = new EuclideanRing(function () {
    return Data_CommutativeRing.commutativeRingInt;
  }, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
  exports["mod"] = mod;
  exports["euclideanRingInt"] = euclideanRingInt;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Time.Duration"] = $PS["Data.Time.Duration"] || {};
  var exports = $PS["Data.Time.Duration"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Milliseconds = function Milliseconds(x) {
    return x;
  };

  var Days = function Days(x) {
    return x;
  };

  var Duration = function Duration(fromDuration, toDuration) {
    this.fromDuration = fromDuration;
    this.toDuration = toDuration;
  };

  var toDuration = function toDuration(dict) {
    return dict.toDuration;
  };

  var newtypeMilliseconds = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Milliseconds);
  var newtypeDays = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Days);
  var durationDays = new Duration(Data_Newtype.over(newtypeDays)(newtypeMilliseconds)(Days)(function (v) {
    return v * 8.64e7;
  }), Data_Newtype.over(newtypeMilliseconds)(newtypeDays)(Milliseconds)(function (v) {
    return v / 8.64e7;
  }));
  exports["toDuration"] = toDuration;
  exports["Milliseconds"] = Milliseconds;
  exports["durationDays"] = durationDays;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Date"] = $PS["Data.Date"] || {};
  var exports = $PS["Data.Date"];
  var $foreign = $PS["Data.Date"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Time_Duration = $PS["Data.Time.Duration"];

  var $$Date = function () {
    function $$Date(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    $$Date.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new $$Date(value0, value1, value2);
        };
      };
    };

    return $$Date;
  }();

  var year = function year(v) {
    return v.value0;
  };

  var month = function month(v) {
    return v.value1;
  };

  var isLeapYear = function isLeapYear(y) {
    var y$prime = Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(y);
    return Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(y$prime)(4) === 0 && (Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(y$prime)(400) === 0 || !(Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(y$prime)(100) === 0));
  };

  var lastDayOfMonth = function lastDayOfMonth(y) {
    return function (m) {
      var unsafeDay = function () {
        var $108 = Data_Maybe.fromJust();
        var $109 = Data_Enum.toEnum(Data_Date_Component.boundedEnumDay);
        return function ($110) {
          return $108($109($110));
        };
      }();

      if (m instanceof Data_Date_Component.January) {
        return unsafeDay(31);
      }

      ;

      if (m instanceof Data_Date_Component.February) {
        if (isLeapYear(y)) {
          return unsafeDay(29);
        }

        ;

        if (Data_Boolean.otherwise) {
          return unsafeDay(28);
        }

        ;
      }

      ;

      if (m instanceof Data_Date_Component.March) {
        return unsafeDay(31);
      }

      ;

      if (m instanceof Data_Date_Component.April) {
        return unsafeDay(30);
      }

      ;

      if (m instanceof Data_Date_Component.May) {
        return unsafeDay(31);
      }

      ;

      if (m instanceof Data_Date_Component.June) {
        return unsafeDay(30);
      }

      ;

      if (m instanceof Data_Date_Component.July) {
        return unsafeDay(31);
      }

      ;

      if (m instanceof Data_Date_Component.August) {
        return unsafeDay(31);
      }

      ;

      if (m instanceof Data_Date_Component.September) {
        return unsafeDay(30);
      }

      ;

      if (m instanceof Data_Date_Component.October) {
        return unsafeDay(31);
      }

      ;

      if (m instanceof Data_Date_Component.November) {
        return unsafeDay(30);
      }

      ;

      if (m instanceof Data_Date_Component.December) {
        return unsafeDay(31);
      }

      ;
      throw new Error("Failed pattern match at Data.Date (line 127, column 22 - line 141, column 27): " + [m.constructor.name]);
    };
  };

  var eqDate = new Data_Eq.Eq(function (x) {
    return function (y) {
      return Data_Eq.eq(Data_Date_Component.eqYear)(x.value0)(y.value0) && Data_Eq.eq(Data_Date_Component.eqMonth)(x.value1)(y.value1) && Data_Eq.eq(Data_Date_Component.eqDay)(x.value2)(y.value2);
    };
  });
  var ordDate = new Data_Ord.Ord(function () {
    return eqDate;
  }, function (x) {
    return function (y) {
      var v = Data_Ord.compare(Data_Date_Component.ordYear)(x.value0)(y.value0);

      if (v instanceof Data_Ordering.LT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v instanceof Data_Ordering.GT) {
        return Data_Ordering.GT.value;
      }

      ;
      var v1 = Data_Ord.compare(Data_Date_Component.ordMonth)(x.value1)(y.value1);

      if (v1 instanceof Data_Ordering.LT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v1 instanceof Data_Ordering.GT) {
        return Data_Ordering.GT.value;
      }

      ;
      return Data_Ord.compare(Data_Date_Component.ordDay)(x.value2)(y.value2);
    };
  });
  var enumDate = new Data_Enum.Enum(function () {
    return ordDate;
  }, function (v) {
    var pm = Data_Enum.pred(Data_Date_Component.enumMonth)(v.value1);
    var pd = Data_Enum.pred(Data_Date_Component.enumDay)(v.value2);

    var y$prime = function () {
      var $73 = Data_Maybe.isNothing(pd) && Data_Maybe.isNothing(pm);

      if ($73) {
        return Data_Enum.pred(Data_Date_Component.enumYear)(v.value0);
      }

      ;
      return new Data_Maybe.Just(v.value0);
    }();

    var m$prime = function () {
      var $74 = Data_Maybe.isNothing(pd);

      if ($74) {
        return Data_Maybe.fromMaybe(Data_Date_Component.December.value)(pm);
      }

      ;
      return v.value1;
    }();

    var l = lastDayOfMonth(v.value0)(m$prime);

    var d$prime = function () {
      var $75 = Data_Maybe.isNothing(pd);

      if ($75) {
        return new Data_Maybe.Just(l);
      }

      ;
      return pd;
    }();

    return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)($$Date.create)(y$prime))(Control_Applicative.pure(Data_Maybe.applicativeMaybe)(m$prime)))(d$prime);
  }, function (v) {
    var sm = Data_Enum.succ(Data_Date_Component.enumMonth)(v.value1);
    var l = lastDayOfMonth(v.value0)(v.value1);

    var sd = function () {
      var v1 = Data_Enum.succ(Data_Date_Component.enumDay)(v.value2);
      var $80 = Data_Ord.greaterThan(Data_Maybe.ordMaybe(Data_Date_Component.ordDay))(v1)(new Data_Maybe.Just(l));

      if ($80) {
        return Data_Maybe.Nothing.value;
      }

      ;
      return v1;
    }();

    var m$prime = function () {
      var $81 = Data_Maybe.isNothing(sd);

      if ($81) {
        return Data_Maybe.fromMaybe(Data_Date_Component.January.value)(sm);
      }

      ;
      return v.value1;
    }();

    var y$prime = function () {
      var $82 = Data_Maybe.isNothing(sd) && Data_Maybe.isNothing(sm);

      if ($82) {
        return Data_Enum.succ(Data_Date_Component.enumYear)(v.value0);
      }

      ;
      return new Data_Maybe.Just(v.value0);
    }();

    var d$prime = function () {
      var $83 = Data_Maybe.isNothing(sd);

      if ($83) {
        return Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)(1);
      }

      ;
      return sd;
    }();

    return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)($$Date.create)(y$prime))(Control_Applicative.pure(Data_Maybe.applicativeMaybe)(m$prime)))(d$prime);
  });

  var diff = function diff(dictDuration) {
    return function (v) {
      return function (v1) {
        return Data_Time_Duration.toDuration(dictDuration)($foreign.calcDiff(v.value0, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(v.value1), v.value2, v1.value0, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(v1.value1), v1.value2));
      };
    };
  };

  var day = function day(v) {
    return v.value2;
  };

  var canonicalDate = function canonicalDate(y) {
    return function (m) {
      return function (d) {
        var mkDate = function mkDate(y$prime) {
          return function (m$prime) {
            return function (d$prime) {
              return new $$Date(y$prime, Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(m$prime)), d$prime);
            };
          };
        };

        return $foreign.canonicalDateImpl(mkDate, y, Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(m), d);
      };
    };
  };

  var adjust = function adjust(v) {
    return function (date) {
      var adj = function adj(v1) {
        return function (v2) {
          if (v1 === 0) {
            return new Data_Maybe.Just(v2);
          }

          ;
          var j = v1 + Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(v2.value2) | 0;
          var low = j < 1;
          var l = lastDayOfMonth(v2.value0)(function () {
            if (low) {
              return Data_Maybe.fromMaybe(Data_Date_Component.December.value)(Data_Enum.pred(Data_Date_Component.enumMonth)(v2.value1));
            }

            ;
            return v2.value1;
          }());
          var hi = j > Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(l);

          var i$prime = function () {
            if (low) {
              return j;
            }

            ;

            if (hi) {
              return (j - Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(l) | 0) - 1 | 0;
            }

            ;

            if (Data_Boolean.otherwise) {
              return 0;
            }

            ;
            throw new Error("Failed pattern match at Data.Date (line 101, column 9 - line 103, column 28): " + []);
          }();

          var dt$prime = function () {
            if (low) {
              return Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Enum.pred(enumDate))(Data_Functor.map(Data_Maybe.functorMaybe)($$Date.create(v2.value0)(v2.value1))(Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)(1)));
            }

            ;

            if (hi) {
              return Data_Enum.succ(enumDate)(new $$Date(v2.value0, v2.value1, l));
            }

            ;

            if (Data_Boolean.otherwise) {
              return Data_Functor.map(Data_Maybe.functorMaybe)($$Date.create(v2.value0)(v2.value1))(Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)(j));
            }

            ;
            throw new Error("Failed pattern match at Data.Date (line 104, column 9 - line 106, column 48): " + []);
          }();

          return Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(adj(i$prime))(dt$prime);
        };
      };

      return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Int.fromNumber(v))(Data_Function.flip(adj)(date));
    };
  };

  exports["canonicalDate"] = canonicalDate;
  exports["year"] = year;
  exports["month"] = month;
  exports["day"] = day;
  exports["diff"] = diff;
  exports["adjust"] = adjust;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.DateTime"] = $PS["Data.DateTime"] || {};
  var exports = $PS["Data.DateTime"];

  var DateTime = function () {
    function DateTime(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    DateTime.create = function (value0) {
      return function (value1) {
        return new DateTime(value0, value1);
      };
    };

    return DateTime;
  }();

  var date = function date(v) {
    return v.value0;
  };

  exports["DateTime"] = DateTime;
  exports["date"] = date;
})(PS);

(function (exports) {
  /* global exports */
  "use strict";

  var createDate = function createDate(y, m, d, h, mi, s, ms) {
    var date = new Date(Date.UTC(y, m, d, h, mi, s, ms));

    if (y >= 0 && y < 100) {
      date.setUTCFullYear(y);
    }

    return date;
  };

  exports.toInstantImpl = function (just) {
    return function (nothing) {
      return function (date) {
        var t = date.getTime();
        return isNaN(t) ? nothing : just(t);
      };
    };
  };

  exports.jsdate = function (parts) {
    return createDate(parts.year, parts.month, parts.day, parts.hour, parts.minute, parts.second, parts.millisecond);
  };

  exports.parse = function (dateString) {
    return function () {
      return new Date(dateString);
    };
  };
})(PS["Data.JSDate"] = PS["Data.JSDate"] || {});

(function (exports) {
  "use strict";

  exports.toDateTimeImpl = function (ctor) {
    return function (instant) {
      var dt = new Date(instant);
      return ctor(dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds());
    };
  };
})(PS["Data.DateTime.Instant"] = PS["Data.DateTime.Instant"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Time.Component"] = $PS["Data.Time.Component"] || {};
  var exports = $PS["Data.Time.Component"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var ordSecond = Data_Ord.ordInt;
  var ordMinute = Data_Ord.ordInt;
  var ordMillisecond = Data_Ord.ordInt;
  var ordHour = Data_Ord.ordInt;
  var eqSecond = Data_Eq.eqInt;
  var eqMinute = Data_Eq.eqInt;
  var eqMillisecond = Data_Eq.eqInt;
  var eqHour = Data_Eq.eqInt;
  var boundedSecond = new Data_Bounded.Bounded(function () {
    return ordSecond;
  }, 0, 59);
  var boundedMinute = new Data_Bounded.Bounded(function () {
    return ordMinute;
  }, 0, 59);
  var boundedMillisecond = new Data_Bounded.Bounded(function () {
    return ordMillisecond;
  }, 0, 999);
  var boundedHour = new Data_Bounded.Bounded(function () {
    return ordHour;
  }, 0, 23);
  var boundedEnumSecond = new Data_Enum.BoundedEnum(function () {
    return boundedSecond;
  }, function () {
    return enumSecond;
  }, 60, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 59) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 90, column 1 - line 95, column 26): " + [n.constructor.name]);
  });
  var enumSecond = new Data_Enum.Enum(function () {
    return ordSecond;
  }, function () {
    var $28 = Data_Enum.toEnum(boundedEnumSecond);
    var $29 = Data_Enum.fromEnum(boundedEnumSecond);
    return function ($30) {
      return $28(function (v) {
        return v - 1 | 0;
      }($29($30)));
    };
  }(), function () {
    var $31 = Data_Enum.toEnum(boundedEnumSecond);
    var $32 = Data_Enum.fromEnum(boundedEnumSecond);
    return function ($33) {
      return $31(function (v) {
        return v + 1 | 0;
      }($32($33)));
    };
  }());
  var boundedEnumMinute = new Data_Enum.BoundedEnum(function () {
    return boundedMinute;
  }, function () {
    return enumMinute;
  }, 60, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 59) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 61, column 1 - line 66, column 26): " + [n.constructor.name]);
  });
  var enumMinute = new Data_Enum.Enum(function () {
    return ordMinute;
  }, function () {
    var $34 = Data_Enum.toEnum(boundedEnumMinute);
    var $35 = Data_Enum.fromEnum(boundedEnumMinute);
    return function ($36) {
      return $34(function (v) {
        return v - 1 | 0;
      }($35($36)));
    };
  }(), function () {
    var $37 = Data_Enum.toEnum(boundedEnumMinute);
    var $38 = Data_Enum.fromEnum(boundedEnumMinute);
    return function ($39) {
      return $37(function (v) {
        return v + 1 | 0;
      }($38($39)));
    };
  }());
  var boundedEnumMillisecond = new Data_Enum.BoundedEnum(function () {
    return boundedMillisecond;
  }, function () {
    return enumMillisecond;
  }, 1000, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 999) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 120, column 1 - line 125, column 31): " + [n.constructor.name]);
  });
  var enumMillisecond = new Data_Enum.Enum(function () {
    return ordMillisecond;
  }, function () {
    var $40 = Data_Enum.toEnum(boundedEnumMillisecond);
    var $41 = Data_Enum.fromEnum(boundedEnumMillisecond);
    return function ($42) {
      return $40(function (v) {
        return v - 1 | 0;
      }($41($42)));
    };
  }(), function () {
    var $43 = Data_Enum.toEnum(boundedEnumMillisecond);
    var $44 = Data_Enum.fromEnum(boundedEnumMillisecond);
    return function ($45) {
      return $43(function (v) {
        return v + 1 | 0;
      }($44($45)));
    };
  }());
  var boundedEnumHour = new Data_Enum.BoundedEnum(function () {
    return boundedHour;
  }, function () {
    return enumHour;
  }, 24, function (v) {
    return v;
  }, function (n) {
    if (n >= 0 && n <= 23) {
      return new Data_Maybe.Just(n);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 32, column 1 - line 37, column 24): " + [n.constructor.name]);
  });
  var enumHour = new Data_Enum.Enum(function () {
    return ordHour;
  }, function () {
    var $46 = Data_Enum.toEnum(boundedEnumHour);
    var $47 = Data_Enum.fromEnum(boundedEnumHour);
    return function ($48) {
      return $46(function (v) {
        return v - 1 | 0;
      }($47($48)));
    };
  }(), function () {
    var $49 = Data_Enum.toEnum(boundedEnumHour);
    var $50 = Data_Enum.fromEnum(boundedEnumHour);
    return function ($51) {
      return $49(function (v) {
        return v + 1 | 0;
      }($50($51)));
    };
  }());
  exports["eqHour"] = eqHour;
  exports["ordHour"] = ordHour;
  exports["boundedHour"] = boundedHour;
  exports["boundedEnumHour"] = boundedEnumHour;
  exports["eqMinute"] = eqMinute;
  exports["ordMinute"] = ordMinute;
  exports["boundedMinute"] = boundedMinute;
  exports["boundedEnumMinute"] = boundedEnumMinute;
  exports["eqSecond"] = eqSecond;
  exports["ordSecond"] = ordSecond;
  exports["boundedSecond"] = boundedSecond;
  exports["boundedEnumSecond"] = boundedEnumSecond;
  exports["eqMillisecond"] = eqMillisecond;
  exports["ordMillisecond"] = ordMillisecond;
  exports["boundedMillisecond"] = boundedMillisecond;
  exports["boundedEnumMillisecond"] = boundedEnumMillisecond;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Time"] = $PS["Data.Time"] || {};
  var exports = $PS["Data.Time"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Time_Component = $PS["Data.Time.Component"];

  var Time = function () {
    function Time(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Time.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Time(value0, value1, value2, value3);
          };
        };
      };
    };

    return Time;
  }();

  var second = function second(v) {
    return v.value2;
  };

  var minute = function minute(v) {
    return v.value1;
  };

  var millisecond = function millisecond(v) {
    return v.value3;
  };

  var hour = function hour(v) {
    return v.value0;
  };

  var eqTime = new Data_Eq.Eq(function (x) {
    return function (y) {
      return Data_Eq.eq(Data_Time_Component.eqHour)(x.value0)(y.value0) && Data_Eq.eq(Data_Time_Component.eqMinute)(x.value1)(y.value1) && Data_Eq.eq(Data_Time_Component.eqSecond)(x.value2)(y.value2) && Data_Eq.eq(Data_Time_Component.eqMillisecond)(x.value3)(y.value3);
    };
  });
  var ordTime = new Data_Ord.Ord(function () {
    return eqTime;
  }, function (x) {
    return function (y) {
      var v = Data_Ord.compare(Data_Time_Component.ordHour)(x.value0)(y.value0);

      if (v instanceof Data_Ordering.LT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v instanceof Data_Ordering.GT) {
        return Data_Ordering.GT.value;
      }

      ;
      var v1 = Data_Ord.compare(Data_Time_Component.ordMinute)(x.value1)(y.value1);

      if (v1 instanceof Data_Ordering.LT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v1 instanceof Data_Ordering.GT) {
        return Data_Ordering.GT.value;
      }

      ;
      var v2 = Data_Ord.compare(Data_Time_Component.ordSecond)(x.value2)(y.value2);

      if (v2 instanceof Data_Ordering.LT) {
        return Data_Ordering.LT.value;
      }

      ;

      if (v2 instanceof Data_Ordering.GT) {
        return Data_Ordering.GT.value;
      }

      ;
      return Data_Ord.compare(Data_Time_Component.ordMillisecond)(x.value3)(y.value3);
    };
  });
  var boundedTime = new Data_Bounded.Bounded(function () {
    return ordTime;
  }, new Time(Data_Bounded.bottom(Data_Time_Component.boundedHour), Data_Bounded.bottom(Data_Time_Component.boundedMinute), Data_Bounded.bottom(Data_Time_Component.boundedSecond), Data_Bounded.bottom(Data_Time_Component.boundedMillisecond)), new Time(Data_Bounded.top(Data_Time_Component.boundedHour), Data_Bounded.top(Data_Time_Component.boundedMinute), Data_Bounded.top(Data_Time_Component.boundedSecond), Data_Bounded.top(Data_Time_Component.boundedMillisecond)));
  exports["Time"] = Time;
  exports["hour"] = hour;
  exports["minute"] = minute;
  exports["second"] = second;
  exports["millisecond"] = millisecond;
  exports["boundedTime"] = boundedTime;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.DateTime.Instant"] = $PS["Data.DateTime.Instant"] || {};
  var exports = $PS["Data.DateTime.Instant"];
  var $foreign = $PS["Data.DateTime.Instant"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Date = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_DateTime = $PS["Data.DateTime"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Time = $PS["Data.Time"];

  var toDateTime = function () {
    var mkDateTime = function mkDateTime(y) {
      return function (mo) {
        return function (d) {
          return function (h) {
            return function (mi) {
              return function (s) {
                return function (ms) {
                  return new Data_DateTime.DateTime(Data_Date.canonicalDate(y)(Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(mo)))(d), new Data_Time.Time(h, mi, s, ms));
                };
              };
            };
          };
        };
      };
    };

    return $foreign.toDateTimeImpl(mkDateTime);
  }();

  var instant = function instant(v) {
    if (v >= -8.6399778816e15 && v <= 8.639977881599999e15) {
      return new Data_Maybe.Just(v);
    }

    ;

    if (Data_Boolean.otherwise) {
      return Data_Maybe.Nothing.value;
    }

    ;
    throw new Error("Failed pattern match at Data.DateTime.Instant (line 44, column 1 - line 44, column 41): " + [v.constructor.name]);
  };

  exports["instant"] = instant;
  exports["toDateTime"] = toDateTime;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.JSDate"] = $PS["Data.JSDate"] || {};
  var exports = $PS["Data.JSDate"];
  var $foreign = $PS["Data.JSDate"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Date = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_DateTime = $PS["Data.DateTime"];
  var Data_DateTime_Instant = $PS["Data.DateTime.Instant"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Time = $PS["Data.Time"];
  var Data_Time_Component = $PS["Data.Time.Component"];
  var Data_Time_Duration = $PS["Data.Time.Duration"];
  var toInstant = Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(function ($4) {
    return Data_DateTime_Instant.instant(Data_Time_Duration.Milliseconds($4));
  })($foreign.toInstantImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value));
  var toDateTime = Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_DateTime_Instant.toDateTime))(toInstant);
  var toDate = Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_DateTime.date))(toDateTime);

  var fromDateTime = function fromDateTime(v) {
    return $foreign.jsdate({
      year: Data_Int.toNumber(Data_Enum.fromEnum(Data_Date_Component.boundedEnumYear)(Data_Date.year(v.value0))),
      month: Data_Int.toNumber(Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0)) - 1 | 0),
      day: Data_Int.toNumber(Data_Enum.fromEnum(Data_Date_Component.boundedEnumDay)(Data_Date.day(v.value0))),
      hour: Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(Data_Time.hour(v.value1))),
      minute: Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMinute)(Data_Time.minute(v.value1))),
      second: Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumSecond)(Data_Time.second(v.value1))),
      millisecond: Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Time.millisecond(v.value1)))
    });
  };

  exports["fromDateTime"] = fromDateTime;
  exports["toDate"] = toDate;
  exports["parse"] = $foreign.parse;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.ModifiedJulianDay"] = $PS["Data.ModifiedJulianDay"] || {};
  var exports = $PS["Data.ModifiedJulianDay"];
  var $foreign = $PS["Data.ModifiedJulianDay"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Date = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_DateTime = $PS["Data.DateTime"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Function = $PS["Data.Function"];
  var Data_Function_Uncurried = $PS["Data.Function.Uncurried"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_JSDate = $PS["Data.JSDate"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Show = $PS["Data.Show"];
  var Data_Time = $PS["Data.Time"];
  var Data_Time_Duration = $PS["Data.Time.Duration"];
  var showDay = new Data_Show.Show(function (v) {
    return "fromModifiedJulianDay " + Data_Show.show(Data_Show.showInt)(v);
  });
  var eqDay = new Data_Eq.Eq(function (x) {
    return function (y) {
      return x === y;
    };
  });
  var ordDay = new Data_Ord.Ord(function () {
    return eqDay;
  }, function (x) {
    return function (y) {
      return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
  });

  var diffDays = function diffDays(v) {
    return function (v1) {
      return v - v1 | 0;
    };
  };

  var date0 = Data_Date.canonicalDate(Data_Enum.toEnumWithDefaults(Data_Date_Component.boundedEnumYear)(Data_Bounded.bottom(Data_Date_Component.boundedYear))(Data_Bounded.top(Data_Date_Component.boundedYear))(1858))(Data_Date_Component.November.value)(Data_Enum.toEnumWithDefaults(Data_Date_Component.boundedEnumDay)(Data_Bounded.bottom(Data_Date_Component.boundedDay))(Data_Bounded.top(Data_Date_Component.boundedDay))(17));

  var fromDate = function fromDate(d) {
    var v = Data_Date.diff(Data_Time_Duration.durationDays)(d)(date0);
    return Data_Int.round(v);
  };

  var fromJSDate = function () {
    var $29 = Data_Functor.map(Data_Maybe.functorMaybe)(fromDate);
    return function ($30) {
      return $29(Data_JSDate.toDate($30));
    };
  }();

  var fromISO8601 = Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(fromJSDate)(Data_Function_Uncurried.runFn3($foreign["_parseIso"])(Data_Maybe.Just.create)(Data_Function["const"](Data_Maybe.Nothing.value)));

  var toDate = function toDate(v) {
    return Data_Maybe.fromMaybe(date0)(Data_Date.adjust(Data_Int.toNumber(v))(date0));
  };

  var toJSDate = function () {
    var $31 = Data_Function.flip(Data_DateTime.DateTime.create)(Data_Bounded.bottom(Data_Time.boundedTime));
    return function ($32) {
      return Data_JSDate.fromDateTime($31(toDate($32)));
    };
  }();

  var toISO8601 = function toISO8601($33) {
    return $foreign["_toIso"](toJSDate($33));
  };

  var addDays = function addDays(x) {
    return function (v) {
      return x + v | 0;
    };
  };

  exports["addDays"] = addDays;
  exports["diffDays"] = diffDays;
  exports["fromISO8601"] = fromISO8601;
  exports["toISO8601"] = toISO8601;
  exports["fromDate"] = fromDate;
  exports["fromJSDate"] = fromJSDate;
  exports["eqDay"] = eqDay;
  exports["ordDay"] = ordDay;
  exports["showDay"] = showDay;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.DProd"] = $PS["Type.DProd"] || {};
  var exports = $PS["Type.DProd"];

  var runDProd = function runDProd(v) {
    return v;
  };

  exports["runDProd"] = runDProd;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.Equality"] = $PS["Type.Equality"] || {};
  var exports = $PS["Type.Equality"];

  var TypeEquals = function TypeEquals(from, to) {
    this.from = from;
    this.to = to;
  };

  var refl = new TypeEquals(function (a) {
    return a;
  }, function (a) {
    return a;
  });
  exports["refl"] = refl;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.Equiv"] = $PS["Type.Equiv"] || {};
  var exports = $PS["Type.Equiv"];
  var Type_Equality = $PS["Type.Equality"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Decide = function Decide(decide) {
    this.decide = decide;
  };

  var refl = function refl(x) {
    return x(Type_Equality.refl);
  };

  var equivToF = function equivToF(v) {
    return Unsafe_Coerce.unsafeCoerce;
  };

  var equivTo = function equivTo(v) {
    return Unsafe_Coerce.unsafeCoerce;
  };

  var equivFromF2 = function equivFromF2(v) {
    return Unsafe_Coerce.unsafeCoerce;
  };

  var equivFromF = function equivFromF(v) {
    return Unsafe_Coerce.unsafeCoerce;
  };

  var equivFrom = function equivFrom(v) {
    return Unsafe_Coerce.unsafeCoerce;
  };

  var decide = function decide(dict) {
    return dict.decide;
  };

  exports["decide"] = decide;
  exports["equivTo"] = equivTo;
  exports["equivToF"] = equivToF;
  exports["equivFrom"] = equivFrom;
  exports["equivFromF"] = equivFromF;
  exports["equivFromF2"] = equivFromF2;
  exports["refl"] = refl;
  exports["Decide"] = Decide;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.GCompare"] = $PS["Type.GCompare"] || {};
  var exports = $PS["Type.GCompare"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];

  var WrEx = function WrEx(x) {
    return x;
  };

  var GLT = function () {
    function GLT() {}

    ;
    GLT.value = new GLT();
    return GLT;
  }();

  var GEQ = function () {
    function GEQ(value0) {
      this.value0 = value0;
    }

    ;

    GEQ.create = function (value0) {
      return new GEQ(value0);
    };

    return GEQ;
  }();

  var GGT = function () {
    function GGT() {}

    ;
    GGT.value = new GGT();
    return GGT;
  }();

  var GShow2 = function GShow2(gshow2) {
    this.gshow2 = gshow2;
  };

  var GShow = function GShow(gshow) {
    this.gshow = gshow;
  };

  var GEq = function GEq(Decide0, geq) {
    this.Decide0 = Decide0;
    this.geq = geq;
  };

  var GOrd = function GOrd(GEq0, gcompare) {
    this.GEq0 = GEq0;
    this.gcompare = gcompare;
  };

  var toOrdering = function toOrdering(v) {
    if (v instanceof GLT) {
      return Data_Ordering.LT.value;
    }

    ;

    if (v instanceof GEQ) {
      return Data_Ordering.EQ.value;
    }

    ;

    if (v instanceof GGT) {
      return Data_Ordering.GT.value;
    }

    ;
    throw new Error("Failed pattern match at Type.GCompare (line 21, column 14 - line 24, column 16): " + [v.constructor.name]);
  };

  var mkWrEx = function mkWrEx($22) {
    return WrEx(Data_Exists.mkExists($22));
  };

  var gshow2 = function gshow2(dict) {
    return dict.gshow2;
  };

  var gshow = function gshow(dict) {
    return dict.gshow;
  };

  var geq = function geq(dict) {
    return dict.geq;
  };

  var gcompare = function gcompare(dict) {
    return dict.gcompare;
  };

  var eqSome = function eqSome(dictGEq) {
    return new Data_Eq.Eq(function (v) {
      return function (v1) {
        return Data_Exists.runExists(function (x) {
          return Data_Exists.runExists(function (y) {
            return Data_Maybe.isJust(geq(dictGEq)(x)(y));
          })(v1);
        })(v);
      };
    });
  };

  var ordSome = function ordSome(dictGOrd) {
    return new Data_Ord.Ord(function () {
      return eqSome(dictGOrd.GEq0());
    }, function (v) {
      return function (v1) {
        return Data_Exists.runExists(function (x) {
          return Data_Exists.runExists(function (y) {
            return toOrdering(gcompare(dictGOrd)(x)(y));
          })(v1);
        })(v);
      };
    });
  };

  exports["geq"] = geq;
  exports["gshow"] = gshow;
  exports["gshow2"] = gshow2;
  exports["GEq"] = GEq;
  exports["GLT"] = GLT;
  exports["GEQ"] = GEQ;
  exports["GGT"] = GGT;
  exports["GOrd"] = GOrd;
  exports["GShow"] = GShow;
  exports["GShow2"] = GShow2;
  exports["mkWrEx"] = mkWrEx;
  exports["eqSome"] = eqSome;
  exports["ordSome"] = ordSome;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.Handler"] = $PS["Type.Handler"] || {};
  var exports = $PS["Type.Handler"];

  var Handle1 = function Handle1(handle1, unHandle1) {
    this.handle1 = handle1;
    this.unHandle1 = unHandle1;
  };

  var handle1 = function handle1(dict) {
    return dict.handle1;
  };

  exports["handle1"] = handle1;
  exports["Handle1"] = Handle1;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["D3.Scatter.Type"] = $PS["D3.Scatter.Type"] || {};
  var exports = $PS["D3.Scatter.Type"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_ModifiedJulianDay = $PS["Data.ModifiedJulianDay"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Show = $PS["Data.Show"];
  var Type_DProd = $PS["Type.DProd"];
  var Type_Equiv = $PS["Type.Equiv"];
  var Type_GCompare = $PS["Type.GCompare"];
  var Type_Handler = $PS["Type.Handler"];

  var Percent = function Percent(x) {
    return x;
  };

  var NInt = function () {
    function NInt(value0) {
      this.value0 = value0;
    }

    ;

    NInt.create = function (value0) {
      return new NInt(value0);
    };

    return NInt;
  }();

  var NNumber = function () {
    function NNumber(value0) {
      this.value0 = value0;
    }

    ;

    NNumber.create = function (value0) {
      return new NNumber(value0);
    };

    return NNumber;
  }();

  var NPercent = function () {
    function NPercent(value0) {
      this.value0 = value0;
    }

    ;

    NPercent.create = function (value0) {
      return new NPercent(value0);
    };

    return NPercent;
  }();

  var Days = function Days(x) {
    return x;
  };

  var SDay = function () {
    function SDay(value0) {
      this.value0 = value0;
    }

    ;

    SDay.create = function (value0) {
      return new SDay(value0);
    };

    return SDay;
  }();

  var SDays = function () {
    function SDays(value0) {
      this.value0 = value0;
    }

    ;

    SDays.create = function (value0) {
      return new SDays(value0);
    };

    return SDays;
  }();

  var SInt = function () {
    function SInt(value0) {
      this.value0 = value0;
    }

    ;

    SInt.create = function (value0) {
      return new SInt(value0);
    };

    return SInt;
  }();

  var SNumber = function () {
    function SNumber(value0) {
      this.value0 = value0;
    }

    ;

    SNumber.create = function (value0) {
      return new SNumber(value0);
    };

    return SNumber;
  }();

  var SPercent = function () {
    function SPercent(value0) {
      this.value0 = value0;
    }

    ;

    SPercent.create = function (value0) {
      return new SPercent(value0);
    };

    return SPercent;
  }();

  var $$Date = function () {
    function $$Date(value0) {
      this.value0 = value0;
    }

    ;

    $$Date.create = function (value0) {
      return new $$Date(value0);
    };

    return $$Date;
  }();

  var Linear = function () {
    function Linear(value0) {
      this.value0 = value0;
    }

    ;

    Linear.create = function (value0) {
      return new Linear(value0);
    };

    return Linear;
  }();

  var Log = function () {
    function Log(value0) {
      this.value0 = value0;
    }

    ;

    Log.create = function (value0) {
      return new Log(value0);
    };

    return Log;
  }();

  var unPercent = function unPercent(v) {
    return v;
  };

  var unDays = function unDays(v) {
    return v;
  };

  var toNType = function toNType(v) {
    if (v instanceof SDay) {
      return new Data_Either.Left(new Data_Either.Left(v.value0));
    }

    ;

    if (v instanceof SDays) {
      return new Data_Either.Left(new Data_Either.Right(v.value0));
    }

    ;

    if (v instanceof SInt) {
      return Data_Either.Right.create(new NInt(v.value0));
    }

    ;

    if (v instanceof SNumber) {
      return Data_Either.Right.create(new NNumber(v.value0));
    }

    ;

    if (v instanceof SPercent) {
      return Data_Either.Right.create(new NPercent(v.value0));
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 197, column 11 - line 203, column 37): " + [v.constructor.name]);
  };

  var showSType = new Data_Show.Show(function (v) {
    if (v instanceof SDay) {
      return "SDay";
    }

    ;

    if (v instanceof SDays) {
      return "SDays";
    }

    ;

    if (v instanceof SInt) {
      return "SInt";
    }

    ;

    if (v instanceof SNumber) {
      return "SNumber";
    }

    ;

    if (v instanceof SPercent) {
      return "SPercent";
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 138, column 12 - line 143, column 31): " + [v.constructor.name]);
  });
  var showDays = new Data_Show.Show(function (v) {
    return "Days " + Data_Show.show(Data_Show.showInt)(v);
  });

  var sTypeIx = function sTypeIx(v) {
    if (v instanceof SDay) {
      return 0;
    }

    ;

    if (v instanceof SDays) {
      return 1;
    }

    ;

    if (v instanceof SInt) {
      return 2;
    }

    ;

    if (v instanceof SNumber) {
      return 3;
    }

    ;

    if (v instanceof SPercent) {
      return 4;
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 148, column 11 - line 153, column 20): " + [v.constructor.name]);
  };

  var sPercent = new SPercent(Type_Equiv.refl);
  var sNumber = new SNumber(Type_Equiv.refl);
  var sInt = new SInt(Type_Equiv.refl);
  var sDays = new SDays(Type_Equiv.refl);
  var sDay = new SDay(Type_Equiv.refl);

  var runNScale = function runNScale(v) {
    return Type_DProd.runDProd(v);
  };

  var percentShow = new Data_Show.Show(function (v) {
    return "Percent " + Data_Show.show(Data_Show.showNumber)(v);
  });

  var sTypeShow = function sTypeShow(v) {
    if (v instanceof SDay) {
      var $204 = Data_Show.show(Data_ModifiedJulianDay.showDay);
      var $205 = Type_Equiv.equivTo(v.value0);
      return function ($206) {
        return $204($205($206));
      };
    }

    ;

    if (v instanceof SDays) {
      var $207 = Data_Show.show(showDays);
      var $208 = Type_Equiv.equivTo(v.value0);
      return function ($209) {
        return $207($208($209));
      };
    }

    ;

    if (v instanceof SInt) {
      var $210 = Data_Show.show(Data_Show.showInt);
      var $211 = Type_Equiv.equivTo(v.value0);
      return function ($212) {
        return $210($211($212));
      };
    }

    ;

    if (v instanceof SNumber) {
      var $213 = Data_Show.show(Data_Show.showNumber);
      var $214 = Type_Equiv.equivTo(v.value0);
      return function ($215) {
        return $213($214($215));
      };
    }

    ;

    if (v instanceof SPercent) {
      var $216 = Data_Show.show(percentShow);
      var $217 = Type_Equiv.equivTo(v.value0);
      return function ($218) {
        return $216($217($218));
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 157, column 13 - line 162, column 37): " + [v.constructor.name]);
  };

  var percentSemiring = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
      return v + v1;
    };
  }, function (v) {
    return function (v1) {
      return v * v1;
    };
  }, 1, 0);
  var percentRing = new Data_Ring.Ring(function () {
    return percentSemiring;
  }, function (v) {
    return function (v1) {
      return v - v1;
    };
  });

  var numberNType = function numberNType(v) {
    if (v instanceof NInt) {
      var $219 = Type_Equiv.equivFrom(v.value0);
      return function ($220) {
        return $219(Data_Int.round($220));
      };
    }

    ;

    if (v instanceof NNumber) {
      return Type_Equiv.equivFrom(v.value0);
    }

    ;

    if (v instanceof NPercent) {
      var $221 = Type_Equiv.equivFrom(v.value0);
      return function ($222) {
        return $221(Percent($222));
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 212, column 15 - line 215, column 42): " + [v.constructor.name]);
  };

  var nTypeSubtract = function nTypeSubtract(v) {
    if (v instanceof NInt) {
      return function (x) {
        return function (y) {
          return Type_Equiv.equivFrom(v.value0)(Type_Equiv.equivTo(v.value0)(x) - Type_Equiv.equivTo(v.value0)(y) | 0);
        };
      };
    }

    ;

    if (v instanceof NNumber) {
      return function (x) {
        return function (y) {
          return Type_Equiv.equivFrom(v.value0)(Type_Equiv.equivTo(v.value0)(x) - Type_Equiv.equivTo(v.value0)(y));
        };
      };
    }

    ;

    if (v instanceof NPercent) {
      return function (x) {
        return function (y) {
          return Type_Equiv.equivFrom(v.value0)(Data_Ring.sub(percentRing)(Type_Equiv.equivTo(v.value0)(x))(Type_Equiv.equivTo(v.value0)(y)));
        };
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 218, column 17 - line 221, column 66): " + [v.constructor.name]);
  };

  var nTypeNumber = function nTypeNumber(v) {
    if (v instanceof NInt) {
      var $223 = Type_Equiv.equivTo(v.value0);
      return function ($224) {
        return Data_Int.toNumber($223($224));
      };
    }

    ;

    if (v instanceof NNumber) {
      return Type_Equiv.equivTo(v.value0);
    }

    ;

    if (v instanceof NPercent) {
      var $225 = Type_Equiv.equivTo(v.value0);
      return function ($226) {
        return unPercent($225($226));
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 206, column 15 - line 209, column 42): " + [v.constructor.name]);
  };

  var nInt = new NInt(Type_Equiv.refl);
  var handle1Scale = new Type_Handler.Handle1(function (v) {
    if (v instanceof $$Date) {
      return function (h) {
        return h.date(v.value0);
      };
    }

    ;

    if (v instanceof Linear) {
      return function (h) {
        return h.linear(v.value0);
      };
    }

    ;

    if (v instanceof Log) {
      return function (h) {
        return h.log(v.value0);
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 341, column 15 - line 344, column 51): " + [v.constructor.name]);
  }, function (v) {
    return v({
      date: $$Date.create,
      linear: Linear.create,
      log: Log.create
    });
  });
  var handle1SType = new Type_Handler.Handle1(function (v) {
    if (v instanceof SDay) {
      return function (h) {
        return h.day(v.value0);
      };
    }

    ;

    if (v instanceof SDays) {
      return function (h) {
        return h.days(v.value0);
      };
    }

    ;

    if (v instanceof SInt) {
      return function (h) {
        return h["int"](v.value0);
      };
    }

    ;

    if (v instanceof SNumber) {
      return function (h) {
        return h.number(v.value0);
      };
    }

    ;

    if (v instanceof SPercent) {
      return function (h) {
        return h.percent(v.value0);
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 360, column 15 - line 365, column 54): " + [v.constructor.name]);
  }, function (v) {
    return v({
      day: SDay.create,
      days: SDays.create,
      "int": SInt.create,
      number: SNumber.create,
      percent: SPercent.create
    });
  });
  var gshowScale = new Type_GCompare.GShow(function (v) {
    if (v instanceof $$Date) {
      return "Date";
    }

    ;

    if (v instanceof Linear) {
      return "Linear";
    }

    ;

    if (v instanceof Log) {
      return "Log";
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 263, column 13 - line 266, column 24): " + [v.constructor.name]);
  });
  var showNScale = new Data_Show.Show(function (v) {
    return Type_GCompare.gshow(gshowScale)(Type_DProd.runDProd(v)(nInt));
  });
  var gshowSType = new Type_GCompare.GShow(Data_Show.show(showSType));
  var eqPercent = new Data_Eq.Eq(function (v) {
    return function (v1) {
      return v === v1;
    };
  });
  var ordPercent = new Data_Ord.Ord(function () {
    return eqPercent;
  }, function (v) {
    return function (v1) {
      return Data_Ord.compare(Data_Ord.ordNumber)(v)(v1);
    };
  });
  var eqDays = new Data_Eq.Eq(function (x) {
    return function (y) {
      return x === y;
    };
  });
  var ordDays = new Data_Ord.Ord(function () {
    return eqDays;
  }, function (x) {
    return function (y) {
      return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
  });

  var sTypeCompare = function sTypeCompare(v) {
    if (v instanceof SDay) {
      return function (x) {
        return function (y) {
          return Data_Ord.compare(Data_ModifiedJulianDay.ordDay)(Type_Equiv.equivTo(v.value0)(x))(Type_Equiv.equivTo(v.value0)(y));
        };
      };
    }

    ;

    if (v instanceof SDays) {
      return function (x) {
        return function (y) {
          return Data_Ord.compare(ordDays)(Type_Equiv.equivTo(v.value0)(x))(Type_Equiv.equivTo(v.value0)(y));
        };
      };
    }

    ;

    if (v instanceof SInt) {
      return function (x) {
        return function (y) {
          return Data_Ord.compare(Data_Ord.ordInt)(Type_Equiv.equivTo(v.value0)(x))(Type_Equiv.equivTo(v.value0)(y));
        };
      };
    }

    ;

    if (v instanceof SNumber) {
      return function (x) {
        return function (y) {
          return Data_Ord.compare(Data_Ord.ordNumber)(Type_Equiv.equivTo(v.value0)(x))(Type_Equiv.equivTo(v.value0)(y));
        };
      };
    }

    ;

    if (v instanceof SPercent) {
      return function (x) {
        return function (y) {
          return Data_Ord.compare(ordPercent)(Type_Equiv.equivTo(v.value0)(x))(Type_Equiv.equivTo(v.value0)(y));
        };
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 165, column 16 - line 170, column 62): " + [v.constructor.name]);
  };

  var decideSType = new Type_Equiv.Decide(function (v) {
    if (v instanceof SDay) {
      return function (v1) {
        if (v1 instanceof SDay) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof SDays) {
      return function (v1) {
        if (v1 instanceof SDays) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof SInt) {
      return function (v1) {
        if (v1 instanceof SInt) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof SNumber) {
      return function (v1) {
        if (v1 instanceof SNumber) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof SPercent) {
      return function (v1) {
        if (v1 instanceof SPercent) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;
    throw new Error("Failed pattern match at D3.Scatter.Type (line 104, column 14 - line 119, column 28): " + [v.constructor.name]);
  });
  var geqSType = new Type_GCompare.GEq(function () {
    return decideSType;
  }, Type_Equiv.decide(decideSType));
  var gordSType = new Type_GCompare.GOrd(function () {
    return geqSType;
  }, function (x) {
    return function (y) {
      var v = Type_GCompare.geq(geqSType)(x)(y);

      if (v instanceof Data_Maybe.Just) {
        return new Type_GCompare.GEQ(v.value0);
      }

      ;

      if (v instanceof Data_Maybe.Nothing) {
        var v1 = Data_Ord.compare(Data_Ord.ordInt)(sTypeIx(x))(sTypeIx(y));

        if (v1 instanceof Data_Ordering.LT) {
          return Type_GCompare.GLT.value;
        }

        ;

        if (v1 instanceof Data_Ordering.EQ) {
          return Type_GCompare.GLT.value;
        }

        ;

        if (v1 instanceof Data_Ordering.GT) {
          return Type_GCompare.GGT.value;
        }

        ;
        throw new Error("Failed pattern match at D3.Scatter.Type (line 126, column 20 - line 129, column 18): " + [v1.constructor.name]);
      }

      ;
      throw new Error("Failed pattern match at D3.Scatter.Type (line 124, column 20 - line 129, column 18): " + [v.constructor.name]);
    };
  });
  exports["Days"] = Days;
  exports["unDays"] = unDays;
  exports["Percent"] = Percent;
  exports["unPercent"] = unPercent;
  exports["SDay"] = SDay;
  exports["SDays"] = SDays;
  exports["SInt"] = SInt;
  exports["SNumber"] = SNumber;
  exports["SPercent"] = SPercent;
  exports["sDay"] = sDay;
  exports["sDays"] = sDays;
  exports["sInt"] = sInt;
  exports["sNumber"] = sNumber;
  exports["sPercent"] = sPercent;
  exports["sTypeShow"] = sTypeShow;
  exports["sTypeCompare"] = sTypeCompare;
  exports["NInt"] = NInt;
  exports["NNumber"] = NNumber;
  exports["NPercent"] = NPercent;
  exports["toNType"] = toNType;
  exports["nTypeNumber"] = nTypeNumber;
  exports["numberNType"] = numberNType;
  exports["nTypeSubtract"] = nTypeSubtract;
  exports["nInt"] = nInt;
  exports["Date"] = $$Date;
  exports["Linear"] = Linear;
  exports["Log"] = Log;
  exports["runNScale"] = runNScale;
  exports["decideSType"] = decideSType;
  exports["gordSType"] = gordSType;
  exports["showSType"] = showSType;
  exports["gshowSType"] = gshowSType;
  exports["showNScale"] = showNScale;
  exports["handle1Scale"] = handle1Scale;
  exports["handle1SType"] = handle1SType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Dated"] = $PS["Data.Dated"] || {};
  var exports = $PS["Data.Dated"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Array = $PS["Data.Array"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_ModifiedJulianDay = $PS["Data.ModifiedJulianDay"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];

  var Dated = function Dated(x) {
    return x;
  };

  var zipDated = function zipDated(f) {
    return function (v) {
      return function (v1) {
        var v2 = Data_Ord.compare(Data_Ord.ordInt)(v.start)(v1.start);

        if (v2 instanceof Data_Ordering.LT) {
          return {
            start: v1.start,
            values: Data_Array.zipWith(f)(Data_Array.drop(v1.start - v.start | 0)(v.values))(v1.values)
          };
        }

        ;

        if (v2 instanceof Data_Ordering.EQ) {
          return {
            start: v.start,
            values: Data_Array.zipWith(f)(v.values)(v1.values)
          };
        }

        ;

        if (v2 instanceof Data_Ordering.GT) {
          return {
            start: v.start,
            values: Data_Array.zipWith(f)(v.values)(Data_Array.drop(v.start - v1.start | 0)(v1.values))
          };
        }

        ;
        throw new Error("Failed pattern match at Data.Dated (line 57, column 42 - line 60, column 90): " + [v2.constructor.name]);
      };
    };
  };

  var takeEnd = function takeEnd(i) {
    return function (v) {
      return {
        start: Data_ModifiedJulianDay.addDays(Data_Array.length(v.values) - i | 0)(v.start),
        values: Data_Array.takeEnd(i)(v.values)
      };
    };
  };

  var take = function take(i) {
    return function (v) {
      return Dated({
        start: v.start,
        values: Data_Array.take(i)(v.values)
      });
    };
  };

  var newtypeDated = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Dated);

  var mapWithIndex = function mapWithIndex(f) {
    return function (v) {
      return {
        start: v.start,
        values: Data_Array.mapWithIndex(function (i) {
          return f(Data_ModifiedJulianDay.addDays(i)(v.start));
        })(v.values)
      };
    };
  };

  var functorDated = new Data_Functor.Functor(function (f) {
    return function (v) {
      return {
        start: v.start,
        values: Data_Functor.map(Data_Functor.functorArray)(f)(v.values)
      };
    };
  });
  var functorWithIndexDated = new Data_FunctorWithIndex.FunctorWithIndex(function () {
    return functorDated;
  }, mapWithIndex);
  var foldedableDated = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      var $93 = Data_Foldable.foldMap(Data_Foldable.foldableArray)(dictMonoid)(f);
      var $94 = Data_Newtype.unwrap(newtypeDated);
      return function ($95) {
        return $93(function (v) {
          return v.values;
        }($94($95)));
      };
    };
  }, function (f) {
    return function (z) {
      var $96 = Data_Foldable.foldl(Data_Foldable.foldableArray)(f)(z);
      var $97 = Data_Newtype.unwrap(newtypeDated);
      return function ($98) {
        return $96(function (v) {
          return v.values;
        }($97($98)));
      };
    };
  }, function (f) {
    return function (z) {
      var $99 = Data_Foldable.foldr(Data_Foldable.foldableArray)(f)(z);
      var $100 = Data_Newtype.unwrap(newtypeDated);
      return function ($101) {
        return $99(function (v) {
          return v.values;
        }($100($101)));
      };
    };
  });

  var findLastIndex = function findLastIndex(p) {
    return function (v) {
      return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_ModifiedJulianDay.addDays)(v.start))(Data_Array.findLastIndex(p)(v.values));
    };
  };

  var findLast = function findLast(v) {
    return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Array.last(v.values))(function (value) {
      return {
        day: Data_ModifiedJulianDay.addDays(Data_Array.length(v.values))(v.start),
        value: value
      };
    });
  };

  var findIndex = function findIndex(p) {
    return function (v) {
      return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_ModifiedJulianDay.addDays)(v.start))(Data_Array.findIndex(p)(v.values));
    };
  };

  var findHead = function findHead(v) {
    return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Array.head(v.values))(function (value) {
      return {
        day: v.start,
        value: value
      };
    });
  };

  var dropBefore = function dropBefore(d0) {
    return function (v) {
      return {
        start: Data_Ord.max(Data_ModifiedJulianDay.ordDay)(d0)(v.start),
        values: Data_Array.drop(Data_ModifiedJulianDay.diffDays(d0)(v.start))(v.values)
      };
    };
  };

  var dropUntil = function dropUntil(f) {
    return function (v) {
      var v1 = findIndex(f)(v);

      if (v1 instanceof Data_Maybe.Nothing) {
        return {
          start: v.start,
          values: []
        };
      }

      ;

      if (v1 instanceof Data_Maybe.Just) {
        return dropBefore(v1.value0)(v);
      }

      ;
      throw new Error("Failed pattern match at Data.Dated (line 134, column 26 - line 136, column 39): " + [v1.constructor.name]);
    };
  };

  var dropAfter = function dropAfter(d0) {
    return function (v) {
      return Dated({
        start: v.start,
        values: Data_Array.take(Data_ModifiedJulianDay.diffDays(d0)(v.start))(v.values)
      });
    };
  };

  var dropUntilEnd = function dropUntilEnd(f) {
    return function (v) {
      var v1 = findLastIndex(f)(v);

      if (v1 instanceof Data_Maybe.Nothing) {
        return {
          start: v.start,
          values: []
        };
      }

      ;

      if (v1 instanceof Data_Maybe.Just) {
        return dropAfter(v1.value0)(v);
      }

      ;
      throw new Error("Failed pattern match at Data.Dated (line 143, column 29 - line 145, column 38): " + [v1.constructor.name]);
    };
  };

  var drop = function drop(i) {
    return function (v) {
      return {
        start: Data_ModifiedJulianDay.addDays(i)(v.start),
        values: Data_Array.drop(i)(v.values)
      };
    };
  };

  var datedValues = function datedValues(v) {
    return v.values;
  };

  var applyDated = new Control_Apply.Apply(function () {
    return functorDated;
  }, zipDated(Data_Function.apply));
  exports["datedValues"] = datedValues;
  exports["take"] = take;
  exports["takeEnd"] = takeEnd;
  exports["findHead"] = findHead;
  exports["findLast"] = findLast;
  exports["dropUntil"] = dropUntil;
  exports["dropUntilEnd"] = dropUntilEnd;
  exports["functorDated"] = functorDated;
  exports["functorWithIndexDated"] = functorWithIndexDated;
  exports["applyDated"] = applyDated;
  exports["foldedableDated"] = foldedableDated;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Number"] = $PS["Data.Number"] || {};
  var exports = $PS["Data.Number"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Global = $PS["Global"];
  var nan = Global.nan;
  var $$isFinite = Global["isFinite"];

  var fromString = function () {
    var check = function check(num) {
      if ($$isFinite(num)) {
        return new Data_Maybe.Just(num);
      }

      ;

      if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
      }

      ;
      throw new Error("Failed pattern match at Data.Number (line 45, column 5 - line 46, column 39): " + [num.constructor.name]);
    };

    return function ($1) {
      return check(Global.readFloat($1));
    };
  }();

  exports["fromString"] = fromString;
  exports["nan"] = nan;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Ord.Max"] = $PS["Data.Ord.Max"] || {};
  var exports = $PS["Data.Ord.Max"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Max = function Max(x) {
    return x;
  };

  var semigroupMax = function semigroupMax(dictOrd) {
    return new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
        return Data_Ord.max(dictOrd)(v)(v1);
      };
    });
  };

  var newtypeMax = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Max);
  exports["newtypeMax"] = newtypeMax;
  exports["semigroupMax"] = semigroupMax;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Array.Partial"] = $PS["Data.Array.Partial"] || {};
  var exports = $PS["Data.Array.Partial"];
  var Data_Array = $PS["Data.Array"];

  var tail = function tail(dictPartial) {
    return function (xs) {
      return Data_Array.slice(1)(Data_Array.length(xs))(xs);
    };
  };

  var head = function head(dictPartial) {
    return function (xs) {
      return xs[0];
    };
  };

  exports["head"] = head;
  exports["tail"] = tail;
})(PS);

(function (exports) {
  "use strict";

  exports.defer = function (thunk) {
    var v = null;
    return function () {
      if (thunk === undefined) return v;
      v = thunk();
      thunk = undefined; // eslint-disable-line no-param-reassign

      return v;
    };
  };

  exports.force = function (l) {
    return l();
  };
})(PS["Data.Lazy"] = PS["Data.Lazy"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Lazy"] = $PS["Data.Lazy"] || {};
  var exports = $PS["Data.Lazy"];
  var $foreign = $PS["Data.Lazy"];
  var Data_Functor = $PS["Data.Functor"];
  var functorLazy = new Data_Functor.Functor(function (f) {
    return function (l) {
      return $foreign.defer(function (v) {
        return f($foreign.force(l));
      });
    };
  });
  exports["functorLazy"] = functorLazy;
  exports["defer"] = $foreign.defer;
  exports["force"] = $foreign.force;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Sequence.Internal"] = $PS["Data.Sequence.Internal"] || {};
  var exports = $PS["Data.Sequence.Internal"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Lazy = $PS["Data.Lazy"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Measured = function Measured(measure) {
    this.measure = measure;
  };

  var measuredElem = new Measured(function (v) {
    return 1;
  });

  var measure = function measure(dict) {
    return dict.measure;
  };

  var measuredArray = function measuredArray(dictMonoid) {
    return function (dictMeasured) {
      return new Measured(function (xs) {
        return Data_Foldable.foldl(Data_Foldable.foldableArray)(function (i) {
          return function (a) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(i)(measure(dictMeasured)(a));
          };
        })(Data_Monoid.mempty(dictMonoid))(xs);
      });
    };
  };

  var measuredLazy = function measuredLazy(dictMonoid) {
    return function (dictMeasured) {
      return new Measured(function (s) {
        return measure(dictMeasured)(Data_Lazy.force(s));
      });
    };
  };

  var mapmap = function mapmap(dictFunctor) {
    return function (dictFunctor1) {
      var $75 = Data_Functor.map(dictFunctor);
      var $76 = Data_Functor.map(dictFunctor1);
      return function ($77) {
        return $75($76($77));
      };
    };
  };

  var mapmapmap = function mapmapmap(dictFunctor) {
    return function (dictFunctor1) {
      return function (dictFunctor2) {
        var $78 = mapmap(dictFunctor)(dictFunctor1);
        var $79 = Data_Functor.map(dictFunctor2);
        return function ($80) {
          return $78($79($80));
        };
      };
    };
  };

  var liftElem = Unsafe_Coerce.unsafeCoerce;
  var lift2Elem = Unsafe_Coerce.unsafeCoerce;

  var getElem = function getElem(v) {
    return v;
  };

  exports["mapmapmap"] = mapmapmap;
  exports["Measured"] = Measured;
  exports["measure"] = measure;
  exports["getElem"] = getElem;
  exports["lift2Elem"] = lift2Elem;
  exports["liftElem"] = liftElem;
  exports["measuredArray"] = measuredArray;
  exports["measuredLazy"] = measuredLazy;
  exports["measuredElem"] = measuredElem;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.FingerTree.Digit"] = $PS["Data.FingerTree.Digit"] || {};
  var exports = $PS["Data.FingerTree.Digit"];
  var Data_Array = $PS["Data.Array"];
  var Data_Array_Partial = $PS["Data.Array.Partial"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Sequence_Internal = $PS["Data.Sequence.Internal"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var runDigit = function runDigit(v) {
    return v;
  };

  var tailDigit = function () {
    var $14 = Data_Array_Partial.tail();
    return function ($15) {
      return $14(runDigit($15));
    };
  }();

  var overDigit = Unsafe_Coerce.unsafeCoerce;

  var snocDigit = function snocDigit(dictPartial) {
    return function (dg) {
      return function (x) {
        return overDigit(function (xs) {
          return Data_Array.snoc(xs)(x);
        })(dg);
      };
    };
  };

  var mkDigitMay = function mkDigitMay(xs) {
    var $13 = Data_Ord.between(Data_Ord.ordInt)(1)(4)(Data_Array.length(xs));

    if ($13) {
      return new Data_Maybe.Just(xs);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  var mkDigit3 = function mkDigit3(x) {
    return function (y) {
      return function (z) {
        return [x, y, z];
      };
    };
  };

  var mkDigit2 = function mkDigit2(x) {
    return function (y) {
      return [x, y];
    };
  };

  var mkDigit1 = function mkDigit1(x) {
    return [x];
  };

  var measuredDigit = function measuredDigit(dictMonoid) {
    return function (dictMeasured) {
      return new Data_Sequence_Internal.Measured(function () {
        var $16 = Data_Sequence_Internal.measure(Data_Sequence_Internal.measuredArray(dictMonoid)(dictMeasured));
        return function ($17) {
          return $16(runDigit($17));
        };
      }());
    };
  };

  var headDigit = function () {
    var $22 = Data_Array_Partial.head();
    return function ($23) {
      return $22(runDigit($23));
    };
  }();

  var functorDigit = Data_Functor.functorArray;
  var foldableDigit = Data_Foldable.foldableArray;

  var consDigit = function consDigit(dictPartial) {
    return function (x) {
      return function (dg) {
        return overDigit(Data_Array.cons(x))(dg);
      };
    };
  };

  exports["mkDigitMay"] = mkDigitMay;
  exports["mkDigit1"] = mkDigit1;
  exports["mkDigit2"] = mkDigit2;
  exports["mkDigit3"] = mkDigit3;
  exports["runDigit"] = runDigit;
  exports["headDigit"] = headDigit;
  exports["tailDigit"] = tailDigit;
  exports["snocDigit"] = snocDigit;
  exports["consDigit"] = consDigit;
  exports["functorDigit"] = functorDigit;
  exports["foldableDigit"] = foldableDigit;
  exports["measuredDigit"] = measuredDigit;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.FingerTree"] = $PS["Data.FingerTree"] || {};
  var exports = $PS["Data.FingerTree"];
  var Data_Array = $PS["Data.Array"];
  var Data_FingerTree_Digit = $PS["Data.FingerTree.Digit"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Lazy = $PS["Data.Lazy"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Sequence_Internal = $PS["Data.Sequence.Internal"];

  var NilL = function () {
    function NilL() {}

    ;
    NilL.value = new NilL();
    return NilL;
  }();

  var ConsL = function () {
    function ConsL(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    ConsL.create = function (value0) {
      return function (value1) {
        return new ConsL(value0, value1);
      };
    };

    return ConsL;
  }();

  var Node2 = function () {
    function Node2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    Node2.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new Node2(value0, value1, value2);
        };
      };
    };

    return Node2;
  }();

  var Node3 = function () {
    function Node3(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Node3.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Node3(value0, value1, value2, value3);
          };
        };
      };
    };

    return Node3;
  }();

  var Empty = function () {
    function Empty() {}

    ;
    Empty.value = new Empty();
    return Empty;
  }();

  var Single = function () {
    function Single(value0) {
      this.value0 = value0;
    }

    ;

    Single.create = function (value0) {
      return new Single(value0);
    };

    return Single;
  }();

  var Deep = function () {
    function Deep(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Deep.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Deep(value0, value1, value2, value3);
          };
        };
      };
    };

    return Deep;
  }();

  var nodeToDigit = function () {
    var go = function go(v) {
      if (v instanceof Node2) {
        return Data_FingerTree_Digit.mkDigit2(v.value1)(v.value2);
      }

      ;

      if (v instanceof Node3) {
        return Data_FingerTree_Digit.mkDigit3(v.value1)(v.value2)(v.value3);
      }

      ;
      throw new Error("Failed pattern match at Data.FingerTree (line 88, column 3 - line 88, column 34): " + [v.constructor.name]);
    };

    return go;
  }();

  var node3 = function node3(dictMonoid) {
    return function (dictMeasured) {
      return function (a) {
        return function (b) {
          return function (c) {
            return new Node3(Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Sequence_Internal.measure(dictMeasured)(a))(Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Sequence_Internal.measure(dictMeasured)(b))(Data_Sequence_Internal.measure(dictMeasured)(c))), a, b, c);
          };
        };
      };
    };
  };

  var node2 = function node2(dictMonoid) {
    return function (dictMeasured) {
      return function (a) {
        return function (b) {
          return new Node2(Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Sequence_Internal.measure(dictMeasured)(a))(Data_Sequence_Internal.measure(dictMeasured)(b)), a, b);
        };
      };
    };
  };

  var nodes = function nodes(dictMonoid) {
    return function (dictMeasured) {
      return function (xs) {
        if (xs.length === 2) {
          return [node2(dictMonoid)(dictMeasured)(xs[0])(xs[1])];
        }

        ;

        if (xs.length === 3) {
          return [node3(dictMonoid)(dictMeasured)(xs[0])(xs[1])(xs[2])];
        }

        ;

        if (xs.length === 4) {
          return [node2(dictMonoid)(dictMeasured)(xs[0])(xs[1]), node2(dictMonoid)(dictMeasured)(xs[2])(xs[3])];
        }

        ;
        var idx = Data_Array.unsafeIndex();
        return Data_Array.cons(node3(dictMonoid)(dictMeasured)(idx(xs)(0))(idx(xs)(1))(idx(xs)(2)))(nodes(dictMonoid)(dictMeasured)(Data_Array.drop(3)(xs)));
      };
    };
  };

  var measuredNode = new Data_Sequence_Internal.Measured(function (v) {
    if (v instanceof Node2) {
      return v.value0;
    }

    ;

    if (v instanceof Node3) {
      return v.value0;
    }

    ;
    throw new Error("Failed pattern match at Data.FingerTree (line 107, column 1 - line 109, column 30): " + [v.constructor.name]);
  });

  var measuredFingerTree = function measuredFingerTree(dictMonoid) {
    return function (dictMeasured) {
      return new Data_Sequence_Internal.Measured(function (v) {
        if (v instanceof Empty) {
          return Data_Monoid.mempty(dictMonoid);
        }

        ;

        if (v instanceof Single) {
          return Data_Sequence_Internal.measure(dictMeasured)(v.value0);
        }

        ;

        if (v instanceof Deep) {
          return Data_Lazy.force(v.value0);
        }

        ;
        throw new Error("Failed pattern match at Data.FingerTree (line 228, column 1 - line 232, column 37): " + [v.constructor.name]);
      });
    };
  };

  var lazyEmpty = Data_Lazy.defer(function (v) {
    return Empty.value;
  });
  var functorNode = new Data_Functor.Functor(function (f) {
    return function (v) {
      if (v instanceof Node2) {
        return new Node2(v.value0, f(v.value1), f(v.value2));
      }

      ;

      if (v instanceof Node3) {
        return new Node3(v.value0, f(v.value1), f(v.value2), f(v.value3));
      }

      ;
      throw new Error("Failed pattern match at Data.FingerTree (line 91, column 1 - line 93, column 52): " + [f.constructor.name, v.constructor.name]);
    };
  });
  var functorFingerTree = new Data_Functor.Functor(function (f) {
    return function (v) {
      if (v instanceof Empty) {
        return Empty.value;
      }

      ;

      if (v instanceof Single) {
        return new Single(f(v.value0));
      }

      ;

      if (v instanceof Deep) {
        return new Deep(v.value0, Data_Functor.map(Data_FingerTree_Digit.functorDigit)(f)(v.value1), Data_Sequence_Internal.mapmapmap(Data_Lazy.functorLazy)(functorFingerTree)(functorNode)(f)(v.value2), Data_Functor.map(Data_FingerTree_Digit.functorDigit)(f)(v.value3));
      }

      ;
      throw new Error("Failed pattern match at Data.FingerTree (line 183, column 1 - line 186, column 68): " + [f.constructor.name, v.constructor.name]);
    };
  });
  var foldableNode = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return function (xs) {
        return Data_Foldable.foldr(foldableNode)(function (x) {
          return function (acc) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(Data_Monoid.mempty(dictMonoid))(xs);
      };
    };
  }, function (l) {
    return function (z) {
      return function (v) {
        if (v instanceof Node2) {
          return l(l(z)(v.value1))(v.value2);
        }

        ;

        if (v instanceof Node3) {
          return l(l(l(z)(v.value1))(v.value2))(v.value3);
        }

        ;
        throw new Error("Failed pattern match at Data.FingerTree (line 95, column 1 - line 100, column 56): " + [l.constructor.name, z.constructor.name, v.constructor.name]);
      };
    };
  }, function (r) {
    return function (z) {
      return function (v) {
        if (v instanceof Node2) {
          return r(v.value1)(r(v.value2)(z));
        }

        ;

        if (v instanceof Node3) {
          return r(v.value1)(r(v.value2)(r(v.value3)(z)));
        }

        ;
        throw new Error("Failed pattern match at Data.FingerTree (line 95, column 1 - line 100, column 56): " + [r.constructor.name, z.constructor.name, v.constructor.name]);
      };
    };
  });
  var foldableFingerTree = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return function (xs) {
        return Data_Foldable.foldr(foldableFingerTree)(function (x) {
          return function (acc) {
            return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(Data_Monoid.mempty(dictMonoid))(xs);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Empty) {
          return z;
        }

        ;

        if (v1 instanceof Single) {
          return v(z)(v1.value0);
        }

        ;

        if (v1 instanceof Deep) {
          var leftFold = Data_Foldable.foldl(Data_FingerTree_Digit.foldableDigit)(v);
          var deepLeftFold = Data_Foldable.foldl(foldableFingerTree)(Data_Foldable.foldl(foldableNode)(v));
          return leftFold(deepLeftFold(leftFold(z)(v1.value1))(Data_Lazy.force(v1.value2)))(v1.value3);
        }

        ;
        throw new Error("Failed pattern match at Data.FingerTree (line 188, column 1 - line 212, column 56): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  }, function (v) {
    return function (z) {
      return function (v1) {
        if (v1 instanceof Empty) {
          return z;
        }

        ;

        if (v1 instanceof Single) {
          return v(v1.value0)(z);
        }

        ;

        if (v1 instanceof Deep) {
          var flipFoldr$prime = Data_Function.flip(Data_Foldable.foldr(Data_FingerTree_Digit.foldableDigit)(v));
          var flipFoldr = Data_Function.flip(Data_Foldable.foldr(Data_FingerTree_Digit.foldableDigit)(v));
          var deepFlipFoldr = Data_Function.flip(Data_Foldable.foldr(foldableFingerTree)(Data_Function.flip(Data_Foldable.foldr(foldableNode)(v))));
          return flipFoldr$prime(v1.value1)(deepFlipFoldr(Data_Lazy.force(v1.value2))(flipFoldr(v1.value3)(z)));
        }

        ;
        throw new Error("Failed pattern match at Data.FingerTree (line 188, column 1 - line 212, column 56): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
      };
    };
  });

  var deep = function deep(dictMonoid) {
    return function (dictMeasured) {
      return function (pr) {
        return function (m) {
          return function (sf) {
            return new Deep(Data_Lazy.defer(function (v) {
              return Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Sequence_Internal.measure(Data_FingerTree_Digit.measuredDigit(dictMonoid)(dictMeasured))(pr))(Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Sequence_Internal.measure(Data_Sequence_Internal.measuredLazy(dictMonoid)(measuredFingerTree(dictMonoid)(measuredNode)))(m))(Data_Sequence_Internal.measure(Data_FingerTree_Digit.measuredDigit(dictMonoid)(dictMeasured))(sf)));
            }), pr, m, sf);
          };
        };
      };
    };
  };

  var snoc = function snoc(dictMonoid) {
    return function (dictMeasured) {
      return function (v) {
        return function (a) {
          if (v instanceof Empty) {
            return new Single(a);
          }

          ;

          if (v instanceof Single) {
            return deep(dictMonoid)(dictMeasured)(Data_FingerTree_Digit.mkDigit1(v.value0))(lazyEmpty)(Data_FingerTree_Digit.mkDigit1(a));
          }

          ;

          if (v instanceof Deep) {
            var v1 = Data_FingerTree_Digit.runDigit(v.value3);

            if (v1.length === 4) {
              var forcedM = Data_Lazy.force(v.value2);
              return deep(dictMonoid)(dictMeasured)(v.value1)(Data_Lazy.defer(function (v2) {
                return snoc(dictMonoid)(measuredNode)(forcedM)(node3(dictMonoid)(dictMeasured)(v1[0])(v1[1])(v1[2]));
              }))(Data_FingerTree_Digit.mkDigit2(v1[3])(a));
            }

            ;
            return deep(dictMonoid)(dictMeasured)(v.value1)(v.value2)(Data_FingerTree_Digit.snocDigit()(v.value3)(a));
          }

          ;
          throw new Error("Failed pattern match at Data.FingerTree (line 262, column 1 - line 263, column 40): " + [v.constructor.name, a.constructor.name]);
        };
      };
    };
  };

  var snocAll = function snocAll(dictMonoid) {
    return function (dictMeasured) {
      return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(snoc(dictMonoid)(dictMeasured));
      };
    };
  };

  var toFingerTree = function toFingerTree(dictMonoid) {
    return function (dictMeasured) {
      return function (dictFoldable) {
        return function (s) {
          return snocAll(dictMonoid)(dictMeasured)(dictFoldable)(Empty.value)(s);
        };
      };
    };
  };

  var viewL = function viewL(dictMonoid) {
    return function (dictMeasured) {
      return function (v) {
        if (v instanceof Empty) {
          return NilL.value;
        }

        ;

        if (v instanceof Single) {
          return new ConsL(v.value0, lazyEmpty);
        }

        ;

        if (v instanceof Deep) {
          return new ConsL(Data_FingerTree_Digit.headDigit(v.value1), Data_Lazy.defer(function (v1) {
            return deepL(dictMonoid)(dictMeasured)(Data_FingerTree_Digit.tailDigit(v.value1))(v.value2)(v.value3);
          }));
        }

        ;
        throw new Error("Failed pattern match at Data.FingerTree (line 294, column 1 - line 295, column 50): " + [v.constructor.name]);
      };
    };
  };

  var deepL = function deepL(dictMonoid) {
    return function (dictMeasured) {
      return function (pr$prime) {
        return function (m) {
          return function (sf) {
            var v = Data_FingerTree_Digit.mkDigitMay(pr$prime);

            if (v instanceof Data_Maybe.Just) {
              return deep(dictMonoid)(dictMeasured)(v.value0)(m)(sf);
            }

            ;

            if (v instanceof Data_Maybe.Nothing) {
              var v1 = viewL(dictMonoid)(measuredNode)(Data_Lazy.force(m));

              if (v1 instanceof NilL) {
                return toFingerTree(dictMonoid)(dictMeasured)(Data_FingerTree_Digit.foldableDigit)(sf);
              }

              ;

              if (v1 instanceof ConsL) {
                return deep(dictMonoid)(dictMeasured)(nodeToDigit(v1.value0))(v1.value1)(sf);
              }

              ;
              throw new Error("Failed pattern match at Data.FingerTree (line 321, column 7 - line 323, column 49): " + [v1.constructor.name]);
            }

            ;
            throw new Error("Failed pattern match at Data.FingerTree (line 317, column 3 - line 323, column 49): " + [v.constructor.name]);
          };
        };
      };
    };
  };

  var cons = function cons(dictMonoid) {
    return function (dictMeasured) {
      return function (a) {
        return function (v) {
          if (v instanceof Empty) {
            return new Single(a);
          }

          ;

          if (v instanceof Single) {
            return deep(dictMonoid)(dictMeasured)(Data_FingerTree_Digit.mkDigit1(a))(lazyEmpty)(Data_FingerTree_Digit.mkDigit1(v.value0));
          }

          ;

          if (v instanceof Deep) {
            var v1 = Data_FingerTree_Digit.runDigit(v.value1);

            if (v1.length === 4) {
              var forcedM = Data_Lazy.force(v.value2);
              return deep(dictMonoid)(dictMeasured)(Data_FingerTree_Digit.mkDigit2(a)(v1[0]))(Data_Lazy.defer(function (v2) {
                return cons(dictMonoid)(measuredNode)(node3(dictMonoid)(dictMeasured)(v1[1])(v1[2])(v1[3]))(forcedM);
              }))(v.value3);
            }

            ;
            var pr$prime = Data_FingerTree_Digit.consDigit()(a)(v.value1);
            return deep(dictMonoid)(dictMeasured)(pr$prime)(v.value2)(v.value3);
          }

          ;
          throw new Error("Failed pattern match at Data.FingerTree (line 234, column 1 - line 235, column 40): " + [a.constructor.name, v.constructor.name]);
        };
      };
    };
  };

  var consAll = function consAll(dictMonoid) {
    return function (dictMeasured) {
      return function (dictFoldable) {
        return Data_Function.flip(Data_Foldable.foldr(dictFoldable)(cons(dictMonoid)(dictMeasured)));
      };
    };
  };

  var app3 = function app3(dictMonoid) {
    return function (dictMeasured) {
      return function (v) {
        return function (ts) {
          return function (v1) {
            if (v instanceof Empty) {
              return consAll(dictMonoid)(dictMeasured)(Data_Foldable.foldableArray)(ts)(v1);
            }

            ;

            if (v1 instanceof Empty) {
              return snocAll(dictMonoid)(dictMeasured)(Data_Foldable.foldableArray)(v)(ts);
            }

            ;

            if (v instanceof Single) {
              return cons(dictMonoid)(dictMeasured)(v.value0)(consAll(dictMonoid)(dictMeasured)(Data_Foldable.foldableArray)(ts)(v1));
            }

            ;

            if (v1 instanceof Single) {
              return snoc(dictMonoid)(dictMeasured)(snocAll(dictMonoid)(dictMeasured)(Data_Foldable.foldableArray)(v)(ts))(v1.value0);
            }

            ;

            if (v instanceof Deep && v1 instanceof Deep) {
              var computeM$prime = function computeM$prime(v2) {
                return app3(dictMonoid)(measuredNode)(Data_Lazy.force(v.value2))(nodes(dictMonoid)(dictMeasured)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_FingerTree_Digit.runDigit(v.value3))(Data_Semigroup.append(Data_Semigroup.semigroupArray)(ts)(Data_FingerTree_Digit.runDigit(v1.value1)))))(Data_Lazy.force(v1.value2));
              };

              return deep(dictMonoid)(dictMeasured)(v.value1)(Data_Lazy.defer(computeM$prime))(v1.value3);
            }

            ;
            throw new Error("Failed pattern match at Data.FingerTree (line 372, column 1 - line 373, column 70): " + [v.constructor.name, ts.constructor.name, v1.constructor.name]);
          };
        };
      };
    };
  };

  var append = function append(dictMonoid) {
    return function (dictMeasured) {
      return function (xs) {
        return function (ys) {
          return app3(dictMonoid)(dictMeasured)(xs)([])(ys);
        };
      };
    };
  };

  exports["Empty"] = Empty;
  exports["cons"] = cons;
  exports["snoc"] = snoc;
  exports["NilL"] = NilL;
  exports["ConsL"] = ConsL;
  exports["viewL"] = viewL;
  exports["append"] = append;
  exports["functorFingerTree"] = functorFingerTree;
  exports["foldableFingerTree"] = foldableFingerTree;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Monoid.Additive"] = $PS["Data.Monoid.Additive"] || {};
  var exports = $PS["Data.Monoid.Additive"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];

  var semigroupAdditive = function semigroupAdditive(dictSemiring) {
    return new Data_Semigroup.Semigroup(function (v) {
      return function (v1) {
        return Data_Semiring.add(dictSemiring)(v)(v1);
      };
    });
  };

  var monoidAdditive = function monoidAdditive(dictSemiring) {
    return new Data_Monoid.Monoid(function () {
      return semigroupAdditive(dictSemiring);
    }, Data_Semiring.zero(dictSemiring));
  };

  exports["monoidAdditive"] = monoidAdditive;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Sequence"] = $PS["Data.Sequence"] || {};
  var exports = $PS["Data.Sequence"];
  var Data_FingerTree = $PS["Data.FingerTree"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Lazy = $PS["Data.Lazy"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid_Additive = $PS["Data.Monoid.Additive"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Sequence_Internal = $PS["Data.Sequence.Internal"];
  var Data_Tuple = $PS["Data.Tuple"];

  var uncons = function uncons(v) {
    var v1 = Data_FingerTree.viewL(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v);

    if (v1 instanceof Data_FingerTree.NilL) {
      return Data_Maybe.Nothing.value;
    }

    ;

    if (v1 instanceof Data_FingerTree.ConsL) {
      return new Data_Maybe.Just(new Data_Tuple.Tuple(Data_Sequence_Internal.getElem(v1.value0), Data_Lazy.force(v1.value1)));
    }

    ;
    throw new Error("Failed pattern match at Data.Sequence (line 218, column 3 - line 220, column 65): " + [v1.constructor.name]);
  };

  var snoc = function snoc(v) {
    return function (x) {
      return Data_FingerTree.snoc(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v)(x);
    };
  };

  var map = function map(f) {
    return function (v) {
      return Data_Functor.map(Data_FingerTree.functorFingerTree)(f)(v);
    };
  };

  var functorSeq = new Data_Functor.Functor(map);
  var foldableSeq = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return function (v) {
        return Data_Foldable.foldMap(Data_FingerTree.foldableFingerTree)(dictMonoid)(Data_Sequence_Internal.liftElem(f))(v);
      };
    };
  }, function (f) {
    return function (z) {
      return function (v) {
        return Data_Foldable.foldl(Data_FingerTree.foldableFingerTree)(Data_Sequence_Internal.lift2Elem(f))(z)(v);
      };
    };
  }, function (f) {
    return function (z) {
      return function (v) {
        return Data_Foldable.foldr(Data_FingerTree.foldableFingerTree)(Data_Sequence_Internal.liftElem(f))(z)(v);
      };
    };
  });
  var empty = Data_FingerTree.Empty.value;

  var cons = function cons(x) {
    return function (v) {
      return Data_FingerTree.cons(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(x)(v);
    };
  };

  var append = function append(v) {
    return function (v1) {
      return Data_FingerTree.append(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v)(v1);
    };
  };

  var semigroupSeq = new Data_Semigroup.Semigroup(append);
  exports["empty"] = empty;
  exports["cons"] = cons;
  exports["snoc"] = snoc;
  exports["uncons"] = uncons;
  exports["semigroupSeq"] = semigroupSeq;
  exports["foldableSeq"] = foldableSeq;
  exports["functorSeq"] = functorSeq;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Sequence.NonEmpty"] = $PS["Data.Sequence.NonEmpty"] || {};
  var exports = $PS["Data.Sequence.NonEmpty"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Semigroup_Foldable = $PS["Data.Semigroup.Foldable"];
  var Data_Sequence = $PS["Data.Sequence"];

  var Seq = function () {
    function Seq(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Seq.create = function (value0) {
      return function (value1) {
        return new Seq(value0, value1);
      };
    };

    return Seq;
  }();

  var toPlain = function toPlain(v) {
    return Data_Sequence.cons(v.value0)(v.value1);
  };

  var tail = function tail(v) {
    return v.value1;
  };

  var snoc = function snoc(v) {
    return function (y) {
      return new Seq(v.value0, Data_Sequence.snoc(v.value1)(y));
    };
  };

  var singleton = function singleton(x) {
    return new Seq(x, Data_Sequence.empty);
  };

  var semigroupSeq = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
      return new Seq(v.value0, Data_Semigroup.append(Data_Sequence.semigroupSeq)(Data_Sequence.snoc(v.value1)(v1.value0))(v1.value1));
    };
  });
  var functorSeq = new Data_Functor.Functor(function (f) {
    return function (v) {
      return new Seq(f(v.value0), Data_Functor.map(Data_Sequence.functorSeq)(f)(v.value1));
    };
  });

  var fromFoldable1 = function fromFoldable1(dictFoldable1) {
    return Data_Semigroup_Foldable.foldMap1(dictFoldable1)(semigroupSeq)(singleton);
  };

  var foldableSeq = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      var $157 = Data_Foldable.foldMap(Data_Sequence.foldableSeq)(dictMonoid)(f);
      return function ($158) {
        return $157(toPlain($158));
      };
    };
  }, function (f) {
    return function (z) {
      var $159 = Data_Foldable.foldl(Data_Sequence.foldableSeq)(f)(z);
      return function ($160) {
        return $159(toPlain($160));
      };
    };
  }, function (f) {
    return function (z) {
      var $161 = Data_Foldable.foldr(Data_Sequence.foldableSeq)(f)(z);
      return function ($162) {
        return $161(toPlain($162));
      };
    };
  });

  var cons = function cons(x) {
    return function (v) {
      return new Seq(x, Data_Sequence.cons(v.value0)(v.value1));
    };
  };

  var append = function append(v) {
    return function (v1) {
      return new Seq(v.value0, Data_Semigroup.append(Data_Sequence.semigroupSeq)(Data_Sequence.snoc(v.value1)(v1.value0))(v1.value1));
    };
  };

  exports["Seq"] = Seq;
  exports["singleton"] = singleton;
  exports["fromFoldable1"] = fromFoldable1;
  exports["tail"] = tail;
  exports["functorSeq"] = functorSeq;
  exports["foldableSeq"] = foldableSeq;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Map.Internal"] = $PS["Data.Map.Internal"] || {};
  var exports = $PS["Data.Map.Internal"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Semigroup = $PS["Data.Semigroup"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];

  var Leaf = function () {
    function Leaf() {}

    ;
    Leaf.value = new Leaf();
    return Leaf;
  }();

  var Two = function () {
    function Two(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Two.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Two(value0, value1, value2, value3);
          };
        };
      };
    };

    return Two;
  }();

  var Three = function () {
    function Three(value0, value1, value2, value3, value4, value5, value6) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
    }

    ;

    Three.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return function (value6) {
                  return new Three(value0, value1, value2, value3, value4, value5, value6);
                };
              };
            };
          };
        };
      };
    };

    return Three;
  }();

  var TwoLeft = function () {
    function TwoLeft(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    TwoLeft.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new TwoLeft(value0, value1, value2);
        };
      };
    };

    return TwoLeft;
  }();

  var TwoRight = function () {
    function TwoRight(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    TwoRight.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new TwoRight(value0, value1, value2);
        };
      };
    };

    return TwoRight;
  }();

  var ThreeLeft = function () {
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }

    ;

    ThreeLeft.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return new ThreeLeft(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };

    return ThreeLeft;
  }();

  var ThreeMiddle = function () {
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }

    ;

    ThreeMiddle.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };

    return ThreeMiddle;
  }();

  var ThreeRight = function () {
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }

    ;

    ThreeRight.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return function (value4) {
              return function (value5) {
                return new ThreeRight(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };

    return ThreeRight;
  }();

  var KickUp = function () {
    function KickUp(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    KickUp.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new KickUp(value0, value1, value2, value3);
          };
        };
      };
    };

    return KickUp;
  }();

  var values = function values(v) {
    if (v instanceof Leaf) {
      return Data_List_Types.Nil.value;
    }

    ;

    if (v instanceof Two) {
      return Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value2))(values(v.value3)));
    }

    ;

    if (v instanceof Three) {
      return Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value2))(Data_Semigroup.append(Data_List_Types.semigroupList)(values(v.value3))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value5))(values(v.value6)))));
    }

    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 612, column 1 - line 612, column 40): " + [v.constructor.name]);
  };

  var singleton = function singleton(k) {
    return function (v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };

  var toUnfoldable = function toUnfoldable(dictUnfoldable) {
    return function (m) {
      var go = function go($copy_v) {
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(v) {
          if (v instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return Data_Maybe.Nothing.value;
          }

          ;

          if (v instanceof Data_List_Types.Cons) {
            if (v.value0 instanceof Leaf) {
              $copy_v = v.value1;
              return;
            }

            ;

            if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf) {
              $tco_done = true;
              return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), v.value1));
            }

            ;

            if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
              $tco_done = true;
              return Data_Maybe.Just.create(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
            }

            ;

            if (v.value0 instanceof Two) {
              $copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
              return;
            }

            ;

            if (v.value0 instanceof Three) {
              $copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, new Data_List_Types.Cons(singleton(v.value0.value4)(v.value0.value5), new Data_List_Types.Cons(v.value0.value6, v.value1)))));
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 577, column 18 - line 586, column 71): " + [v.value0.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 576, column 3 - line 576, column 19): " + [v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }

        ;
        return $tco_result;
      };

      return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_List_Types.Cons(m, Data_List_Types.Nil.value));
    };
  };

  var lookup = function lookup(dictOrd) {
    return function (k) {
      var comp = Data_Ord.compare(dictOrd);

      var go = function go($copy_v) {
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Data_Maybe.Nothing.value;
          }

          ;

          if (v instanceof Two) {
            var v2 = comp(k)(v.value1);

            if (v2 instanceof Data_Ordering.EQ) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value2);
            }

            ;

            if (v2 instanceof Data_Ordering.LT) {
              $copy_v = v.value0;
              return;
            }

            ;
            $copy_v = v.value3;
            return;
          }

          ;

          if (v instanceof Three) {
            var v3 = comp(k)(v.value1);

            if (v3 instanceof Data_Ordering.EQ) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value2);
            }

            ;
            var v4 = comp(k)(v.value4);

            if (v4 instanceof Data_Ordering.EQ) {
              $tco_done = true;
              return new Data_Maybe.Just(v.value5);
            }

            ;

            if (v3 instanceof Data_Ordering.LT) {
              $copy_v = v.value0;
              return;
            }

            ;

            if (v4 instanceof Data_Ordering.GT) {
              $copy_v = v.value6;
              return;
            }

            ;
            $copy_v = v.value3;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 200, column 5 - line 200, column 22): " + [v.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }

        ;
        return $tco_result;
      };

      return go;
    };
  };

  var member = function member(dictOrd) {
    return function (k) {
      return function (m) {
        return Data_Maybe.isJust(lookup(dictOrd)(k)(m));
      };
    };
  };

  var keys = function keys(v) {
    if (v instanceof Leaf) {
      return Data_List_Types.Nil.value;
    }

    ;

    if (v instanceof Two) {
      return Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value1))(keys(v.value3)));
    }

    ;

    if (v instanceof Three) {
      return Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value1))(Data_Semigroup.append(Data_List_Types.semigroupList)(keys(v.value3))(Data_Semigroup.append(Data_List_Types.semigroupList)(Control_Applicative.pure(Data_List_Types.applicativeList)(v.value4))(keys(v.value6)))));
    }

    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 606, column 1 - line 606, column 38): " + [v.constructor.name]);
  };

  var functorMap = new Data_Functor.Functor(function (v) {
    return function (v1) {
      if (v1 instanceof Leaf) {
        return Leaf.value;
      }

      ;

      if (v1 instanceof Two) {
        return new Two(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3));
      }

      ;

      if (v1 instanceof Three) {
        return new Three(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), Data_Functor.map(functorMap)(v)(v1.value6));
      }

      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 96, column 1 - line 99, column 110): " + [v.constructor.name, v1.constructor.name]);
    };
  });

  var fromZipper = function fromZipper($copy_dictOrd) {
    return function ($copy_v) {
      return function ($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Data_List_Types.Nil) {
            $tco_done = true;
            return tree;
          }

          ;

          if (v instanceof Data_List_Types.Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }

            ;

            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }

            ;

            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }

            ;

            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }

            ;

            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 418, column 3 - line 423, column 88): " + [v.value0.constructor.name]);
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 415, column 1 - line 415, column 80): " + [v.constructor.name, tree.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }

        ;
        return $tco_result;
      };
    };
  };

  var insert = function insert(dictOrd) {
    return function (k) {
      return function (v) {
        var up = function up($copy_v1) {
          return function ($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;

            function $tco_loop(v1, v2) {
              if (v1 instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }

              ;

              if (v1 instanceof Data_List_Types.Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }

                ;

                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }

                ;

                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }

                ;

                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }

                ;

                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }

                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 454, column 5 - line 459, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }

              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 451, column 3 - line 451, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }

            ;

            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }

            ;
            return $tco_result;
          };
        };

        var comp = Data_Ord.compare(dictOrd);

        var down = function down($copy_ctx) {
          return function ($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done = false;
            var $tco_result;

            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }

              ;

              if (v1 instanceof Two) {
                var v2 = comp(k)(v1.value1);

                if (v2 instanceof Data_Ordering.EQ) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                }

                ;

                if (v2 instanceof Data_Ordering.LT) {
                  $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }

                ;
                $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }

              ;

              if (v1 instanceof Three) {
                var v3 = comp(k)(v1.value1);

                if (v3 instanceof Data_Ordering.EQ) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }

                ;
                var v4 = comp(k)(v1.value4);

                if (v4 instanceof Data_Ordering.EQ) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }

                ;

                if (v3 instanceof Data_Ordering.LT) {
                  $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }

                ;

                if (v3 instanceof Data_Ordering.GT && v4 instanceof Data_Ordering.LT) {
                  $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }

                ;
                $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }

              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 434, column 3 - line 434, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }

            ;

            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }

            ;
            return $tco_result;
          };
        };

        return down(Data_List_Types.Nil.value);
      };
    };
  };

  var pop = function pop(dictOrd) {
    return function (k) {
      var up = function up($copy_ctxs) {
        return function ($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Data_List_Types.Nil) {
              $tco_done = true;
              return tree;
            }

            ;

            if (ctxs instanceof Data_List_Types.Cons) {
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }

              ;

              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }

              ;

              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }

              ;

              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }

              ;

              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Leaf && ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Leaf && ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value0 instanceof Leaf && ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }

              ;

              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }

              ;

              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }

              ;

              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }

              ;

              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }

              ;

              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }

              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 511, column 9 - line 528, column 136): " + [ctxs.value0.constructor.name, tree.constructor.name]);
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 508, column 5 - line 528, column 136): " + [ctxs.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }

          ;
          return $tco_result;
        };
      };

      var removeMaxNode = function removeMaxNode($copy_ctx) {
        return function ($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(ctx, m) {
            if (m instanceof Two && m.value0 instanceof Leaf && m.value3 instanceof Leaf) {
              $tco_done = true;
              return up(ctx)(Leaf.value);
            }

            ;

            if (m instanceof Two) {
              $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }

            ;

            if (m instanceof Three && m.value0 instanceof Leaf && m.value3 instanceof Leaf && m.value6 instanceof Leaf) {
              $tco_done = true;
              return up(new Data_List_Types.Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }

            ;

            if (m instanceof Three) {
              $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 540, column 5 - line 544, column 107): " + [m.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }

          ;
          return $tco_result;
        };
      };

      var maxNode = function maxNode($copy_m) {
        var $tco_done = false;
        var $tco_result;

        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }

          ;

          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }

          ;

          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }

          ;

          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 531, column 33 - line 535, column 45): " + [m.constructor.name]);
        }

        ;

        while (!$tco_done) {
          $tco_result = $tco_loop($copy_m);
        }

        ;
        return $tco_result;
      };

      var comp = Data_Ord.compare(dictOrd);

      var down = function down($copy_ctx) {
        return function ($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done = false;
          var $tco_result;

          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done = true;
              return Data_Maybe.Nothing.value;
            }

            ;

            if (m instanceof Two) {
              var v = comp(k)(m.value1);

              if (m.value3 instanceof Leaf && v instanceof Data_Ordering.EQ) {
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, up(ctx)(Leaf.value)));
              }

              ;

              if (v instanceof Data_Ordering.EQ) {
                var max = maxNode(m.value0);
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new TwoLeft(max.key, max.value, m.value3), ctx))(m.value0)));
              }

              ;

              if (v instanceof Data_Ordering.LT) {
                $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }

              ;
              $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }

            ;

            if (m instanceof Three) {
              var leaves = function () {
                if (m.value0 instanceof Leaf && m.value3 instanceof Leaf && m.value6 instanceof Leaf) {
                  return true;
                }

                ;
                return false;
              }();

              var v = comp(k)(m.value4);
              var v3 = comp(k)(m.value1);

              if (leaves && v3 instanceof Data_Ordering.EQ) {
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }

              ;

              if (leaves && v instanceof Data_Ordering.EQ) {
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }

              ;

              if (v3 instanceof Data_Ordering.EQ) {
                var max = maxNode(m.value0);
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new ThreeLeft(max.key, max.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }

              ;

              if (v instanceof Data_Ordering.EQ) {
                var max = maxNode(m.value3);
                $tco_done = true;
                return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, removeMaxNode(new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max.key, max.value, m.value6), ctx))(m.value3)));
              }

              ;

              if (v3 instanceof Data_Ordering.LT) {
                $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }

              ;

              if (v3 instanceof Data_Ordering.GT && v instanceof Data_Ordering.LT) {
                $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }

              ;
              $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }

            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 481, column 34 - line 504, column 80): " + [m.constructor.name]);
          }

          ;

          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }

          ;
          return $tco_result;
        };
      };

      return down(Data_List_Types.Nil.value);
    };
  };

  var foldableMap = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      return function (m) {
        return Data_Foldable.foldMap(Data_List_Types.foldableList)(dictMonoid)(f)(values(m));
      };
    };
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldl(Data_List_Types.foldableList)(f)(z)(values(m));
      };
    };
  }, function (f) {
    return function (z) {
      return function (m) {
        return Data_Foldable.foldr(Data_List_Types.foldableList)(f)(z)(values(m));
      };
    };
  });
  var empty = Leaf.value;

  var $$delete = function $$delete(dictOrd) {
    return function (k) {
      return function (m) {
        return Data_Maybe.maybe(m)(Data_Tuple.snd)(pop(dictOrd)(k)(m));
      };
    };
  };

  var alter = function alter(dictOrd) {
    return function (f) {
      return function (k) {
        return function (m) {
          var v = f(lookup(dictOrd)(k)(m));

          if (v instanceof Data_Maybe.Nothing) {
            return $$delete(dictOrd)(k)(m);
          }

          ;

          if (v instanceof Data_Maybe.Just) {
            return insert(dictOrd)(k)(v.value0)(m);
          }

          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 549, column 15 - line 551, column 25): " + [v.constructor.name]);
        };
      };
    };
  };

  var unionWith = function unionWith(dictOrd) {
    return function (f) {
      return function (m1) {
        return function (m2) {
          var go = function go(m) {
            return function (v) {
              return alter(dictOrd)(function () {
                var $769 = Data_Maybe.maybe(v.value1)(f(v.value1));
                return function ($770) {
                  return Data_Maybe.Just.create($769($770));
                };
              }())(v.value0)(m);
            };
          };

          return Data_Foldable.foldl(Data_List_Types.foldableList)(go)(m2)(toUnfoldable(Data_List_Types.unfoldableList)(m1));
        };
      };
    };
  };

  var union = function union(dictOrd) {
    return unionWith(dictOrd)(Data_Function["const"]);
  };

  exports["empty"] = empty;
  exports["insert"] = insert;
  exports["lookup"] = lookup;
  exports["delete"] = $$delete;
  exports["pop"] = pop;
  exports["member"] = member;
  exports["alter"] = alter;
  exports["keys"] = keys;
  exports["union"] = union;
  exports["functorMap"] = functorMap;
  exports["foldableMap"] = foldableMap;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Set"] = $PS["Data.Set"] || {};
  var exports = $PS["Data.Set"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];
  var Data_Unit = $PS["Data.Unit"];

  var union = function union(dictOrd) {
    return function (v) {
      return function (v1) {
        return Data_Map_Internal.union(dictOrd)(v)(v1);
      };
    };
  };

  var toList = function toList(v) {
    return Data_Map_Internal.keys(v);
  };

  var showSet = function showSet(dictShow) {
    return new Data_Show.Show(function (s) {
      return "(fromFoldable " + (Data_Show.show(Data_List_Types.showList(dictShow))(toList(s)) + ")");
    });
  };

  var member = function member(dictOrd) {
    return function (a) {
      return function (v) {
        return Data_Map_Internal.member(dictOrd)(a)(v);
      };
    };
  };

  var insert = function insert(dictOrd) {
    return function (a) {
      return function (v) {
        return Data_Map_Internal.insert(dictOrd)(a)(Data_Unit.unit)(v);
      };
    };
  };

  var foldableSet = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
      var $65 = Data_Foldable.foldMap(Data_List_Types.foldableList)(dictMonoid)(f);
      return function ($66) {
        return $65(toList($66));
      };
    };
  }, function (f) {
    return function (x) {
      var $67 = Data_Foldable.foldl(Data_List_Types.foldableList)(f)(x);
      return function ($68) {
        return $67(toList($68));
      };
    };
  }, function (f) {
    return function (x) {
      var $69 = Data_Foldable.foldr(Data_List_Types.foldableList)(f)(x);
      return function ($70) {
        return $69(toList($70));
      };
    };
  });
  var empty = Data_Map_Internal.empty;

  var fromFoldable = function fromFoldable(dictFoldable) {
    return function (dictOrd) {
      return Data_Foldable.foldl(dictFoldable)(function (m) {
        return function (a) {
          return insert(dictOrd)(a)(m);
        };
      })(empty);
    };
  };

  var mapMaybe = function mapMaybe(dictOrd) {
    return function (f) {
      return Data_Foldable.foldr(foldableSet)(function (a) {
        return function (acc) {
          return Data_Maybe.maybe(acc)(function (b) {
            return insert(dictOrd)(b)(acc);
          })(f(a));
        };
      })(empty);
    };
  };

  var $$delete = function $$delete(dictOrd) {
    return function (a) {
      return function (v) {
        return Data_Map_Internal["delete"](dictOrd)(a)(v);
      };
    };
  };

  exports["fromFoldable"] = fromFoldable;
  exports["empty"] = empty;
  exports["member"] = member;
  exports["delete"] = $$delete;
  exports["union"] = union;
  exports["mapMaybe"] = mapMaybe;
  exports["showSet"] = showSet;
  exports["foldableSet"] = foldableSet;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Const"] = $PS["Data.Const"] || {};
  var exports = $PS["Data.Const"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Semigroup = $PS["Data.Semigroup"];

  var Const = function Const(x) {
    return x;
  };

  var newtypeConst = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Const);
  var functorConst = new Data_Functor.Functor(function (f) {
    return function (m) {
      return m;
    };
  });

  var applyConst = function applyConst(dictSemigroup) {
    return new Control_Apply.Apply(function () {
      return functorConst;
    }, function (v) {
      return function (v1) {
        return Data_Semigroup.append(dictSemigroup)(v)(v1);
      };
    });
  };

  var applicativeConst = function applicativeConst(dictMonoid) {
    return new Control_Applicative.Applicative(function () {
      return applyConst(dictMonoid.Semigroup0());
    }, function (v) {
      return Data_Monoid.mempty(dictMonoid);
    });
  };

  exports["Const"] = Const;
  exports["newtypeConst"] = newtypeConst;
  exports["applicativeConst"] = applicativeConst;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.Ap"] = $PS["Type.Ap"] || {};
  var exports = $PS["Type.Ap"];

  var withAp = function withAp(v) {
    return v;
  };

  var mkAp = function mkAp(x) {
    return function (y) {
      return function (f) {
        return f(x)(y);
      };
    };
  };

  exports["mkAp"] = mkAp;
  exports["withAp"] = withAp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.Chain"] = $PS["Type.Chain"] || {};
  var exports = $PS["Type.Chain"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Const = $PS["Data.Const"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Show = $PS["Data.Show"];
  var Type_Ap = $PS["Type.Ap"];
  var Type_Equiv = $PS["Type.Equiv"];
  var Type_GCompare = $PS["Type.GCompare"];

  var Nil = function () {
    function Nil(value0) {
      this.value0 = value0;
    }

    ;

    Nil.create = function (value0) {
      return new Nil(value0);
    };

    return Nil;
  }();

  var Cons = function () {
    function Cons(value0) {
      this.value0 = value0;
    }

    ;

    Cons.create = function (value0) {
      return new Cons(value0);
    };

    return Cons;
  }();

  var runChain = function runChain(f) {
    var go = function go(v) {
      if (v instanceof Nil) {
        return Type_Equiv.equivToF(v.value0);
      }

      ;

      if (v instanceof Cons) {
        return function (x) {
          return Type_Ap.withAp(v.value0)(function (y) {
            return function (c) {
              return go(c)(f(y)(x));
            };
          });
        };
      }

      ;
      throw new Error("Failed pattern match at Type.Chain (line 44, column 10 - line 46, column 56): " + [v.constructor.name]);
    };

    return go;
  };

  var nil = new Nil(Type_Equiv.refl);

  var cons = function cons(x) {
    return function (xs) {
      return new Cons(Type_Ap.mkAp(x)(xs));
    };
  };

  var hoistChainA = function hoistChainA(dictApplicative) {
    return function (f) {
      var go = function go(v) {
        if (v instanceof Nil) {
          return Control_Applicative.pure(dictApplicative)(new Nil(v.value0));
        }

        ;

        if (v instanceof Cons) {
          return Type_Ap.withAp(v.value0)(function (x) {
            return function (xs) {
              return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map(dictApplicative.Apply0().Functor0())(cons)(f(x)))(go(xs));
            };
          });
        }

        ;
        throw new Error("Failed pattern match at Type.Chain (line 68, column 10 - line 70, column 61): " + [v.constructor.name]);
      };

      return go;
    };
  };

  var foldMapChain = function foldMapChain(dictMonoid) {
    return function (f) {
      var $42 = Data_Newtype.unwrap(Data_Const.newtypeConst);
      var $43 = hoistChainA(Data_Const.applicativeConst(dictMonoid))(function ($45) {
        return Data_Const.Const(f($45));
      });
      return function ($44) {
        return $42($43($44));
      };
    };
  };

  var gshow2Chain = function gshow2Chain(dictGShow2) {
    return new Type_GCompare.GShow2(function () {
      var brack = function brack(x) {
        return "[" + (x + "]");
      };

      var $46 = Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(", ");
      var $47 = foldMapChain(Data_Monoid.monoidArray)(function () {
        var $49 = Type_GCompare.gshow2(dictGShow2);
        return function ($50) {
          return Data_Array.singleton($49($50));
        };
      }());
      return function ($48) {
        return brack($46($47($48)));
      };
    }());
  };

  var gshowChain = function gshowChain(dictGShow2) {
    return new Type_GCompare.GShow(Type_GCompare.gshow2(gshow2Chain(dictGShow2)));
  };

  var showChain = function showChain(dictGShow2) {
    return new Data_Show.Show(Type_GCompare.gshow(gshowChain(dictGShow2)));
  };

  var singleton = function singleton(x) {
    return cons(x)(nil);
  };

  var snoc = function snoc(v) {
    if (v instanceof Nil) {
      return function (x) {
        return cons(Type_Equiv.equivFromF2(v.value0)(x))(nil);
      };
    }

    ;

    if (v instanceof Cons) {
      return function (y) {
        return Type_Ap.withAp(v.value0)(function (x) {
          return function (xs) {
            return cons(x)(snoc(xs)(y));
          };
        });
      };
    }

    ;
    throw new Error("Failed pattern match at Type.Chain (line 31, column 8 - line 33, column 61): " + [v.constructor.name]);
  };

  var splitAt = function splitAt(n) {
    return function (xs) {
      if (n <= 0) {
        return Type_Ap.mkAp(nil)(xs);
      }

      ;

      if (Data_Boolean.otherwise) {
        if (xs instanceof Nil) {
          return Type_Equiv.equivToF(xs.value0)(Type_Ap.mkAp(nil)(nil));
        }

        ;

        if (xs instanceof Cons) {
          return Type_Ap.withAp(xs.value0)(function (x) {
            return function (xs2) {
              return Type_Ap.withAp(splitAt(n - 1 | 0)(xs2))(function (ys) {
                return function (zs) {
                  return Type_Ap.mkAp(cons(x)(ys))(zs);
                };
              });
            };
          });
        }

        ;
        throw new Error("Failed pattern match at Type.Chain (line 119, column 19 - line 125, column 10): " + [xs.constructor.name]);
      }

      ;
      throw new Error("Failed pattern match at Type.Chain (line 112, column 1 - line 116, column 34): " + [n.constructor.name, xs.constructor.name]);
    };
  };

  var unsnoc = function () {
    var go = function go(x) {
      return function (v) {
        if (v instanceof Nil) {
          return Type_Ap.mkAp(nil)(Type_Equiv.equivToF(v.value0)(x));
        }

        ;

        if (v instanceof Cons) {
          return Type_Ap.withAp(v.value0)(function (y) {
            return function (ys) {
              return Type_Ap.withAp(go(y)(ys))(function (zs) {
                return function (z) {
                  return Type_Ap.mkAp(cons(x)(zs))(z);
                };
              });
            };
          });
        }

        ;
        throw new Error("Failed pattern match at Type.Chain (line 104, column 12 - line 110, column 10): " + [v.constructor.name]);
      };
    };

    return function (v) {
      if (v instanceof Nil) {
        return new Data_Either.Left(v.value0);
      }

      ;

      if (v instanceof Cons) {
        return Type_Ap.withAp(v.value0)(function (xs) {
          return function (x) {
            return new Data_Either.Right(go(xs)(x));
          };
        });
      }

      ;
      throw new Error("Failed pattern match at Type.Chain (line 98, column 10 - line 100, column 52): " + [v.constructor.name]);
    };
  }();

  exports["Nil"] = Nil;
  exports["Cons"] = Cons;
  exports["nil"] = nil;
  exports["snoc"] = snoc;
  exports["runChain"] = runChain;
  exports["hoistChainA"] = hoistChainA;
  exports["foldMapChain"] = foldMapChain;
  exports["unsnoc"] = unsnoc;
  exports["splitAt"] = splitAt;
  exports["showChain"] = showChain;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Type.DSum"] = $PS["Type.DSum"] || {};
  var exports = $PS["Type.DSum"];
  var Data_Show = $PS["Data.Show"];
  var Type_GCompare = $PS["Type.GCompare"];

  var withDSum = function withDSum(v) {
    return v;
  };

  var showDSum = function showDSum(dictGShow) {
    return function (dictGShow1) {
      return new Data_Show.Show(function (ds) {
        return withDSum(ds)(function (x) {
          return function (y) {
            return Type_GCompare.gshow(dictGShow)(x) + (" :=> " + Type_GCompare.gshow(dictGShow1)(y));
          };
        });
      });
    };
  };

  var dsum = function dsum(k) {
    return function (v) {
      return function (f) {
        return f(k)(v);
      };
    };
  };

  exports["dsum"] = dsum;
  exports["withDSum"] = withDSum;
  exports["showDSum"] = showDSum;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Corona.Chart"] = $PS["Corona.Chart"] || {};
  var exports = $PS["Corona.Chart"];
  var $foreign = $PS["Corona.Chart"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var D3_Scatter_Type = $PS["D3.Scatter.Type"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Dated = $PS["Data.Dated"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Int = $PS["Data.Int"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_ModifiedJulianDay = $PS["Data.ModifiedJulianDay"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_NonEmpty = $PS["Data.NonEmpty"];
  var Data_Number = $PS["Data.Number"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ord_Max = $PS["Data.Ord.Max"];
  var Data_Ordering = $PS["Data.Ordering"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Sequence = $PS["Data.Sequence"];
  var Data_Sequence_NonEmpty = $PS["Data.Sequence.NonEmpty"];
  var Data_Set = $PS["Data.Set"];
  var Data_Show = $PS["Data.Show"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Unfoldable = $PS["Data.Unfoldable"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Type_Chain = $PS["Type.Chain"];
  var Type_DSum = $PS["Type.DSum"];
  var Type_Equiv = $PS["Type.Equiv"];
  var Type_GCompare = $PS["Type.GCompare"];

  var I2N = function () {
    function I2N(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    I2N.create = function (value0) {
      return function (value1) {
        return new I2N(value0, value1);
      };
    };

    return I2N;
  }();

  var N2N = function () {
    function N2N(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    N2N.create = function (value0) {
      return function (value1) {
        return new N2N(value0, value1);
      };
    };

    return N2N;
  }();

  var P2P = function () {
    function P2P(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    P2P.create = function (value0) {
      return function (value1) {
        return new P2P(value0, value1);
      };
    };

    return P2P;
  }();

  var After = function () {
    function After() {}

    ;
    After.value = new After();
    return After;
  }();

  var Before = function () {
    function Before() {}

    ;
    Before.value = new Before();
    return Before;
  }();

  var AtLeast = function () {
    function AtLeast(value0) {
      this.value0 = value0;
    }

    ;

    AtLeast.create = function (value0) {
      return new AtLeast(value0);
    };

    return AtLeast;
  }();

  var AtMost = function () {
    function AtMost(value0) {
      this.value0 = value0;
    }

    ;

    AtMost.create = function (value0) {
      return new AtMost(value0);
    };

    return AtMost;
  }();

  var Delta = function () {
    function Delta(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Delta.create = function (value0) {
      return function (value1) {
        return new Delta(value0, value1);
      };
    };

    return Delta;
  }();

  var PGrowth = function () {
    function PGrowth(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    PGrowth.create = function (value0) {
      return function (value1) {
        return new PGrowth(value0, value1);
      };
    };

    return PGrowth;
  }();

  var Window = function () {
    function Window(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Window.create = function (value0) {
      return function (value1) {
        return new Window(value0, value1);
      };
    };

    return Window;
  }();

  var PMax = function () {
    function PMax(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    PMax.create = function (value0) {
      return function (value1) {
        return new PMax(value0, value1);
      };
    };

    return PMax;
  }();

  var Restrict = function () {
    function Restrict(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Restrict.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Restrict(value0, value1, value2, value3);
          };
        };
      };
    };

    return Restrict;
  }();

  var Take = function () {
    function Take(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    Take.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new Take(value0, value1, value2);
        };
      };
    };

    return Take;
  }();

  var DayNumber = function () {
    function DayNumber(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    DayNumber.create = function (value0) {
      return function (value1) {
        return new DayNumber(value0, value1);
      };
    };

    return DayNumber;
  }();

  var PointDate = function () {
    function PointDate(value0) {
      this.value0 = value0;
    }

    ;

    PointDate.create = function (value0) {
      return new PointDate(value0);
    };

    return PointDate;
  }();

  var Time = function () {
    function Time(value0) {
      this.value0 = value0;
    }

    ;

    Time.create = function (value0) {
      return new Time(value0);
    };

    return Time;
  }();

  var Confirmed = function () {
    function Confirmed(value0) {
      this.value0 = value0;
    }

    ;

    Confirmed.create = function (value0) {
      return new Confirmed(value0);
    };

    return Confirmed;
  }();

  var Deaths = function () {
    function Deaths(value0) {
      this.value0 = value0;
    }

    ;

    Deaths.create = function (value0) {
      return new Deaths(value0);
    };

    return Deaths;
  }();

  var Recovered = function () {
    function Recovered(value0) {
      this.value0 = value0;
    }

    ;

    Recovered.create = function (value0) {
      return new Recovered(value0);
    };

    return Recovered;
  }();

  var withProjection = function withProjection(v) {
    return v;
  };

  var withConsec = function withConsec(f) {
    return function (xs) {
      var xs$prime = Data_List.fromFoldable(Data_Foldable.foldableArray)(xs);
      return Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_List.zipWith(f)(xs$prime)(Data_List.drop(1)(xs$prime)));
    };
  };

  var windows = function windows(n) {
    var go = function go(xs) {
      return function (x) {
        var v = Data_Sequence.uncons(Data_Sequence_NonEmpty.tail(xs));

        if (v instanceof Data_Maybe.Nothing) {
          return Data_Sequence_NonEmpty.singleton(x);
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return new Data_Sequence_NonEmpty.Seq(v.value0.value0, Data_Sequence.snoc(v.value0.value1)(x));
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart (line 294, column 15 - line 296, column 55): " + [v.constructor.name]);
      };
    };

    return function (v) {
      if (v instanceof Data_List_Types.Cons) {
        var zs = Data_List.drop(2 * n | 0)(v.value1);
        var ys = Data_Sequence_NonEmpty.fromFoldable1(Data_NonEmpty.foldable1NonEmpty(Data_List_Types.foldableList))(new Data_NonEmpty.NonEmpty(v.value0, Data_List.take(2 * n | 0)(v.value1)));
        return new Data_List_Types.Cons(ys, Data_Traversable.scanl(Data_List_Types.traversableList)(go)(ys)(zs));
      }

      ;

      if (v instanceof Data_List_Types.Nil) {
        return Data_List_Types.Nil.value;
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 286, column 13 - line 291, column 19): " + [v.constructor.name]);
    };
  };

  var withWindow = function withWindow(n) {
    return function (f) {
      var $279 = Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray);
      var $280 = Data_Functor.map(Data_List_Types.functorList)(f);
      var $281 = windows(n);
      var $282 = Data_List.fromFoldable(Data_Foldable.foldableArray);
      return function ($283) {
        return $279($280($281($282($283))));
      };
    };
  };

  var toFractionalOut = function toFractionalOut(v) {
    if (v instanceof I2N) {
      return new D3_Scatter_Type.NNumber(v.value1);
    }

    ;

    if (v instanceof N2N) {
      return new D3_Scatter_Type.NNumber(v.value1);
    }

    ;

    if (v instanceof P2P) {
      return new D3_Scatter_Type.NPercent(v.value1);
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 167, column 19 - line 170, column 26): " + [v.constructor.name]);
  };

  var toFractionalIn = function toFractionalIn(v) {
    if (v instanceof I2N) {
      return new D3_Scatter_Type.NInt(v.value0);
    }

    ;

    if (v instanceof N2N) {
      return new D3_Scatter_Type.NNumber(v.value0);
    }

    ;

    if (v instanceof P2P) {
      return new D3_Scatter_Type.NPercent(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 162, column 18 - line 165, column 26): " + [v.constructor.name]);
  };

  var showCutoffType = new Data_Show.Show(function (v) {
    if (v instanceof After) {
      return "After";
    }

    ;

    if (v instanceof Before) {
      return "Before";
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 208, column 12 - line 210, column 25): " + [v.constructor.name]);
  });

  var projection = function projection(x) {
    return function (f) {
      return f(x);
    };
  };

  var percentGrowth = function percentGrowth(x) {
    return function (y) {
      if (x === 0) {
        return Data_Number.nan;
      }

      ;

      if (Data_Boolean.otherwise) {
        return (y - x) / x;
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 252, column 1 - line 255, column 15): " + [x.constructor.name, y.constructor.name]);
    };
  };

  var gshowCondition = function gshowCondition(t) {
    return function (v) {
      if (v instanceof AtLeast) {
        return "AtLeast " + D3_Scatter_Type.sTypeShow(t)(v.value0);
      }

      ;

      if (v instanceof AtMost) {
        return "AtMost " + D3_Scatter_Type.sTypeShow(t)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 194, column 20 - line 196, column 47): " + [v.constructor.name]);
    };
  };

  var gshowBase = new Type_GCompare.GShow(function (v) {
    if (v instanceof Time) {
      return "Time";
    }

    ;

    if (v instanceof Confirmed) {
      return "Confirmed";
    }

    ;

    if (v instanceof Deaths) {
      return "Deaths";
    }

    ;

    if (v instanceof Recovered) {
      return "Recovered";
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 74, column 13 - line 78, column 33): " + [v.constructor.name]);
  });
  var showBase = new Data_Show.Show(Type_GCompare.gshow(gshowBase));
  var gshow2Operation = new Type_GCompare.GShow2(function (v) {
    if (v instanceof Delta) {
      return "Delta";
    }

    ;

    if (v instanceof PGrowth) {
      return "PGrowth";
    }

    ;

    if (v instanceof Window) {
      return "Window " + Data_Show.show(Data_Show.showInt)(v.value1);
    }

    ;

    if (v instanceof PMax) {
      return "PMax";
    }

    ;

    if (v instanceof Restrict) {
      return "Restrict " + (Data_Show.show(showCutoffType)(v.value2) + (" (" + (gshowCondition(v.value0)(v.value3) + ")")));
    }

    ;

    if (v instanceof Take) {
      return "Take " + (Data_Show.show(Data_Show.showInt)(v.value1) + (" " + Data_Show.show(showCutoffType)(v.value2)));
    }

    ;

    if (v instanceof DayNumber) {
      return "DayNumber " + Data_Show.show(showCutoffType)(v.value1);
    }

    ;

    if (v instanceof PointDate) {
      return "PointDate";
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 225, column 14 - line 233, column 33): " + [v.constructor.name]);
  });
  var gshowProjection = new Type_GCompare.GShow(function (spr) {
    return withProjection(spr)(function (pr) {
      return "{" + (Data_Show.show(showBase)(pr.base) + ("}: " + Data_Show.show(Type_Chain.showChain(gshow2Operation))(pr.operations)));
    });
  });
  var functorCondition = new Data_Functor.Functor(function (f) {
    return function (v) {
      if (v instanceof AtLeast) {
        return new AtLeast(f(v.value0));
      }

      ;

      if (v instanceof AtMost) {
        return new AtMost(f(v.value0));
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 184, column 13 - line 186, column 32): " + [v.constructor.name]);
    };
  });
  var eqCutoffType = new Data_Eq.Eq(function (x) {
    return function (y) {
      if (x instanceof After && y instanceof After) {
        return true;
      }

      ;

      if (x instanceof Before && y instanceof Before) {
        return true;
      }

      ;
      return false;
    };
  });

  var eqCondition = function eqCondition(dictEq) {
    return new Data_Eq.Eq(function (x) {
      return function (y) {
        if (x instanceof AtLeast && y instanceof AtLeast) {
          return Data_Eq.eq(dictEq)(x.value0)(y.value0);
        }

        ;

        if (x instanceof AtMost && y instanceof AtMost) {
          return Data_Eq.eq(dictEq)(x.value0)(y.value0);
        }

        ;
        return false;
      };
    });
  };

  var decideBaseProjection = new Type_Equiv.Decide(function (v) {
    if (v instanceof Time) {
      return function (v1) {
        if (v1 instanceof Time) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof Confirmed) {
      return function (v1) {
        if (v1 instanceof Confirmed) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof Deaths) {
      return function (v1) {
        if (v1 instanceof Deaths) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;

    if (v instanceof Recovered) {
      return function (v1) {
        if (v1 instanceof Recovered) {
          return new Data_Maybe.Just(Type_Equiv.equivFromF(v1.value0)(v.value0));
        }

        ;
        return Data_Maybe.Nothing.value;
      };
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 92, column 14 - line 104, column 27): " + [v.constructor.name]);
  });
  var geqBaseProjection = new Type_GCompare.GEq(function () {
    return decideBaseProjection;
  }, Type_Equiv.decide(decideBaseProjection));

  var conditionValue = function conditionValue(v) {
    if (v instanceof AtLeast) {
      return v.value0;
    }

    ;

    if (v instanceof AtMost) {
      return v.value0;
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 199, column 18 - line 201, column 19): " + [v.constructor.name]);
  };

  var conditionLabel = function conditionLabel(t) {
    return function (v) {
      if (v instanceof AtLeast) {
        return "at least " + D3_Scatter_Type.sTypeShow(t)(v.value0);
      }

      ;

      if (v instanceof AtMost) {
        return "at most " + D3_Scatter_Type.sTypeShow(t)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 416, column 20 - line 418, column 45): " + [v.constructor.name]);
    };
  };

  var operationLabel = function operationLabel(v) {
    if (v instanceof Delta) {
      return "Daily Change in";
    }

    ;

    if (v instanceof PGrowth) {
      return "Daily Percent Growth in";
    }

    ;

    if (v instanceof Window) {
      return "Moving Average (+/-" + (Data_Show.show(Data_Show.showInt)(v.value1) + ") of");
    }

    ;

    if (v instanceof PMax) {
      return "Percent of maximum in";
    }

    ;

    if (v instanceof Restrict) {
      return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(["(With only points ", function () {
        if (v.value2 instanceof After) {
          return "after";
        }

        ;

        if (v.value2 instanceof Before) {
          return "before";
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart (line 397, column 11 - line 399, column 31): " + [v.value2.constructor.name]);
      }(), " being ", conditionLabel(v.value0)(v.value3), ")"]);
    }

    ;

    if (v instanceof Take) {
      var cStr = function () {
        if (v.value2 instanceof After) {
          return "last";
        }

        ;

        if (v.value2 instanceof Before) {
          return "first";
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart (line 405, column 18 - line 407, column 30): " + [v.value2.constructor.name]);
      }();

      return "Keeping the " + (cStr + (" " + (Data_Show.show(Data_Show.showInt)(v.value1) + " points of")));
    }

    ;

    if (v instanceof DayNumber) {
      if (v.value1 instanceof After) {
        return "Number of days since initial";
      }

      ;

      if (v.value1 instanceof Before) {
        return "Number of days until final";
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 409, column 22 - line 411, column 45): " + [v.value1.constructor.name]);
    }

    ;

    if (v instanceof PointDate) {
      return "Observed date for value of";
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 390, column 18 - line 412, column 48): " + [v.constructor.name]);
  };

  var operationsLabel = function operationsLabel(c) {
    var go = function go(o) {
      return function (v) {
        return operationLabel(o) + (" " + v);
      };
    };

    var v = Type_Chain.runChain(go)(c)("");
    return v;
  };

  var baseType = function baseType(v) {
    if (v instanceof Time) {
      return new D3_Scatter_Type.SDay(v.value0);
    }

    ;

    if (v instanceof Confirmed) {
      return new D3_Scatter_Type.SInt(v.value0);
    }

    ;

    if (v instanceof Deaths) {
      return new D3_Scatter_Type.SInt(v.value0);
    }

    ;

    if (v instanceof Recovered) {
      return new D3_Scatter_Type.SInt(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 65, column 12 - line 69, column 32): " + [v.constructor.name]);
  };

  var setBase = function setBase(base) {
    return function (dp) {
      var tA = baseType(base);
      return Type_DSum.withDSum(dp)(function (tB) {
        return function (spr) {
          return withProjection(spr)(function (pr) {
            var tC = baseType(pr.base);
            var v = Type_Equiv.decide(D3_Scatter_Type.decideSType)(tA)(tC);

            if (v instanceof Data_Maybe.Just) {
              return Type_DSum.dsum(tB)(projection({
                base: Type_Equiv.equivToF(v.value0)(base),
                operations: pr.operations
              }));
            }

            ;

            if (v instanceof Data_Maybe.Nothing) {
              return Type_DSum.dsum(tA)(projection({
                base: base,
                operations: Type_Chain.nil
              }));
            }

            ;
            throw new Error("Failed pattern match at Corona.Chart (line 471, column 13 - line 479, column 18): " + [v.constructor.name]);
          });
        };
      });
    };
  };

  var baseProjectionLabel = function baseProjectionLabel(v) {
    if (v instanceof Time) {
      return "Date";
    }

    ;

    if (v instanceof Confirmed) {
      return "Confirmed Cases";
    }

    ;

    if (v instanceof Deaths) {
      return "Deaths";
    }

    ;

    if (v instanceof Recovered) {
      return "Recovered Cases";
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 383, column 23 - line 387, column 37): " + [v.constructor.name]);
  };

  var projectionLabel = function projectionLabel(spr) {
    return withProjection(spr)(function (pr) {
      return operationsLabel(pr.operations) + baseProjectionLabel(pr.base);
    });
  };

  var toAxisConf = function toAxisConf(spr) {
    return function (scl) {
      return {
        scale: scl,
        label: projectionLabel(spr)
      };
    };
  };

  var baseProjection = function baseProjection(pr) {
    return withProjection(pr)(function ($284) {
      return Data_Exists.mkExists(function (v) {
        return v.base;
      }($284));
    });
  };

  var bTime = new Time(Type_Equiv.refl);
  var bRecovered = new Recovered(Type_Equiv.refl);
  var bDeaths = new Deaths(Type_Equiv.refl);
  var bConfirmed = new Confirmed(Type_Equiv.refl);

  var applyCondition = function applyCondition(t) {
    return function (v) {
      if (v instanceof AtLeast) {
        return function (x) {
          return Data_Eq.eq(Data_Ordering.eqOrdering)(D3_Scatter_Type.sTypeCompare(t)(x)(v.value0))(Data_Ordering.GT.value);
        };
      }

      ;

      if (v instanceof AtMost) {
        return function (x) {
          return Data_Eq.eq(Data_Ordering.eqOrdering)(D3_Scatter_Type.sTypeCompare(t)(x)(v.value0))(Data_Ordering.LT.value);
        };
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart (line 354, column 20 - line 356, column 48): " + [v.constructor.name]);
    };
  };

  var applyOperation = function applyOperation(v) {
    if (v instanceof Delta) {
      return function (v1) {
        return {
          start: Data_ModifiedJulianDay.addDays(-1 | 0)(v1.start),
          values: withConsec(function (y0) {
            return function (y1) {
              return Type_Equiv.equivTo(v.value1)(D3_Scatter_Type.nTypeSubtract(v.value0)(y1)(y0));
            };
          })(v1.values)
        };
      };
    }

    ;

    if (v instanceof PGrowth) {
      return function (v1) {
        return {
          start: Data_ModifiedJulianDay.addDays(-1 | 0)(v1.start),
          values: withConsec(function (y0) {
            return function (y1) {
              return Type_Equiv.equivFrom(v.value1)(percentGrowth(D3_Scatter_Type.nTypeNumber(v.value0)(y0))(D3_Scatter_Type.nTypeNumber(v.value0)(y1)));
            };
          })(v1.values)
        };
      };
    }

    ;

    if (v instanceof Window) {
      return function (v1) {
        return {
          start: Data_ModifiedJulianDay.addDays(-v.value1 | 0)(v1.start),
          values: Data_Function.flip(withWindow(v.value1))(v1.values)(function (ys) {
            return D3_Scatter_Type.numberNType(toFractionalOut(v.value0))(Data_Foldable.sum(Data_Sequence_NonEmpty.foldableSeq)(Data_Semiring.semiringNumber)(Data_Functor.map(Data_Sequence_NonEmpty.functorSeq)(D3_Scatter_Type.nTypeNumber(toFractionalIn(v.value0)))(ys)) / Data_Int.toNumber((v.value1 * 2 | 0) + 1 | 0));
          })
        };
      };
    }

    ;

    if (v instanceof PMax) {
      return function (xs) {
        var ys = Data_Functor.map(Data_Dated.functorDated)(D3_Scatter_Type.nTypeNumber(v.value0))(xs);
        var maxAbs = Data_Maybe.maybe(Data_Int.toNumber(1))(Data_Newtype.unwrap(Data_Ord_Max.newtypeMax))(Data_Function.flip(Data_Foldable.foldMap(Data_Dated.foldedableDated)(Data_Maybe.monoidMaybe(Data_Ord_Max.semigroupMax(Data_Ord.ordNumber))))(ys)(function (y) {
          var $253 = y === 0;

          if ($253) {
            return Data_Maybe.Nothing.value;
          }

          ;
          return new Data_Maybe.Just(Data_Ord.abs(Data_Ord.ordNumber)(Data_Ring.ringNumber)(y));
        }));
        return Type_Equiv.equivFromF(v.value1)(Data_Functor.map(Data_Dated.functorDated)(function ($285) {
          return D3_Scatter_Type.Percent(function (v1) {
            return v1 / maxAbs;
          }($285));
        })(ys));
      };
    }

    ;

    if (v instanceof Restrict) {
      var dropper = function () {
        if (v.value2 instanceof After) {
          return Data_Dated.dropUntil;
        }

        ;

        if (v.value2 instanceof Before) {
          return Data_Dated.dropUntilEnd;
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart (line 334, column 21 - line 336, column 37): " + [v.value2.constructor.name]);
      }();

      var $286 = Type_Equiv.equivToF(v.value1);
      var $287 = dropper(applyCondition(v.value0)(v.value3));
      return function ($288) {
        return $286($287($288));
      };
    }

    ;

    if (v instanceof Take) {
      var dropper = function () {
        if (v.value2 instanceof After) {
          return Data_Dated.take;
        }

        ;

        if (v.value2 instanceof Before) {
          return Data_Dated.takeEnd;
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart (line 339, column 21 - line 341, column 32): " + [v.value2.constructor.name]);
      }();

      var $289 = Type_Equiv.equivToF(v.value0);
      var $290 = dropper(v.value1);
      return function ($291) {
        return $289($290($291));
      };
    }

    ;

    if (v instanceof DayNumber) {
      return function (v1) {
        var emptyDated = {
          start: v1.start,
          values: []
        };
        return Data_Maybe.maybe(emptyDated)(Type_Equiv.equivFromF(v.value0))(function () {
          if (v.value1 instanceof After) {
            return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Dated.findHead(v1))(function (v2) {
              return Data_FunctorWithIndex.mapWithIndex(Data_Dated.functorWithIndexDated)(function (i) {
                return function (v3) {
                  return D3_Scatter_Type.Days(Data_ModifiedJulianDay.diffDays(i)(v2.day));
                };
              })(v1);
            });
          }

          ;

          if (v.value1 instanceof Before) {
            return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Dated.findLast(v1))(function (v2) {
              return Data_FunctorWithIndex.mapWithIndex(Data_Dated.functorWithIndexDated)(function (i) {
                return function (v3) {
                  return D3_Scatter_Type.Days(Data_ModifiedJulianDay.diffDays(i)(v2.day));
                };
              })(v1);
            });
          }

          ;
          throw new Error("Failed pattern match at Corona.Chart (line 346, column 13 - line 350, column 76): " + [v.value1.constructor.name]);
        }());
      };
    }

    ;

    if (v instanceof PointDate) {
      var $292 = Type_Equiv.equivFromF(v.value0);
      var $293 = Data_FunctorWithIndex.mapWithIndex(Data_Dated.functorWithIndexDated)(Data_Function["const"]);
      return function ($294) {
        return $292($293($294));
      };
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 303, column 18 - line 351, column 59): " + [v.constructor.name]);
  };

  var applyBaseProjection = function applyBaseProjection(v) {
    if (v instanceof Time) {
      return Data_FunctorWithIndex.mapWithIndex(Data_Dated.functorWithIndexDated)(function (i) {
        return function (v1) {
          return Type_Equiv.equivFrom(v.value0)(i);
        };
      });
    }

    ;

    if (v instanceof Confirmed) {
      return Data_Functor.map(Data_Dated.functorDated)(function (c) {
        return Type_Equiv.equivFrom(v.value0)(c.confirmed);
      });
    }

    ;

    if (v instanceof Deaths) {
      return Data_Functor.map(Data_Dated.functorDated)(function (c) {
        return Type_Equiv.equivFrom(v.value0)(c.deaths);
      });
    }

    ;

    if (v instanceof Recovered) {
      return Data_Functor.map(Data_Dated.functorDated)(function (c) {
        return Type_Equiv.equivFrom(v.value0)(c.recovered);
      });
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart (line 363, column 23 - line 367, column 61): " + [v.constructor.name]);
  };

  var applyProjection = function applyProjection(spr) {
    return function (allDat) {
      return withProjection(spr)(function (pr) {
        var $295 = Type_Chain.runChain(applyOperation)(pr.operations);
        var $296 = applyBaseProjection(pr.base);
        return function ($297) {
          return $295($296($297));
        };
      })(allDat);
    };
  };

  var toSeries = function toSeries(pX) {
    return function (pY) {
      return function (ps) {
        return Data_Dated.datedValues($foreign.spy("xy")(Control_Apply.lift2(Data_Dated.applyDated)(function (x) {
          return function (y) {
            return {
              x: x,
              y: y
            };
          };
        })($foreign.spy("x")(applyProjection(pX)(ps)))($foreign.spy("y")(applyProjection(pY)(ps)))));
      };
    };
  };

  var toScatterPlot = function toScatterPlot(dat) {
    return function (pX) {
      return function (sX) {
        return function (pY) {
          return function (sY) {
            return function (ctrys) {
              return {
                xAxis: toAxisConf(pX)(sX),
                yAxis: toAxisConf(pY)(sY),
                series: Data_Function.flip(Data_Array.mapMaybe)(Data_Array.fromFoldable(Data_Set.foldableSet)(ctrys))(function (ctry) {
                  return Control_Bind.bind(Data_Maybe.bindMaybe)(Foreign_Object.lookup(ctry)(dat.counts))(function (cdat) {
                    return Control_Applicative.pure(Data_Maybe.applicativeMaybe)({
                      name: ctry,
                      values: toSeries(pX)(pY)({
                        start: dat.start,
                        values: cdat
                      })
                    });
                  });
                })
              };
            };
          };
        };
      };
    };
  };

  var allBaseProjections = [Data_Exists.mkExists(new Time(Type_Equiv.refl)), Data_Exists.mkExists(new Confirmed(Type_Equiv.refl)), Data_Exists.mkExists(new Deaths(Type_Equiv.refl)), Data_Exists.mkExists(new Recovered(Type_Equiv.refl))];
  exports["Time"] = Time;
  exports["Confirmed"] = Confirmed;
  exports["baseType"] = baseType;
  exports["bTime"] = bTime;
  exports["bConfirmed"] = bConfirmed;
  exports["bDeaths"] = bDeaths;
  exports["bRecovered"] = bRecovered;
  exports["allBaseProjections"] = allBaseProjections;
  exports["withProjection"] = withProjection;
  exports["projection"] = projection;
  exports["I2N"] = I2N;
  exports["N2N"] = N2N;
  exports["P2P"] = P2P;
  exports["AtLeast"] = AtLeast;
  exports["AtMost"] = AtMost;
  exports["conditionValue"] = conditionValue;
  exports["After"] = After;
  exports["Before"] = Before;
  exports["Delta"] = Delta;
  exports["PGrowth"] = PGrowth;
  exports["Window"] = Window;
  exports["PMax"] = PMax;
  exports["Restrict"] = Restrict;
  exports["Take"] = Take;
  exports["DayNumber"] = DayNumber;
  exports["PointDate"] = PointDate;
  exports["baseProjection"] = baseProjection;
  exports["baseProjectionLabel"] = baseProjectionLabel;
  exports["projectionLabel"] = projectionLabel;
  exports["toScatterPlot"] = toScatterPlot;
  exports["setBase"] = setBase;
  exports["geqBaseProjection"] = geqBaseProjection;
  exports["gshowProjection"] = gshowProjection;
  exports["eqCondition"] = eqCondition;
  exports["functorCondition"] = functorCondition;
  exports["eqCutoffType"] = eqCutoffType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["DOM.HTML.Indexed.InputType"] = $PS["DOM.HTML.Indexed.InputType"] || {};
  var exports = $PS["DOM.HTML.Indexed.InputType"];

  var InputButton = function () {
    function InputButton() {}

    ;
    InputButton.value = new InputButton();
    return InputButton;
  }();

  var InputCheckbox = function () {
    function InputCheckbox() {}

    ;
    InputCheckbox.value = new InputCheckbox();
    return InputCheckbox;
  }();

  var InputColor = function () {
    function InputColor() {}

    ;
    InputColor.value = new InputColor();
    return InputColor;
  }();

  var InputDate = function () {
    function InputDate() {}

    ;
    InputDate.value = new InputDate();
    return InputDate;
  }();

  var InputDatetimeLocal = function () {
    function InputDatetimeLocal() {}

    ;
    InputDatetimeLocal.value = new InputDatetimeLocal();
    return InputDatetimeLocal;
  }();

  var InputEmail = function () {
    function InputEmail() {}

    ;
    InputEmail.value = new InputEmail();
    return InputEmail;
  }();

  var InputFile = function () {
    function InputFile() {}

    ;
    InputFile.value = new InputFile();
    return InputFile;
  }();

  var InputHidden = function () {
    function InputHidden() {}

    ;
    InputHidden.value = new InputHidden();
    return InputHidden;
  }();

  var InputImage = function () {
    function InputImage() {}

    ;
    InputImage.value = new InputImage();
    return InputImage;
  }();

  var InputMonth = function () {
    function InputMonth() {}

    ;
    InputMonth.value = new InputMonth();
    return InputMonth;
  }();

  var InputNumber = function () {
    function InputNumber() {}

    ;
    InputNumber.value = new InputNumber();
    return InputNumber;
  }();

  var InputPassword = function () {
    function InputPassword() {}

    ;
    InputPassword.value = new InputPassword();
    return InputPassword;
  }();

  var InputRadio = function () {
    function InputRadio() {}

    ;
    InputRadio.value = new InputRadio();
    return InputRadio;
  }();

  var InputRange = function () {
    function InputRange() {}

    ;
    InputRange.value = new InputRange();
    return InputRange;
  }();

  var InputReset = function () {
    function InputReset() {}

    ;
    InputReset.value = new InputReset();
    return InputReset;
  }();

  var InputSearch = function () {
    function InputSearch() {}

    ;
    InputSearch.value = new InputSearch();
    return InputSearch;
  }();

  var InputSubmit = function () {
    function InputSubmit() {}

    ;
    InputSubmit.value = new InputSubmit();
    return InputSubmit;
  }();

  var InputTel = function () {
    function InputTel() {}

    ;
    InputTel.value = new InputTel();
    return InputTel;
  }();

  var InputText = function () {
    function InputText() {}

    ;
    InputText.value = new InputText();
    return InputText;
  }();

  var InputTime = function () {
    function InputTime() {}

    ;
    InputTime.value = new InputTime();
    return InputTime;
  }();

  var InputUrl = function () {
    function InputUrl() {}

    ;
    InputUrl.value = new InputUrl();
    return InputUrl;
  }();

  var InputWeek = function () {
    function InputWeek() {}

    ;
    InputWeek.value = new InputWeek();
    return InputWeek;
  }();

  var renderInputType = function renderInputType(v) {
    if (v instanceof InputButton) {
      return "button";
    }

    ;

    if (v instanceof InputCheckbox) {
      return "checkbox";
    }

    ;

    if (v instanceof InputColor) {
      return "color";
    }

    ;

    if (v instanceof InputDate) {
      return "date";
    }

    ;

    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }

    ;

    if (v instanceof InputEmail) {
      return "email";
    }

    ;

    if (v instanceof InputFile) {
      return "file";
    }

    ;

    if (v instanceof InputHidden) {
      return "hidden";
    }

    ;

    if (v instanceof InputImage) {
      return "image";
    }

    ;

    if (v instanceof InputMonth) {
      return "month";
    }

    ;

    if (v instanceof InputNumber) {
      return "number";
    }

    ;

    if (v instanceof InputPassword) {
      return "password";
    }

    ;

    if (v instanceof InputRadio) {
      return "radio";
    }

    ;

    if (v instanceof InputRange) {
      return "range";
    }

    ;

    if (v instanceof InputReset) {
      return "reset";
    }

    ;

    if (v instanceof InputSearch) {
      return "search";
    }

    ;

    if (v instanceof InputSubmit) {
      return "submit";
    }

    ;

    if (v instanceof InputTel) {
      return "tel";
    }

    ;

    if (v instanceof InputText) {
      return "text";
    }

    ;

    if (v instanceof InputTime) {
      return "time";
    }

    ;

    if (v instanceof InputUrl) {
      return "url";
    }

    ;

    if (v instanceof InputWeek) {
      return "week";
    }

    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 28, column 19 - line 50, column 22): " + [v.constructor.name]);
  };

  exports["InputDate"] = InputDate;
  exports["InputNumber"] = InputNumber;
  exports["InputText"] = InputText;
  exports["renderInputType"] = renderInputType;
})(PS);

(function (exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };

  exports.warn = function (s) {
    return function () {
      console.warn(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  exports["log"] = $foreign.log;
  exports["warn"] = $foreign.warn;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Class.Console"] = $PS["Effect.Class.Console"] || {};
  var exports = $PS["Effect.Class.Console"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Console = $PS["Effect.Console"];

  var log = function log(dictMonadEffect) {
    var $30 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($31) {
      return $30(Effect_Console.log($31));
    };
  };

  exports["log"] = log;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Coyoneda"] = $PS["Data.Coyoneda"] || {};
  var exports = $PS["Data.Coyoneda"];
  var Control_Category = $PS["Control.Category"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Functor = $PS["Data.Functor"];

  var CoyonedaF = function () {
    function CoyonedaF(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    CoyonedaF.create = function (value0) {
      return function (value1) {
        return new CoyonedaF(value0, value1);
      };
    };

    return CoyonedaF;
  }();

  var Coyoneda = function Coyoneda(x) {
    return x;
  };

  var unCoyoneda = function unCoyoneda(f) {
    return function (v) {
      return Data_Exists.runExists(function (v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };

  var coyoneda = function coyoneda(k) {
    return function (fi) {
      return Coyoneda(Data_Exists.mkExists(new CoyonedaF(k, fi)));
    };
  };

  var functorCoyoneda = new Data_Functor.Functor(function (f) {
    return function (v) {
      return Data_Exists.runExists(function (v1) {
        return coyoneda(function ($85) {
          return f(v1.value0($85));
        })(v1.value1);
      })(v);
    };
  });

  var hoistCoyoneda = function hoistCoyoneda(nat) {
    return function (v) {
      return Data_Exists.runExists(function (v1) {
        return coyoneda(v1.value0)(nat(v1.value1));
      })(v);
    };
  };

  var liftCoyoneda = coyoneda(Control_Category.identity(Control_Category.categoryFn));
  exports["unCoyoneda"] = unCoyoneda;
  exports["liftCoyoneda"] = liftCoyoneda;
  exports["hoistCoyoneda"] = hoistCoyoneda;
  exports["functorCoyoneda"] = functorCoyoneda;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Data.OrdBox"] = $PS["Halogen.Data.OrdBox"] || {};
  var exports = $PS["Halogen.Data.OrdBox"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Ord = $PS["Data.Ord"];

  var OrdBox = function () {
    function OrdBox(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    OrdBox.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new OrdBox(value0, value1, value2);
        };
      };
    };

    return OrdBox;
  }();

  var mkOrdBox = function mkOrdBox(dictOrd) {
    return OrdBox.create(Data_Eq.eq(dictOrd.Eq0()))(Data_Ord.compare(dictOrd));
  };

  var eqOrdBox = new Data_Eq.Eq(function (v) {
    return function (v1) {
      return v.value0(v.value2)(v1.value2);
    };
  });
  var ordOrdBox = new Data_Ord.Ord(function () {
    return eqOrdBox;
  }, function (v) {
    return function (v1) {
      return v.value1(v.value2)(v1.value2);
    };
  });
  exports["mkOrdBox"] = mkOrdBox;
  exports["ordOrdBox"] = ordOrdBox;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Data.Slot"] = $PS["Halogen.Data.Slot"] || {};
  var exports = $PS["Halogen.Data.Slot"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Halogen_Data_OrdBox = $PS["Halogen.Data.OrdBox"];

  var pop = function pop(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (sym) {
          return function (key) {
            return function (v) {
              return Data_Map_Internal.pop(Data_Tuple.ordTuple(Data_Ord.ordString)(Halogen_Data_OrdBox.ordOrdBox))(new Data_Tuple.Tuple(Data_Symbol.reflectSymbol(dictIsSymbol)(sym), Halogen_Data_OrdBox.mkOrdBox(dictOrd)(key)))(v);
            };
          };
        };
      };
    };
  };

  var lookup = function lookup(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (sym) {
          return function (key) {
            return function (v) {
              return Data_Map_Internal.lookup(Data_Tuple.ordTuple(Data_Ord.ordString)(Halogen_Data_OrdBox.ordOrdBox))(new Data_Tuple.Tuple(Data_Symbol.reflectSymbol(dictIsSymbol)(sym), Halogen_Data_OrdBox.mkOrdBox(dictOrd)(key)))(v);
            };
          };
        };
      };
    };
  };

  var insert = function insert(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (sym) {
          return function (key) {
            return function (val) {
              return function (v) {
                return Data_Map_Internal.insert(Data_Tuple.ordTuple(Data_Ord.ordString)(Halogen_Data_OrdBox.ordOrdBox))(new Data_Tuple.Tuple(Data_Symbol.reflectSymbol(dictIsSymbol)(sym), Halogen_Data_OrdBox.mkOrdBox(dictOrd)(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };

  var foreachSlot = function foreachSlot(dictApplicative) {
    return function (v) {
      return function (k) {
        return Data_Foldable.traverse_(dictApplicative)(Data_Map_Internal.foldableMap)(function ($37) {
          return k($37);
        })(v);
      };
    };
  };

  var empty = Data_Map_Internal.empty;
  exports["empty"] = empty;
  exports["lookup"] = lookup;
  exports["insert"] = insert;
  exports["pop"] = pop;
  exports["foreachSlot"] = foreachSlot;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Query.ChildQuery"] = $PS["Halogen.Query.ChildQuery"] || {};
  var exports = $PS["Halogen.Query.ChildQuery"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var ChildQuery = function () {
    function ChildQuery(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    ChildQuery.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new ChildQuery(value0, value1, value2);
        };
      };
    };

    return ChildQuery;
  }();

  var unChildQueryBox = Unsafe_Coerce.unsafeCoerce;
  var mkChildQueryBox = Unsafe_Coerce.unsafeCoerce;
  exports["ChildQuery"] = ChildQuery;
  exports["mkChildQueryBox"] = mkChildQueryBox;
  exports["unChildQueryBox"] = unChildQueryBox;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Query.HalogenM"] = $PS["Halogen.Query.HalogenM"] || {};
  var exports = $PS["Halogen.Query.HalogenM"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Applicative_Free = $PS["Control.Applicative.Free"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Free = $PS["Control.Monad.Free"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Class = $PS["Effect.Class"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Halogen_Query_ChildQuery = $PS["Halogen.Query.ChildQuery"];

  var SubscriptionId = function SubscriptionId(x) {
    return x;
  };

  var ForkId = function ForkId(x) {
    return x;
  };

  var State = function () {
    function State(value0) {
      this.value0 = value0;
    }

    ;

    State.create = function (value0) {
      return new State(value0);
    };

    return State;
  }();

  var Subscribe = function () {
    function Subscribe(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Subscribe.create = function (value0) {
      return function (value1) {
        return new Subscribe(value0, value1);
      };
    };

    return Subscribe;
  }();

  var Unsubscribe = function () {
    function Unsubscribe(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Unsubscribe.create = function (value0) {
      return function (value1) {
        return new Unsubscribe(value0, value1);
      };
    };

    return Unsubscribe;
  }();

  var Lift = function () {
    function Lift(value0) {
      this.value0 = value0;
    }

    ;

    Lift.create = function (value0) {
      return new Lift(value0);
    };

    return Lift;
  }();

  var ChildQuery = function () {
    function ChildQuery(value0) {
      this.value0 = value0;
    }

    ;

    ChildQuery.create = function (value0) {
      return new ChildQuery(value0);
    };

    return ChildQuery;
  }();

  var Raise = function () {
    function Raise(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Raise.create = function (value0) {
      return function (value1) {
        return new Raise(value0, value1);
      };
    };

    return Raise;
  }();

  var Par = function () {
    function Par(value0) {
      this.value0 = value0;
    }

    ;

    Par.create = function (value0) {
      return new Par(value0);
    };

    return Par;
  }();

  var Fork = function () {
    function Fork(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Fork.create = function (value0) {
      return function (value1) {
        return new Fork(value0, value1);
      };
    };

    return Fork;
  }();

  var Kill = function () {
    function Kill(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Kill.create = function (value0) {
      return function (value1) {
        return new Kill(value0, value1);
      };
    };

    return Kill;
  }();

  var GetRef = function () {
    function GetRef(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    GetRef.create = function (value0) {
      return function (value1) {
        return new GetRef(value0, value1);
      };
    };

    return GetRef;
  }();

  var HalogenAp = function HalogenAp(x) {
    return x;
  };

  var HalogenM = function HalogenM(x) {
    return x;
  };

  var raise = function raise(o) {
    return HalogenM(Control_Monad_Free.liftF(new Raise(o, Data_Unit.unit)));
  };

  var query = function query(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (label) {
          return function (p) {
            return function (q) {
              return HalogenM(Control_Monad_Free.liftF(ChildQuery.create(Halogen_Query_ChildQuery.mkChildQueryBox(new Halogen_Query_ChildQuery.ChildQuery(function (dictApplicative) {
                return function (k) {
                  var $135 = Data_Maybe.maybe(Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value))(k);
                  var $136 = Halogen_Data_Slot.lookup()(dictIsSymbol)(dictOrd)(label)(p);
                  return function ($137) {
                    return $135($136($137));
                  };
                };
              }, q, Control_Category.identity(Control_Category.categoryFn))))));
            };
          };
        };
      };
    };
  };

  var ordSubscriptionId = Data_Ord.ordInt;
  var ordForkId = Data_Ord.ordInt;
  var newtypeHalogenAp = new Data_Newtype.Newtype(function (n) {
    return n;
  }, HalogenAp);
  var monadHalogenM = Control_Monad_Free.freeMonad;
  var monadStateHalogenM = new Control_Monad_State_Class.MonadState(function () {
    return monadHalogenM;
  }, function ($139) {
    return HalogenM(Control_Monad_Free.liftF(State.create($139)));
  });

  var monadEffectHalogenM = function monadEffectHalogenM(dictMonadEffect) {
    return new Effect_Class.MonadEffect(function () {
      return monadHalogenM;
    }, function () {
      var $144 = Effect_Class.liftEffect(dictMonadEffect);
      return function ($145) {
        return HalogenM(Control_Monad_Free.liftF(Lift.create($144($145))));
      };
    }());
  };

  var mapOutput = function mapOutput(f) {
    return function (v) {
      var go = function go(v1) {
        if (v1 instanceof State) {
          return new State(v1.value0);
        }

        ;

        if (v1 instanceof Subscribe) {
          return new Subscribe(v1.value0, v1.value1);
        }

        ;

        if (v1 instanceof Unsubscribe) {
          return new Unsubscribe(v1.value0, v1.value1);
        }

        ;

        if (v1 instanceof Lift) {
          return new Lift(v1.value0);
        }

        ;

        if (v1 instanceof ChildQuery) {
          return new ChildQuery(v1.value0);
        }

        ;

        if (v1 instanceof Raise) {
          return new Raise(f(v1.value0), v1.value1);
        }

        ;

        if (v1 instanceof Par) {
          return new Par(Data_Newtype.over(newtypeHalogenAp)(newtypeHalogenAp)(HalogenAp)(Control_Applicative_Free.hoistFreeAp(mapOutput(f)))(v1.value0));
        }

        ;

        if (v1 instanceof Fork) {
          return new Fork(mapOutput(f)(v1.value0), v1.value1);
        }

        ;

        if (v1 instanceof Kill) {
          return new Kill(v1.value0, v1.value1);
        }

        ;

        if (v1 instanceof GetRef) {
          return new GetRef(v1.value0, v1.value1);
        }

        ;
        throw new Error("Failed pattern match at Halogen.Query.HalogenM (line 260, column 8 - line 270, column 29): " + [v1.constructor.name]);
      };

      return Control_Monad_Free.hoistFree(go)(v);
    };
  };

  var getRef = function getRef(p) {
    return HalogenM(Control_Monad_Free.liftF(new GetRef(p, Control_Category.identity(Control_Category.categoryFn))));
  };

  var functorHalogenM = Control_Monad_Free.freeFunctor;
  var bindHalogenM = Control_Monad_Free.freeBind;
  var applicativeHalogenM = Control_Monad_Free.freeApplicative;
  exports["State"] = State;
  exports["Subscribe"] = Subscribe;
  exports["Unsubscribe"] = Unsubscribe;
  exports["Lift"] = Lift;
  exports["ChildQuery"] = ChildQuery;
  exports["Raise"] = Raise;
  exports["Par"] = Par;
  exports["Fork"] = Fork;
  exports["Kill"] = Kill;
  exports["GetRef"] = GetRef;
  exports["raise"] = raise;
  exports["query"] = query;
  exports["SubscriptionId"] = SubscriptionId;
  exports["ForkId"] = ForkId;
  exports["getRef"] = getRef;
  exports["mapOutput"] = mapOutput;
  exports["functorHalogenM"] = functorHalogenM;
  exports["applicativeHalogenM"] = applicativeHalogenM;
  exports["bindHalogenM"] = bindHalogenM;
  exports["monadHalogenM"] = monadHalogenM;
  exports["monadEffectHalogenM"] = monadEffectHalogenM;
  exports["monadStateHalogenM"] = monadStateHalogenM;
  exports["ordSubscriptionId"] = ordSubscriptionId;
  exports["ordForkId"] = ordForkId;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Query.HalogenQ"] = $PS["Halogen.Query.HalogenQ"] || {};
  var exports = $PS["Halogen.Query.HalogenQ"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Coyoneda = $PS["Data.Coyoneda"];
  var Data_Functor = $PS["Data.Functor"];

  var Initialize = function () {
    function Initialize(value0) {
      this.value0 = value0;
    }

    ;

    Initialize.create = function (value0) {
      return new Initialize(value0);
    };

    return Initialize;
  }();

  var Finalize = function () {
    function Finalize(value0) {
      this.value0 = value0;
    }

    ;

    Finalize.create = function (value0) {
      return new Finalize(value0);
    };

    return Finalize;
  }();

  var Receive = function () {
    function Receive(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Receive.create = function (value0) {
      return function (value1) {
        return new Receive(value0, value1);
      };
    };

    return Receive;
  }();

  var Action = function () {
    function Action(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Action.create = function (value0) {
      return function (value1) {
        return new Action(value0, value1);
      };
    };

    return Action;
  }();

  var Query = function () {
    function Query(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Query.create = function (value0) {
      return function (value1) {
        return new Query(value0, value1);
      };
    };

    return Query;
  }();

  var bifunctorHalogenQ = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return function (v) {
        if (v instanceof Initialize) {
          return new Initialize(g(v.value0));
        }

        ;

        if (v instanceof Finalize) {
          return new Finalize(g(v.value0));
        }

        ;

        if (v instanceof Receive) {
          return new Receive(f(v.value0), g(v.value1));
        }

        ;

        if (v instanceof Action) {
          return new Action(v.value0, g(v.value1));
        }

        ;

        if (v instanceof Query) {
          return new Query(Data_Functor.map(Data_Coyoneda.functorCoyoneda)(g)(v.value0), Data_Functor.map(Data_Functor.functorFn)(g)(v.value1));
        }

        ;
        throw new Error("Failed pattern match at Halogen.Query.HalogenQ (line 16, column 15 - line 21, column 45): " + [v.constructor.name]);
      };
    };
  });
  exports["Initialize"] = Initialize;
  exports["Finalize"] = Finalize;
  exports["Receive"] = Receive;
  exports["Action"] = Action;
  exports["Query"] = Query;
  exports["bifunctorHalogenQ"] = bifunctorHalogenQ;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Component"] = $PS["Halogen.Component"] || {};
  var exports = $PS["Halogen.Component"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Data_Coyoneda = $PS["Data.Coyoneda"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var ComponentSlot = function () {
    function ComponentSlot(value0) {
      this.value0 = value0;
    }

    ;

    ComponentSlot.create = function (value0) {
      return new ComponentSlot(value0);
    };

    return ComponentSlot;
  }();

  var ThunkSlot = function () {
    function ThunkSlot(value0) {
      this.value0 = value0;
    }

    ;

    ThunkSlot.create = function (value0) {
      return new ThunkSlot(value0);
    };

    return ThunkSlot;
  }();

  var unComponentSlot = Unsafe_Coerce.unsafeCoerce;
  var unComponent = Unsafe_Coerce.unsafeCoerce;

  var mkEval = function mkEval(args) {
    return function (v) {
      if (v instanceof Halogen_Query_HalogenQ.Initialize) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(args.handleAction)(args.initialize))(v.value0);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Finalize) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(args.handleAction)(args.finalize))(v.value0);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Receive) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(Data_Foldable.traverse_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(args.handleAction)(args.receive(v.value0)))(v.value1);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Action) {
        return Data_Functor.voidLeft(Halogen_Query_HalogenM.functorHalogenM)(args.handleAction(v.value0))(v.value1);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Query) {
        return Data_Coyoneda.unCoyoneda(function (g) {
          var $28 = Data_Functor.map(Halogen_Query_HalogenM.functorHalogenM)(Data_Maybe.maybe(v.value1(Data_Unit.unit))(g));
          return function ($29) {
            return $28(args.handleQuery($29));
          };
        })(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Halogen.Component (line 172, column 15 - line 182, column 70): " + [v.constructor.name]);
    };
  };

  var mkComponentSlot = Unsafe_Coerce.unsafeCoerce;
  var mkComponent = Unsafe_Coerce.unsafeCoerce;
  var defaultEval = {
    handleAction: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit)),
    handleQuery: Data_Function["const"](Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Maybe.Nothing.value)),
    receive: Data_Function["const"](Data_Maybe.Nothing.value),
    initialize: Data_Maybe.Nothing.value,
    finalize: Data_Maybe.Nothing.value
  };

  var componentSlot = function componentSlot(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (label) {
          return function (p) {
            return function (comp) {
              return function (input) {
                return function (output) {
                  return mkComponentSlot({
                    get: Halogen_Data_Slot.lookup()(dictIsSymbol)(dictOrd)(label)(p),
                    pop: Halogen_Data_Slot.pop()(dictIsSymbol)(dictOrd)(label)(p),
                    set: Halogen_Data_Slot.insert()(dictIsSymbol)(dictOrd)(label)(p),
                    component: comp,
                    input: input,
                    output: output
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  exports["mkComponent"] = mkComponent;
  exports["unComponent"] = unComponent;
  exports["mkEval"] = mkEval;
  exports["defaultEval"] = defaultEval;
  exports["ComponentSlot"] = ComponentSlot;
  exports["ThunkSlot"] = ThunkSlot;
  exports["componentSlot"] = componentSlot;
  exports["unComponentSlot"] = unComponentSlot;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["DOM.HTML.Indexed.ButtonType"] = $PS["DOM.HTML.Indexed.ButtonType"] || {};
  var exports = $PS["DOM.HTML.Indexed.ButtonType"];

  var ButtonButton = function () {
    function ButtonButton() {}

    ;
    ButtonButton.value = new ButtonButton();
    return ButtonButton;
  }();

  var ButtonSubmit = function () {
    function ButtonSubmit() {}

    ;
    ButtonSubmit.value = new ButtonSubmit();
    return ButtonSubmit;
  }();

  var ButtonReset = function () {
    function ButtonReset() {}

    ;
    ButtonReset.value = new ButtonReset();
    return ButtonReset;
  }();

  var renderButtonType = function renderButtonType(v) {
    if (v instanceof ButtonButton) {
      return "button";
    }

    ;

    if (v instanceof ButtonSubmit) {
      return "submit";
    }

    ;

    if (v instanceof ButtonReset) {
      return "reset";
    }

    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.ButtonType (line 9, column 20 - line 12, column 25): " + [v.constructor.name]);
  };

  exports["ButtonButton"] = ButtonButton;
  exports["renderButtonType"] = renderButtonType;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.Machine"] = $PS["Halogen.VDom.Machine"] || {};
  var exports = $PS["Halogen.VDom.Machine"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Step = function () {
    function Step(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Step.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Step(value0, value1, value2, value3);
          };
        };
      };
    };

    return Step;
  }();

  var unStep = Unsafe_Coerce.unsafeCoerce;

  var step = function step(v, a) {
    return v.value2(v.value1, a);
  };

  var mkStep = Unsafe_Coerce.unsafeCoerce;

  var halt = function halt(v) {
    return v.value3(v.value1);
  };

  var extract = unStep(function (v) {
    return v.value0;
  });
  exports["Step"] = Step;
  exports["mkStep"] = mkStep;
  exports["unStep"] = unStep;
  exports["extract"] = extract;
  exports["step"] = step;
  exports["halt"] = halt;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeGetAny = function (key, obj) {
    return obj[key];
  };

  exports.unsafeHasAny = function (key, obj) {
    return obj.hasOwnProperty(key);
  };

  exports.unsafeSetAny = function (key, val, obj) {
    obj[key] = val;
  };

  exports.forE = function (a, f) {
    var b = [];

    for (var i = 0; i < a.length; i++) {
      b.push(f(i, a[i]));
    }

    return b;
  };

  exports.forEachE = function (a, f) {
    for (var i = 0; i < a.length; i++) {
      f(a[i]);
    }
  };

  exports.forInE = function (o, f) {
    var ks = Object.keys(o);

    for (var i = 0; i < ks.length; i++) {
      var k = ks[i];
      f(k, o[k]);
    }
  };

  exports.diffWithIxE = function (a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i = 0;

    while (1) {
      if (i < l1) {
        if (i < l2) {
          a3.push(f1(i, a1[i], a2[i]));
        } else {
          f2(i, a1[i]);
        }
      } else if (i < l2) {
        a3.push(f3(i, a2[i]));
      } else {
        break;
      }

      i++;
    }

    return a3;
  };

  exports.strMapWithIxE = function (as, fk, f) {
    var o = {};

    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);
      o[k] = f(k, i, a);
    }

    return o;
  };

  exports.diffWithKeyAndIxE = function (o1, as, fk, f1, f2, f3) {
    var o2 = {};

    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);

      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i, o1[k], a);
      } else {
        o2[k] = f3(k, i, a);
      }
    }

    for (var k in o1) {
      if (k in o2) {
        continue;
      }

      f2(k, o1[k]);
    }

    return o2;
  };

  exports.refEq = function (a, b) {
    return a === b;
  };

  exports.createTextNode = function (s, doc) {
    return doc.createTextNode(s);
  };

  exports.setTextContent = function (s, n) {
    n.textContent = s;
  };

  exports.createElement = function (ns, name, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name);
    } else {
      return doc.createElement(name);
    }
  };

  exports.insertChildIx = function (i, a, b) {
    var n = b.childNodes.item(i) || null;

    if (n !== a) {
      b.insertBefore(a, n);
    }
  };

  exports.removeChild = function (a, b) {
    if (b && a.parentNode === b) {
      b.removeChild(a);
    }
  };

  exports.parentNode = function (a) {
    return a.parentNode;
  };

  exports.setAttribute = function (ns, attr, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr, val);
    } else {
      el.setAttribute(attr, val);
    }
  };

  exports.removeAttribute = function (ns, attr, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr);
    } else {
      el.removeAttribute(attr);
    }
  };

  exports.hasAttribute = function (ns, attr, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr);
    } else {
      return el.hasAttribute(attr);
    }
  };

  exports.addEventListener = function (ev, listener, el) {
    el.addEventListener(ev, listener, false);
  };

  exports.removeEventListener = function (ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  };

  exports.jsUndefined = void 0;
})(PS["Halogen.VDom.Util"] = PS["Halogen.VDom.Util"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.Util"] = $PS["Halogen.VDom.Util"] || {};
  var exports = $PS["Halogen.VDom.Util"];
  var $foreign = $PS["Halogen.VDom.Util"];
  var Foreign_Object_ST = $PS["Foreign.Object.ST"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var unsafeLookup = $foreign.unsafeGetAny;
  var unsafeFreeze = Unsafe_Coerce.unsafeCoerce;
  var pokeMutMap = $foreign.unsafeSetAny;
  var newMutMap = Foreign_Object_ST["new"];
  exports["newMutMap"] = newMutMap;
  exports["pokeMutMap"] = pokeMutMap;
  exports["unsafeFreeze"] = unsafeFreeze;
  exports["unsafeLookup"] = unsafeLookup;
  exports["unsafeGetAny"] = $foreign.unsafeGetAny;
  exports["unsafeHasAny"] = $foreign.unsafeHasAny;
  exports["unsafeSetAny"] = $foreign.unsafeSetAny;
  exports["forE"] = $foreign.forE;
  exports["forEachE"] = $foreign.forEachE;
  exports["forInE"] = $foreign.forInE;
  exports["diffWithIxE"] = $foreign.diffWithIxE;
  exports["diffWithKeyAndIxE"] = $foreign.diffWithKeyAndIxE;
  exports["strMapWithIxE"] = $foreign.strMapWithIxE;
  exports["refEq"] = $foreign.refEq;
  exports["createTextNode"] = $foreign.createTextNode;
  exports["setTextContent"] = $foreign.setTextContent;
  exports["createElement"] = $foreign.createElement;
  exports["insertChildIx"] = $foreign.insertChildIx;
  exports["removeChild"] = $foreign.removeChild;
  exports["parentNode"] = $foreign.parentNode;
  exports["setAttribute"] = $foreign.setAttribute;
  exports["removeAttribute"] = $foreign.removeAttribute;
  exports["hasAttribute"] = $foreign.hasAttribute;
  exports["addEventListener"] = $foreign.addEventListener;
  exports["removeEventListener"] = $foreign.removeEventListener;
  exports["jsUndefined"] = $foreign.jsUndefined;
})(PS);

(function (exports) {
  "use strict";

  exports.eventListener = function (fn) {
    return function () {
      return function (event) {
        return fn(event)();
      };
    };
  };

  exports.addEventListener = function (type) {
    return function (listener) {
      return function (useCapture) {
        return function (target) {
          return function () {
            return target.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  };

  exports.removeEventListener = function (type) {
    return function (listener) {
      return function (useCapture) {
        return function (target) {
          return function () {
            return target.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  };
})(PS["Web.Event.EventTarget"] = PS["Web.Event.EventTarget"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.Event.EventTarget"] = $PS["Web.Event.EventTarget"] || {};
  var exports = $PS["Web.Event.EventTarget"];
  var $foreign = $PS["Web.Event.EventTarget"];
  exports["eventListener"] = $foreign.eventListener;
  exports["addEventListener"] = $foreign.addEventListener;
  exports["removeEventListener"] = $foreign.removeEventListener;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.DOM.Prop"] = $PS["Halogen.VDom.DOM.Prop"] || {};
  var exports = $PS["Halogen.VDom.DOM.Prop"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Foreign = $PS["Foreign"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Util = $PS["Halogen.VDom.Util"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var Web_Event_EventTarget = $PS["Web.Event.EventTarget"];

  var Created = function () {
    function Created(value0) {
      this.value0 = value0;
    }

    ;

    Created.create = function (value0) {
      return new Created(value0);
    };

    return Created;
  }();

  var Removed = function () {
    function Removed(value0) {
      this.value0 = value0;
    }

    ;

    Removed.create = function (value0) {
      return new Removed(value0);
    };

    return Removed;
  }();

  var Attribute = function () {
    function Attribute(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    Attribute.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new Attribute(value0, value1, value2);
        };
      };
    };

    return Attribute;
  }();

  var Property = function () {
    function Property(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Property.create = function (value0) {
      return function (value1) {
        return new Property(value0, value1);
      };
    };

    return Property;
  }();

  var Handler = function () {
    function Handler(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    Handler.create = function (value0) {
      return function (value1) {
        return new Handler(value0, value1);
      };
    };

    return Handler;
  }();

  var Ref = function () {
    function Ref(value0) {
      this.value0 = value0;
    }

    ;

    Ref.create = function (value0) {
      return new Ref(value0);
    };

    return Ref;
  }();

  var unsafeGetProperty = Halogen_VDom_Util.unsafeGetAny;
  var setProperty = Halogen_VDom_Util.unsafeSetAny;

  var removeProperty = function removeProperty(key, el) {
    var v = Halogen_VDom_Util.hasAttribute(Data_Nullable["null"], key, el);

    if (v) {
      return Halogen_VDom_Util.removeAttribute(Data_Nullable["null"], key, el);
    }

    ;
    var v1 = Foreign.typeOf(Halogen_VDom_Util.unsafeGetAny(key, el));

    if (v1 === "string") {
      return Halogen_VDom_Util.unsafeSetAny(key, "", el);
    }

    ;

    if (key === "rowSpan") {
      return Halogen_VDom_Util.unsafeSetAny(key, 1, el);
    }

    ;

    if (key === "colSpan") {
      return Halogen_VDom_Util.unsafeSetAny(key, 1, el);
    }

    ;
    return Halogen_VDom_Util.unsafeSetAny(key, Halogen_VDom_Util.jsUndefined, el);
  };

  var propToStrKey = function propToStrKey(v) {
    if (v instanceof Attribute && v.value0 instanceof Data_Maybe.Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }

    ;

    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }

    ;

    if (v instanceof Property) {
      return "prop/" + v.value0;
    }

    ;

    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }

    ;

    if (v instanceof Ref) {
      return "ref";
    }

    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };

  var propFromString = Unsafe_Coerce.unsafeCoerce;
  var propFromBoolean = Unsafe_Coerce.unsafeCoerce;

  var buildProp = function buildProp(emit) {
    return function (el) {
      var removeProp = function removeProp(prevEvents) {
        return function (v, v1) {
          if (v1 instanceof Attribute) {
            return Halogen_VDom_Util.removeAttribute(Data_Nullable.toNullable(v1.value0), v1.value1, el);
          }

          ;

          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }

          ;

          if (v1 instanceof Handler) {
            var handler = Halogen_VDom_Util.unsafeLookup(v1.value0, prevEvents);
            return Halogen_VDom_Util.removeEventListener(v1.value0, Data_Tuple.fst(handler), el);
          }

          ;

          if (v1 instanceof Ref) {
            return Data_Unit.unit;
          }

          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };

      var mbEmit = function mbEmit(v) {
        if (v instanceof Data_Maybe.Just) {
          return emit(v.value0)();
        }

        ;
        return Data_Unit.unit;
      };

      var haltProp = function haltProp(state) {
        var v = Foreign_Object.lookup("ref")(state.props);

        if (v instanceof Data_Maybe.Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }

        ;
        return Data_Unit.unit;
      };

      var diffProp = function diffProp(prevEvents, events) {
        return function (v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $56 = v11.value2 === v2.value2;

            if ($56) {
              return v2;
            }

            ;
            Halogen_VDom_Util.setAttribute(Data_Nullable.toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }

          ;

          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = Halogen_VDom_Util.refEq(v11.value1, v2.value1);

            if (v4) {
              return v2;
            }

            ;

            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $65 = Halogen_VDom_Util.refEq(elVal, v2.value1);

              if ($65) {
                return v2;
              }

              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }

            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }

          ;

          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler = Halogen_VDom_Util.unsafeLookup(v2.value0, prevEvents);
            Effect_Ref.write(v2.value1)(Data_Tuple.snd(handler))();
            Halogen_VDom_Util.pokeMutMap(v2.value0, handler, events);
            return v2;
          }

          ;
          return v2;
        };
      };

      var applyProp = function applyProp(events) {
        return function (v, v1, v2) {
          if (v2 instanceof Attribute) {
            Halogen_VDom_Util.setAttribute(Data_Nullable.toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }

          ;

          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }

          ;

          if (v2 instanceof Handler) {
            var v3 = Halogen_VDom_Util.unsafeGetAny(v2.value0, events);

            if (Halogen_VDom_Util.unsafeHasAny(v2.value0, events)) {
              Effect_Ref.write(v2.value1)(Data_Tuple.snd(v3))();
              return v2;
            }

            ;
            var ref = Effect_Ref["new"](v2.value1)();
            var listener = Web_Event_EventTarget.eventListener(function (ev) {
              return function __do() {
                var f$prime = Effect_Ref.read(ref)();
                return mbEmit(f$prime(ev));
              };
            })();
            Halogen_VDom_Util.pokeMutMap(v2.value0, new Data_Tuple.Tuple(listener, ref), events);
            Halogen_VDom_Util.addEventListener(v2.value0, listener, el);
            return v2;
          }

          ;

          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }

          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };

      var patchProp = function patchProp(state, ps2) {
        var events = Halogen_VDom_Util.newMutMap();
        var onThis = removeProp(state.events);
        var onThese = diffProp(state.events, events);
        var onThat = applyProp(events);
        var props = Halogen_VDom_Util.diffWithKeyAndIxE(state.props, ps2, propToStrKey, onThese, onThis, onThat);
        var nextState = {
          events: Halogen_VDom_Util.unsafeFreeze(events),
          props: props
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Data_Unit.unit, nextState, patchProp, haltProp));
      };

      var renderProp = function renderProp(ps1) {
        var events = Halogen_VDom_Util.newMutMap();
        var ps1$prime = Halogen_VDom_Util.strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state = {
          events: Halogen_VDom_Util.unsafeFreeze(events),
          props: ps1$prime
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Data_Unit.unit, state, patchProp, haltProp));
      };

      return renderProp;
    };
  };

  exports["Property"] = Property;
  exports["Handler"] = Handler;
  exports["Ref"] = Ref;
  exports["Created"] = Created;
  exports["Removed"] = Removed;
  exports["propFromString"] = propFromString;
  exports["propFromBoolean"] = propFromBoolean;
  exports["buildProp"] = buildProp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.Types"] = $PS["Halogen.VDom.Types"] || {};
  var exports = $PS["Halogen.VDom.Types"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];

  var Text = function () {
    function Text(value0) {
      this.value0 = value0;
    }

    ;

    Text.create = function (value0) {
      return new Text(value0);
    };

    return Text;
  }();

  var Elem = function () {
    function Elem(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Elem.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Elem(value0, value1, value2, value3);
          };
        };
      };
    };

    return Elem;
  }();

  var Keyed = function () {
    function Keyed(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Keyed.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Keyed(value0, value1, value2, value3);
          };
        };
      };
    };

    return Keyed;
  }();

  var Widget = function () {
    function Widget(value0) {
      this.value0 = value0;
    }

    ;

    Widget.create = function (value0) {
      return new Widget(value0);
    };

    return Widget;
  }();

  var Grafted = function () {
    function Grafted(value0) {
      this.value0 = value0;
    }

    ;

    Grafted.create = function (value0) {
      return new Grafted(value0);
    };

    return Grafted;
  }();

  var Graft = function () {
    function Graft(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }

    ;

    Graft.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return new Graft(value0, value1, value2);
        };
      };
    };

    return Graft;
  }();

  var unGraft = function unGraft(f) {
    return function ($58) {
      return f($58);
    };
  };

  var graft = Unsafe_Coerce.unsafeCoerce;
  var bifunctorGraft = new Data_Bifunctor.Bifunctor(function (f) {
    return function (g) {
      return unGraft(function (v) {
        return graft(new Graft(function ($60) {
          return f(v.value0($60));
        }, function ($61) {
          return g(v.value1($61));
        }, v.value2));
      });
    };
  });
  var runGraft = unGraft(function (v) {
    var go = function go(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }

      ;

      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), Data_Functor.map(Data_Functor.functorArray)(go)(v2.value3));
      }

      ;

      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Data_Tuple.functorTuple)(go))(v2.value3));
      }

      ;

      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }

      ;

      if (v2 instanceof Grafted) {
        return new Grafted(Data_Bifunctor.bimap(bifunctorGraft)(v.value0)(v.value1)(v2.value0));
      }

      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };

    return go(v.value2);
  });
  exports["Text"] = Text;
  exports["Elem"] = Elem;
  exports["Keyed"] = Keyed;
  exports["Widget"] = Widget;
  exports["Grafted"] = Grafted;
  exports["runGraft"] = runGraft;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.HTML.Core"] = $PS["Halogen.HTML.Core"] || {};
  var exports = $PS["Halogen.HTML.Core"];
  var DOM_HTML_Indexed_ButtonType = $PS["DOM.HTML.Indexed.ButtonType"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Halogen_VDom_DOM_Prop = $PS["Halogen.VDom.DOM.Prop"];
  var Halogen_VDom_Types = $PS["Halogen.VDom.Types"];

  var HTML = function HTML(x) {
    return x;
  };

  var ClassName = function ClassName(x) {
    return x;
  };

  var IsProp = function IsProp(toPropValue) {
    this.toPropValue = toPropValue;
  };

  var widget = function widget($30) {
    return HTML(Halogen_VDom_Types.Widget.create($30));
  };

  var toPropValue = function toPropValue(dict) {
    return dict.toPropValue;
  };

  var text = function text($31) {
    return HTML(Halogen_VDom_Types.Text.create($31));
  };

  var ref = function ref(f) {
    return Halogen_VDom_DOM_Prop.Ref.create(function ($32) {
      return f(function (v) {
        if (v instanceof Halogen_VDom_DOM_Prop.Created) {
          return new Data_Maybe.Just(v.value0);
        }

        ;

        if (v instanceof Halogen_VDom_DOM_Prop.Removed) {
          return Data_Maybe.Nothing.value;
        }

        ;
        throw new Error("Failed pattern match at Halogen.HTML.Core (line 104, column 21 - line 106, column 23): " + [v.constructor.name]);
      }($32));
    });
  };

  var prop = function prop(dictIsProp) {
    return function (v) {
      var $33 = Halogen_VDom_DOM_Prop.Property.create(v);
      var $34 = toPropValue(dictIsProp);
      return function ($35) {
        return $33($34($35));
      };
    };
  };

  var newtypeHTML = new Data_Newtype.Newtype(function (n) {
    return n;
  }, HTML);
  var newtypeClassName = new Data_Newtype.Newtype(function (n) {
    return n;
  }, ClassName);
  var isPropString = new IsProp(Halogen_VDom_DOM_Prop.propFromString);
  var isPropInputType = new IsProp(function ($49) {
    return Halogen_VDom_DOM_Prop.propFromString(DOM_HTML_Indexed_InputType.renderInputType($49));
  });
  var isPropButtonType = new IsProp(function ($54) {
    return Halogen_VDom_DOM_Prop.propFromString(DOM_HTML_Indexed_ButtonType.renderButtonType($54));
  });
  var isPropBoolean = new IsProp(Halogen_VDom_DOM_Prop.propFromBoolean);
  var handler = Halogen_VDom_DOM_Prop.Handler.create;

  var element = function element(ns) {
    return function (name) {
      return function (props) {
        return function (children) {
          return new Halogen_VDom_Types.Elem(ns, name, props, children);
        };
      };
    };
  };

  exports["widget"] = widget;
  exports["text"] = text;
  exports["element"] = element;
  exports["prop"] = prop;
  exports["handler"] = handler;
  exports["ref"] = ref;
  exports["newtypeHTML"] = newtypeHTML;
  exports["isPropString"] = isPropString;
  exports["isPropBoolean"] = isPropBoolean;
  exports["isPropButtonType"] = isPropButtonType;
  exports["isPropInputType"] = isPropInputType;
  exports["newtypeClassName"] = newtypeClassName;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.HTML"] = $PS["Halogen.HTML"] || {};
  var exports = $PS["Halogen.HTML"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];

  var slot = function slot(dictCons) {
    return function (dictIsSymbol) {
      return function (dictOrd) {
        return function (label) {
          return function (p) {
            return function (component) {
              return function (input) {
                return function (outputQuery) {
                  return Halogen_HTML_Core.widget(new Halogen_Component.ComponentSlot(Halogen_Component.componentSlot()(dictIsSymbol)(dictOrd)(label)(p)(component)(input)(outputQuery)));
                };
              };
            };
          };
        };
      };
    };
  };

  exports["slot"] = slot;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.HTML.Elements"] = $PS["Halogen.HTML.Elements"] || {};
  var exports = $PS["Halogen.HTML.Elements"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var element = Halogen_HTML_Core.element(Data_Maybe.Nothing.value);
  var h2 = element("h2");
  var h2_ = h2([]);
  var h3 = element("h3");
  var h3_ = h3([]);

  var input = function input(props) {
    return element("input")(props)([]);
  };

  var li = element("li");
  var li_ = li([]);
  var option = element("option");
  var option_ = option([]);
  var select = element("select");
  var select_ = select([]);
  var span = element("span");
  var span_ = span([]);
  var ul = element("ul");
  var div = element("div");
  var div_ = div([]);
  var button = element("button");
  exports["button"] = button;
  exports["div"] = div;
  exports["div_"] = div_;
  exports["h2_"] = h2_;
  exports["h3_"] = h3_;
  exports["input"] = input;
  exports["li"] = li;
  exports["li_"] = li_;
  exports["option"] = option;
  exports["option_"] = option_;
  exports["select"] = select;
  exports["select_"] = select_;
  exports["span"] = span;
  exports["span_"] = span_;
  exports["ul"] = ul;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeReadPropImpl = function (f, s, key, value) {
    return value == null ? f : s(value[key]);
  };
})(PS["Foreign.Index"] = PS["Foreign.Index"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Foreign.Index"] = $PS["Foreign.Index"] || {};
  var exports = $PS["Foreign.Index"];
  var $foreign = $PS["Foreign.Index"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Identity = $PS["Data.Identity"];
  var Foreign = $PS["Foreign"];

  var unsafeReadProp = function unsafeReadProp(k) {
    return function (value) {
      return $foreign.unsafeReadPropImpl(Foreign.fail(new Foreign.TypeMismatch("object", Foreign.typeOf(value))), Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Data_Identity.monadIdentity)), k, value);
    };
  };

  var readProp = unsafeReadProp;
  exports["readProp"] = readProp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Query.Input"] = $PS["Halogen.Query.Input"] || {};
  var exports = $PS["Halogen.Query.Input"];

  var RefUpdate = function () {
    function RefUpdate(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }

    ;

    RefUpdate.create = function (value0) {
      return function (value1) {
        return new RefUpdate(value0, value1);
      };
    };

    return RefUpdate;
  }();

  var Action = function () {
    function Action(value0) {
      this.value0 = value0;
    }

    ;

    Action.create = function (value0) {
      return new Action(value0);
    };

    return Action;
  }();

  exports["RefUpdate"] = RefUpdate;
  exports["Action"] = Action;
})(PS);

(function (exports) {
  "use strict";

  exports._currentTarget = function (e) {
    return e.currentTarget;
  };
})(PS["Web.Event.Event"] = PS["Web.Event.Event"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.Event.Event"] = $PS["Web.Event.Event"] || {};
  var exports = $PS["Web.Event.Event"];
  var $foreign = $PS["Web.Event.Event"];
  var Data_Nullable = $PS["Data.Nullable"];

  var currentTarget = function currentTarget($8) {
    return Data_Nullable.toMaybe($foreign["_currentTarget"]($8));
  };

  exports["currentTarget"] = currentTarget;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.Event.EventTypes"] = $PS["Web.HTML.Event.EventTypes"] || {};
  var exports = $PS["Web.HTML.Event.EventTypes"];
  var input = "input";
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";
  exports["change"] = change;
  exports["domcontentloaded"] = domcontentloaded;
  exports["input"] = input;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.UIEvent.MouseEvent.EventTypes"] = $PS["Web.UIEvent.MouseEvent.EventTypes"] || {};
  var exports = $PS["Web.UIEvent.MouseEvent.EventTypes"];
  var click = "click";
  exports["click"] = click;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.HTML.Events"] = $PS["Halogen.HTML.Events"] || {};
  var exports = $PS["Halogen.HTML.Events"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Except = $PS["Control.Monad.Except"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Identity = $PS["Data.Identity"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Foreign = $PS["Foreign"];
  var Foreign_Index = $PS["Foreign.Index"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var Web_Event_Event = $PS["Web.Event.Event"];
  var Web_HTML_Event_EventTypes = $PS["Web.HTML.Event.EventTypes"];
  var Web_UIEvent_MouseEvent_EventTypes = $PS["Web.UIEvent.MouseEvent.EventTypes"];
  var mouseHandler = Unsafe_Coerce.unsafeCoerce;

  var handler = function handler(et) {
    var $1 = Halogen_HTML_Core.handler(et);
    var $2 = Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(Halogen_Query_Input.Action.create));
    return function ($3) {
      return $1($2($3));
    };
  };

  var onClick = function () {
    var $4 = handler(Web_UIEvent_MouseEvent_EventTypes.click);
    return function ($5) {
      return $4(mouseHandler($5));
    };
  }();

  var addForeignPropHandler = function addForeignPropHandler(key) {
    return function (prop) {
      return function (reader) {
        return function (f) {
          var go = function go(a) {
            return Control_Bind.composeKleisliFlipped(Control_Monad_Except_Trans.bindExceptT(Data_Identity.monadIdentity))(reader)(Foreign_Index.readProp(prop))(Foreign.unsafeToForeign(a));
          };

          return handler(key)(Control_Bind.composeKleisli(Data_Maybe.bindMaybe)(Web_Event_Event.currentTarget)(function (e) {
            return Data_Either.either(Data_Function["const"](Data_Maybe.Nothing.value))(f)(Control_Monad_Except.runExcept(go(e)));
          }));
        };
      };
    };
  };

  var onSelectedIndexChange = addForeignPropHandler(Web_HTML_Event_EventTypes.change)("selectedIndex")(Foreign.readInt);
  var onValueInput = addForeignPropHandler(Web_HTML_Event_EventTypes.input)("value")(Foreign.readString);
  exports["onClick"] = onClick;
  exports["onValueInput"] = onValueInput;
  exports["onSelectedIndexChange"] = onSelectedIndexChange;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.HTML.Properties"] = $PS["Halogen.HTML.Properties"] || {};
  var exports = $PS["Halogen.HTML.Properties"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];

  var ref = function () {
    var go = function go(p) {
      return function (mel) {
        return new Data_Maybe.Just(new Halogen_Query_Input.RefUpdate(p, mel));
      };
    };

    return function ($11) {
      return Halogen_HTML_Core.ref(go($11));
    };
  }();

  var prop = function prop(dictIsProp) {
    return Halogen_HTML_Core.prop(dictIsProp);
  };

  var selected = prop(Halogen_HTML_Core.isPropBoolean)("selected");

  var type_ = function type_(dictIsProp) {
    return prop(dictIsProp)("type");
  };

  var value = prop(Halogen_HTML_Core.isPropString)("value");
  var placeholder = prop(Halogen_HTML_Core.isPropString)("placeholder");
  var multiple = prop(Halogen_HTML_Core.isPropBoolean)("multiple");

  var class_ = function () {
    var $18 = prop(Halogen_HTML_Core.isPropString)("className");
    var $19 = Data_Newtype.unwrap(Halogen_HTML_Core.newtypeClassName);
    return function ($20) {
      return $18($19($20));
    };
  }();

  exports["ref"] = ref;
  exports["class_"] = class_;
  exports["type_"] = type_;
  exports["value"] = value;
  exports["selected"] = selected;
  exports["placeholder"] = placeholder;
  exports["multiple"] = multiple;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Component.Profunctor"] = $PS["Halogen.Component.Profunctor"] || {};
  var exports = $PS["Halogen.Component.Profunctor"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];

  var ProComponent = function ProComponent(x) {
    return x;
  };

  var newtypeProComponent = new Data_Newtype.Newtype(function (n) {
    return n;
  }, ProComponent);

  var dimapSpec = function dimapSpec(dictFunctor) {
    return function (f) {
      return function (g) {
        return function (spec) {
          return {
            initialState: function initialState($9) {
              return spec.initialState(f($9));
            },
            render: spec.render,
            "eval": Data_Profunctor.dimap(Data_Profunctor.profunctorFn)(Data_Bifunctor.lmap(Halogen_Query_HalogenQ.bifunctorHalogenQ)(f))(Halogen_Query_HalogenM.mapOutput(g))(spec["eval"])
          };
        };
      };
    };
  };

  var profunctorProComponent = function profunctorProComponent(dictFunctor) {
    return new Data_Profunctor.Profunctor(function (f) {
      return function (g) {
        return function (v) {
          return Halogen_Component.unComponent(function () {
            var $10 = dimapSpec(dictFunctor)(f)(g);
            return function ($11) {
              return Halogen_Component.mkComponent($10($11));
            };
          }())(v);
        };
      };
    });
  };

  exports["ProComponent"] = ProComponent;
  exports["newtypeProComponent"] = newtypeProComponent;
  exports["profunctorProComponent"] = profunctorProComponent;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Util"] = $PS["Halogen.Util"] || {};
  var exports = $PS["Halogen.Util"];
  var Data_Coyoneda = $PS["Data.Coyoneda"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_Component_Profunctor = $PS["Halogen.Component.Profunctor"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];

  var reHalogenQ = function reHalogenQ(f) {
    return function (v) {
      if (v instanceof Halogen_Query_HalogenQ.Initialize) {
        return new Halogen_Query_HalogenQ.Initialize(v.value0);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Finalize) {
        return new Halogen_Query_HalogenQ.Finalize(v.value0);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Receive) {
        return new Halogen_Query_HalogenQ.Receive(v.value0, v.value1);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Action) {
        return new Halogen_Query_HalogenQ.Action(v.value0, v.value1);
      }

      ;

      if (v instanceof Halogen_Query_HalogenQ.Query) {
        return new Halogen_Query_HalogenQ.Query(Data_Coyoneda.hoistCoyoneda(f)(v.value0), v.value1);
      }

      ;
      throw new Error("Failed pattern match at Halogen.Util (line 25, column 16 - line 30, column 45): " + [v.constructor.name]);
    };
  };

  var hoistQuery = function hoistQuery(f) {
    return Halogen_Component.unComponent(function (c) {
      return Halogen_Component.mkComponent({
        "eval": function () {
          var $12 = reHalogenQ(f);
          return function ($13) {
            return c["eval"]($12($13));
          };
        }(),
        initialState: c.initialState,
        render: c.render
      });
    });
  };

  var trimapComponent = function trimapComponent(dictFunctor) {
    return function (f) {
      return function (g) {
        return function (h) {
          var $14 = Data_Newtype.unwrap(Halogen_Component_Profunctor.newtypeProComponent);
          var $15 = Data_Profunctor.dimap(Halogen_Component_Profunctor.profunctorProComponent(dictFunctor))(g)(h);
          var $16 = hoistQuery(f);
          return function ($17) {
            return $14($15(Halogen_Component_Profunctor.ProComponent($16($17))));
          };
        };
      };
    };
  };

  var classProp = function classProp(cl) {
    return Halogen_HTML_Properties.class_(cl);
  };

  exports["hoistQuery"] = hoistQuery;
  exports["trimapComponent"] = trimapComponent;
  exports["classProp"] = classProp;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Corona.Chart.UI.Op"] = $PS["Corona.Chart.UI.Op"] || {};
  var exports = $PS["Corona.Chart.UI.Op"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Corona_Chart = $PS["Corona.Chart"];
  var D3_Scatter_Type = $PS["D3.Scatter.Type"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Bounded = $PS["Data.Bounded"];
  var Data_Date = $PS["Data.Date"];
  var Data_Date_Component = $PS["Data.Date.Component"];
  var Data_Enum = $PS["Data.Enum"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_ModifiedJulianDay = $PS["Data.ModifiedJulianDay"];
  var Data_Number = $PS["Data.Number"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Ring = $PS["Data.Ring"];
  var Data_Show = $PS["Data.Show"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Class_Console = $PS["Effect.Class.Console"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Util = $PS["Halogen.Util"];
  var Type_DSum = $PS["Type.DSum"];
  var Type_Equiv = $PS["Type.Equiv"];

  var TASetType = function () {
    function TASetType(value0) {
      this.value0 = value0;
    }

    ;

    TASetType.create = function (value0) {
      return new TASetType(value0);
    };

    return TASetType;
  }();

  var TASetAmount = function () {
    function TASetAmount(value0) {
      this.value0 = value0;
    }

    ;

    TASetAmount.create = function (value0) {
      return new TASetAmount(value0);
    };

    return TASetAmount;
  }();

  var RASetType = function () {
    function RASetType(value0) {
      this.value0 = value0;
    }

    ;

    RASetType.create = function (value0) {
      return new RASetType(value0);
    };

    return RASetType;
  }();

  var RASetCondType = function () {
    function RASetCondType(value0) {
      this.value0 = value0;
    }

    ;

    RASetCondType.create = function (value0) {
      return new RASetCondType(value0);
    };

    return RASetCondType;
  }();

  var RASetLimit = function () {
    function RASetLimit(value0) {
      this.value0 = value0;
    }

    ;

    RASetLimit.create = function (value0) {
      return new RASetLimit(value0);
    };

    return RASetLimit;
  }();

  var QueryOp = function () {
    function QueryOp(value0) {
      this.value0 = value0;
    }

    ;

    QueryOp.create = function (value0) {
      return new QueryOp(value0);
    };

    return QueryOp;
  }();

  var PQState = function () {
    function PQState(value0) {
      this.value0 = value0;
    }

    ;

    PQState.create = function (value0) {
      return new PQState(value0);
    };

    return PQState;
  }();

  var PickOpUpdate = function () {
    function PickOpUpdate() {}

    ;
    PickOpUpdate.value = new PickOpUpdate();
    return PickOpUpdate;
  }();

  var ChangeEvent = function () {
    function ChangeEvent(value0) {
      this.value0 = value0;
    }

    ;

    ChangeEvent.create = function (value0) {
      return new ChangeEvent(value0);
    };

    return ChangeEvent;
  }();

  var SetPickOpIx = function () {
    function SetPickOpIx(value0) {
      this.value0 = value0;
    }

    ;

    SetPickOpIx.create = function (value0) {
      return new SetPickOpIx(value0);
    };

    return SetPickOpIx;
  }();

  var TriggerUpdate = function () {
    function TriggerUpdate() {}

    ;
    TriggerUpdate.value = new TriggerUpdate();
    return TriggerUpdate;
  }();

  var windowPickOp = function windowPickOp(tf) {
    var parseWindow = function () {
      var $182 = Data_Functor.map(Data_Maybe.functorMaybe)(function () {
        var $184 = Data_Ord.abs(Data_Ord.ordInt)(Data_Ring.ringInt);
        return function ($185) {
          return $184(Data_Int.round($185));
        };
      }());
      return function ($183) {
        return $182(Data_Number.fromString($183));
      };
    }();

    return {
      label: "Moving Average",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return 1;
        },
        render: function render(st) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("moving-average")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Window size (before/after)")]), Halogen_HTML_Elements.input([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputNumber.value), Halogen_HTML_Properties.value(Data_Show.show(Data_Show.showInt)(st)), Halogen_HTML_Events.onValueInput(parseWindow)])]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(st) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(st))(function () {
              return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
            });
          },
          handleQuery: function handleQuery(v1) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (i) {
              var v2 = v1.value0(new Corona_Chart.Window(tf, i));
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
                if (v2.value1 instanceof Corona_Chart.Window) {
                  return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(v2.value1.value1);
                }

                ;
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
              }())(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v2.value0));
              });
            });
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  };

  var inputField = function inputField(t) {
    var showPrecision = function showPrecision(x) {
      var $75 = Data_Int.toNumber(Data_Int.round(x)) === x;

      if ($75) {
        return Data_Show.show(Data_Show.showInt)(Data_Int.round(x));
      }

      ;
      return Data_Show.show(Data_Show.showNumber)(x);
    };

    return {
      inputType: function () {
        if (t instanceof D3_Scatter_Type.SDay) {
          return DOM_HTML_Indexed_InputType.InputDate.value;
        }

        ;
        return DOM_HTML_Indexed_InputType.InputNumber.value;
      }(),
      inputParse: function () {
        if (t instanceof D3_Scatter_Type.SDay) {
          var $186 = Type_Equiv.equivFromF(t.value0);
          return function ($187) {
            return $186(Data_ModifiedJulianDay.fromISO8601($187));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SDays) {
          var $188 = Data_Functor.map(Data_Maybe.functorMaybe)(function () {
            var $190 = Type_Equiv.equivFrom(t.value0);
            return function ($191) {
              return $190(D3_Scatter_Type.Days(Data_Int.round($191)));
            };
          }());
          return function ($189) {
            return $188(Data_Number.fromString($189));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SInt) {
          var $192 = Data_Functor.map(Data_Maybe.functorMaybe)(function () {
            var $194 = Type_Equiv.equivFrom(t.value0);
            return function ($195) {
              return $194(Data_Int.round($195));
            };
          }());
          return function ($193) {
            return $192(Data_Number.fromString($193));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SNumber) {
          var $196 = Data_Functor.map(Data_Maybe.functorMaybe)(Type_Equiv.equivFrom(t.value0));
          return function ($197) {
            return $196(Data_Number.fromString($197));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SPercent) {
          var $198 = Data_Functor.map(Data_Maybe.functorMaybe)(function () {
            var $200 = Type_Equiv.equivFrom(t.value0);
            return function ($201) {
              return $200(D3_Scatter_Type.Percent(function (v) {
                return v / Data_Int.toNumber(100);
              }($201)));
            };
          }());
          return function ($199) {
            return $198(Data_Number.fromString($199));
          };
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 505, column 19 - line 511, column 38): " + [t.constructor.name]);
      }(),
      inputShow: function () {
        if (t instanceof D3_Scatter_Type.SDay) {
          var $202 = Type_Equiv.equivTo(t.value0);
          return function ($203) {
            return Data_ModifiedJulianDay.toISO8601($202($203));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SDays) {
          var $204 = Data_Show.show(Data_Show.showInt);
          var $205 = Type_Equiv.equivTo(t.value0);
          return function ($206) {
            return $204(D3_Scatter_Type.unDays($205($206)));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SInt) {
          var $207 = Data_Show.show(Data_Show.showInt);
          var $208 = Type_Equiv.equivTo(t.value0);
          return function ($209) {
            return $207($208($209));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SNumber) {
          var $210 = Type_Equiv.equivTo(t.value0);
          return function ($211) {
            return showPrecision($210($211));
          };
        }

        ;

        if (t instanceof D3_Scatter_Type.SPercent) {
          var $212 = Type_Equiv.equivTo(t.value0);
          return function ($213) {
            return showPrecision(function (v) {
              return v * Data_Int.toNumber(100);
            }(D3_Scatter_Type.unPercent($212($213))));
          };
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 512, column 18 - line 517, column 90): " + [t.constructor.name]);
      }()
    };
  };

  var handleAction = function handleAction(dictMonadEffect) {
    return function (pos) {
      return function (act) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.gets(Halogen_Query_HalogenM.monadStateHalogenM)(function (v) {
          return v.pickOpIx;
        }))(function (oldIx) {
          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (st) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
              if (act instanceof SetPickOpIx) {
                return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)({
                  pickOpIx: act.value0
                });
              }

              ;

              if (act instanceof TriggerUpdate) {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
              }

              ;
              throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 164, column 5 - line 166, column 33): " + [act.constructor.name]);
            }())(function () {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.gets(Halogen_Query_HalogenM.monadStateHalogenM)(function (v) {
                return v.pickOpIx;
              }))(function (newIx) {
                var v = Data_Array.index(pos)(newIx);

                if (v instanceof Data_Maybe.Nothing) {
                  return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))("hey what gives");
                }

                ;

                if (v instanceof Data_Maybe.Just) {
                  return Type_DSum.withDSum(v.value0)(function (t) {
                    return function (v1) {
                      return Halogen_Query_HalogenM.raise(new ChangeEvent(Data_Exists.mkExists(t)));
                    };
                  });
                }

                ;
                throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 169, column 5 - line 173, column 8): " + [v.constructor.name]);
              });
            });
          });
        });
      };
    };
  };

  var fakeState = function fakeState(dictApplicative) {
    return function (x) {
      return function (f) {
        return Control_Applicative.pure(dictApplicative)(new Data_Maybe.Just(Data_Tuple.fst(f(x))));
      };
    };
  };

  var pgrowthPickOp = function pgrowthPickOp(nt) {
    return {
      label: "Percent Growth",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return Data_Unit.unit;
        },
        render: function render(v) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("percent-growth")])([]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(v1) {
            return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
          },
          handleQuery: function handleQuery(v1) {
            return fakeState(Halogen_Query_HalogenM.applicativeHalogenM)(new Corona_Chart.PGrowth(nt, Type_Equiv.refl))(v1.value0);
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  };

  var pmaxPickOp = function pmaxPickOp(nt) {
    return {
      label: "Percent of Maximum",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return Data_Unit.unit;
        },
        render: function render(v) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("percent-of-maximum")])([]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(v1) {
            return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
          },
          handleQuery: function handleQuery(v1) {
            return fakeState(Halogen_Query_HalogenM.applicativeHalogenM)(new Corona_Chart.PMax(nt, Type_Equiv.refl))(v1.value0);
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  };

  var pointDatePickOp = {
    label: "Date Observed",
    component: Halogen_Component.mkComponent({
      initialState: function initialState(v) {
        return Data_Unit.unit;
      },
      render: function render(v) {
        return Halogen_HTML_Elements.div([Halogen_Util.classProp("date-observed")])([]);
      },
      "eval": Halogen_Component.mkEval({
        handleAction: function handleAction(v1) {
          return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
        },
        handleQuery: function handleQuery(v1) {
          return fakeState(Halogen_Query_HalogenM.applicativeHalogenM)(new Corona_Chart.PointDate(Type_Equiv.refl))(v1.value0);
        },
        receive: Halogen_Component.defaultEval.receive,
        initialize: Halogen_Component.defaultEval.initialize,
        finalize: Halogen_Component.defaultEval.finalize
      })
    })
  };

  var deltaPickOp = function deltaPickOp(nt) {
    return {
      label: "Daily Change",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return Data_Unit.unit;
        },
        render: function render(v) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("daily-change")])([]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(v1) {
            return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
          },
          handleQuery: function handleQuery(v1) {
            return fakeState(Halogen_Query_HalogenM.applicativeHalogenM)(new Corona_Chart.Delta(nt, Type_Equiv.refl))(v1.value0);
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  };

  var cutoffList = [Corona_Chart.After.value, Corona_Chart.Before.value];

  var dayNumberPickOp = function () {
    var showCutoff = function showCutoff(v) {
      if (v instanceof Corona_Chart.After) {
        return "first day";
      }

      ;

      if (v instanceof Corona_Chart.Before) {
        return "last day";
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 468, column 18 - line 470, column 27): " + [v.constructor.name]);
    };

    return {
      label: "Day Count",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return Corona_Chart.After.value;
        },
        render: function render(st) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("day-count")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Days since/until...")]), Halogen_HTML_Elements.select([Halogen_Util.classProp("cutoff-list"), Halogen_HTML_Events.onSelectedIndexChange(function (v) {
            return Data_Array.index(cutoffList)(v);
          })])(Data_Functor.mapFlipped(Data_Functor.functorArray)(cutoffList)(function (c) {
            var isSelected = Data_Eq.eq(Corona_Chart.eqCutoffType)(c)(st);
            return Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(isSelected)])([Halogen_HTML_Core.text(showCutoff(c))]);
          }))]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(st) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(st))(function () {
              return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
            });
          },
          handleQuery: function handleQuery(v1) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (c) {
              var v2 = v1.value0(new Corona_Chart.DayNumber(Type_Equiv.refl, c));
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
                if (v2.value1 instanceof Corona_Chart.DayNumber) {
                  return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(v2.value1.value1);
                }

                ;
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
              }())(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v2.value0));
              });
            });
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  }();

  var takePickOp = function () {
    var showCutoff = function showCutoff(v) {
      if (v instanceof Corona_Chart.After) {
        return "first";
      }

      ;

      if (v instanceof Corona_Chart.Before) {
        return "last";
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 435, column 18 - line 437, column 23): " + [v.constructor.name]);
    };

    var parseAmount = function () {
      var $214 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Int.round);
      return function ($215) {
        return $214(Data_Number.fromString($215));
      };
    }();

    return {
      label: "Take Amount",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return {
            cutoffType: Corona_Chart.Before.value,
            amount: 30
          };
        },
        render: function render(st) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("take-amount")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Keep only the...")]), Halogen_HTML_Elements.select([Halogen_Util.classProp("cutoff-list"), Halogen_HTML_Events.onSelectedIndexChange(function () {
            var $216 = Data_Functor.map(Data_Maybe.functorMaybe)(TASetType.create);
            return function ($217) {
              return $216(function (v) {
                return Data_Array.index(cutoffList)(v);
              }($217));
            };
          }())])(Data_Functor.mapFlipped(Data_Functor.functorArray)(cutoffList)(function (c) {
            var isSelected = Data_Eq.eq(Corona_Chart.eqCutoffType)(c)(st.cutoffType);
            return Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(isSelected)])([Halogen_HTML_Core.text(showCutoff(c))]);
          })), Halogen_HTML_Elements.div([Halogen_Util.classProp("cond-num-picker")])([Halogen_HTML_Elements.input([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputNumber.value), Halogen_HTML_Properties.value(Data_Show.show(Data_Show.showInt)(st.amount)), Halogen_HTML_Events.onValueInput(function () {
            var $218 = Data_Functor.map(Data_Maybe.functorMaybe)(TASetAmount.create);
            return function ($219) {
              return $218(parseAmount($219));
            };
          }())])]), Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("...points")])]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(act) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function () {
              if (act instanceof TASetType) {
                return function (v1) {
                  var $113 = {};

                  for (var $114 in v1) {
                    if ({}.hasOwnProperty.call(v1, $114)) {
                      $113[$114] = v1[$114];
                    }

                    ;
                  }

                  ;
                  $113.cutoffType = act.value0;
                  return $113;
                };
              }

              ;

              if (act instanceof TASetAmount) {
                return function (v2) {
                  var $117 = {};

                  for (var $118 in v2) {
                    if ({}.hasOwnProperty.call(v2, $118)) {
                      $117[$118] = v2[$118];
                    }

                    ;
                  }

                  ;
                  $117.amount = act.value0;
                  return $117;
                };
              }

              ;
              throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 418, column 15 - line 420, column 50): " + [act.constructor.name]);
            }()))(function () {
              return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
            });
          },
          handleQuery: function handleQuery(v1) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (st) {
              var v2 = v1.value0(new Corona_Chart.Take(Type_Equiv.refl, st.amount, st.cutoffType));
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
                if (v2.value1 instanceof Corona_Chart.Take) {
                  return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)({
                    cutoffType: v2.value1.value2,
                    amount: v2.value1.value1
                  });
                }

                ;
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
              }())(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v2.value0));
              });
            });
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  }();

  var condList = [new Corona_Chart.AtLeast(Data_Unit.unit), new Corona_Chart.AtMost(Data_Unit.unit)];

  var restrictPickOp = function restrictPickOp(t) {
    var v = inputField(t);

    var showCutoff = function showCutoff(v1) {
      if (v1 instanceof Corona_Chart.After) {
        return "after";
      }

      ;

      if (v1 instanceof Corona_Chart.Before) {
        return "before";
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 374, column 18 - line 376, column 25): " + [v1.constructor.name]);
    };

    var showCond = function showCond(v1) {
      if (v1 instanceof Corona_Chart.AtLeast) {
        return "at least";
      }

      ;

      if (v1 instanceof Corona_Chart.AtMost) {
        return "at most";
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 377, column 16 - line 379, column 29): " + [v1.constructor.name]);
    };

    return {
      label: "Restrict",
      component: Halogen_Component.mkComponent({
        initialState: function initialState(v1) {
          return {
            cutoffType: Corona_Chart.After.value,
            condition: Corona_Chart.AtLeast.create(function () {
              if (t instanceof D3_Scatter_Type.SDay) {
                return Type_Equiv.equivFrom(t.value0)(Data_ModifiedJulianDay.fromDate(Data_Date.canonicalDate(Data_Enum.toEnumWithDefaults(Data_Date_Component.boundedEnumYear)(Data_Bounded.bottom(Data_Date_Component.boundedYear))(Data_Bounded.top(Data_Date_Component.boundedYear))(2020))(Data_Date_Component.January.value)(Data_Enum.toEnumWithDefaults(Data_Date_Component.boundedEnumDay)(Data_Bounded.bottom(Data_Date_Component.boundedDay))(Data_Bounded.top(Data_Date_Component.boundedDay))(22))));
              }

              ;

              if (t instanceof D3_Scatter_Type.SDays) {
                return Type_Equiv.equivFrom(t.value0)(0);
              }

              ;

              if (t instanceof D3_Scatter_Type.SInt) {
                return Type_Equiv.equivFrom(t.value0)(100);
              }

              ;

              if (t instanceof D3_Scatter_Type.SNumber) {
                return Type_Equiv.equivFrom(t.value0)(Data_Int.toNumber(100));
              }

              ;

              if (t instanceof D3_Scatter_Type.SPercent) {
                return Type_Equiv.equivFrom(t.value0)(0.2);
              }

              ;
              throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 317, column 36 - line 326, column 59): " + [t.constructor.name]);
            }())
          };
        },
        render: function render(st) {
          return Halogen_HTML_Elements.div([Halogen_Util.classProp("restrict")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Keep points...")]), Halogen_HTML_Elements.select([Halogen_Util.classProp("cutoff-list"), Halogen_HTML_Events.onSelectedIndexChange(function () {
            var $220 = Data_Functor.map(Data_Maybe.functorMaybe)(RASetType.create);
            return function ($221) {
              return $220(function (v1) {
                return Data_Array.index(cutoffList)(v1);
              }($221));
            };
          }())])(Data_Functor.mapFlipped(Data_Functor.functorArray)(cutoffList)(function (c) {
            var isSelected = Data_Eq.eq(Corona_Chart.eqCutoffType)(c)(st.cutoffType);
            return Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(isSelected)])([Halogen_HTML_Core.text(showCutoff(c))]);
          })), Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("...being...")]), Halogen_HTML_Elements.select([Halogen_Util.classProp("condition-list"), Halogen_HTML_Events.onSelectedIndexChange(function () {
            var $222 = Data_Functor.map(Data_Maybe.functorMaybe)(RASetCondType.create);
            return function ($223) {
              return $222(function (v1) {
                return Data_Array.index(condList)(v1);
              }($223));
            };
          }())])(Data_Functor.mapFlipped(Data_Functor.functorArray)(condList)(function (c) {
            var isSelected = Data_Eq.eq(Corona_Chart.eqCondition(Data_Eq.eqUnit))(c)(Data_Functor["void"](Corona_Chart.functorCondition)(st.condition));
            return Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(isSelected)])([Halogen_HTML_Core.text(showCond(c))]);
          })), Halogen_HTML_Elements.div([Halogen_Util.classProp("cond-num-picker")])([Halogen_HTML_Elements.input([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(v.inputType), Halogen_HTML_Properties.value(v.inputShow(Corona_Chart.conditionValue(st.condition))), Halogen_HTML_Events.onValueInput(function () {
            var $224 = Data_Functor.map(Data_Maybe.functorMaybe)(RASetLimit.create);
            return function ($225) {
              return $224(v.inputParse($225));
            };
          }())])])]);
        },
        "eval": Halogen_Component.mkEval({
          handleAction: function handleAction(act) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
              if (act instanceof RASetType) {
                var $142 = {};

                for (var $143 in st) {
                  if ({}.hasOwnProperty.call(st, $143)) {
                    $142[$143] = st[$143];
                  }

                  ;
                }

                ;
                $142.cutoffType = act.value0;
                return $142;
              }

              ;

              if (act instanceof RASetCondType) {
                var $146 = {};

                for (var $147 in st) {
                  if ({}.hasOwnProperty.call(st, $147)) {
                    $146[$147] = st[$147];
                  }

                  ;
                }

                ;
                $146.condition = Data_Functor.voidRight(Corona_Chart.functorCondition)(Corona_Chart.conditionValue(st.condition))(act.value0);
                return $146;
              }

              ;

              if (act instanceof RASetLimit) {
                var $150 = {};

                for (var $151 in st) {
                  if ({}.hasOwnProperty.call(st, $151)) {
                    $150[$151] = st[$151];
                  }

                  ;
                }

                ;
                $150.condition = Data_Functor.voidRight(Corona_Chart.functorCondition)(act.value0)(st.condition);
                return $150;
              }

              ;
              throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 354, column 15 - line 359, column 55): " + [act.constructor.name]);
            }))(function () {
              return Halogen_Query_HalogenM.raise(PickOpUpdate.value);
            });
          },
          handleQuery: function handleQuery(v2) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (st) {
              var v3 = v2.value0(new Corona_Chart.Restrict(t, Type_Equiv.refl, st.cutoffType, st.condition));
              return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
                if (v3.value1 instanceof Corona_Chart.Restrict) {
                  return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)({
                    cutoffType: v3.value1.value2,
                    condition: v3.value1.value3
                  });
                }

                ;
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
              }())(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v3.value0));
              });
            });
          },
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      })
    };
  };

  var validPickOps = function validPickOps(t0) {
    if (t0 instanceof D3_Scatter_Type.SDay) {
      return [Type_DSum.dsum(t0)(restrictPickOp(t0)), Type_DSum.dsum(t0)(takePickOp), Type_DSum.dsum(D3_Scatter_Type.sDays)(dayNumberPickOp), Type_DSum.dsum(D3_Scatter_Type.sDay)(pointDatePickOp)];
    }

    ;

    if (t0 instanceof D3_Scatter_Type.SDays) {
      return [Type_DSum.dsum(t0)(restrictPickOp(t0)), Type_DSum.dsum(t0)(takePickOp), Type_DSum.dsum(D3_Scatter_Type.sDays)(dayNumberPickOp), Type_DSum.dsum(D3_Scatter_Type.sDay)(pointDatePickOp)];
    }

    ;

    if (t0 instanceof D3_Scatter_Type.SInt) {
      return [Type_DSum.dsum(t0)(deltaPickOp(new D3_Scatter_Type.NInt(t0.value0))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(pgrowthPickOp(new D3_Scatter_Type.NInt(t0.value0))), Type_DSum.dsum(D3_Scatter_Type.sNumber)(windowPickOp(new Corona_Chart.I2N(t0.value0, Type_Equiv.refl))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(pmaxPickOp(new D3_Scatter_Type.NInt(t0.value0))), Type_DSum.dsum(t0)(restrictPickOp(t0)), Type_DSum.dsum(t0)(takePickOp), Type_DSum.dsum(D3_Scatter_Type.sDays)(dayNumberPickOp), Type_DSum.dsum(D3_Scatter_Type.sDay)(pointDatePickOp)];
    }

    ;

    if (t0 instanceof D3_Scatter_Type.SNumber) {
      return [Type_DSum.dsum(t0)(deltaPickOp(new D3_Scatter_Type.NNumber(t0.value0))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(pgrowthPickOp(new D3_Scatter_Type.NNumber(t0.value0))), Type_DSum.dsum(D3_Scatter_Type.sNumber)(windowPickOp(new Corona_Chart.N2N(t0.value0, Type_Equiv.refl))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(pmaxPickOp(new D3_Scatter_Type.NNumber(t0.value0))), Type_DSum.dsum(t0)(restrictPickOp(t0)), Type_DSum.dsum(t0)(takePickOp), Type_DSum.dsum(D3_Scatter_Type.sDays)(dayNumberPickOp), Type_DSum.dsum(D3_Scatter_Type.sDay)(pointDatePickOp)];
    }

    ;

    if (t0 instanceof D3_Scatter_Type.SPercent) {
      return [Type_DSum.dsum(t0)(deltaPickOp(new D3_Scatter_Type.NPercent(t0.value0))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(pgrowthPickOp(new D3_Scatter_Type.NPercent(t0.value0))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(windowPickOp(new Corona_Chart.P2P(t0.value0, Type_Equiv.refl))), Type_DSum.dsum(D3_Scatter_Type.sPercent)(pmaxPickOp(new D3_Scatter_Type.NPercent(t0.value0))), Type_DSum.dsum(t0)(restrictPickOp(t0)), Type_DSum.dsum(t0)(takePickOp), Type_DSum.dsum(D3_Scatter_Type.sDays)(dayNumberPickOp), Type_DSum.dsum(D3_Scatter_Type.sDay)(pointDatePickOp)];
    }

    ;
    throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 45, column 19 - line 87, column 8): " + [t0.constructor.name]);
  };

  var _pickOp = Data_Symbol.SProxy.value;

  var handleQuery = function handleQuery(dictMonadEffect) {
    return function (v) {
      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (st) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
          return "pickOp";
        }))(Data_Ord.ordInt)(_pickOp)(st.pickOpIx)(function (t) {
          return function (op) {
            return new Data_Tuple.Tuple(Type_DSum.dsum(t)(op), op);
          };
        }))(function (subSt) {
          return Data_Traversable["for"](Halogen_Query_HalogenM.applicativeHalogenM)(Data_Traversable.traversableMaybe)(subSt)(function (o) {
            var v1 = Type_DSum.withDSum(o)(v.value0);
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v1.value0);
          });
        });
      });
    };
  };

  var render = function render(pos) {
    return function (st) {
      var mkSlot = function mkSlot(dspo) {
        return Type_DSum.withDSum(dspo)(function (t) {
          return function (v) {
            return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
              return "pickOp";
            }))(Data_Ord.ordInt)(_pickOp)(st.pickOpIx)(Halogen_Util.hoistQuery(function (v1) {
              return new PQState(v1(t));
            })(v.component))(Data_Unit.unit)(Data_Function["const"](new Data_Maybe.Just(TriggerUpdate.value)));
          };
        });
      };

      var goodIx = function goodIx(i) {
        if (i < Data_Array.length(pos)) {
          return new Data_Maybe.Just(i);
        }

        ;

        if (Data_Boolean.otherwise) {
          return Data_Maybe.Nothing.value;
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart.UI.Op (line 144, column 5 - line 146, column 35): " + [i.constructor.name]);
      };

      return Halogen_HTML_Elements.div([Halogen_Util.classProp("single-op-picker")])([Halogen_HTML_Elements.select([Halogen_HTML_Events.onSelectedIndexChange(function () {
        var $226 = Data_Functor.map(Data_Maybe.functorMaybe)(SetPickOpIx.create);
        return function ($227) {
          return $226(goodIx($227));
        };
      }())])(Data_Function.flip(Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray))(pos)(function (i) {
        return function (dspo) {
          return Type_DSum.withDSum(dspo)(function (t) {
            return function (v) {
              return Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(i === st.pickOpIx)])([Halogen_HTML_Core.text(v.label)]);
            };
          });
        };
      })), Halogen_HTML_Elements.div([Halogen_Util.classProp("single-op-options")])(Data_Maybe.maybe([])(function ($228) {
        return Data_Array.singleton(mkSlot($228));
      })(Data_Array.index(pos)(st.pickOpIx)))]);
    };
  };

  var component = function component(dictMonadEffect) {
    return function (t0) {
      var pos = validPickOps(t0);
      return Halogen_Component.mkComponent({
        initialState: function initialState(v) {
          return {
            pickOpIx: 0
          };
        },
        render: render(pos),
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction(dictMonadEffect)(pos),
          handleQuery: handleQuery(dictMonadEffect),
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      });
    };
  };

  exports["QueryOp"] = QueryOp;
  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Functor.Compose"] = $PS["Data.Functor.Compose"] || {};
  var exports = $PS["Data.Functor.Compose"];
  var Data_Newtype = $PS["Data.Newtype"];

  var Compose = function Compose(x) {
    return x;
  };

  var newtypeCompose = new Data_Newtype.Newtype(function (n) {
    return n;
  }, Compose);
  exports["Compose"] = Compose;
  exports["newtypeCompose"] = newtypeCompose;
})(PS);

(function (exports) {
  "use strict"; // Alias require to prevent webpack or browserify from actually requiring.

  var req = typeof module === "undefined" ? undefined : module.require;
  var util = req === undefined ? undefined : req("util");

  exports.trace = function () {
    return function (x) {
      return function (k) {
        // node only recurses two levels into an object before printing
        // "[object]" for further objects when using console.log()
        if (util !== undefined) {
          console.log(util.inspect(x, {
            depth: null,
            colors: true
          }));
        } else {
          console.log(x);
        }

        return k({});
      };
    };
  };
})(PS["Debug.Trace"] = PS["Debug.Trace"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Debug.Trace"] = $PS["Debug.Trace"] || {};
  var exports = $PS["Debug.Trace"];
  var $foreign = $PS["Debug.Trace"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Data_Unit = $PS["Data.Unit"];

  var traceM = function traceM(dictDebugWarning) {
    return function (dictMonad) {
      return function (s) {
        return Control_Bind.discard(Control_Bind.discardUnit)(dictMonad.Bind1())(Control_Applicative.pure(dictMonad.Applicative0())(Data_Unit.unit))(function () {
          return $foreign.trace()(s)(function (v) {
            return Control_Applicative.pure(dictMonad.Applicative0())(Data_Unit.unit);
          });
        });
      };
    };
  };

  exports["traceM"] = traceM;
  exports["trace"] = $foreign.trace;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.ChainPicker"] = $PS["Halogen.ChainPicker"] || {};
  var exports = $PS["Halogen.ChainPicker"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_State_Trans = $PS["Control.Monad.State.Trans"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var DOM_HTML_Indexed_ButtonType = $PS["DOM.HTML.Indexed.ButtonType"];
  var Data_Array = $PS["Data.Array"];
  var Data_Bifunctor = $PS["Data.Bifunctor"];
  var Data_Either = $PS["Data.Either"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Functor_Compose = $PS["Data.Functor.Compose"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Monoid = $PS["Data.Monoid"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Show = $PS["Data.Show"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Data_Unit = $PS["Data.Unit"];
  var Debug_Trace = $PS["Debug.Trace"];
  var Effect_Class_Console = $PS["Effect.Class.Console"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Util = $PS["Halogen.Util"];
  var Type_Ap = $PS["Type.Ap"];
  var Type_Chain = $PS["Type.Chain"];
  var Type_DProd = $PS["Type.DProd"];
  var Type_DSum = $PS["Type.DSum"];
  var Type_Equiv = $PS["Type.Equiv"];
  var Type_GCompare = $PS["Type.GCompare"];

  var SQ = function () {
    function SQ(value0) {
      this.value0 = value0;
    }

    ;

    SQ.create = function (value0) {
      return new SQ(value0);
    };

    return SQ;
  }();

  var StateSelected = function () {
    function StateSelected(value0) {
      this.value0 = value0;
    }

    ;

    StateSelected.create = function (value0) {
      return new StateSelected(value0);
    };

    return StateSelected;
  }();

  var ChainUpdate = function () {
    function ChainUpdate(value0) {
      this.value0 = value0;
    }

    ;

    ChainUpdate.create = function (value0) {
      return new ChainUpdate(value0);
    };

    return ChainUpdate;
  }();

  var AEOutType = function () {
    function AEOutType(value0) {
      this.value0 = value0;
    }

    ;

    AEOutType.create = function (value0) {
      return new AEOutType(value0);
    };

    return AEOutType;
  }();

  var AEInType = function () {
    function AEInType(value0) {
      this.value0 = value0;
    }

    ;

    AEInType.create = function (value0) {
      return new AEInType(value0);
    };

    return AEInType;
  }();

  var AENoResponse = function () {
    function AENoResponse(value0) {
      this.value0 = value0;
    }

    ;

    AENoResponse.create = function (value0) {
      return new AENoResponse(value0);
    };

    return AENoResponse;
  }();

  var AddLink = function () {
    function AddLink(value0) {
      this.value0 = value0;
    }

    ;

    AddLink.create = function (value0) {
      return new AddLink(value0);
    };

    return AddLink;
  }();

  var RemoveLink = function () {
    function RemoveLink(value0) {
      this.value0 = value0;
    }

    ;

    RemoveLink.create = function (value0) {
      return new RemoveLink(value0);
    };

    return RemoveLink;
  }();

  var TriggerUpdate = function () {
    function TriggerUpdate(value0) {
      this.value0 = value0;
    }

    ;

    TriggerUpdate.create = function (value0) {
      return new TriggerUpdate(value0);
    };

    return TriggerUpdate;
  }();

  var unSomeQuery = function unSomeQuery(dictApplicative) {
    return function (dictDecide) {
      return function (tExpected) {
        return function (v) {
          return v.typeMatch(function (tActual) {
            return function (sq) {
              var v1 = Type_Equiv.decide(dictDecide)(tExpected)(tActual);

              if (v1 instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)(v.typeMismatch(tActual));
              }

              ;

              if (v1 instanceof Data_Maybe.Just) {
                return Type_Equiv.equivFromF2(v1.value0)(sq);
              }

              ;
              throw new Error("Failed pattern match at Halogen.ChainPicker (line 391, column 5 - line 393, column 39): " + [v1.constructor.name]);
            };
          });
        };
      };
    };
  };

  var subQueryAsk = new SQ(function (t) {
    return function (x) {
      return new Data_Tuple.Tuple(Type_DSum.dsum(t)(x), Type_DSum.dsum(t)(x));
    };
  });

  var someQuery = function someQuery(dictFunctor) {
    return function (t) {
      return function (q) {
        return {
          typeMismatch: function typeMismatch($139) {
            return Data_Either.Left.create(Data_Exists.mkExists($139));
          },
          typeMatch: function typeMatch(f) {
            return f(t)(Data_Functor.map(dictFunctor)(Data_Either.Right.create)(q));
          }
        };
      };
    };
  };

  var showAssembleError = function showAssembleError(dictGShow) {
    return new Data_Show.Show(function (v) {
      if (v instanceof AEOutType) {
        return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(["Output type mismatch at index ", Data_Show.show(Data_Show.showInt)(v.value0.ix), "; Expected ", Data_Exists.runExists(Type_GCompare.gshow(dictGShow))(v.value0.expected), ", got ", Data_Exists.runExists(Type_GCompare.gshow(dictGShow))(v.value0.expected)]);
      }

      ;

      if (v instanceof AEInType) {
        return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(["Input type mismatch at index ", Data_Show.show(Data_Show.showInt)(v.value0.ix), "; Expected ", Data_Exists.runExists(Type_GCompare.gshow(dictGShow))(v.value0.expected), ", got ", Data_Exists.runExists(Type_GCompare.gshow(dictGShow))(v.value0.expected)]);
      }

      ;

      if (v instanceof AENoResponse) {
        return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(["Component at index ", Data_Show.show(Data_Show.showInt)(v.value0.ix), " outputted no result; expecting ", Data_Exists.runExists(Type_GCompare.gshow(dictGShow))(v.value0.expected)]);
      }

      ;
      throw new Error("Failed pattern match at Halogen.ChainPicker (line 313, column 12 - line 327, column 10): " + [v.constructor.name]);
    });
  };

  var lastTag = function lastTag(t0) {
    return Control_Monad_State_Class.gets(Halogen_Query_HalogenM.monadStateHalogenM)(function (s0) {
      return Type_DSum.withDSum(s0.tagChain)(function (tLast) {
        return function (v) {
          return Data_Exists.mkExists(tLast);
        };
      });
    });
  };

  var initialState = function initialState(t0) {
    return function (v) {
      return {
        tagChain: Type_DSum.dsum(t0)(Type_Chain.nil)
      };
    };
  };

  var handleAction = function handleAction(dictDecide) {
    return function (dictGShow) {
      return function (dictMonadEffect) {
        return function (t0) {
          return function (act) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
              if (act instanceof AddLink) {
                return Data_Exists.runExists(function (tNewOut) {
                  return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                    return {
                      tagChain: Type_DSum.withDSum(st.tagChain)(function (tOldOut) {
                        return function (c) {
                          return Type_DSum.dsum(tNewOut)(Type_Chain.snoc(c)({
                            tagIn: tOldOut,
                            tagOut: tNewOut
                          }));
                        };
                      })
                    };
                  });
                })(act.value0);
              }

              ;

              if (act instanceof RemoveLink) {
                return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                  return {
                    tagChain: Type_DSum.withDSum(st.tagChain)(function (v) {
                      return function (c) {
                        var v1 = Type_Chain.unsnoc(c);

                        if (v1 instanceof Data_Either.Left) {
                          return Type_DSum.dsum(t0)(new Type_Chain.Nil(Type_Equiv.refl));
                        }

                        ;

                        if (v1 instanceof Data_Either.Right) {
                          return Type_Ap.withAp(v1.value0)(function (xs) {
                            return function (v2) {
                              return Type_DSum.dsum(v2.tagIn)(xs);
                            };
                          });
                        }

                        ;
                        throw new Error("Failed pattern match at Halogen.ChainPicker (line 253, column 13 - line 257, column 16): " + [v1.constructor.name]);
                      };
                    })
                  };
                });
              }

              ;

              if (act instanceof TriggerUpdate) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (st) {
                  return Type_DSum.withDSum(st.tagChain)(function (v) {
                    return function (c) {
                      return Type_Ap.withAp(Type_Chain.splitAt(act.value0.ix)(c))(function (xs) {
                        return function (ys) {
                          if (ys instanceof Type_Chain.Nil) {
                            return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))("wee woo wee woo");
                          }

                          ;

                          if (ys instanceof Type_Chain.Cons) {
                            return Type_Ap.withAp(ys.value0)(function (v1) {
                              return function (zs) {
                                return Data_Exists.runExists(function (tNew) {
                                  var v2 = Type_Equiv.decide(dictDecide)(tNew)(v1.tagOut);

                                  if (v2 instanceof Data_Maybe.Nothing) {
                                    var zNew = {
                                      tagIn: v1.tagIn,
                                      tagOut: tNew
                                    };
                                    return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)({
                                      tagChain: Type_DSum.dsum(tNew)(Type_Chain.snoc(xs)(zNew))
                                    });
                                  }

                                  ;

                                  if (v2 instanceof Data_Maybe.Just) {
                                    return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
                                  }

                                  ;
                                  throw new Error("Failed pattern match at Halogen.ChainPicker (line 284, column 21 - line 288, column 45): " + [v2.constructor.name]);
                                })(act.value0.outputType);
                              };
                            });
                          }

                          ;
                          throw new Error("Failed pattern match at Halogen.ChainPicker (line 280, column 13 - line 290, column 18): " + [ys.constructor.name]);
                        };
                      });
                    };
                  });
                });
              }

              ;
              throw new Error("Failed pattern match at Halogen.ChainPicker (line 243, column 5 - line 292, column 10): " + [act.constructor.name]);
            }())(function () {
              return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (s) {
                return Control_Bind.bindFlipped(Halogen_Query_HalogenM.bindHalogenM)(function ($140) {
                  return Halogen_Query_HalogenM.raise(ChainUpdate.create($140));
                })(lastTag(t0));
              });
            });
          };
        };
      };
    };
  };

  var functorSubQuery = new Data_Functor.Functor(function (f) {
    return function (v) {
      return new SQ(function (t) {
        var $141 = Data_Bifunctor.lmap(Data_Tuple.bifunctorTuple)(f);
        var $142 = v.value0(t);
        return function ($143) {
          return $141($142($143));
        };
      });
    };
  });
  var functorQuery = new Data_Functor.Functor(function (f) {
    return function (v) {
      return new StateSelected(function () {
        var $144 = Data_Bifunctor.lmap(Data_Tuple.bifunctorTuple)(f);
        return function ($145) {
          return $144(v.value0($145));
        };
      }());
    };
  });
  var askSelected = new StateSelected(function (x) {
    return new Data_Tuple.Tuple(x, x);
  });
  var applySubQuery = new Control_Apply.Apply(function () {
    return functorSubQuery;
  }, function (v) {
    return function (v1) {
      return new SQ(function (t0) {
        return function (s0) {
          var v2 = v.value0(t0)(s0);
          return Type_DSum.withDSum(v2.value1)(function (t1) {
            return function (s1) {
              var v3 = v1.value0(t1)(s1);
              return new Data_Tuple.Tuple(v2.value0(v3.value0), v3.value1);
            };
          });
        };
      });
    };
  });
  var applyQuery = new Control_Apply.Apply(function () {
    return functorQuery;
  }, function (v) {
    return function (v1) {
      return StateSelected.create(function (s0) {
        var v2 = v.value0(s0);
        var v3 = v1.value0(v2.value1);
        return new Data_Tuple.Tuple(v2.value0(v3.value0), v3.value1);
      });
    };
  });
  var applicativeSubQuery = new Control_Applicative.Applicative(function () {
    return applySubQuery;
  }, function (x) {
    return new SQ(function (t) {
      return function (s) {
        return new Data_Tuple.Tuple(x, Type_DSum.dsum(t)(s));
      };
    });
  });
  var applicativeQuery = new Control_Applicative.Applicative(function () {
    return applyQuery;
  }, function (x) {
    return new StateSelected(function (s) {
      return new Data_Tuple.Tuple(x, s);
    });
  });
  var _chainLink = Data_Symbol.SProxy.value;

  var assembleChain = function assembleChain(dictGOrd) {
    return function (t0) {
      var go = function go(v) {
        return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM)))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM))))(function (i) {
          return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM)))(Control_Monad_State_Class.modify_(Control_Monad_State_Trans.monadStateStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM)))(function (v1) {
            return v1 + 1 | 0;
          }))(function () {
            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM)))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM))(Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(Halogen_Query_HalogenM.monadHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
              return "chainLink";
            }))(Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()(new Data_Symbol.IsSymbol(function () {
              return "tagIn";
            }))(Type_GCompare.ordSome(dictGOrd)))()(new Data_Symbol.IsSymbol(function () {
              return "ix";
            }))(Data_Ord.ordInt)))(_chainLink)({
              ix: i,
              tagIn: Type_GCompare.mkWrEx(v.tagIn)
            })(someQuery(functorSubQuery)(v.tagIn)(subQueryAsk)))))(function (res) {
              if (res instanceof Data_Maybe.Nothing) {
                return Control_Monad_Error_Class.throwError(Control_Monad_State_Trans.monadThrowStateT(Control_Monad_Except_Trans.monadThrowExceptT(Halogen_Query_HalogenM.monadHalogenM)))(new AENoResponse({
                  ix: i,
                  expected: Data_Exists.mkExists(v.tagIn)
                }));
              }

              ;

              if (res instanceof Data_Maybe.Just && res.value0 instanceof Data_Either.Left) {
                return Control_Monad_Error_Class.throwError(Control_Monad_State_Trans.monadThrowStateT(Control_Monad_Except_Trans.monadThrowExceptT(Halogen_Query_HalogenM.monadHalogenM)))(new AEInType({
                  ix: i,
                  expected: Data_Exists.mkExists(v.tagIn),
                  actual: res.value0.value0
                }));
              }

              ;

              if (res instanceof Data_Maybe.Just && res.value0 instanceof Data_Either.Right) {
                return Type_DSum.withDSum(res.value0.value0)(function (t2) {
                  return function (r) {
                    var v1 = Type_Equiv.decide(dictGOrd.GEq0().Decide0())(v.tagOut)(t2);

                    if (v1 instanceof Data_Maybe.Nothing) {
                      return Control_Monad_Error_Class.throwError(Control_Monad_State_Trans.monadThrowStateT(Control_Monad_Except_Trans.monadThrowExceptT(Halogen_Query_HalogenM.monadHalogenM)))(new AEOutType({
                        ix: i,
                        expected: Data_Exists.mkExists(v.tagOut),
                        actual: Data_Exists.mkExists(t2)
                      }));
                    }

                    ;

                    if (v1 instanceof Data_Maybe.Just) {
                      return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM)))(Type_Equiv.equivFromF(v1.value0)(r));
                    }

                    ;
                    throw new Error("Failed pattern match at Halogen.ChainPicker (line 365, column 13 - line 368, column 52): " + [v1.constructor.name]);
                  };
                });
              }

              ;
              throw new Error("Failed pattern match at Halogen.ChainPicker (line 361, column 7 - line 369, column 12): " + [res.constructor.name]);
            });
          });
        });
      };

      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (s0) {
        return Type_DSum.withDSum(s0.tagChain)(function (tLast) {
          return function (chain) {
            return Control_Monad_Except_Trans.runExceptT(Data_Function.flip(Control_Monad_State_Trans.evalStateT(Control_Monad_Except_Trans.functorExceptT(Halogen_Query_HalogenM.functorHalogenM)))(0)(Data_Functor.map(Control_Monad_State_Trans.functorStateT(Control_Monad_Except_Trans.functorExceptT(Halogen_Query_HalogenM.functorHalogenM)))(Type_DSum.dsum(tLast))(Type_Chain.hoistChainA(Control_Monad_State_Trans.applicativeStateT(Control_Monad_Except_Trans.monadExceptT(Halogen_Query_HalogenM.monadHalogenM)))(go)(chain))));
          };
        });
      });
    };
  };

  var handleQuery = function handleQuery(dictDecide) {
    return function (dictGOrd) {
      return function (dictGShow) {
        return function (t0) {
          return function (v) {
            return Data_Functor.mapFlipped(Halogen_Query_HalogenM.functorHalogenM)(assembleChain(dictGOrd)(t0))(function (v1) {
              if (v1 instanceof Data_Either.Left) {
                return Debug_Trace.trace()(Data_Show.show(showAssembleError(dictGShow))(v1.value0))(Data_Function["const"](Data_Maybe.Nothing.value));
              }

              ;

              if (v1 instanceof Data_Either.Right) {
                return new Data_Maybe.Just(Data_Tuple.fst(v.value0(v1.value0)));
              }

              ;
              throw new Error("Failed pattern match at Halogen.ChainPicker (line 335, column 45 - line 337, column 36): " + [v1.constructor.name]);
            });
          };
        };
      };
    };
  };

  var render = function render(dictGOrd) {
    return function (dictGShow) {
      return function (pickerMap) {
        return function (t0) {
          return function (s) {
            var outList = Type_DSum.withDSum(s.tagChain)(function (v) {
              return Type_Chain.foldMapChain(Data_Monoid.monoidArray)(function (v1) {
                return [Data_Exists.mkExists(v1.tagOut)];
              });
            });
            var inList = Type_DSum.withDSum(s.tagChain)(function (v) {
              return Type_Chain.foldMapChain(Data_Monoid.monoidArray)(function (v1) {
                return [Data_Exists.mkExists(v1.tagIn)];
              });
            });
            return Halogen_HTML_Elements.div_([Halogen_HTML_Elements.ul([Halogen_Util.classProp("op-list")])(Data_Array.catMaybes(Data_Function.flip(Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray))(inList)(function (i) {
              return function (sT) {
                return Data_Exists.runExists(function (t) {
                  return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Newtype.unwrap(Data_Functor_Compose.newtypeCompose)(Type_DProd.runDProd(pickerMap)(t)))(function (v) {
                    return Halogen_HTML_Elements.li_([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                      return "chainLink";
                    }))(Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()(new Data_Symbol.IsSymbol(function () {
                      return "tagIn";
                    }))(Type_GCompare.ordSome(dictGOrd)))()(new Data_Symbol.IsSymbol(function () {
                      return "ix";
                    }))(Data_Ord.ordInt)))(_chainLink)({
                      ix: i,
                      tagIn: Type_GCompare.mkWrEx(t)
                    })(Halogen_Util.hoistQuery(unSomeQuery(applicativeSubQuery)(dictGOrd.GEq0().Decide0())(t))(v.component))(Data_Unit.unit)(function (tOut) {
                      return new Data_Maybe.Just(new TriggerUpdate({
                        ix: i,
                        outputType: tOut
                      }));
                    })]);
                  });
                })(sT);
              };
            }))), Halogen_HTML_Elements.div([Halogen_Util.classProp("op-buttons")])(Data_Array.catMaybes([Data_Exists.runExists(function (lastOut) {
              return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Newtype.unwrap(Data_Functor_Compose.newtypeCompose)(Type_DProd.runDProd(pickerMap)(lastOut)))(function (v) {
                return Halogen_HTML_Elements.button([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropButtonType)(DOM_HTML_Indexed_ButtonType.ButtonButton.value), Halogen_HTML_Events.onClick(function (v1) {
                  return new Data_Maybe.Just(new AddLink(v.initialOut));
                }), Halogen_Util.classProp("add-op")])([Halogen_HTML_Core.text("Add")]);
              });
            })(Data_Maybe.fromMaybe(Data_Exists.mkExists(t0))(Data_Array.last(outList))), Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Array.last(inList))(function (tLast) {
              return Halogen_HTML_Elements.button([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropButtonType)(DOM_HTML_Indexed_ButtonType.ButtonButton.value), Halogen_HTML_Events.onClick(function (v) {
                return new Data_Maybe.Just(new RemoveLink(tLast));
              }), Halogen_Util.classProp("remove-op")])([Halogen_HTML_Core.text("Remove")]);
            })]))]);
          };
        };
      };
    };
  };

  var component = function component(dictGOrd) {
    return function (dictGShow) {
      return function (dictMonadEffect) {
        return function (picker) {
          return function (t0) {
            return Halogen_Component.mkComponent({
              initialState: initialState(t0),
              render: render(dictGOrd)(dictGShow)(picker)(t0),
              "eval": Halogen_Component.mkEval({
                handleAction: handleAction(dictGOrd.GEq0().Decide0())(dictGShow)(dictMonadEffect)(t0),
                handleQuery: handleQuery(dictGOrd.GEq0().Decide0())(dictGOrd)(dictGShow)(t0),
                receive: Halogen_Component.defaultEval.receive,
                initialize: Halogen_Component.defaultEval.initialize,
                finalize: Halogen_Component.defaultEval.finalize
              })
            });
          };
        };
      };
    };
  };

  var wrappedComponent = function wrappedComponent(dictGOrd) {
    return function (dictGShow) {
      return function (dictMonadEffect) {
        return function (picker) {
          return function (t0) {
            return Halogen_Util.hoistQuery(unSomeQuery(applicativeQuery)(dictGOrd.GEq0().Decide0())(t0))(component(dictGOrd)(dictGShow)(dictMonadEffect)(picker)(t0));
          };
        };
      };
    };
  };

  exports["askSelected"] = askSelected;
  exports["wrappedComponent"] = wrappedComponent;
  exports["someQuery"] = someQuery;
  exports["functorSubQuery"] = functorSubQuery;
  exports["functorQuery"] = functorQuery;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Corona.Chart.UI.Projection"] = $PS["Corona.Chart.UI.Projection"] || {};
  var exports = $PS["Corona.Chart.UI.Projection"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Corona_Chart = $PS["Corona.Chart"];
  var Corona_Chart_UI_Op = $PS["Corona.Chart.UI.Op"];
  var D3_Scatter_Type = $PS["D3.Scatter.Type"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Exists = $PS["Data.Exists"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Functor_Compose = $PS["Data.Functor.Compose"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Show = $PS["Data.Show"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Class_Console = $PS["Effect.Class.Console"];
  var Halogen_ChainPicker = $PS["Halogen.ChainPicker"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Util = $PS["Halogen.Util"];
  var Type_DSum = $PS["Type.DSum"];
  var Type_GCompare = $PS["Type.GCompare"];

  var QueryOp = function () {
    function QueryOp(value0) {
      this.value0 = value0;
    }

    ;

    QueryOp.create = function (value0) {
      return new QueryOp(value0);
    };

    return QueryOp;
  }();

  var Update = function () {
    function Update(value0) {
      this.value0 = value0;
    }

    ;

    Update.create = function (value0) {
      return new Update(value0);
    };

    return Update;
  }();

  var SetBase = function () {
    function SetBase(value0) {
      this.value0 = value0;
    }

    ;

    SetBase.create = function (value0) {
      return new SetBase(value0);
    };

    return SetBase;
  }();

  var SetOps = function () {
    function SetOps(value0) {
      this.value0 = value0;
    }

    ;

    SetOps.create = function (value0) {
      return new SetOps(value0);
    };

    return SetOps;
  }();

  var SetNumScale = function () {
    function SetNumScale(value0) {
      this.value0 = value0;
    }

    ;

    SetNumScale.create = function (value0) {
      return new SetNumScale(value0);
    };

    return SetNumScale;
  }();

  var handleQuery = function handleQuery(v) {
    return Data_Functor.map(Halogen_Query_HalogenM.functorHalogenM)(Data_Maybe.Just.create)(Control_Monad_State_Class.state(Halogen_Query_HalogenM.monadStateHalogenM)(v.value0));
  };

  var chainPicker = function chainPicker(dictMonadEffect) {
    return Halogen_ChainPicker.wrappedComponent(D3_Scatter_Type.gordSType)(D3_Scatter_Type.gshowSType)(dictMonadEffect)(function (t) {
      return Data_Functor_Compose.Compose(Data_Maybe.Just.create({
        component: Halogen_Util.trimapComponent(Halogen_ChainPicker.functorSubQuery)(function (v) {
          return new Corona_Chart_UI_Op.QueryOp(v.value0);
        })(Control_Category.identity(Control_Category.categoryFn))(function (v) {
          return v.value0;
        })(Corona_Chart_UI_Op.component(dictMonadEffect)(t)),
        initialOut: function () {
          if (t instanceof D3_Scatter_Type.SDay) {
            return Data_Exists.mkExists(D3_Scatter_Type.sDays);
          }

          ;

          if (t instanceof D3_Scatter_Type.SDays) {
            return Data_Exists.mkExists(D3_Scatter_Type.sDays);
          }

          ;

          if (t instanceof D3_Scatter_Type.SInt) {
            return Data_Exists.mkExists(D3_Scatter_Type.sInt);
          }

          ;

          if (t instanceof D3_Scatter_Type.SNumber) {
            return Data_Exists.mkExists(D3_Scatter_Type.sNumber);
          }

          ;

          if (t instanceof D3_Scatter_Type.SPercent) {
            return Data_Exists.mkExists(D3_Scatter_Type.sPercent);
          }

          ;
          throw new Error("Failed pattern match at Corona.Chart.UI.Projection (line 168, column 23 - line 173, column 47): " + [t.constructor.name]);
        }()
      }));
    });
  };

  var _opselect = Data_Symbol.SProxy.value;

  var handleAction = function handleAction(dictMonadEffect) {
    return function (act) {
      return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
        if (act instanceof SetBase) {
          return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
            var $32 = {};

            for (var $33 in st) {
              if ({}.hasOwnProperty.call(st, $33)) {
                $32[$33] = st[$33];
              }

              ;
            }

            ;
            $32.projection = Data_Exists.runExists(Data_Function.flip(Corona_Chart.setBase)(st.projection))(act.value0);
            return $32;
          });
        }

        ;

        if (act instanceof SetOps) {
          return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.gets(Halogen_Query_HalogenM.monadStateHalogenM)(function (v) {
            return v.projection;
          }))(function (dsp) {
            return Type_DSum.withDSum(dsp)(function (tOut) {
              return function (proj) {
                return Corona_Chart.withProjection(proj)(function (pr) {
                  var tIn = Corona_Chart.baseType(pr.base);
                  return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
                    return "opselect";
                  }))(Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()(new Data_Symbol.IsSymbol(function () {
                    return "tagIn";
                  }))(Type_GCompare.ordSome(D3_Scatter_Type.gordSType))))(_opselect)({
                    tagIn: Type_GCompare.mkWrEx(tIn)
                  })(Halogen_ChainPicker.someQuery(Halogen_ChainPicker.functorQuery)(tIn)(Halogen_ChainPicker.askSelected)))(function (qres) {
                    if (qres instanceof Data_Maybe.Nothing) {
                      return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))("no response from component");
                    }

                    ;

                    if (qres instanceof Data_Maybe.Just && qres.value0 instanceof Data_Either.Left) {
                      return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))("type mismatch: " + Data_Exists.runExists(Data_Show.show(D3_Scatter_Type.showSType))(qres.value0.value0));
                    }

                    ;

                    if (qres instanceof Data_Maybe.Just && qres.value0 instanceof Data_Either.Right) {
                      return Type_DSum.withDSum(qres.value0.value0)(function (tNewOut) {
                        return function (chain) {
                          return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
                            var $39 = {};

                            for (var $40 in st) {
                              if ({}.hasOwnProperty.call(st, $40)) {
                                $39[$40] = st[$40];
                              }

                              ;
                            }

                            ;
                            $39.projection = Type_DSum.dsum(tNewOut)(Corona_Chart.projection({
                              base: pr.base,
                              operations: chain
                            }));
                            return $39;
                          });
                        };
                      });
                    }

                    ;
                    throw new Error("Failed pattern match at Corona.Chart.UI.Projection (line 191, column 13 - line 199, column 16): " + [qres.constructor.name]);
                  });
                });
              };
            });
          });
        }

        ;

        if (act instanceof SetNumScale) {
          return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v) {
            var $45 = {};

            for (var $46 in v) {
              if ({}.hasOwnProperty.call(v, $46)) {
                $45[$46] = v[$46];
              }

              ;
            }

            ;
            $45.numScale = act.value0;
            return $45;
          });
        }

        ;
        throw new Error("Failed pattern match at Corona.Chart.UI.Projection (line 182, column 5 - line 202, column 54): " + [act.constructor.name]);
      }())(function () {
        return Control_Bind.bindFlipped(Halogen_Query_HalogenM.bindHalogenM)(function ($59) {
          return Halogen_Query_HalogenM.raise(Update.create($59));
        })(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM));
      });
    };
  };

  var render = function render(dictMonadEffect) {
    return function (label) {
      return function (aState) {
        var indexToNScale = function indexToNScale(v) {
          if (v === 0) {
            return new Data_Maybe.Just(function ($60) {
              return D3_Scatter_Type.Linear.create(Data_Either.Right.create($60));
            });
          }

          ;

          if (v === 1) {
            return new Data_Maybe.Just(D3_Scatter_Type.Log.create);
          }

          ;
          return Data_Maybe.Nothing.value;
        };

        var indexToBase = function indexToBase(v) {
          if (v === 0) {
            return new Data_Maybe.Just(Data_Exists.mkExists(Corona_Chart.bTime));
          }

          ;

          if (v === 1) {
            return new Data_Maybe.Just(Data_Exists.mkExists(Corona_Chart.bConfirmed));
          }

          ;

          if (v === 2) {
            return new Data_Maybe.Just(Data_Exists.mkExists(Corona_Chart.bDeaths));
          }

          ;

          if (v === 3) {
            return new Data_Maybe.Just(Data_Exists.mkExists(Corona_Chart.bRecovered));
          }

          ;
          return Data_Maybe.Nothing.value;
        };

        return Halogen_HTML_Elements.div([Halogen_Util.classProp("axis-options")])([Halogen_HTML_Elements.h3_([Halogen_HTML_Core.text(label)]), Halogen_HTML_Elements.div([Halogen_Util.classProp("base-projection")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Base Projection")]), Halogen_HTML_Elements.select([Halogen_HTML_Events.onSelectedIndexChange(function () {
          var $61 = Data_Functor.map(Data_Maybe.functorMaybe)(SetBase.create);
          return function ($62) {
            return $61(indexToBase($62));
          };
        }())])(Data_Functor.mapFlipped(Data_Functor.functorArray)(Corona_Chart.allBaseProjections)(function (sbp) {
          return Data_Exists.runExists(function (bp) {
            var isSelected = Type_DSum.withDSum(aState.projection)(function (v) {
              return function (pr) {
                return Data_Eq.eq(Type_GCompare.eqSome(Corona_Chart.geqBaseProjection))(sbp)(Corona_Chart.baseProjection(pr));
              };
            });
            return Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(isSelected)])([Halogen_HTML_Core.text(Corona_Chart.baseProjectionLabel(bp))]);
          })(sbp);
        }))]), Halogen_HTML_Elements.div([Halogen_Util.classProp("axis-op-chain")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Transformations")]), Type_DSum.withDSum(aState.projection)(function (v) {
          return function (spr) {
            return Corona_Chart.withProjection(spr)(function (pr) {
              var tBase = Corona_Chart.baseType(pr.base);
              return Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
                return "opselect";
              }))(Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()(new Data_Symbol.IsSymbol(function () {
                return "tagIn";
              }))(Type_GCompare.ordSome(D3_Scatter_Type.gordSType))))(_opselect)({
                tagIn: Type_GCompare.mkWrEx(tBase)
              })(chainPicker(dictMonadEffect)(tBase))(Data_Unit.unit)(function (v1) {
                return new Data_Maybe.Just(new SetOps(v1.value0));
              });
            });
          };
        })]), Halogen_HTML_Elements.div([Halogen_Util.classProp("axis-scale")])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text("Scale")]), Type_DSum.withDSum(aState.projection)(function (t) {
          return function (v) {
            var v1 = D3_Scatter_Type.toNType(t);

            if (v1 instanceof Data_Either.Left && v1.value0 instanceof Data_Either.Left) {
              return Halogen_HTML_Elements.select_([Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(true)])([Halogen_HTML_Core.text("Date")])]);
            }

            ;

            if (v1 instanceof Data_Either.Left && v1.value0 instanceof Data_Either.Right) {
              return Halogen_HTML_Elements.select_([Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(true)])([Halogen_HTML_Core.text("Days")])]);
            }

            ;

            if (v1 instanceof Data_Either.Right) {
              return Halogen_HTML_Elements.select([Halogen_HTML_Events.onSelectedIndexChange(function () {
                var $63 = Data_Functor.map(Data_Maybe.functorMaybe)(SetNumScale.create);
                return function ($64) {
                  return $63(indexToNScale($64));
                };
              }())])([Halogen_HTML_Elements.option_([Halogen_HTML_Core.text("Linear")]), Halogen_HTML_Elements.option([Halogen_HTML_Properties.selected(true)])([Halogen_HTML_Core.text("Log")])]);
            }

            ;
            throw new Error("Failed pattern match at Corona.Chart.UI.Projection (line 125, column 11 - line 134, column 16): " + [v1.constructor.name]);
          };
        })])]);
      };
    };
  };

  var component = function component(dictMonadEffect) {
    return function (label) {
      return Halogen_Component.mkComponent({
        initialState: Control_Category.identity(Control_Category.categoryFn),
        render: render(dictMonadEffect)(label),
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction(dictMonadEffect),
          handleQuery: handleQuery,
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      });
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Void"] = $PS["Data.Void"] || {};
  var exports = $PS["Data.Void"];

  var absurd = function absurd(a) {
    var spin = function spin($copy_v) {
      var $tco_result;

      function $tco_loop(v) {
        $copy_v = v;
        return;
      }

      ;

      while (!false) {
        $tco_result = $tco_loop($copy_v);
      }

      ;
      return $tco_result;
    };

    return spin(a);
  };

  exports["absurd"] = absurd;
})(PS);

(function (exports) {
  "use strict";

  exports.unsafeParseInt = function unsafeParseInt(input, base) {
    return parseInt(input, base);
  };
})(PS["Data.Int.Parse"] = PS["Data.Int.Parse"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.Int.Parse"] = $PS["Data.Int.Parse"] || {};
  var exports = $PS["Data.Int.Parse"];
  var $foreign = $PS["Data.Int.Parse"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Global = $PS["Global"];

  var toRadix = function toRadix(x) {
    var $1 = x < 2;

    if ($1) {
      return 2;
    }

    ;
    var $2 = x > 36;

    if ($2) {
      return 36;
    }

    ;
    return x;
  };

  var $$parseInt = function $$parseInt(s) {
    return function (v) {
      var x = $foreign.unsafeParseInt(s, v);
      var $5 = Global["isNaN"](x);

      if ($5) {
        return Data_Maybe.Nothing.value;
      }

      ;
      return new Data_Maybe.Just(Data_Int.round(x));
    };
  };

  exports["toRadix"] = toRadix;
  exports["parseInt"] = $$parseInt;
})(PS);

(function (exports) {
  "use strict";

  exports._indexOf = function (just) {
    return function (nothing) {
      return function (x) {
        return function (s) {
          var i = s.indexOf(x);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
})(PS["Data.String.CodeUnits"] = PS["Data.String.CodeUnits"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.String.CodeUnits"] = $PS["Data.String.CodeUnits"] || {};
  var exports = $PS["Data.String.CodeUnits"];
  var $foreign = $PS["Data.String.CodeUnits"];
  var Data_Maybe = $PS["Data.Maybe"];
  var indexOf = $foreign["_indexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

  var contains = function contains(pat) {
    var $16 = indexOf(pat);
    return function ($17) {
      return Data_Maybe.isJust($16($17));
    };
  };

  exports["contains"] = contains;
})(PS);

(function (exports) {
  "use strict";

  exports["regex'"] = function (left) {
    return function (right) {
      return function (s1) {
        return function (s2) {
          try {
            return right(new RegExp(s1, s2));
          } catch (e) {
            return left(e.message);
          }
        };
      };
    };
  };

  exports.replace = function (r) {
    return function (s1) {
      return function (s2) {
        return s2.replace(r, s1);
      };
    };
  };
})(PS["Data.String.Regex"] = PS["Data.String.Regex"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.String.Regex"] = $PS["Data.String.Regex"] || {};
  var exports = $PS["Data.String.Regex"];
  var $foreign = $PS["Data.String.Regex"];
  var Data_Either = $PS["Data.Either"];

  var renderFlags = function renderFlags(v) {
    return function () {
      if (v.value0.global) {
        return "g";
      }

      ;
      return "";
    }() + (function () {
      if (v.value0.ignoreCase) {
        return "i";
      }

      ;
      return "";
    }() + (function () {
      if (v.value0.multiline) {
        return "m";
      }

      ;
      return "";
    }() + (function () {
      if (v.value0.sticky) {
        return "y";
      }

      ;
      return "";
    }() + function () {
      if (v.value0.unicode) {
        return "u";
      }

      ;
      return "";
    }())));
  };

  var regex = function regex(s) {
    return function (f) {
      return $foreign["regex'"](Data_Either.Left.create)(Data_Either.Right.create)(s)(renderFlags(f));
    };
  };

  exports["regex"] = regex;
  exports["replace"] = $foreign.replace;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Data.String.Regex.Flags"] = $PS["Data.String.Regex.Flags"] || {};
  var exports = $PS["Data.String.Regex.Flags"];

  var RegexFlags = function () {
    function RegexFlags(value0) {
      this.value0 = value0;
    }

    ;

    RegexFlags.create = function (value0) {
      return new RegexFlags(value0);
    };

    return RegexFlags;
  }();

  var global = new RegexFlags({
    global: true,
    ignoreCase: false,
    multiline: false,
    sticky: false,
    unicode: false
  });
  exports["global"] = global;
})(PS);

(function (exports) {
  "use strict";

  exports.toArray = function (list) {
    return function () {
      return [].slice.call(list);
    };
  };
})(PS["Web.DOM.HTMLCollection"] = PS["Web.DOM.HTMLCollection"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.DOM.HTMLCollection"] = $PS["Web.DOM.HTMLCollection"] || {};
  var exports = $PS["Web.DOM.HTMLCollection"];
  var $foreign = $PS["Web.DOM.HTMLCollection"];
  exports["toArray"] = $foreign.toArray;
})(PS);

(function (exports) {
  "use strict"; // ----------------------------------------------------------------------------

  exports.value = function (option) {
    return function () {
      return option.value;
    };
  };
})(PS["Web.HTML.HTMLOptionElement"] = PS["Web.HTML.HTMLOptionElement"] || {});

(function (exports) {
  "use strict";

  exports._unsafeReadProtoTagged = function (nothing, just, name, value) {
    if (typeof window !== "undefined") {
      var ty = window[name];

      if (ty != null && value instanceof ty) {
        return just(value);
      }

      return nothing;
    }

    var obj = value;

    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;

      if (constructorName === name) {
        return just(value);
      } else if (constructorName === "Object") {
        return nothing;
      }

      obj = proto;
    }

    return nothing;
  };
})(PS["Web.Internal.FFI"] = PS["Web.Internal.FFI"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.Internal.FFI"] = $PS["Web.Internal.FFI"] || {};
  var exports = $PS["Web.Internal.FFI"];
  var $foreign = $PS["Web.Internal.FFI"];
  var Data_Maybe = $PS["Data.Maybe"];

  var unsafeReadProtoTagged = function unsafeReadProtoTagged(name) {
    return function (value) {
      return $foreign["_unsafeReadProtoTagged"](Data_Maybe.Nothing.value, Data_Maybe.Just.create, name, value);
    };
  };

  exports["unsafeReadProtoTagged"] = unsafeReadProtoTagged;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.HTMLOptionElement"] = $PS["Web.HTML.HTMLOptionElement"] || {};
  var exports = $PS["Web.HTML.HTMLOptionElement"];
  var $foreign = $PS["Web.HTML.HTMLOptionElement"];
  var Web_Internal_FFI = $PS["Web.Internal.FFI"];
  var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLOptionElement");
  exports["fromElement"] = fromElement;
  exports["value"] = $foreign.value;
})(PS);

(function (exports) {
  "use strict"; // ----------------------------------------------------------------------------

  exports.selectedOptions = function (select) {
    return function () {
      return select.selectedOptions;
    };
  };
})(PS["Web.HTML.HTMLSelectElement"] = PS["Web.HTML.HTMLSelectElement"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.HTMLSelectElement"] = $PS["Web.HTML.HTMLSelectElement"] || {};
  var exports = $PS["Web.HTML.HTMLSelectElement"];
  var $foreign = $PS["Web.HTML.HTMLSelectElement"];
  var Web_Internal_FFI = $PS["Web.Internal.FFI"];
  var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLSelectElement");
  exports["fromElement"] = fromElement;
  exports["selectedOptions"] = $foreign.selectedOptions;
})(PS);

(function (exports) {
  "use strict";

  exports.buttons = function (e) {
    return e.buttons;
  };
})(PS["Web.UIEvent.MouseEvent"] = PS["Web.UIEvent.MouseEvent"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.UIEvent.MouseEvent"] = $PS["Web.UIEvent.MouseEvent"] || {};
  var exports = $PS["Web.UIEvent.MouseEvent"];
  var $foreign = $PS["Web.UIEvent.MouseEvent"];
  exports["buttons"] = $foreign.buttons;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.MultiSelect"] = $PS["Halogen.MultiSelect"] || {};
  var exports = $PS["Halogen.MultiSelect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Maybe_Trans = $PS["Control.Monad.Maybe.Trans"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_MonadZero = $PS["Control.MonadZero"];
  var Control_Plus = $PS["Control.Plus"];
  var DOM_HTML_Indexed_ButtonType = $PS["DOM.HTML.Indexed.ButtonType"];
  var DOM_HTML_Indexed_InputType = $PS["DOM.HTML.Indexed.InputType"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_FunctorWithIndex = $PS["Data.FunctorWithIndex"];
  var Data_Int_Parse = $PS["Data.Int.Parse"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Set = $PS["Data.Set"];
  var Data_Show = $PS["Data.Show"];
  var Data_String_CodeUnits = $PS["Data.String.CodeUnits"];
  var Data_String_Common = $PS["Data.String.Common"];
  var Data_String_Regex = $PS["Data.String.Regex"];
  var Data_String_Regex_Flags = $PS["Data.String.Regex.Flags"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Effect = $PS["Effect"];
  var Effect_Class = $PS["Effect.Class"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Events = $PS["Halogen.HTML.Events"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Util = $PS["Halogen.Util"];
  var Web_DOM_HTMLCollection = $PS["Web.DOM.HTMLCollection"];
  var Web_HTML_HTMLOptionElement = $PS["Web.HTML.HTMLOptionElement"];
  var Web_HTML_HTMLSelectElement = $PS["Web.HTML.HTMLSelectElement"];
  var Web_UIEvent_MouseEvent = $PS["Web.UIEvent.MouseEvent"];

  var SelectionChanged = function () {
    function SelectionChanged(value0) {
      this.value0 = value0;
    }

    ;

    SelectionChanged.create = function (value0) {
      return new SelectionChanged(value0);
    };

    return SelectionChanged;
  }();

  var AskSelected = function () {
    function AskSelected(value0) {
      this.value0 = value0;
    }

    ;

    AskSelected.create = function (value0) {
      return new AskSelected(value0);
    };

    return AskSelected;
  }();

  var SetState = function () {
    function SetState(value0) {
      this.value0 = value0;
    }

    ;

    SetState.create = function (value0) {
      return new SetState(value0);
    };

    return SetState;
  }();

  var AddValues = function () {
    function AddValues() {}

    ;
    AddValues.value = new AddValues();
    return AddValues;
  }();

  var RemoveValue = function () {
    function RemoveValue(value0) {
      this.value0 = value0;
    }

    ;

    RemoveValue.create = function (value0) {
      return new RemoveValue(value0);
    };

    return RemoveValue;
  }();

  var RemoveAll = function () {
    function RemoveAll() {}

    ;
    RemoveAll.value = new RemoveAll();
    return RemoveAll;
  }();

  var SetFilter = function () {
    function SetFilter(value0) {
      this.value0 = value0;
    }

    ;

    SetFilter.create = function (value0) {
      return new SetFilter(value0);
    };

    return SetFilter;
  }();

  var selRef = "multiselect-sel";

  var getSelected = function getSelected(dictMonadState) {
    return Control_Bind.bind(dictMonadState.Monad0().Bind1())(Control_Monad_State_Class.get(dictMonadState))(function (st) {
      return Control_Applicative.pure(dictMonadState.Monad0().Applicative0())(Data_Array.fromFoldable(Data_List_Types.foldableList)(Data_List.mapMaybe(function (i) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (lv) {
          return lv.value;
        })(Data_Array.index(st.options)(i));
      })(Data_List.fromFoldable(Data_Set.foldableSet)(st.selected))));
    });
  };

  var handleQuery = function handleQuery(v) {
    if (v instanceof AskSelected) {
      return Data_Functor.map(Halogen_Query_HalogenM.functorHalogenM)(function ($34) {
        return Data_Maybe.Just.create(v.value0($34));
      })(getSelected(Halogen_Query_HalogenM.monadStateHalogenM));
    }

    ;

    if (v instanceof SetState) {
      return Control_Monad_State_Class.state(Halogen_Query_HalogenM.monadStateHalogenM)(function (s) {
        var sr = v.value0(s);
        return new Data_Tuple.Tuple(new Data_Maybe.Just(sr.next), sr["new"]);
      });
    }

    ;
    throw new Error("Failed pattern match at Halogen.MultiSelect (line 153, column 15 - line 156, column 76): " + [v.constructor.name]);
  };

  var raiseChange = Control_Bind.bindFlipped(Halogen_Query_HalogenM.bindHalogenM)(function ($35) {
    return Halogen_Query_HalogenM.raise(SelectionChanged.create($35));
  })(getSelected(Halogen_Query_HalogenM.monadStateHalogenM));

  var handleAction = function handleAction(dictMonadEffect) {
    return function (v) {
      if (v instanceof AddValues) {
        return Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Control_Monad_Maybe_Trans.runMaybeT(Control_Bind.bind(Control_Monad_Maybe_Trans.bindMaybeT(Halogen_Query_HalogenM.monadHalogenM))(Control_Monad_Maybe_Trans.MaybeT(Halogen_Query_HalogenM.getRef(selRef)))(function (e) {
          return Control_Bind.bind(Control_Monad_Maybe_Trans.bindMaybeT(Halogen_Query_HalogenM.monadHalogenM))(Data_Maybe.maybe(Control_Plus.empty(Control_Monad_Maybe_Trans.plusMaybeT(Halogen_Query_HalogenM.monadHalogenM)))(Control_Applicative.pure(Control_Monad_Maybe_Trans.applicativeMaybeT(Halogen_Query_HalogenM.monadHalogenM)))(Web_HTML_HTMLSelectElement.fromElement(e)))(function (se) {
            return Control_Bind.bind(Control_Monad_Maybe_Trans.bindMaybeT(Halogen_Query_HalogenM.monadHalogenM))(Effect_Class.liftEffect(Control_Monad_Maybe_Trans.monadEffectMaybe(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect)))(Control_Bind.bindFlipped(Effect.bindEffect)(Web_DOM_HTMLCollection.toArray)(Web_HTML_HTMLSelectElement.selectedOptions(se))))(function (es) {
              var opts = Data_Array.mapMaybe(Web_HTML_HTMLOptionElement.fromElement)(es);
              return Control_Bind.bind(Control_Monad_Maybe_Trans.bindMaybeT(Halogen_Query_HalogenM.monadHalogenM))(Effect_Class.liftEffect(Control_Monad_Maybe_Trans.monadEffectMaybe(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect)))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Web_HTML_HTMLOptionElement.value)(opts)))(function (vals) {
                var valInts = Data_Set.fromFoldable(Data_Foldable.foldableArray)(Data_Ord.ordInt)(Data_Array.mapMaybe(Data_Function.flip(Data_Int_Parse["parseInt"])(Data_Int_Parse.toRadix(10)))(vals));
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Maybe_Trans.bindMaybeT(Halogen_Query_HalogenM.monadHalogenM))(Control_Monad_State_Class.modify_(Control_Monad_Maybe_Trans.monadStateMaybeT(Halogen_Query_HalogenM.monadStateHalogenM))(function (s) {
                  var $16 = {};

                  for (var $17 in s) {
                    if ({}.hasOwnProperty.call(s, $17)) {
                      $16[$17] = s[$17];
                    }

                    ;
                  }

                  ;
                  $16.selected = Data_Set.union(Data_Ord.ordInt)(valInts)(s.selected);
                  return $16;
                }))(function () {
                  return Control_Monad_Trans_Class.lift(Control_Monad_Maybe_Trans.monadTransMaybeT)(Halogen_Query_HalogenM.monadHalogenM)(raiseChange);
                });
              });
            });
          });
        })));
      }

      ;

      if (v instanceof RemoveValue) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (s) {
          var $19 = {};

          for (var $20 in s) {
            if ({}.hasOwnProperty.call(s, $20)) {
              $19[$20] = s[$20];
            }

            ;
          }

          ;
          $19.selected = Data_Set["delete"](Data_Ord.ordInt)(v.value0)(s.selected);
          return $19;
        }))(function () {
          return raiseChange;
        });
      }

      ;

      if (v instanceof RemoveAll) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (s) {
          return {
            options: s.options,
            selected: Data_Set.empty,
            filter: s.filter
          };
        }))(function () {
          return raiseChange;
        });
      }

      ;

      if (v instanceof SetFilter) {
        return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (s) {
          var $23 = {};

          for (var $24 in s) {
            if ({}.hasOwnProperty.call(s, $24)) {
              $23[$24] = s[$24];
            }

            ;
          }

          ;
          $23.filter = v.value0;
          return $23;
        });
      }

      ;
      throw new Error("Failed pattern match at Halogen.MultiSelect (line 134, column 16 - line 150, column 55): " + [v.constructor.name]);
    };
  };

  var applyFilter = function applyFilter(s) {
    return function (v) {
      if (Data_String_Common["null"](s)) {
        return true;
      }

      ;

      if (Data_Boolean.otherwise) {
        var norm = function () {
          var v2 = Data_String_Regex.regex("\\W")(Data_String_Regex_Flags.global);

          if (v2 instanceof Data_Either.Left) {
            return Control_Category.identity(Control_Category.categoryFn);
          }

          ;

          if (v2 instanceof Data_Either.Right) {
            var $36 = Data_String_Regex.replace(v2.value0)("");
            return function ($37) {
              return $36(Data_String_Common.toLower($37));
            };
          }

          ;
          throw new Error("Failed pattern match at Halogen.MultiSelect (line 180, column 12 - line 182, column 55): " + [v2.constructor.name]);
        }();

        return Data_String_CodeUnits.contains(norm(s))(norm(v));
      }

      ;
      throw new Error("Failed pattern match at Halogen.MultiSelect (line 172, column 1 - line 175, column 15): " + [s.constructor.name, v.constructor.name]);
    };
  };

  var render = function render(lab) {
    return function (st) {
      var selectedItems = Data_Array.fromFoldable(Data_List_Types.foldableList)(Data_List.mapMaybe(function (i) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (lv) {
          return {
            ix: i,
            label: lv.label
          };
        })(Data_Array.index(st.options)(i));
      })(Data_List.fromFoldable(Data_Set.foldableSet)(st.selected)));
      var renderSelected = [Halogen_HTML_Elements.ul([Halogen_Util.classProp("selected-list")])(Data_Functor.map(Data_Functor.functorArray)(function (il) {
        return Halogen_HTML_Elements.li([Halogen_Util.classProp("selected-item"), Halogen_HTML_Events.onClick(function (e) {
          var $32 = Web_UIEvent_MouseEvent.buttons(e) === 0;

          if ($32) {
            return new Data_Maybe.Just(new RemoveValue(il.ix));
          }

          ;
          return Data_Maybe.Nothing.value;
        })])([Halogen_HTML_Elements.span_([Halogen_HTML_Core.text(il.label)])]);
      })(selectedItems)), Halogen_HTML_Elements.button([Halogen_HTML_Events.onClick(function (v) {
        return new Data_Maybe.Just(RemoveAll.value);
      }), Halogen_Util.classProp("clear-button")])([Halogen_HTML_Core.text("Clear Selection")])];
      return Halogen_HTML_Elements.div([Halogen_Util.classProp("multiselect")])([Halogen_HTML_Elements.h3_([Halogen_HTML_Core.text(lab)]), Halogen_HTML_Elements.div([Halogen_Util.classProp("select-options grid__col grid__col--1-of-2")])([Halogen_HTML_Elements.input([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropInputType)(DOM_HTML_Indexed_InputType.InputText.value), Halogen_HTML_Properties.placeholder("type to filter"), Halogen_HTML_Events.onValueInput(function ($38) {
        return Data_Maybe.Just.create(SetFilter.create($38));
      })]), Halogen_HTML_Elements.select([Halogen_HTML_Properties.multiple(true), Halogen_HTML_Properties.ref(selRef)])(Data_Array.catMaybes(Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray)(function (i) {
        return function (o) {
          return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_MonadZero.guard(Data_Maybe.monadZeroMaybe)(!Data_Set.member(Data_Ord.ordInt)(i)(st.selected)))(function () {
            return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_MonadZero.guard(Data_Maybe.monadZeroMaybe)(applyFilter(st.filter)(o.label)))(function () {
              return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Halogen_HTML_Elements.option([Halogen_HTML_Properties.value(Data_Show.show(Data_Show.showInt)(i))])([Halogen_HTML_Core.text(o.label)]));
            });
          });
        };
      })(st.options))), Halogen_HTML_Elements.button([Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropButtonType)(DOM_HTML_Indexed_ButtonType.ButtonButton.value), Halogen_HTML_Events.onClick(function (v) {
        return new Data_Maybe.Just(AddValues.value);
      }), Halogen_Util.classProp("add-button")])([Halogen_HTML_Core.text("Add")])]), Halogen_HTML_Elements.div([Halogen_Util.classProp("selected grid__col grid__col--1-of-2")])(function () {
        var $33 = Data_Foldable["null"](Data_Set.foldableSet)(st.selected);

        if ($33) {
          return [Halogen_HTML_Elements.span([Halogen_Util.classProp("none-selected")])([Halogen_HTML_Core.text("(nothing selected yet)")])];
        }

        ;
        return renderSelected;
      }())]);
    };
  };

  var component = function component(dictMonadEffect) {
    return function (lab) {
      return Halogen_Component.mkComponent({
        initialState: Control_Category.identity(Control_Category.categoryFn),
        render: render(lab),
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction(dictMonadEffect),
          handleQuery: handleQuery,
          receive: Halogen_Component.defaultEval.receive,
          initialize: Halogen_Component.defaultEval.initialize,
          finalize: Halogen_Component.defaultEval.finalize
        })
      });
    };
  };

  exports["component"] = component;
})(PS);

(function (exports) {
  "use strict";

  var msPerDay = 24 * 60 * 60 * 1000;
  var mjdShift = 40587;

  var fromMJD = function fromMJD(mjd) {
    return new Date((mjd - mjdShift) * msPerDay);
  };

  var findByX = function findByX(ps, x, lo) {
    return d3.bisector(function (p) {
      return p.x;
    }).left(ps, x, lo);
  };

  var mantissa = function mantissa(x, n) {
    return Math.round(x / Math.pow(10, exponent(x) - n + 1));
  };

  var exponent = function exponent(x) {
    return Math.floor(Math.log10(x));
  };

  var suffices = ["", "k", "M", "B", "T", "q", "Q", "s", "S", "o", "N", "d"];

  var fmtPrefix = function fmtPrefix(i, n) {
    var ipos = Math.abs(i);
    var mant = mantissa(ipos, n);
    var expo = exponent(ipos);
    var extraDigits = expo % 3;
    var sections = Math.floor(expo / 3);
    var numberPart = (mant * Math.pow(10, extraDigits - n + 1)).toFixed(n - extraDigits - 1);
    var pref = i < 0 ? "-" : "";
    return pref + numberPart + suffices[sections];
  };

  exports._mkSvg = function (elem, dim) {
    return function () {
      var svg = d3.select(elem).append("svg") // .classed("svg-content-responsive",true)
      // .attr("viewBox", [0,0,1000, 600])
      .attr("viewBox", [0, 0, dim.width, dim.height]).style("overflow", "visible");
      return {
        svg: svg,
        dimensions: dim
      };
    };
  };

  exports.clearSvg = function (svg) {
    return function () {
      svg.selectAll("*").remove();
    };
  }; // { xAxis : { scale : Scale, label : String}
  // , yAxis : { scale : Scale, label : String}
  // , series : [{ name : String, values: [{x, y}]}]
  // }


  exports._drawData = function (handleType, handleScale, typeX, typeY, svgdat, scatter) {
    var svg = svgdat.svg;
    var width = svgdat.dimensions.width;
    var height = svgdat.dimensions.height; // const width = 1000;
    // const height = 600;

    var scaleFunc = function scaleFunc(scale) {
      return handleScale(scale)({
        date: function date() {
          return d3.scaleUtc();
        },
        linear: function linear() {
          return d3.scaleLinear();
        },
        log: function log() {
          return d3.scaleLog();
        }
      });
    };

    var fmt = function fmt(tp) {
      return function (val) {
        return handleType(tp)({
          day: function day() {
            return val.toLocaleDateString(undefined, {
              month: "numeric",
              day: "numeric"
            });
          },
          days: function days() {
            return val;
          },
          "int": function int() {
            return Math.abs(val) < 1000 ? val : fmtPrefix(val, 4);
          },
          number: function number() {
            return Math.abs(val) < 1 ? val : Math.abs(val) < 1000 ? val.toFixed(2) : fmtPrefix(val, 4);
          },
          percent: function percent() {
            return (Math.abs(val) < 1 ? val.toFixed(2) : Math.abs(val) < 1000 ? val.toFixed(1) : fmtPrefix(val, 3)) + "%";
          }
        });
      };
    };

    var fmtX = fmt(typeX);
    var fmtY = fmt(typeY);

    var validVal = function validVal(scale) {
      return function (val) {
        return !isNaN(val) && handleScale(scale)({
          date: function date() {
            return true;
          },
          linear: function linear() {
            return true;
          },
          log: function log() {
            return val > 0;
          }
        });
      };
    };

    var validX = validVal(scatter.xAxis.scale);
    var validY = validVal(scatter.yAxis.scale);

    var validPair = function validPair(p) {
      return validX(p.x) && validY(p.y);
    };

    var convert = function convert(tp) {
      return function (val) {
        return handleType(tp)({
          day: function day() {
            return fromMJD(val);
          },
          days: function days() {
            return val;
          },
          "int": function int() {
            return val;
          },
          number: function number() {
            return val;
          },
          percent: function percent() {
            return val * 100;
          }
        });
      };
    };

    var convertX = convert(typeX);
    var convertY = convert(typeY);

    var convertPair = function convertPair(p) {
      return {
        x: convertX(p.x),
        y: convertY(p.y)
      };
    };

    return function () {
      exports.clearSvg(svg)();
      console.log(scatter);
      var margin = {
        top: 20,
        right: 20,
        bottom: 20,
        left: 50
      };
      var series = scatter.series.map(function (s) {
        return {
          name: s.name,
          values: s.values.filter(validPair).map(convertPair)
        };
      });
      console.log(series);
      var allx = series.map(function (s) {
        return s.values.map(function (p) {
          return p.x;
        });
      }).flat();
      var ally = series.map(function (s) {
        return s.values.map(function (p) {
          return p.y;
        });
      }).flat();
      var x = scaleFunc(scatter.xAxis.scale).domain(d3.extent(allx)).nice().range([margin.left, width - margin.right]);
      var y = scaleFunc(scatter.yAxis.scale).domain(d3.extent(ally)).nice().range([height - margin.bottom, margin.top]);
      var line = d3.line() // .defined(validPair)
      .x(function (d) {
        return x(d.x);
      }).y(function (d) {
        return y(d.y);
      });

      var xAxis = function xAxis(g) {
        g.attr("transform", "translate(0,".concat(height - margin.bottom, ")")) // .call(d3.axisBottom(x).ticks(width/80).tickSizeOuter(0));
        .call(d3.axisBottom(x).ticks(10, function (k) {
          return fmt(typeX)(k);
        })).call(function (g) {
          return g.append("text").attr("x", width - margin.right).attr("y", -3).attr("fill", "currentColor").attr("font-weight", "bold").text(scatter.xAxis.label);
        }).call(function (g) {
          return g.selectAll(".tick line").clone().attr("stroke-opacity", 0.1).attr("y1", -height + margin.top + margin.bottom);
        });
      };

      var yAxis = function yAxis(g) {
        g.attr("transform", "translate(".concat(margin.left, ",0)")).call(d3.axisLeft(y).ticks(10, fmtY)).call(function (g) {
          return g.selectAll(".tick line").clone().attr("stroke-opacity", 0.1).attr("x2", width - margin.left - margin.right);
        }).call(function (g) {
          return g.select(".tick:last-of-type text").clone().attr("x", 3).attr("text-anchor", "start").attr("font-weight", "bold").text(scatter.yAxis.label);
        });
      };

      var hover = function hover(svg, path) {
        if ("ontouchstart" in document) svg.style("-webkit-tap-highlight-color", "transparent").on("touchmove", moved).on("touchstart", entered).on("touchend", left);else svg.on("mousemove", moved).on("mouseenter", entered).on("mouseleave", left);
        var dot = svg.append("g").attr("display", "none");
        dot.append("circle").attr("r", 2.5);
        dot.append("text").attr("font-family", "sans-serif").attr("font-size", 10).attr("text-anchor", "middle").attr("y", -8);

        function moved() {
          d3.event.preventDefault();
          var mouse = d3.mouse(this);
          var xm = x.invert(mouse[0]);
          var ym = y.invert(mouse[1]);
          var i1 = d3.bisectLeft(allx, xm, 1);
          var i0 = i1 - 1;
          var i = xm - allx[i0] > allx[i1] - xm ? i1 : i0;
          var s = d3.least(series, function (d) {
            return Math.abs(d.values[findByX(d.values, xm, 1)].y - ym);
          });
          var di1 = findByX(s.values, xm, 1);
          var di0 = di1 - 1;
          var di = xm - s.values[di0].x > s.values[di1].x - xm ? di1 : di0;
          path.attr("stroke", function (d) {
            return d === s ? null : "#ddd";
          }).filter(function (d) {
            return d === s;
          }).raise();
          dot.attr("transform", "translate(".concat(x(allx[i]), ",").concat(y(s.values[di].y), ")"));
          dot.select("text").text(s.name + ": " + fmtX(s.values[di].x) + ", " + fmtY(s.values[di].y));
        }

        function entered() {
          path.style("mix-blend-mode", null).attr("stroke", "#ddd");
          dot.attr("display", null);
        }

        function left() {
          path.style("mix-blend-mode", "multiply").attr("stroke", null);
          dot.attr("display", "none");
        }
      };

      svg.append("g").call(xAxis);
      svg.append("g").call(yAxis);
      var path = svg.append("g").attr("fill", "none").attr("stroke", "steelblue").attr("stroke-width", 1.5).attr("stroke-linejoin", "round").attr("stroke-linecap", "round").selectAll("path").data(series).join("path").style("mix-blend-mode", "multiply").attr("d", function (d) {
        return line(d.values);
      });
      svg.call(hover, path);
    };
  };
})(PS["D3.Scatter"] = PS["D3.Scatter"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["D3.Scatter"] = $PS["D3.Scatter"] || {};
  var exports = $PS["D3.Scatter"];
  var $foreign = $PS["D3.Scatter"];
  var D3_Scatter_Type = $PS["D3.Scatter.Type"];
  var Data_Function_Uncurried = $PS["Data.Function.Uncurried"];
  var Type_Handler = $PS["Type.Handler"];
  var mkSvg = Data_Function_Uncurried.runFn2($foreign["_mkSvg"]);
  var drawData_ = Data_Function_Uncurried.runFn6($foreign["_drawData"])(Type_Handler.handle1(D3_Scatter_Type.handle1SType))(Type_Handler.handle1(D3_Scatter_Type.handle1Scale));
  exports["drawData_"] = drawData_;
  exports["mkSvg"] = mkSvg;
  exports["clearSvg"] = $foreign.clearSvg;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Scatter"] = $PS["Halogen.Scatter"] || {};
  var exports = $PS["Halogen.Scatter"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var D3_Scatter = $PS["D3.Scatter"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Class = $PS["Effect.Class"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_HTML_Properties = $PS["Halogen.HTML.Properties"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];

  var Initialize = function () {
    function Initialize(value0) {
      this.value0 = value0;
    }

    ;

    Initialize.create = function (value0) {
      return new Initialize(value0);
    };

    return Initialize;
  }();

  var Finalize = function () {
    function Finalize() {}

    ;
    Finalize.value = new Finalize();
    return Finalize;
  }();

  var scatterRef = "scatter-plot";

  var renderH = function renderH(v) {
    return Halogen_HTML_Elements.div([Halogen_HTML_Properties.ref(scatterRef)])([]);
  };

  var initialState = function initialState(v) {
    return {
      chart: Data_Maybe.Nothing.value
    };
  };

  var handleQuery = function handleQuery(dictMonadEffect) {
    return function (v) {
      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (s) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))(function () {
          if (s.chart instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
          }

          ;

          if (s.chart instanceof Data_Maybe.Just) {
            return v.update(function (a) {
              return function (b) {
                return D3_Scatter.drawData_(a)(b)(s.chart.value0);
              };
            });
          }

          ;
          throw new Error("Failed pattern match at Halogen.Scatter (line 80, column 16 - line 82, column 52): " + [s.chart.constructor.name]);
        }()))(function () {
          return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.next));
        });
      });
    };
  };

  var handleAction = function handleAction(dictMonadEffect) {
    return function (v) {
      if (v instanceof Initialize) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Halogen_Query_HalogenM.getRef(scatterRef))(function (r) {
          return Data_Foldable.for_(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Foldable.foldableMaybe)(r)(function (ref) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))(D3_Scatter.mkSvg(ref)(v.value0)))(function (chart) {
              return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                return {
                  chart: new Data_Maybe.Just(chart)
                };
              });
            });
          });
        });
      }

      ;

      if (v instanceof Finalize) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (s) {
          if (s.chart instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
          }

          ;

          if (s.chart instanceof Data_Maybe.Just) {
            return Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))(D3_Scatter.clearSvg(s.chart.value0));
          }

          ;
          throw new Error("Failed pattern match at Halogen.Scatter (line 70, column 7 - line 72, column 43): " + [s.chart.constructor.name]);
        });
      }

      ;
      throw new Error("Failed pattern match at Halogen.Scatter (line 62, column 16 - line 72, column 43): " + [v.constructor.name]);
    };
  };

  var component = function component(dictMonadEffect) {
    return function (dim) {
      return Halogen_Component.mkComponent({
        initialState: initialState,
        render: renderH,
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction(dictMonadEffect),
          handleQuery: handleQuery(dictMonadEffect),
          receive: Halogen_Component.defaultEval.receive,
          initialize: new Data_Maybe.Just(new Initialize(dim)),
          finalize: new Data_Maybe.Just(Finalize.value)
        })
      });
    };
  };

  exports["component"] = component;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Corona.Chart.UI"] = $PS["Corona.Chart.UI"] || {};
  var exports = $PS["Corona.Chart.UI"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_State_Class = $PS["Control.Monad.State.Class"];
  var Corona_Chart = $PS["Corona.Chart"];
  var Corona_Chart_UI_Projection = $PS["Corona.Chart.UI.Projection"];
  var D3_Scatter_Type = $PS["D3.Scatter.Type"];
  var Data_Array = $PS["Data.Array"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Set = $PS["Data.Set"];
  var Data_Show = $PS["Data.Show"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Unit = $PS["Data.Unit"];
  var Data_Void = $PS["Data.Void"];
  var Debug_Trace = $PS["Debug.Trace"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML = $PS["Halogen.HTML"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_HTML_Elements = $PS["Halogen.HTML.Elements"];
  var Halogen_MultiSelect = $PS["Halogen.MultiSelect"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Scatter = $PS["Halogen.Scatter"];
  var Halogen_Util = $PS["Halogen.Util"];
  var Type_Chain = $PS["Type.Chain"];
  var Type_DSum = $PS["Type.DSum"];
  var Type_Equiv = $PS["Type.Equiv"];

  var SetCountries = function () {
    function SetCountries(value0) {
      this.value0 = value0;
    }

    ;

    SetCountries.create = function (value0) {
      return new SetCountries(value0);
    };

    return SetCountries;
  }();

  var SetXProjection = function () {
    function SetXProjection(value0) {
      this.value0 = value0;
    }

    ;

    SetXProjection.create = function (value0) {
      return new SetXProjection(value0);
    };

    return SetXProjection;
  }();

  var SetYProjection = function () {
    function SetYProjection(value0) {
      this.value0 = value0;
    }

    ;

    SetYProjection.create = function (value0) {
      return new SetYProjection(value0);
    };

    return SetYProjection;
  }();

  var SetZProjection = function () {
    function SetZProjection(value0) {
      this.value0 = value0;
    }

    ;

    SetZProjection.create = function (value0) {
      return new SetZProjection(value0);
    };

    return SetZProjection;
  }();

  var lookupScale = function lookupScale(st) {
    return function (ns) {
      var v = D3_Scatter_Type.toNType(st);

      if (v instanceof Data_Either.Left && v.value0 instanceof Data_Either.Left) {
        return new D3_Scatter_Type["Date"](v.value0.value0);
      }

      ;

      if (v instanceof Data_Either.Left && v.value0 instanceof Data_Either.Right) {
        return new D3_Scatter_Type.Linear(new Data_Either.Left(v.value0.value0));
      }

      ;

      if (v instanceof Data_Either.Right) {
        return D3_Scatter_Type.runNScale(ns)(v.value0);
      }

      ;
      throw new Error("Failed pattern match at Corona.Chart.UI (line 62, column 21 - line 65, column 41): " + [v.constructor.name]);
    };
  };

  var initialCountries = Data_Set.fromFoldable(Data_Foldable.foldableArray)(Data_Ord.ordString)(["US", "Egypt", "Italy"]);

  var initialState = function initialState(v) {
    var testConf = Type_DSum.dsum(D3_Scatter_Type.nInt)(Corona_Chart.projection({
      base: new Corona_Chart.Confirmed(Type_Equiv.refl),
      operations: Type_Chain.nil
    }));
    return {
      xAxis: {
        projection: Type_DSum.dsum(D3_Scatter_Type.sDay)(Corona_Chart.projection({
          base: new Corona_Chart.Time(Type_Equiv.refl),
          operations: Type_Chain.nil
        })),
        numScale: D3_Scatter_Type.Log.create
      },
      yAxis: {
        projection: Type_DSum.dsum(D3_Scatter_Type.sInt)(Corona_Chart.projection({
          base: new Corona_Chart.Confirmed(Type_Equiv.refl),
          operations: new Type_Chain.Nil(Type_Equiv.refl)
        })),
        numScale: D3_Scatter_Type.Log.create
      },
      zAxis: {
        projection: Type_DSum.dsum(D3_Scatter_Type.sDay)(Corona_Chart.projection({
          base: new Corona_Chart.Time(Type_Equiv.refl),
          operations: Type_Chain.nil
        })),
        numScale: D3_Scatter_Type.Log.create
      },
      countries: initialCountries
    };
  };

  var _zProjection = Data_Symbol.SProxy.value;
  var _yProjection = Data_Symbol.SProxy.value;
  var _xProjection = Data_Symbol.SProxy.value;
  var _scatter = Data_Symbol.SProxy.value;

  var reRender = function reRender(dictMonadEffect) {
    return function (dat) {
      return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Debug_Trace.traceM()(Halogen_Query_HalogenM.monadHalogenM)(Data_Show.show(Data_Show.showRecord()(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "countries";
        }))(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "xAxis";
        }))(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "yAxis";
        }))(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "zAxis";
        }))(Data_Show.showRecordFieldsNil)(Data_Show.showRecord()(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "numScale";
        }))(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "projection";
        }))(Data_Show.showRecordFieldsNil)(Type_DSum.showDSum(D3_Scatter_Type.gshowSType)(Corona_Chart.gshowProjection)))(D3_Scatter_Type.showNScale))))(Data_Show.showRecord()(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "numScale";
        }))(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "projection";
        }))(Data_Show.showRecordFieldsNil)(Type_DSum.showDSum(D3_Scatter_Type.gshowSType)(Corona_Chart.gshowProjection)))(D3_Scatter_Type.showNScale))))(Data_Show.showRecord()(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "numScale";
        }))(Data_Show.showRecordFieldsCons(new Data_Symbol.IsSymbol(function () {
          return "projection";
        }))(Data_Show.showRecordFieldsNil)(Type_DSum.showDSum(D3_Scatter_Type.gshowSType)(Corona_Chart.gshowProjection)))(D3_Scatter_Type.showNScale))))(Data_Set.showSet(Data_Show.showString))))(v)))(function () {
          return Type_DSum.withDSum(v.xAxis.projection)(function (tX) {
            return function (pX) {
              return Type_DSum.withDSum(v.yAxis.projection)(function (tY) {
                return function (pY) {
                  return Data_Functor["void"](Halogen_Query_HalogenM.functorHalogenM)(Halogen_Query_HalogenM.query()(new Data_Symbol.IsSymbol(function () {
                    return "scatter";
                  }))(Data_Ord.ordUnit)(_scatter)(Data_Unit.unit)({
                    update: function update(f) {
                      return f(tX)(tY)(Corona_Chart.toScatterPlot(dat)(pX)(lookupScale(tX)(v.xAxis.numScale))(pY)(lookupScale(tY)(v.yAxis.numScale))(v.countries));
                    },
                    next: Data_Unit.unit
                  }));
                };
              });
            };
          });
        });
      });
    };
  };

  var handleAction = function handleAction(dictMonadEffect) {
    return function (dat) {
      return function (act) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(function () {
          if (act instanceof SetCountries) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
              var $33 = {};

              for (var $34 in st) {
                if ({}.hasOwnProperty.call(st, $34)) {
                  $33[$34] = st[$34];
                }

                ;
              }

              ;
              $33.countries = act.value0;
              return $33;
            });
          }

          ;

          if (act instanceof SetXProjection) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
              var $37 = {};

              for (var $38 in st) {
                if ({}.hasOwnProperty.call(st, $38)) {
                  $37[$38] = st[$38];
                }

                ;
              }

              ;
              $37.xAxis = act.value0;
              return $37;
            });
          }

          ;

          if (act instanceof SetYProjection) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
              var $41 = {};

              for (var $42 in st) {
                if ({}.hasOwnProperty.call(st, $42)) {
                  $41[$42] = st[$42];
                }

                ;
              }

              ;
              $41.yAxis = act.value0;
              return $41;
            });
          }

          ;

          if (act instanceof SetZProjection) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (st) {
              var $45 = {};

              for (var $46 in st) {
                if ({}.hasOwnProperty.call(st, $46)) {
                  $45[$46] = st[$46];
                }

                ;
              }

              ;
              $45.zAxis = act.value0;
              return $45;
            });
          }

          ;
          throw new Error("Failed pattern match at Corona.Chart.UI (line 176, column 5 - line 180, column 67): " + [act.constructor.name]);
        }())(function () {
          return reRender(dictMonadEffect)(dat);
        });
      };
    };
  };

  var _multiselect = Data_Symbol.SProxy.value;

  var render = function render(dictMonadEffect) {
    return function (dat) {
      return function (st) {
        var sel0 = function () {
          var opts = Data_Functor.mapFlipped(Data_Functor.functorArray)(Foreign_Object.keys(dat.counts))(function (cty) {
            return {
              label: cty,
              value: cty
            };
          });
          return {
            options: opts,
            selected: Data_Set.mapMaybe(Data_Ord.ordInt)(function (c) {
              return Data_Array.findIndex(function (x) {
                return x.value === c;
              })(opts);
            })(initialCountries),
            filter: ""
          };
        }();

        var projLabel = function projLabel(dp) {
          return Type_DSum.withDSum(dp)(function (v) {
            return Corona_Chart.projectionLabel;
          });
        };

        var title = projLabel(st.yAxis.projection) + (" vs. " + projLabel(st.xAxis.projection));
        var hw = {
          height: Data_Int.toNumber(600),
          width: Data_Int.toNumber(1000)
        };
        return Halogen_HTML_Elements.div([Halogen_Util.classProp("ui-wrapper")])([Halogen_HTML_Elements.div([Halogen_Util.classProp("grid__col grid__col--5-of-5 plot-title")])([Halogen_HTML_Elements.h2_([Halogen_HTML_Core.text(title)])]), Halogen_HTML_Elements.div([Halogen_Util.classProp("grid__col grid__col--1-of-5 axis-y")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
          return "xProjection";
        }))(Data_Ord.ordUnit)(_xProjection)(Data_Unit.unit)(Corona_Chart_UI_Projection.component(dictMonadEffect)("Y Axis"))(st.yAxis)(function (v) {
          return new Data_Maybe.Just(new SetYProjection(v.value0));
        })]), Halogen_HTML_Elements.div([Halogen_Util.classProp("grid__col grid__col--4-of-5 plot")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
          return "scatter";
        }))(Data_Ord.ordUnit)(_scatter)(Data_Unit.unit)(Halogen_Scatter.component(dictMonadEffect)(hw))(Data_Unit.unit)(Data_Void.absurd)]), Halogen_HTML_Elements.div([Halogen_Util.classProp("grid__col grid__col--1-of-5 axis-z")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
          return "zProjection";
        }))(Data_Ord.ordUnit)(_zProjection)(Data_Unit.unit)(Corona_Chart_UI_Projection.component(dictMonadEffect)("Time Axis"))(st.zAxis)(function (v) {
          return new Data_Maybe.Just(new SetZProjection(v.value0));
        })]), Halogen_HTML_Elements.div([Halogen_Util.classProp("grid__col grid__col--3-of-5 countries")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
          return "multiselect";
        }))(Data_Ord.ordUnit)(_multiselect)(Data_Unit.unit)(Halogen_MultiSelect.component(dictMonadEffect)("Countries"))(sel0)(function (v) {
          return new Data_Maybe.Just(new SetCountries(Data_Set.fromFoldable(Data_Foldable.foldableArray)(Data_Ord.ordString)(v.value0)));
        })]), Halogen_HTML_Elements.div([Halogen_Util.classProp("grid__col grid__col--1-of-5 axis-x")])([Halogen_HTML.slot()(new Data_Symbol.IsSymbol(function () {
          return "yProjection";
        }))(Data_Ord.ordUnit)(_yProjection)(Data_Unit.unit)(Corona_Chart_UI_Projection.component(dictMonadEffect)("X Axis"))(st.xAxis)(function (v) {
          return new Data_Maybe.Just(new SetXProjection(v.value0));
        })])]);
      };
    };
  };

  var component = function component(dictMonadEffect) {
    return function (dat) {
      return Halogen_Component.mkComponent({
        initialState: initialState,
        render: render(dictMonadEffect)(dat),
        "eval": Halogen_Component.mkEval({
          handleAction: handleAction(dictMonadEffect)(dat),
          handleQuery: Halogen_Component.defaultEval.handleQuery,
          receive: Halogen_Component.defaultEval.receive,
          initialize: new Data_Maybe.Just(new SetCountries(initialCountries)),
          finalize: Halogen_Component.defaultEval.finalize
        })
      });
    };
  };

  exports["component"] = component;
})(PS);

(function (exports) {
  "use strict";

  exports.parseCSV = function (csv) {
    return Papa.parse(csv);
  };
})(PS["Foreign.Papa"] = PS["Foreign.Papa"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Foreign.Papa"] = $PS["Foreign.Papa"] || {};
  var exports = $PS["Foreign.Papa"];
  var $foreign = $PS["Foreign.Papa"];
  exports["parseCSV"] = $foreign.parseCSV;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Corona.JHU"] = $PS["Corona.JHU"] || {};
  var exports = $PS["Corona.JHU"];
  var Affjax = $PS["Affjax"];
  var Affjax_ResponseFormat = $PS["Affjax.ResponseFormat"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_Maybe_Trans = $PS["Control.Monad.Maybe.Trans"];
  var Control_Plus = $PS["Control.Plus"];
  var Data_Array = $PS["Data.Array"];
  var Data_Either = $PS["Data.Either"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HTTP_Method = $PS["Data.HTTP.Method"];
  var Data_Int_Parse = $PS["Data.Int.Parse"];
  var Data_JSDate = $PS["Data.JSDate"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_ModifiedJulianDay = $PS["Data.ModifiedJulianDay"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Data_Traversable = $PS["Data.Traversable"];
  var Data_TraversableWithIndex = $PS["Data.TraversableWithIndex"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Foreign_Object = $PS["Foreign.Object"];
  var Foreign_Papa = $PS["Foreign.Papa"];
  var recoveredUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv";
  var deathsUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv";
  var confirmedUrl = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv";

  var buildData = function buildData(xs) {
    var go = function go(val) {
      return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Array.index(val)(1))(function (country) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(Data_Function.flip(Data_Int_Parse["parseInt"])(Data_Int_Parse.toRadix(10)))(Data_Array.drop(4)(val)))(function (valnum) {
          return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(country, valnum));
        });
      });
    };

    var process = function process(ht) {
      return Control_Monad_Maybe_Trans.runMaybeT(Control_Bind.bind(Control_Monad_Maybe_Trans.bindMaybeT(Effect.monadEffect))(Data_Maybe.maybe(Control_Plus.empty(Control_Monad_Maybe_Trans.plusMaybeT(Effect.monadEffect)))(Control_Applicative.pure(Control_Monad_Maybe_Trans.applicativeMaybeT(Effect.monadEffect)))(Data_Array.index(ht.head)(4)))(function (d0) {
        return Control_Bind.bind(Control_Monad_Maybe_Trans.bindMaybeT(Effect.monadEffect))(Control_Monad_Maybe_Trans.MaybeT(Data_Functor.map(Effect.functorEffect)(Data_ModifiedJulianDay.fromJSDate)(Data_JSDate.parse(d0))))(function (start) {
          var vals = Data_Array.mapMaybe(go)(ht.tail);
          var counts = Foreign_Object.fromFoldableWith(Data_Foldable.foldableArray)(Data_Array.zipWith(Data_Semiring.add(Data_Semiring.semiringInt)))(vals);
          return Control_Applicative.pure(Control_Monad_Maybe_Trans.applicativeMaybeT(Effect.monadEffect))({
            start: start,
            counts: counts
          });
        });
      }));
    };

    var v = Data_Array.uncons(xs);

    if (v instanceof Data_Maybe.Nothing) {
      return Control_Applicative.pure(Effect.applicativeEffect)(Data_Maybe.Nothing.value);
    }

    ;

    if (v instanceof Data_Maybe.Just) {
      return process(v.value0);
    }

    ;
    throw new Error("Failed pattern match at Corona.JHU (line 87, column 16 - line 89, column 38): " + [v.constructor.name]);
  };

  var fetchData = function fetchData(url) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Affjax.request({
      method: new Data_Either.Left(Data_HTTP_Method.GET.value),
      url: url,
      headers: Affjax.defaultRequest.headers,
      content: Affjax.defaultRequest.content,
      username: Affjax.defaultRequest.username,
      password: Affjax.defaultRequest.password,
      withCredentials: Affjax.defaultRequest.withCredentials,
      responseFormat: Affjax_ResponseFormat.string
    }))(function (result) {
      if (result instanceof Data_Either.Left) {
        return Control_Applicative.pure(Effect_Aff.applicativeAff)(new Data_Either.Left(Affjax.printError(result.value0)));
      }

      ;

      if (result instanceof Data_Either.Right) {
        return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
          var dat = buildData(Foreign_Papa.parseCSV(result.value0.body).data)();

          if (dat instanceof Data_Maybe.Nothing) {
            return new Data_Either.Left("data not accumulated");
          }

          ;

          if (dat instanceof Data_Maybe.Just) {
            return new Data_Either.Right(dat.value0);
          }

          ;
          throw new Error("Failed pattern match at Corona.JHU (line 115, column 15 - line 117, column 30): " + [dat.constructor.name]);
        });
      }

      ;
      throw new Error("Failed pattern match at Corona.JHU (line 111, column 5 - line 117, column 30): " + [result.constructor.name]);
    });
  };

  var fetchCoronaData = Control_Monad_Except_Trans.runExceptT(Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect_Aff.monadAff))(Control_Monad_Except_Trans.ExceptT(fetchData(confirmedUrl)))(function (confirmed) {
    return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect_Aff.monadAff))(Control_Monad_Except_Trans.ExceptT(fetchData(deathsUrl)))(function (deaths) {
      return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect_Aff.monadAff))(Control_Monad_Except_Trans.ExceptT(fetchData(recoveredUrl)))(function (recovered) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Except_Trans.bindExceptT(Effect_Aff.monadAff))(Control_Applicative.unless(Control_Monad_Except_Trans.applicativeExceptT(Effect_Aff.monadAff))(Data_Eq.eq(Data_ModifiedJulianDay.eqDay)(confirmed.start)(deaths.start) && Data_Eq.eq(Data_ModifiedJulianDay.eqDay)(deaths.start)(recovered.start))(Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Effect_Aff.monadAff))("non-matching dates for time serieses")))(function () {
          var newCounts = Data_TraversableWithIndex.forWithIndex(Data_Maybe.applicativeMaybe)(Foreign_Object.traversableWithIndexObject)(confirmed.counts)(function (k) {
            return function (c) {
              return Control_Bind.bind(Data_Maybe.bindMaybe)(Foreign_Object.lookup(k)(deaths.counts))(function (d) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(Foreign_Object.lookup(k)(recovered.counts))(function (r) {
                  return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Array.zipWith(Data_Function.apply)(Data_Array.zipWith(function (c$prime) {
                    return function (d$prime) {
                      return function (r$prime) {
                        return {
                          confirmed: c$prime,
                          deaths: d$prime,
                          recovered: r$prime
                        };
                      };
                    };
                  })(c)(d))(r));
                });
              });
            };
          });

          if (newCounts instanceof Data_Maybe.Nothing) {
            return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(Effect_Aff.monadAff))("missing data");
          }

          ;

          if (newCounts instanceof Data_Maybe.Just) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(Effect_Aff.monadAff))({
              start: confirmed.start,
              counts: newCounts.value0
            });
          }

          ;
          throw new Error("Failed pattern match at Corona.JHU (line 73, column 5 - line 78, column 10): " + [newCounts.constructor.name]);
        });
      });
    });
  }));
  exports["fetchCoronaData"] = fetchCoronaData;
})(PS);

(function (exports) {
  /* globals exports, setTimeout */
  "use strict";

  var AVar = function () {
    function MutableQueue() {
      this.head = null;
      this.last = null;
      this.size = 0;
    }

    function MutableCell(queue, value) {
      this.queue = queue;
      this.value = value;
      this.next = null;
      this.prev = null;
    }

    function AVar(value) {
      this.draining = false;
      this.error = null;
      this.value = value;
      this.takes = new MutableQueue();
      this.reads = new MutableQueue();
      this.puts = new MutableQueue();
    }

    var EMPTY = {};

    function runEff(eff) {
      try {
        eff();
      } catch (error) {
        setTimeout(function () {
          throw error;
        }, 0);
      }
    }

    function putLast(queue, value) {
      var cell = new MutableCell(queue, value);

      switch (queue.size) {
        case 0:
          queue.head = cell;
          break;

        case 1:
          cell.prev = queue.head;
          queue.head.next = cell;
          queue.last = cell;
          break;

        default:
          cell.prev = queue.last;
          queue.last.next = cell;
          queue.last = cell;
      }

      queue.size++;
      return cell;
    }

    function takeLast(queue) {
      var cell;

      switch (queue.size) {
        case 0:
          return null;

        case 1:
          cell = queue.head;
          queue.head = null;
          break;

        case 2:
          cell = queue.last;
          queue.head.next = null;
          queue.last = null;
          break;

        default:
          cell = queue.last;
          queue.last = cell.prev;
          queue.last.next = null;
      }

      cell.prev = null;
      cell.queue = null;
      queue.size--;
      return cell.value;
    }

    function takeHead(queue) {
      var cell;

      switch (queue.size) {
        case 0:
          return null;

        case 1:
          cell = queue.head;
          queue.head = null;
          break;

        case 2:
          cell = queue.head;
          queue.last.prev = null;
          queue.head = queue.last;
          queue.last = null;
          break;

        default:
          cell = queue.head;
          queue.head = cell.next;
          queue.head.prev = null;
      }

      cell.next = null;
      cell.queue = null;
      queue.size--;
      return cell.value;
    }

    function deleteCell(cell) {
      if (cell.queue === null) {
        return;
      }

      if (cell.queue.last === cell) {
        takeLast(cell.queue);
        return;
      }

      if (cell.queue.head === cell) {
        takeHead(cell.queue);
        return;
      }

      if (cell.prev) {
        cell.prev.next = cell.next;
      }

      if (cell.next) {
        cell.next.prev = cell.prev;
      }

      cell.queue.size--;
      cell.queue = null;
      cell.value = null;
      cell.next = null;
      cell.prev = null;
    }

    function drainVar(util, avar) {
      if (avar.draining) {
        return;
      }

      var ps = avar.puts;
      var ts = avar.takes;
      var rs = avar.reads;
      var p, r, t, value, rsize;
      avar.draining = true;

      while (1) {
        // eslint-disable-line no-constant-condition
        p = null;
        r = null;
        t = null;
        value = avar.value;
        rsize = rs.size;

        if (avar.error !== null) {
          value = util.left(avar.error);

          while (p = takeHead(ps)) {
            // eslint-disable-line no-cond-assign
            runEff(p.cb(value));
          }

          while (r = takeHead(rs)) {
            // eslint-disable-line no-cond-assign
            runEff(r(value));
          }

          while (t = takeHead(ts)) {
            // eslint-disable-line no-cond-assign
            runEff(t(value));
          }

          break;
        } // Process the next put. We do not immediately invoke the callback
        // because we want to preserve ordering. If there are takes/reads
        // we want to run those first.


        if (value === EMPTY && (p = takeHead(ps))) {
          avar.value = value = p.value;
        }

        if (value !== EMPTY) {
          // We go ahead and queue up the next take for the same reasons as
          // above. Invoking the read callbacks can affect the mutable queue.
          t = takeHead(ts); // We only want to process the reads queued up before running these
          // callbacks so we guard on rsize.

          while (rsize-- && (r = takeHead(rs))) {
            runEff(r(util.right(value)));
          }

          if (t !== null) {
            avar.value = EMPTY;
            runEff(t(util.right(value)));
          }
        }

        if (p !== null) {
          runEff(p.cb(util.right(void 0)));
        } // Callbacks could have queued up more items so we need to guard on the
        // actual mutable properties.


        if (avar.value === EMPTY && ps.size === 0 || avar.value !== EMPTY && ts.size === 0) {
          break;
        }
      }

      avar.draining = false;
    }

    AVar.EMPTY = EMPTY;
    AVar.putLast = putLast;
    AVar.takeLast = takeLast;
    AVar.takeHead = takeHead;
    AVar.deleteCell = deleteCell;
    AVar.drainVar = drainVar;
    return AVar;
  }();

  exports.empty = function () {
    return new AVar(AVar.EMPTY);
  };

  exports._killVar = function (util, error, avar) {
    return function () {
      if (avar.error === null) {
        avar.error = error;
        avar.value = AVar.EMPTY;
        AVar.drainVar(util, avar);
      }
    };
  };

  exports._putVar = function (util, value, avar, cb) {
    return function () {
      var cell = AVar.putLast(avar.puts, {
        cb: cb,
        value: value
      });
      AVar.drainVar(util, avar);
      return function () {
        AVar.deleteCell(cell);
      };
    };
  };

  exports._takeVar = function (util, avar, cb) {
    return function () {
      var cell = AVar.putLast(avar.takes, cb);
      AVar.drainVar(util, avar);
      return function () {
        AVar.deleteCell(cell);
      };
    };
  };
})(PS["Effect.AVar"] = PS["Effect.AVar"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.AVar"] = $PS["Effect.AVar"] || {};
  var exports = $PS["Effect.AVar"];
  var $foreign = $PS["Effect.AVar"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Killed = function () {
    function Killed(value0) {
      this.value0 = value0;
    }

    ;

    Killed.create = function (value0) {
      return new Killed(value0);
    };

    return Killed;
  }();

  var Filled = function () {
    function Filled(value0) {
      this.value0 = value0;
    }

    ;

    Filled.create = function (value0) {
      return new Filled(value0);
    };

    return Filled;
  }();

  var Empty = function () {
    function Empty() {}

    ;
    Empty.value = new Empty();
    return Empty;
  }();

  var ffiUtil = {
    left: Data_Either.Left.create,
    right: Data_Either.Right.create,
    nothing: Data_Maybe.Nothing.value,
    just: Data_Maybe.Just.create,
    killed: Killed.create,
    filled: Filled.create,
    empty: Empty.value
  };

  var kill = function kill(err) {
    return function (avar) {
      return $foreign["_killVar"](ffiUtil, err, avar);
    };
  };

  var put = function put(value) {
    return function (avar) {
      return function (cb) {
        return $foreign["_putVar"](ffiUtil, value, avar, cb);
      };
    };
  };

  var take = function take(avar) {
    return function (cb) {
      return $foreign["_takeVar"](ffiUtil, avar, cb);
    };
  };

  exports["take"] = take;
  exports["put"] = put;
  exports["kill"] = kill;
  exports["empty"] = $foreign.empty;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Effect.Aff.AVar"] = $PS["Effect.Aff.AVar"] || {};
  var exports = $PS["Effect.Aff.AVar"];
  var Effect_AVar = $PS["Effect.AVar"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];

  var take = function take(avar) {
    return Effect_Aff.makeAff(function (k) {
      return function __do() {
        var c = Effect_AVar.take(avar)(k)();
        return Effect_Aff.effectCanceler(c);
      };
    });
  };

  var put = function put(value) {
    return function (avar) {
      return Effect_Aff.makeAff(function (k) {
        return function __do() {
          var c = Effect_AVar.put(value)(avar)(k)();
          return Effect_Aff.effectCanceler(c);
        };
      });
    };
  };

  var kill = function kill(error) {
    var $11 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
    var $12 = Effect_AVar.kill(error);
    return function ($13) {
      return $11($12($13));
    };
  };

  var empty = Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_AVar.empty);
  exports["empty"] = empty;
  exports["take"] = take;
  exports["put"] = put;
  exports["kill"] = kill;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Aff.Driver.State"] = $PS["Halogen.Aff.Driver.State"] || {};
  var exports = $PS["Halogen.Aff.Driver.State"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var unRenderStateX = Unsafe_Coerce.unsafeCoerce;
  var unDriverStateX = Unsafe_Coerce.unsafeCoerce;

  var renderStateX_ = function renderStateX_(dictApplicative) {
    return function (f) {
      return unDriverStateX(function (st) {
        return Data_Foldable.traverse_(dictApplicative)(Data_Foldable.foldableMaybe)(f)(st.rendering);
      });
    };
  };

  var mkRenderStateX = Unsafe_Coerce.unsafeCoerce;

  var renderStateX = function renderStateX(dictFunctor) {
    return function (f) {
      return unDriverStateX(function (st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };

  var mkDriverStateXRef = Unsafe_Coerce.unsafeCoerce;

  var mapDriverState = function mapDriverState(f) {
    return function (v) {
      return f(v);
    };
  };

  var initDriverState = function initDriverState(component) {
    return function (input) {
      return function (handler) {
        return function (lchs) {
          return function __do() {
            var selfRef = Effect_Ref["new"]({})();
            var childrenIn = Effect_Ref["new"](Halogen_Data_Slot.empty)();
            var childrenOut = Effect_Ref["new"](Halogen_Data_Slot.empty)();
            var handlerRef = Effect_Ref["new"](handler)();
            var pendingQueries = Effect_Ref["new"](new Data_Maybe.Just(Data_List_Types.Nil.value))();
            var pendingOuts = Effect_Ref["new"](new Data_Maybe.Just(Data_List_Types.Nil.value))();
            var pendingHandlers = Effect_Ref["new"](Data_Maybe.Nothing.value)();
            var fresh = Effect_Ref["new"](1)();
            var subscriptions = Effect_Ref["new"](new Data_Maybe.Just(Data_Map_Internal.empty))();
            var forks = Effect_Ref["new"](Data_Map_Internal.empty)();
            var ds = {
              component: component,
              state: component.initialState(input),
              refs: Data_Map_Internal.empty,
              children: Halogen_Data_Slot.empty,
              childrenIn: childrenIn,
              childrenOut: childrenOut,
              selfRef: selfRef,
              handlerRef: handlerRef,
              pendingQueries: pendingQueries,
              pendingOuts: pendingOuts,
              pendingHandlers: pendingHandlers,
              rendering: Data_Maybe.Nothing.value,
              fresh: fresh,
              subscriptions: subscriptions,
              forks: forks,
              lifecycleHandlers: lchs
            };
            Effect_Ref.write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  exports["mapDriverState"] = mapDriverState;
  exports["unDriverStateX"] = unDriverStateX;
  exports["renderStateX"] = renderStateX;
  exports["renderStateX_"] = renderStateX_;
  exports["unRenderStateX"] = unRenderStateX;
  exports["initDriverState"] = initDriverState;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Query.EventSource"] = $PS["Halogen.Query.EventSource"] || {};
  var exports = $PS["Halogen.Query.EventSource"];

  var finalize = function finalize(v) {
    return v;
  };

  exports["finalize"] = finalize;
})(PS);

(function (exports) {
  "use strict";

  exports.reallyUnsafeRefEq = function (a) {
    return function (b) {
      return a === b;
    };
  };
})(PS["Unsafe.Reference"] = PS["Unsafe.Reference"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Unsafe.Reference"] = $PS["Unsafe.Reference"] || {};
  var exports = $PS["Unsafe.Reference"];
  var $foreign = $PS["Unsafe.Reference"];
  var unsafeRefEq = $foreign.reallyUnsafeRefEq;
  exports["unsafeRefEq"] = unsafeRefEq;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Aff.Driver.Eval"] = $PS["Halogen.Aff.Driver.Eval"] || {};
  var exports = $PS["Halogen.Aff.Driver.Eval"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Applicative_Free = $PS["Control.Applicative.Free"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Coroutine = $PS["Control.Coroutine"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Fork_Class = $PS["Control.Monad.Fork.Class"];
  var Control_Monad_Free = $PS["Control.Monad.Free"];
  var Control_Monad_Free_Trans = $PS["Control.Monad.Free.Trans"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Control_Parallel = $PS["Control.Parallel"];
  var Control_Parallel_Class = $PS["Control.Parallel.Class"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Coyoneda = $PS["Data.Coyoneda"];
  var Data_Eq = $PS["Data.Eq"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Driver_State = $PS["Halogen.Aff.Driver.State"];
  var Halogen_Query_ChildQuery = $PS["Halogen.Query.ChildQuery"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];
  var Halogen_Query_HalogenM = $PS["Halogen.Query.HalogenM"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var Unsafe_Reference = $PS["Unsafe.Reference"];

  var unsubscribe = function unsubscribe(sid) {
    return function (ref) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v.subscriptions)))(function (subs) {
          return Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableMaybe)(Halogen_Query_EventSource.finalize)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Map_Internal.lookup(Halogen_Query_HalogenM.ordSubscriptionId)(sid))(subs));
        });
      });
    };
  };

  var queueOrRun = function queueOrRun(ref) {
    return function (au) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
          return au;
        }

        ;

        if (v instanceof Data_Maybe.Just) {
          return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write(new Data_Maybe.Just(new Data_List_Types.Cons(au, v.value0)))(ref));
        }

        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 195, column 33 - line 197, column 57): " + [v.constructor.name]);
      });
    };
  };

  var handleLifecycle = function handleLifecycle(lchs) {
    return function (f) {
      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write({
        initializers: Data_List_Types.Nil.value,
        finalizers: Data_List_Types.Nil.value
      })(lchs)))(function () {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(f))(function (result) {
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(lchs)))(function (v) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff))(v.finalizers))(function () {
              return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Parallel.parSequence_(Effect_Aff.parallelAff)(Data_List_Types.foldableList)(v.initializers))(function () {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(result);
              });
            });
          });
        });
      });
    };
  };

  var fresh = function fresh(f) {
    return function (ref) {
      return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
        return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["modify'"](function (i) {
          return {
            state: i + 1 | 0,
            value: f(i)
          };
        })(v.fresh));
      });
    };
  };

  var evalQ = function evalQ(render) {
    return function (ref) {
      return function (q) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
          return evalM(render)(ref)(v["component"]["eval"](new Halogen_Query_HalogenQ.Query(Data_Functor.map(Data_Coyoneda.functorCoyoneda)(Data_Maybe.Just.create)(Data_Coyoneda.liftCoyoneda(q)), Data_Function["const"](Data_Maybe.Nothing.value))));
        });
      };
    };
  };

  var evalM = function evalM(render) {
    return function (initRef) {
      return function (v) {
        var evalChildQuery = function evalChildQuery(ref) {
          return function (cqb) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v1) {
              return Halogen_Query_ChildQuery.unChildQueryBox(function (v2) {
                var evalChild = function evalChild(v3) {
                  return Control_Parallel_Class.parallel(Effect_Aff.parallelAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v3)))(function (dsx) {
                    return Halogen_Aff_Driver_State.unDriverStateX(function (ds) {
                      return evalQ(render)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };

                return Data_Functor.map(Effect_Aff.functorAff)(v2.value2)(Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(v2.value0(Effect_Aff.applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };

        var go = function go(ref) {
          return function (v1) {
            if (v1 instanceof Halogen_Query_HalogenM.State) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                var v3 = v1.value0(v2.state);

                if (Unsafe_Reference.unsafeRefEq(v2.state)(v3.value1)) {
                  return Control_Applicative.pure(Effect_Aff.applicativeAff)(v3.value0);
                }

                ;

                if (Data_Boolean.otherwise) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write({
                    component: v2.component,
                    state: v3.value1,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(handleLifecycle(v2.lifecycleHandlers)(render(v2.lifecycleHandlers)(ref)))(function () {
                      return Control_Applicative.pure(Effect_Aff.applicativeAff)(v3.value0);
                    });
                  });
                }

                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 85, column 7 - line 91, column 21): " + [v3.constructor.name]);
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Subscribe) {
              return Control_Bind.bind(Effect_Aff.bindAff)(fresh(Halogen_Query_HalogenM.SubscriptionId)(ref))(function (sid) {
                var v2 = v1.value0(sid);
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v3) {
                  return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Control_Bind.bind(Effect_Aff.bindAff)(v2)(function (v4) {
                    var done = Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v3.subscriptions)))(function (subs) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal["delete"](Halogen_Query_HalogenM.ordSubscriptionId)(sid)))(v3.subscriptions)))(function () {
                        return Control_Applicative.when(Effect_Aff.applicativeAff)(Data_Maybe.maybe(false)(Data_Map_Internal.member(Halogen_Query_HalogenM.ordSubscriptionId)(sid))(subs))(Halogen_Query_EventSource.finalize(v4.finalizer));
                      });
                    });
                    var consumer = Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Control_Coroutine["await"](Effect_Aff.monadAff))(function (act) {
                      return Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Effect_Aff.monadAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v3.subscriptions))))(function (subs) {
                        return Control_Applicative.when(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqBoolean))(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal.member(Halogen_Query_HalogenM.ordSubscriptionId)(sid))(subs))(new Data_Maybe.Just(true)))(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Effect_Aff.monadAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(evalF(render)(ref)(new Halogen_Query_Input.Action(act)))))(function () {
                          return consumer;
                        }));
                      });
                    });
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal.insert(Halogen_Query_HalogenM.ordSubscriptionId)(sid)(done)))(v3.subscriptions)))(function () {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Coroutine.runProcess(Effect_Aff.monadRecAff)(Control_Coroutine.pullFrom(Effect_Aff.monadRecAff)(consumer)(v4.producer)))(function () {
                        return Halogen_Query_EventSource.finalize(done);
                      });
                    });
                  })))(function () {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(sid));
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Unsubscribe) {
              return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(unsubscribe(v1.value0)(ref))(function () {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Lift) {
              return v1.value0;
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.ChildQuery) {
              return evalChildQuery(ref)(v1.value0);
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Raise) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v2.handlerRef)))(function (handler) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(queueOrRun(v2.pendingOuts)(handler(v1.value0)))(function () {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Par) {
              return Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(Control_Applicative_Free.retractFreeAp(Effect_Aff.applicativeParAff)(Control_Applicative_Free.hoistFreeAp(function () {
                var $85 = Control_Parallel_Class.parallel(Effect_Aff.parallelAff);
                var $86 = evalM(render)(ref);
                return function ($87) {
                  return $85($86($87));
                };
              }())(v1.value0)));
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Fork) {
              return Control_Bind.bind(Effect_Aff.bindAff)(fresh(Halogen_Query_HalogenM.ForkId)(ref))(function (fid) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                  return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](false)))(function (doneRef) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Effect_Aff["finally"](Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                      Effect_Ref.modify_(Data_Map_Internal["delete"](Halogen_Query_HalogenM.ordForkId)(fid))(v2.forks)();
                      return Effect_Ref.write(true)(doneRef)();
                    }))(evalM(render)(ref)(v1.value0))))(function (fiber) {
                      return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Control_Monad.unlessM(Effect.monadEffect)(Effect_Ref.read(doneRef))(Effect_Ref.modify_(Data_Map_Internal.insert(Halogen_Query_HalogenM.ordForkId)(fid)(fiber))(v2.forks))))(function () {
                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.Kill) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v2.forks)))(function (forkMap) {
                  return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableMaybe)(Effect_Aff.killFiber(Effect_Exception.error("Cancelled")))(Data_Map_Internal.lookup(Halogen_Query_HalogenM.ordForkId)(v1.value0)(forkMap)))(function () {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                  });
                });
              });
            }

            ;

            if (v1 instanceof Halogen_Query_HalogenM.GetRef) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(Data_Map_Internal.lookup(Data_Ord.ordString)(v1.value0)(v2.refs)));
              });
            }

            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 82, column 12 - line 146, column 33): " + [v1.constructor.name]);
          };
        };

        return Control_Monad_Free.foldFree(Effect_Aff.monadRecAff)(go(initRef))(v);
      };
    };
  };

  var evalF = function evalF(render) {
    return function (ref) {
      return function (v) {
        if (v instanceof Halogen_Query_Input.RefUpdate) {
          return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Function.flip(Effect_Ref.modify_)(ref)(Halogen_Aff_Driver_State.mapDriverState(function (st) {
            return {
              component: st.component,
              state: st.state,
              refs: Data_Map_Internal.alter(Data_Ord.ordString)(Data_Function["const"](v.value1))(v.value0)(st.refs),
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }

        ;

        if (v instanceof Halogen_Query_Input.Action) {
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v1) {
            return evalM(render)(ref)(v1["component"]["eval"](new Halogen_Query_HalogenQ.Action(v.value0, Data_Unit.unit)));
          });
        }

        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 51, column 20 - line 57, column 62): " + [v.constructor.name]);
      };
    };
  };

  exports["evalF"] = evalF;
  exports["evalQ"] = evalQ;
  exports["evalM"] = evalM;
  exports["handleLifecycle"] = handleLifecycle;
  exports["queueOrRun"] = queueOrRun;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Aff.Driver"] = $PS["Halogen.Aff.Driver"] || {};
  var exports = $PS["Halogen.Aff.Driver"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Coroutine = $PS["Control.Coroutine"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Fork_Class = $PS["Control.Monad.Fork.Class"];
  var Control_Monad_Rec_Class = $PS["Control.Monad.Rec.Class"];
  var Control_Parallel = $PS["Control.Parallel"];
  var Data_Either = $PS["Data.Either"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_List = $PS["Data.List"];
  var Data_List_Types = $PS["Data.List.Types"];
  var Data_Map_Internal = $PS["Data.Map.Internal"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Ord = $PS["Data.Ord"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Aff_AVar = $PS["Effect.Aff.AVar"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Console = $PS["Effect.Console"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Driver_Eval = $PS["Halogen.Aff.Driver.Eval"];
  var Halogen_Aff_Driver_State = $PS["Halogen.Aff.Driver.State"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_Data_Slot = $PS["Halogen.Data.Slot"];
  var Halogen_Query_EventSource = $PS["Halogen.Query.EventSource"];
  var Halogen_Query_HalogenQ = $PS["Halogen.Query.HalogenQ"];
  var Halogen_Query_Input = $PS["Halogen.Query.Input"];
  var newLifecycleHandlers = Effect_Ref["new"]({
    initializers: Data_List_Types.Nil.value,
    finalizers: Data_List_Types.Nil.value
  });
  var handleAff = Effect_Aff.runAff_(Data_Either.either(Effect_Exception.throwException)(Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit))));

  var handlePending = function handlePending(ref) {
    return function __do() {
      var queue = Effect_Ref.read(ref)();
      Effect_Ref.write(Data_Maybe.Nothing.value)(ref)();
      return Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(queue)(function () {
        var $25 = Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff));
        return function ($26) {
          return handleAff($25(Data_List.reverse($26)));
        };
      }())();
    };
  };

  var cleanupSubscriptionsAndForks = function cleanupSubscriptionsAndForks(v) {
    return function __do() {
      Control_Bind.bindFlipped(Effect.bindEffect)(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function () {
        var $27 = Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Map_Internal.foldableMap)(function () {
          var $29 = Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff);
          return function ($30) {
            return $29(Halogen_Query_EventSource.finalize($30));
          };
        }());
        return function ($28) {
          return handleAff($27($28));
        };
      }()))(Effect_Ref.read(v.subscriptions))();
      Effect_Ref.write(Data_Maybe.Nothing.value)(v.subscriptions)();
      Control_Bind.bindFlipped(Effect.bindEffect)(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Map_Internal.foldableMap)(function () {
        var $31 = Effect_Aff.killFiber(Effect_Exception.error("finalized"));
        return function ($32) {
          return handleAff($31($32));
        };
      }()))(Effect_Ref.read(v.forks))();
      return Effect_Ref.write(Data_Map_Internal.empty)(v.forks)();
    };
  };

  var runUI = function runUI(renderSpec) {
    return function (component) {
      return function (i) {
        var subscribe = function subscribe(fresh) {
          return function (ref) {
            return function (consumer) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_AVar.empty)(function (inputVar) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                  var listenerId = Effect_Ref.read(fresh)();
                  Effect_Ref.modify_(function (v) {
                    return v + 1 | 0;
                  })(fresh)();
                  Effect_Ref.modify_(Data_Map_Internal.insert(Data_Ord.ordInt)(listenerId)(inputVar))(ref)();
                  return listenerId;
                }))(function (listenerId) {
                  var producer = Control_Coroutine.producer(Effect_Aff.monadAff)(Data_Functor.map(Effect_Aff.functorAff)(Data_Either.either(Data_Function["const"](new Data_Either.Right(Data_Unit.unit)))(Data_Either.Left.create))(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(Effect_Aff_AVar.take(inputVar))));
                  return Data_Functor["void"](Effect_Aff.functorAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Coroutine.runProcess(Effect_Aff.monadRecAff)(Control_Coroutine.connect(Effect_Aff.monadRecAff)(Effect_Aff.parallelAff)(producer)(consumer)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Map_Internal["delete"](Data_Ord.ordInt)(listenerId))(ref)))(function () {
                      return Effect_Aff_AVar.kill(Effect_Exception.error("ended"))(inputVar);
                    });
                  })));
                });
              });
            };
          };
        };

        var rootHandler = function rootHandler(ref) {
          return function (message) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (listeners) {
              return Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Map_Internal.foldableMap)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff))(Data_Functor.map(Data_Map_Internal.functorMap)(Effect_Aff_AVar.put(message))(listeners));
            });
          };
        };

        var squashChildInitializers = function squashChildInitializers(lchs) {
          return function (preInits) {
            return Halogen_Aff_Driver_State.unDriverStateX(function (st) {
              var parentInitializer = Halogen_Aff_Driver_Eval.evalM(render)(st.selfRef)(st["component"]["eval"](new Halogen_Query_HalogenQ.Initialize(Data_Unit.unit)));
              return Effect_Ref.modify_(function (handlers) {
                return {
                  initializers: new Data_List_Types.Cons(Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Parallel.parSequence_(Effect_Aff.parallelAff)(Data_List_Types.foldableList)(Data_List.reverse(handlers.initializers)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(parentInitializer)(function () {
                      return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };

        var runComponent = function runComponent(lchs) {
          return function (handler) {
            return function (j) {
              return Halogen_Component.unComponent(function (c) {
                return function __do() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var = Halogen_Aff_Driver_State.initDriverState(c)(j)(handler)(lchs$prime)();
                  var pre = Effect_Ref.read(lchs)();
                  Effect_Ref.write({
                    initializers: Data_List_Types.Nil.value,
                    finalizers: pre.finalizers
                  })(lchs)();
                  Control_Bind.bindFlipped(Effect.bindEffect)(Halogen_Aff_Driver_State.unDriverStateX(function () {
                    var $33 = render(lchs);
                    return function ($34) {
                      return $33(function (v) {
                        return v.selfRef;
                      }($34));
                    };
                  }()))(Effect_Ref.read($$var))();
                  Control_Bind.bindFlipped(Effect.bindEffect)(squashChildInitializers(lchs)(pre.initializers))(Effect_Ref.read($$var))();
                  return $$var;
                };
              });
            };
          };
        };

        var renderChild = function renderChild(lchs) {
          return function (handler) {
            return function (childrenInRef) {
              return function (childrenOutRef) {
                return Halogen_Component.unComponentSlot(function (slot) {
                  return function __do() {
                    var childrenIn = Data_Functor.map(Effect.functorEffect)(slot.pop)(Effect_Ref.read(childrenInRef))();

                    var $$var = function () {
                      if (childrenIn instanceof Data_Maybe.Just) {
                        Effect_Ref.write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = Effect_Ref.read(childrenIn.value0.value0)();
                        Halogen_Aff_Driver_State.unDriverStateX(function (st) {
                          return function __do() {
                            Data_Function.flip(Effect_Ref.write)(st.handlerRef)(function () {
                              var $35 = Data_Maybe.maybe(Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Unit.unit))(handler);
                              return function ($36) {
                                return $35(slot.output($36));
                              };
                            }())();
                            return handleAff(Halogen_Aff_Driver_Eval.evalM(render)(st.selfRef)(st["component"]["eval"](new Halogen_Query_HalogenQ.Receive(slot.input, Data_Unit.unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }

                      ;

                      if (childrenIn instanceof Data_Maybe.Nothing) {
                        return runComponent(lchs)(function () {
                          var $37 = Data_Maybe.maybe(Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Unit.unit))(handler);
                          return function ($38) {
                            return $37(slot.output($38));
                          };
                        }())(slot.input)(slot.component)();
                      }

                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 236, column 14 - line 245, column 98): " + [childrenIn.constructor.name]);
                    }();

                    var isDuplicate = Data_Functor.map(Effect.functorEffect)(function ($39) {
                      return Data_Maybe.isJust(slot.get($39));
                    })(Effect_Ref.read(childrenOutRef))();
                    Control_Applicative.when(Effect.applicativeEffect)(isDuplicate)(Effect_Console.warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    Effect_Ref.modify_(slot.set($$var))(childrenOutRef)();
                    return Control_Bind.bind(Effect.bindEffect)(Effect_Ref.read($$var))(Halogen_Aff_Driver_State.renderStateX(Effect.functorEffect)(function (v) {
                      if (v instanceof Data_Maybe.Nothing) {
                        return Effect_Exception["throw"]("Halogen internal error: child was not initialized in renderChild");
                      }

                      ;

                      if (v instanceof Data_Maybe.Just) {
                        return Control_Applicative.pure(Effect.applicativeEffect)(renderSpec.renderChild(v.value0));
                      }

                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 250, column 37 - line 252, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };

        var render = function render(lchs) {
          return function ($$var) {
            return function __do() {
              var v = Effect_Ref.read($$var)();
              var shouldProcessHandlers = Data_Functor.map(Effect.functorEffect)(Data_Maybe.isNothing)(Effect_Ref.read(v.pendingHandlers))();
              Control_Applicative.when(Effect.applicativeEffect)(shouldProcessHandlers)(Effect_Ref.write(new Data_Maybe.Just(Data_List_Types.Nil.value))(v.pendingHandlers))();
              Effect_Ref.write(Halogen_Data_Slot.empty)(v.childrenOut)();
              Effect_Ref.write(v.children)(v.childrenIn)();
              var selfRef = Control_Category.identity(Control_Category.categoryFn)(v.selfRef);
              var pendingQueries = Control_Category.identity(Control_Category.categoryFn)(v.pendingQueries);
              var pendingHandlers = Control_Category.identity(Control_Category.categoryFn)(v.pendingHandlers);

              var handler = function () {
                var $40 = Halogen_Aff_Driver_Eval.queueOrRun(pendingHandlers);
                var $41 = Data_Functor["void"](Effect_Aff.functorAff);
                var $42 = Halogen_Aff_Driver_Eval.evalF(render)(selfRef);
                return function ($43) {
                  return $40($41($42($43)));
                };
              }();

              var childHandler = function () {
                var $44 = Halogen_Aff_Driver_Eval.queueOrRun(pendingQueries);
                return function ($45) {
                  return $44(handler(Halogen_Query_Input.Action.create($45)));
                };
              }();

              var rendering = renderSpec.render(function ($46) {
                return handleAff(handler($46));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children = Effect_Ref.read(v.childrenOut)();
              var childrenIn = Effect_Ref.read(v.childrenIn)();
              Halogen_Data_Slot.foreachSlot(Effect.applicativeEffect)(childrenIn)(function (v1) {
                return function __do() {
                  var childDS = Effect_Ref.read(v1)();
                  Halogen_Aff_Driver_State.renderStateX_(Effect.applicativeEffect)(renderSpec.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              Data_Function.flip(Effect_Ref.modify_)(v.selfRef)(Halogen_Aff_Driver_State.mapDriverState(function (ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  children: children,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Data_Maybe.Just(rendering),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return Control_Applicative.when(Effect.applicativeEffect)(shouldProcessHandlers)(Data_Function.flip(Control_Monad_Rec_Class.tailRecM(Control_Monad_Rec_Class.monadRecEffect))(Data_Unit.unit)(function (v1) {
                return function __do() {
                  var handlers = Effect_Ref.read(pendingHandlers)();
                  Effect_Ref.write(new Data_Maybe.Just(Data_List_Types.Nil.value))(pendingHandlers)();
                  Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function () {
                    var $47 = Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff));
                    return function ($48) {
                      return handleAff($47(Data_List.reverse($48)));
                    };
                  }())(handlers)();
                  var mmore = Effect_Ref.read(pendingHandlers)();
                  var $21 = Data_Maybe.maybe(false)(Data_List["null"])(mmore);

                  if ($21) {
                    return Data_Functor.voidLeft(Effect.functorEffect)(Effect_Ref.write(Data_Maybe.Nothing.value)(pendingHandlers))(new Control_Monad_Rec_Class.Done(Data_Unit.unit))();
                  }

                  ;
                  return new Control_Monad_Rec_Class.Loop(Data_Unit.unit);
                };
              }))();
            };
          };
        };

        var finalize = function finalize(lchs) {
          return Halogen_Aff_Driver_State.unDriverStateX(function (st) {
            return function __do() {
              cleanupSubscriptionsAndForks(st)();
              var f = Halogen_Aff_Driver_Eval.evalM(render)(st.selfRef)(st["component"]["eval"](new Halogen_Query_HalogenQ.Finalize(Data_Unit.unit)));
              Effect_Ref.modify_(function (handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Data_List_Types.Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return Halogen_Data_Slot.foreachSlot(Effect.applicativeEffect)(st.children)(function (v) {
                return function __do() {
                  var dsx = Effect_Ref.read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };

        var evalDriver = function evalDriver(disposed) {
          return function (ref) {
            return function (q) {
              return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(disposed)))(function (v) {
                if (v) {
                  return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Maybe.Nothing.value);
                }

                ;
                return Halogen_Aff_Driver_Eval.evalQ(render)(ref)(q);
              });
            };
          };
        };

        var dispose = function dispose(disposed) {
          return function (lchs) {
            return function (dsx) {
              return function (subsRef) {
                return Halogen_Aff_Driver_Eval.handleLifecycle(lchs)(function __do() {
                  var v = Effect_Ref.read(disposed)();

                  if (v) {
                    return Data_Unit.unit;
                  }

                  ;
                  Effect_Ref.write(true)(disposed)();
                  Control_Bind.bindFlipped(Effect.bindEffect)(Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Map_Internal.foldableMap)(function () {
                    var $49 = Effect_Aff_AVar.kill(Effect_Exception.error("disposed"));
                    return function ($50) {
                      return Effect_Aff.launchAff_($49($50));
                    };
                  }()))(Effect_Ref.read(subsRef))();
                  finalize(lchs)(dsx)();
                  return Halogen_Aff_Driver_State.unDriverStateX(function () {
                    var $51 = Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(renderSpec.dispose);
                    return function ($52) {
                      return $51(function (v1) {
                        return v1.rendering;
                      }($52));
                    };
                  }())(dsx)();
                });
              };
            };
          };
        };

        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(newLifecycleHandlers))(function (lchs) {
          return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](0)))(function (fresh) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](false)))(function (disposed) {
              return Halogen_Aff_Driver_Eval.handleLifecycle(lchs)(function __do() {
                var listeners = Effect_Ref["new"](Data_Map_Internal.empty)();
                var dsx = Control_Bind.bindFlipped(Effect.bindEffect)(Effect_Ref.read)(runComponent(lchs)(rootHandler(listeners))(i)(component))();
                return Halogen_Aff_Driver_State.unDriverStateX(function (st) {
                  return Control_Applicative.pure(Effect.applicativeEffect)({
                    query: evalDriver(disposed)(st.selfRef),
                    subscribe: subscribe(fresh)(listeners),
                    dispose: dispose(disposed)(lchs)(dsx)(listeners)
                  });
                })(dsx)();
              });
            });
          });
        });
      };
    };
  };

  exports["runUI"] = runUI;
})(PS);

(function (exports) {
  "use strict";

  exports._querySelector = function (selector) {
    return function (node) {
      return function () {
        return node.querySelector(selector);
      };
    };
  };
})(PS["Web.DOM.ParentNode"] = PS["Web.DOM.ParentNode"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.DOM.ParentNode"] = $PS["Web.DOM.ParentNode"] || {};
  var exports = $PS["Web.DOM.ParentNode"];
  var $foreign = $PS["Web.DOM.ParentNode"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];

  var querySelector = function querySelector(qs) {
    var $3 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $4 = $foreign["_querySelector"](qs);
    return function ($5) {
      return $3($4($5));
    };
  };

  exports["querySelector"] = querySelector;
})(PS);

(function (exports) {
  /* global window */
  "use strict";

  exports.window = function () {
    return window;
  };
})(PS["Web.HTML"] = PS["Web.HTML"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML"] = $PS["Web.HTML"] || {};
  var exports = $PS["Web.HTML"];
  var $foreign = $PS["Web.HTML"];
  exports["window"] = $foreign.window;
})(PS);

(function (exports) {
  "use strict";

  exports._readyState = function (doc) {
    return function () {
      return doc.readyState;
    };
  };
})(PS["Web.HTML.HTMLDocument"] = PS["Web.HTML.HTMLDocument"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.HTMLDocument.ReadyState"] = $PS["Web.HTML.HTMLDocument.ReadyState"] || {};
  var exports = $PS["Web.HTML.HTMLDocument.ReadyState"];
  var Data_Maybe = $PS["Data.Maybe"];

  var Loading = function () {
    function Loading() {}

    ;
    Loading.value = new Loading();
    return Loading;
  }();

  var Interactive = function () {
    function Interactive() {}

    ;
    Interactive.value = new Interactive();
    return Interactive;
  }();

  var Complete = function () {
    function Complete() {}

    ;
    Complete.value = new Complete();
    return Complete;
  }();

  var parse = function parse(v) {
    if (v === "loading") {
      return new Data_Maybe.Just(Loading.value);
    }

    ;

    if (v === "interactive") {
      return new Data_Maybe.Just(Interactive.value);
    }

    ;

    if (v === "complete") {
      return new Data_Maybe.Just(Complete.value);
    }

    ;
    return Data_Maybe.Nothing.value;
  };

  exports["Loading"] = Loading;
  exports["parse"] = parse;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.HTMLDocument"] = $PS["Web.HTML.HTMLDocument"] || {};
  var exports = $PS["Web.HTML.HTMLDocument"];
  var $foreign = $PS["Web.HTML.HTMLDocument"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Effect = $PS["Effect"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var Web_HTML_HTMLDocument_ReadyState = $PS["Web.HTML.HTMLDocument.ReadyState"];
  var toParentNode = Unsafe_Coerce.unsafeCoerce;
  var toDocument = Unsafe_Coerce.unsafeCoerce;

  var readyState = function () {
    var $0 = Data_Functor.map(Effect.functorEffect)(function () {
      var $2 = Data_Maybe.fromMaybe(Web_HTML_HTMLDocument_ReadyState.Loading.value);
      return function ($3) {
        return $2(Web_HTML_HTMLDocument_ReadyState.parse($3));
      };
    }());
    return function ($1) {
      return $0($foreign["_readyState"]($1));
    };
  }();

  exports["toDocument"] = toDocument;
  exports["toParentNode"] = toParentNode;
  exports["readyState"] = readyState;
})(PS);

(function (exports) {
  "use strict";

  exports._read = function (nothing, just, value) {
    var tag = Object.prototype.toString.call(value);

    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value);
    } else {
      return nothing;
    }
  };
})(PS["Web.HTML.HTMLElement"] = PS["Web.HTML.HTMLElement"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.HTMLElement"] = $PS["Web.HTML.HTMLElement"] || {};
  var exports = $PS["Web.HTML.HTMLElement"];
  var $foreign = $PS["Web.HTML.HTMLElement"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toNode = Unsafe_Coerce.unsafeCoerce;

  var fromElement = function fromElement(x) {
    return $foreign["_read"](Data_Maybe.Nothing.value, Data_Maybe.Just.create, x);
  };

  exports["fromElement"] = fromElement;
  exports["toNode"] = toNode;
})(PS);

(function (exports) {
  "use strict";

  exports.document = function (window) {
    return function () {
      return window.document;
    };
  };
})(PS["Web.HTML.Window"] = PS["Web.HTML.Window"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.HTML.Window"] = $PS["Web.HTML.Window"] || {};
  var exports = $PS["Web.HTML.Window"];
  var $foreign = $PS["Web.HTML.Window"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toEventTarget = Unsafe_Coerce.unsafeCoerce;
  exports["toEventTarget"] = toEventTarget;
  exports["document"] = $foreign.document;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.Aff.Util"] = $PS["Halogen.Aff.Util"] || {};
  var exports = $PS["Halogen.Aff.Util"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Function = $PS["Data.Function"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Web_DOM_ParentNode = $PS["Web.DOM.ParentNode"];
  var Web_Event_EventTarget = $PS["Web.Event.EventTarget"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_Event_EventTypes = $PS["Web.HTML.Event.EventTypes"];
  var Web_HTML_HTMLDocument = $PS["Web.HTML.HTMLDocument"];
  var Web_HTML_HTMLDocument_ReadyState = $PS["Web.HTML.HTMLDocument.ReadyState"];
  var Web_HTML_HTMLElement = $PS["Web.HTML.HTMLElement"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];

  var selectElement = function selectElement(query) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Control_Bind.bindFlipped(Effect.bindEffect)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)(function () {
      var $2 = Web_DOM_ParentNode.querySelector(query);
      return function ($3) {
        return $2(Web_HTML_HTMLDocument.toParentNode($3));
      };
    }())(Web_HTML_Window.document))(Web_HTML.window)))(function (mel) {
      return Control_Applicative.pure(Effect_Aff.applicativeAff)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Web_HTML_HTMLElement.fromElement)(mel));
    });
  };

  var runHalogenAff = Effect_Aff.runAff_(Data_Either.either(Effect_Exception.throwException)(Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit))));
  var awaitLoad = Effect_Aff.makeAff(function (callback) {
    return function __do() {
      var rs = Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_HTMLDocument.readyState)(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window))();

      if (rs instanceof Web_HTML_HTMLDocument_ReadyState.Loading) {
        var et = Data_Functor.map(Effect.functorEffect)(Web_HTML_Window.toEventTarget)(Web_HTML.window)();
        var listener = Web_Event_EventTarget.eventListener(function (v) {
          return callback(new Data_Either.Right(Data_Unit.unit));
        })();
        Web_Event_EventTarget.addEventListener(Web_HTML_Event_EventTypes.domcontentloaded)(listener)(false)(et)();
        return Effect_Aff.effectCanceler(Web_Event_EventTarget.removeEventListener(Web_HTML_Event_EventTypes.domcontentloaded)(listener)(false)(et));
      }

      ;
      callback(new Data_Either.Right(Data_Unit.unit))();
      return Effect_Aff.nonCanceler;
    };
  });
  var awaitBody = Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(awaitLoad)(function () {
    return Control_Bind.bind(Effect_Aff.bindAff)(selectElement("body"))(function (body) {
      return Data_Maybe.maybe(Control_Monad_Error_Class.throwError(Effect_Aff.monadThrowAff)(Effect_Exception.error("Could not find body")))(Control_Applicative.pure(Effect_Aff.applicativeAff))(body);
    });
  });
  exports["awaitBody"] = awaitBody;
  exports["selectElement"] = selectElement;
  exports["runHalogenAff"] = runHalogenAff;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.DOM.Element"] = $PS["Web.DOM.Element"] || {};
  var exports = $PS["Web.DOM.Element"];
  var Unsafe_Coerce = $PS["Unsafe.Coerce"];
  var toNode = Unsafe_Coerce.unsafeCoerce;
  exports["toNode"] = toNode;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.DOM"] = $PS["Halogen.VDom.DOM"] || {};
  var exports = $PS["Halogen.VDom.DOM"];
  var Data_Array = $PS["Data.Array"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Data_Tuple = $PS["Data.Tuple"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Types = $PS["Halogen.VDom.Types"];
  var Halogen_VDom_Util = $PS["Halogen.VDom.Util"];
  var Web_DOM_Element = $PS["Web.DOM.Element"];

  var haltWidget = function haltWidget(v) {
    return Halogen_VDom_Machine.halt(v.widget);
  };

  var patchWidget = function patchWidget(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchWidget(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Widget) {
      var res = Halogen_VDom_Machine.step(state.widget, vdom.value0);
      var res$prime = Halogen_VDom_Machine.unStep(function (v) {
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(v.value0, {
          build: state.build,
          widget: res
        }, patchWidget, haltWidget));
      })(res);
      return res$prime;
    }

    ;
    haltWidget(state);
    return state.build(vdom);
  };

  var haltText = function haltText(v) {
    var parent = Halogen_VDom_Util.parentNode(v.node);
    return Halogen_VDom_Util.removeChild(v.node, parent);
  };

  var patchText = function patchText(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchText(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Text) {
      if (state.value === vdom.value0) {
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, state, patchText, haltText));
      }

      ;

      if (Data_Boolean.otherwise) {
        var nextState = {
          build: state.build,
          node: state.node,
          value: vdom.value0
        };
        Halogen_VDom_Util.setTextContent(vdom.value0, state.node);
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchText, haltText));
      }

      ;
    }

    ;
    haltText(state);
    return state.build(vdom);
  };

  var haltKeyed = function haltKeyed(v) {
    var parent = Halogen_VDom_Util.parentNode(v.node);
    Halogen_VDom_Util.removeChild(v.node, parent);
    Halogen_VDom_Util.forInE(v.children, function (v1, s) {
      return Halogen_VDom_Machine.halt(s);
    });
    return Halogen_VDom_Machine.halt(v.attrs);
  };

  var haltElem = function haltElem(v) {
    var parent = Halogen_VDom_Util.parentNode(v.node);
    Halogen_VDom_Util.removeChild(v.node, parent);
    Halogen_VDom_Util.forEachE(v.children, Halogen_VDom_Machine.halt);
    return Halogen_VDom_Machine.halt(v.attrs);
  };

  var eqElemSpec = function eqElemSpec(ns1, v, ns2, v1) {
    var $56 = v === v1;

    if ($56) {
      if (ns1 instanceof Data_Maybe.Just && ns2 instanceof Data_Maybe.Just && ns1.value0 === ns2.value0) {
        return true;
      }

      ;

      if (ns1 instanceof Data_Maybe.Nothing && ns2 instanceof Data_Maybe.Nothing) {
        return true;
      }

      ;
      return false;
    }

    ;
    return false;
  };

  var patchElem = function patchElem(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchElem(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Elem && eqElemSpec(state.ns, state.name, vdom.value0, vdom.value1)) {
      var v = Data_Array.length(vdom.value3);
      var v1 = Data_Array.length(state.children);

      if (v1 === 0 && v === 0) {
        var attrs2 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
        var nextState = {
          build: state.build,
          node: state.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: state.children
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchElem, haltElem));
      }

      ;

      var onThis = function onThis(ix, s) {
        return Halogen_VDom_Machine.halt(s);
      };

      var onThese = function onThese(ix, s, v2) {
        var res = Halogen_VDom_Machine.step(s, v2);
        Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(res), state.node);
        return res;
      };

      var onThat = function onThat(ix, v2) {
        var res = state.build(v2);
        Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(res), state.node);
        return res;
      };

      var children2 = Halogen_VDom_Util.diffWithIxE(state.children, vdom.value3, onThese, onThis, onThat);
      var attrs2 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
      var nextState = {
        build: state.build,
        node: state.node,
        attrs: attrs2,
        ns: vdom.value0,
        name: vdom.value1,
        children: children2
      };
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchElem, haltElem));
    }

    ;
    haltElem(state);
    return state.build(vdom);
  };

  var patchKeyed = function patchKeyed(state, vdom) {
    if (vdom instanceof Halogen_VDom_Types.Grafted) {
      return patchKeyed(state, Halogen_VDom_Types.runGraft(vdom.value0));
    }

    ;

    if (vdom instanceof Halogen_VDom_Types.Keyed && eqElemSpec(state.ns, state.name, vdom.value0, vdom.value1)) {
      var v = Data_Array.length(vdom.value3);

      if (state.length === 0 && v === 0) {
        var attrs2 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
        var nextState = {
          build: state.build,
          node: state.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: state.children,
          length: 0
        };
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchKeyed, haltKeyed));
      }

      ;

      var onThis = function onThis(v2, s) {
        return Halogen_VDom_Machine.halt(s);
      };

      var onThese = function onThese(v2, ix$prime, s, v3) {
        var res = Halogen_VDom_Machine.step(s, v3.value1);
        Halogen_VDom_Util.insertChildIx(ix$prime, Halogen_VDom_Machine.extract(res), state.node);
        return res;
      };

      var onThat = function onThat(v2, ix, v3) {
        var res = state.build(v3.value1);
        Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(res), state.node);
        return res;
      };

      var children2 = Halogen_VDom_Util.diffWithKeyAndIxE(state.children, vdom.value3, Data_Tuple.fst, onThese, onThis, onThat);
      var attrs2 = Halogen_VDom_Machine.step(state.attrs, vdom.value2);
      var nextState = {
        build: state.build,
        node: state.node,
        attrs: attrs2,
        ns: vdom.value0,
        name: vdom.value1,
        children: children2,
        length: v
      };
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(state.node, nextState, patchKeyed, haltKeyed));
    }

    ;
    haltKeyed(state);
    return state.build(vdom);
  };

  var buildWidget = function buildWidget(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = Halogen_VDom_Machine.unStep(function (v1) {
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(v1.value0, {
        build: build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };

  var buildText = function buildText(v, build, s) {
    var node = Halogen_VDom_Util.createTextNode(s, v.document);
    var state = {
      build: build,
      node: node,
      value: s
    };
    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, state, patchText, haltText));
  };

  var buildKeyed = function buildKeyed(v, build, ns1, name1, as1, ch1) {
    var el = Halogen_VDom_Util.createElement(Data_Nullable.toNullable(ns1), name1, v.document);
    var node = Web_DOM_Element.toNode(el);

    var onChild = function onChild(k, ix, v1) {
      var res = build(v1.value1);
      Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(res), node);
      return res;
    };

    var children = Halogen_VDom_Util.strMapWithIxE(ch1, Data_Tuple.fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state = {
      build: build,
      node: node,
      attrs: attrs,
      ns: ns1,
      name: name1,
      children: children,
      length: Data_Array.length(ch1)
    };
    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, state, patchKeyed, haltKeyed));
  };

  var buildElem = function buildElem(v, build, ns1, name1, as1, ch1) {
    var el = Halogen_VDom_Util.createElement(Data_Nullable.toNullable(ns1), name1, v.document);
    var node = Web_DOM_Element.toNode(el);

    var onChild = function onChild(ix, child) {
      var res = build(child);
      Halogen_VDom_Util.insertChildIx(ix, Halogen_VDom_Machine.extract(res), node);
      return res;
    };

    var children = Halogen_VDom_Util.forE(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state = {
      build: build,
      node: node,
      attrs: attrs,
      ns: ns1,
      name: name1,
      children: children
    };
    return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, state, patchElem, haltElem));
  };

  var buildVDom = function buildVDom(spec) {
    var build = function build(v) {
      if (v instanceof Halogen_VDom_Types.Text) {
        return buildText(spec, build, v.value0);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Elem) {
        return buildElem(spec, build, v.value0, v.value1, v.value2, v.value3);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Keyed) {
        return buildKeyed(spec, build, v.value0, v.value1, v.value2, v.value3);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Widget) {
        return buildWidget(spec, build, v.value0);
      }

      ;

      if (v instanceof Halogen_VDom_Types.Grafted) {
        return build(Halogen_VDom_Types.runGraft(v.value0));
      }

      ;
      throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
    };

    return build;
  };

  exports["buildVDom"] = buildVDom;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.Thunk"] = $PS["Halogen.VDom.Thunk"] || {};
  var exports = $PS["Halogen.VDom.Thunk"];
  var Halogen_VDom_DOM = $PS["Halogen.VDom.DOM"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Util = $PS["Halogen.VDom.Util"];

  var Thunk = function () {
    function Thunk(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }

    ;

    Thunk.create = function (value0) {
      return function (value1) {
        return function (value2) {
          return function (value3) {
            return new Thunk(value0, value1, value2, value3);
          };
        };
      };
    };

    return Thunk;
  }();

  var unsafeEqThunk = function unsafeEqThunk(v, v1) {
    return Halogen_VDom_Util.refEq(v.value0, v1.value0) && Halogen_VDom_Util.refEq(v.value1, v1.value1) && v.value1(v.value3, v1.value3);
  };

  var thunk = function thunk(tid, eqFn, f, a) {
    return new Thunk(tid, eqFn, f, a);
  };

  var runThunk = function runThunk(v) {
    return v.value2(v.value3);
  };

  var buildThunk = function buildThunk(toVDom) {
    var haltThunk = function haltThunk(state) {
      return Halogen_VDom_Machine.halt(state.vdom);
    };

    var patchThunk = function patchThunk(state, t2) {
      var $43 = unsafeEqThunk(state.thunk, t2);

      if ($43) {
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(state.vdom), state, patchThunk, haltThunk));
      }

      ;
      var vdom = Halogen_VDom_Machine.step(state.vdom, toVDom(runThunk(t2)));
      return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(vdom), {
        vdom: vdom,
        thunk: t2
      }, patchThunk, haltThunk));
    };

    var renderThunk = function renderThunk(spec) {
      return function (t) {
        var vdom = Halogen_VDom_DOM.buildVDom(spec)(toVDom(runThunk(t)));
        return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(vdom), {
          thunk: t,
          vdom: vdom
        }, patchThunk, haltThunk));
      };
    };

    return renderThunk;
  };

  exports["buildThunk"] = buildThunk;
})(PS);

(function (exports) {
  "use strict";

  var getEffProp = function getEffProp(name) {
    return function (node) {
      return function () {
        return node[name];
      };
    };
  };

  exports._parentNode = getEffProp("parentNode");
  exports._nextSibling = getEffProp("nextSibling");

  exports.insertBefore = function (node1) {
    return function (node2) {
      return function (parent) {
        return function () {
          return parent.insertBefore(node1, node2);
        };
      };
    };
  };

  exports.appendChild = function (node) {
    return function (parent) {
      return function () {
        return parent.appendChild(node);
      };
    };
  };

  exports.removeChild = function (node) {
    return function (parent) {
      return function () {
        return parent.removeChild(node);
      };
    };
  };
})(PS["Web.DOM.Node"] = PS["Web.DOM.Node"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Web.DOM.Node"] = $PS["Web.DOM.Node"] || {};
  var exports = $PS["Web.DOM.Node"];
  var $foreign = $PS["Web.DOM.Node"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Nullable = $PS["Data.Nullable"];
  var Effect = $PS["Effect"];

  var parentNode = function () {
    var $3 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($4) {
      return $3($foreign["_parentNode"]($4));
    };
  }();

  var nextSibling = function () {
    var $14 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($15) {
      return $14($foreign["_nextSibling"]($15));
    };
  }();

  exports["parentNode"] = parentNode;
  exports["nextSibling"] = nextSibling;
  exports["insertBefore"] = $foreign.insertBefore;
  exports["appendChild"] = $foreign.appendChild;
  exports["removeChild"] = $foreign.removeChild;
})(PS);

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Halogen.VDom.Driver"] = $PS["Halogen.VDom.Driver"] || {};
  var exports = $PS["Halogen.VDom.Driver"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Data_Foldable = $PS["Data.Foldable"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_HeytingAlgebra = $PS["Data.HeytingAlgebra"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect = $PS["Effect"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Ref = $PS["Effect.Ref"];
  var Halogen_Aff_Driver = $PS["Halogen.Aff.Driver"];
  var Halogen_Aff_Driver_State = $PS["Halogen.Aff.Driver.State"];
  var Halogen_Component = $PS["Halogen.Component"];
  var Halogen_HTML_Core = $PS["Halogen.HTML.Core"];
  var Halogen_VDom_DOM = $PS["Halogen.VDom.DOM"];
  var Halogen_VDom_DOM_Prop = $PS["Halogen.VDom.DOM.Prop"];
  var Halogen_VDom_Machine = $PS["Halogen.VDom.Machine"];
  var Halogen_VDom_Thunk = $PS["Halogen.VDom.Thunk"];
  var Unsafe_Reference = $PS["Unsafe.Reference"];
  var Web_DOM_Node = $PS["Web.DOM.Node"];
  var Web_HTML = $PS["Web.HTML"];
  var Web_HTML_HTMLDocument = $PS["Web.HTML.HTMLDocument"];
  var Web_HTML_HTMLElement = $PS["Web.HTML.HTMLElement"];
  var Web_HTML_Window = $PS["Web.HTML.Window"];

  var substInParent = function substInParent(v) {
    return function (v1) {
      return function (v2) {
        if (v1 instanceof Data_Maybe.Just && v2 instanceof Data_Maybe.Just) {
          return Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.insertBefore(v)(v1.value0)(v2.value0));
        }

        ;

        if (v1 instanceof Data_Maybe.Nothing && v2 instanceof Data_Maybe.Just) {
          return Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.appendChild(v)(v2.value0));
        }

        ;
        return Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit);
      };
    };
  };

  var removeChild = function removeChild(v) {
    return function __do() {
      var npn = Web_DOM_Node.parentNode(v.node)();
      return Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(function (pn) {
        return Web_DOM_Node.removeChild(v.node)(pn);
      })(npn)();
    };
  };

  var mkSpec = function mkSpec(handler) {
    return function (renderChildRef) {
      return function (document) {
        var getNode = Halogen_Aff_Driver_State.unRenderStateX(function (v) {
          return v.node;
        });

        var done = function done(st) {
          if (st instanceof Data_Maybe.Just) {
            return Halogen_VDom_Machine.halt(st.value0);
          }

          ;
          return Data_Unit.unit;
        };

        var buildWidget = function buildWidget(spec) {
          var buildThunk = Halogen_VDom_Thunk.buildThunk(Data_Newtype.unwrap(Halogen_HTML_Core.newtypeHTML))(spec);

          var renderComponentSlot = function renderComponentSlot(cs) {
            var renderChild = Effect_Ref.read(renderChildRef)();
            var rsx = renderChild(cs)();
            var node = getNode(rsx);
            return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(node, Data_Maybe.Nothing.value, patch, done));
          };

          var render = function render(slot) {
            if (slot instanceof Halogen_Component.ComponentSlot) {
              return renderComponentSlot(slot.value0);
            }

            ;

            if (slot instanceof Halogen_Component.ThunkSlot) {
              var step = buildThunk(slot.value0);
              return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(step), new Data_Maybe.Just(step), patch, done));
            }

            ;
            throw new Error("Failed pattern match at Halogen.VDom.Driver (line 85, column 7 - line 90, column 75): " + [slot.constructor.name]);
          };

          var patch = function patch(st, slot) {
            if (st instanceof Data_Maybe.Just) {
              if (slot instanceof Halogen_Component.ComponentSlot) {
                Halogen_VDom_Machine.halt(st.value0);
                return renderComponentSlot(slot.value0);
              }

              ;

              if (slot instanceof Halogen_Component.ThunkSlot) {
                var step$prime = Halogen_VDom_Machine.step(st.value0, slot.value0);
                return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Halogen_VDom_Machine.extract(step$prime), new Data_Maybe.Just(step$prime), patch, done));
              }

              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 98, column 22 - line 104, column 79): " + [slot.constructor.name]);
            }

            ;
            return render(slot);
          };

          return render;
        };

        var buildAttributes = Halogen_VDom_DOM_Prop.buildProp(handler);
        return {
          buildWidget: buildWidget,
          buildAttributes: buildAttributes,
          document: document
        };
      };
    };
  };

  var renderSpec = function renderSpec(document) {
    return function (container) {
      var render = function render(handler) {
        return function (child) {
          return function (v) {
            return function (v1) {
              if (v1 instanceof Data_Maybe.Nothing) {
                return function __do() {
                  var renderChildRef = Effect_Ref["new"](child)();
                  var spec = mkSpec(handler)(renderChildRef)(document);
                  var machine = Halogen_VDom_DOM.buildVDom(spec)(v);
                  var node = Halogen_VDom_Machine.extract(machine);
                  Data_Functor["void"](Effect.functorEffect)(Web_DOM_Node.appendChild(node)(Web_HTML_HTMLElement.toNode(container)))();
                  return {
                    machine: machine,
                    node: node,
                    renderChildRef: renderChildRef
                  };
                };
              }

              ;

              if (v1 instanceof Data_Maybe.Just) {
                return function __do() {
                  Effect_Ref.write(child)(v1.value0.renderChildRef)();
                  var parent = Web_DOM_Node.parentNode(v1.value0.node)();
                  var nextSib = Web_DOM_Node.nextSibling(v1.value0.node)();
                  var machine$prime = Halogen_VDom_Machine.step(v1.value0.machine, v);
                  var newNode = Halogen_VDom_Machine.extract(machine$prime);
                  Control_Applicative.when(Effect.applicativeEffect)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean)))(Unsafe_Reference.unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }

              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 159, column 5 - line 175, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };

      return {
        render: render,
        renderChild: Control_Category.identity(Control_Category.categoryFn),
        removeChild: removeChild,
        dispose: removeChild
      };
    };
  };

  var runUI = function runUI(component) {
    return function (i) {
      return function (element) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Functor.map(Effect.functorEffect)(Web_HTML_HTMLDocument.toDocument)(Control_Bind.bindFlipped(Effect.bindEffect)(Web_HTML_Window.document)(Web_HTML.window))))(function (document) {
          return Halogen_Aff_Driver.runUI(renderSpec(document)(element))(component)(i);
        });
      };
    };
  };

  exports["runUI"] = runUI;
})(PS);

(function (exports) {
  "use strict"; // exports.testRec = function(rec) {
  //   return rec.foo;
  // }

  exports.logMe = function (x) {
    return function () {
      console.log(x);
    };
  };
})(PS["Main"] = PS["Main"] || {});

(function ($PS) {
  // Generated by purs version 0.13.6
  "use strict";

  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var $foreign = $PS["Main"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Corona_Chart_UI = $PS["Corona.Chart.UI"];
  var Corona_JHU = $PS["Corona.JHU"];
  var Data_Either = $PS["Data.Either"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Unit = $PS["Data.Unit"];
  var Effect_Aff = $PS["Effect.Aff"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Exception = $PS["Effect.Exception"];
  var Halogen_Aff_Util = $PS["Halogen.Aff.Util"];
  var Halogen_VDom_Driver = $PS["Halogen.VDom.Driver"];
  var main = Halogen_Aff_Util.runHalogenAff(Control_Bind.bind(Effect_Aff.bindAff)(Control_Bind.bind(Effect_Aff.bindAff)(Corona_JHU.fetchCoronaData)(function (v) {
    if (v instanceof Data_Either.Right) {
      return Control_Applicative.pure(Effect_Aff.applicativeAff)(v.value0);
    }

    ;

    if (v instanceof Data_Either.Left) {
      return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception.throwException(Effect_Exception.error(v.value0)));
    }

    ;
    throw new Error("Failed pattern match at Main (line 19, column 30 - line 21, column 53): " + [v.constructor.name]);
  }))(function (dat) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Halogen_Aff_Util.awaitBody)(function () {
      return Control_Bind.bind(Effect_Aff.bindAff)(Halogen_Aff_Util.selectElement("#ui"))(function (container) {
        if (container instanceof Data_Maybe.Nothing) {
          return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception.throwException(Effect_Exception.error("#ui not found")));
        }

        ;

        if (container instanceof Data_Maybe.Just) {
          return Halogen_VDom_Driver.runUI(Corona_Chart_UI.component(Effect_Aff.monadEffectAff)(dat))(Data_Unit.unit)(container.value0);
        }

        ;
        throw new Error("Failed pattern match at Main (line 24, column 3 - line 26, column 52): " + [container.constructor.name]);
      });
    });
  }));
  exports["main"] = main;
  exports["logMe"] = $foreign.logMe;
})(PS);

PS["Main"].main();
},{"process":"xND8"}]},{},["EcpK"], null)
//# sourceMappingURL=app.4f36bfbf.js.map