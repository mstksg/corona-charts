
"use strict";

exports._initAutocomplete = function (opts,sel,place,callback) {

    return function () {

        const options = opts.slice(0);
        const selector = "#" + sel;

        // console.log(options,sel, place);

        const ac = new autoComplete({
            data: {
                src: function () {
                    // console.log(options.slice(0));
                    return options.slice(0);
                },
                cache: false
            },
            placeholder: place,
            selector: selector,
            searchEngine: 'loose',
            highlight: true,
            maxResults: 5,
            resultsList: {                       // Rendered results list object      | (Optional)
                render: true,
                container: source => {
                    source.setAttribute("id", "autocomplete-list-" + sel);
                },
                destination: document.querySelector(selector),
                position: "afterend",
                element: "ul"
            },
            resultItem: (function (data, source) { source.innerHTML = data; }),
            onSelection: function(feedback) {
              // console.log(feedback);
              callback(feedback.selection.value)();
            }
        });

        // console.log(ac);

        const setOpts = function (newOpts) {
            options.splice(0,options.length);
            options.push.apply(options,newOpts);
        };

        return { setter: setOpts };

    }

};

exports._setOpts = function(ac, xs) {
    return function () {
        ac.setter(xs);
    }
}
