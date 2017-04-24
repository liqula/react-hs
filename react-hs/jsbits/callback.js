function hsreact$mk_arguments_callback(f) {
    return function() {
        var args = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++) {
            args[i] = arguments[i];
        }
        f(args);
    };
}

function hsreact$wrap_callback_returning_element(f) {
    return function() {
        var args = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++) {
            args[i] = arguments[i];
        }
        var ret = {};
        f(ret, args);
        return ret.elem;
    };
}
