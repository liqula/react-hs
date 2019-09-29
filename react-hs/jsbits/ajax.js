/* jshint sub:true */

var hsreact$ajaxCallback;

function hsreact$setAjaxCallback(c) {
    hsreact$ajaxCallback = c;
}

function hsreact$ajax(req, handler) {
    var xhr = new XMLHttpRequest();

    xhr['open'](req['reqMethod'], req['reqURI']);

    var lst = req['reqHeaders'];
    for (var i = 0; i < lst.length; i++) {
        xhr['setRequestHeader'](lst[i][0], lst[i][1]);
    }

    if (req['reqTimeout']) {
        xhr['timeout'] = req['reqTimeout'];
        xhr['ontimeout'] = function() {
            hsreact$ajaxCallback(xhr, {hs: handler, timedOut: true});
        }
    }

    xhr['onreadystatechange'] = function() {
        // a status of 0 with DONE occurs when the request is timed out
        if (xhr['readyState'] === XMLHttpRequest['DONE'] && xhr['status'] !== 0) {
            hsreact$ajaxCallback(xhr, {hs: handler, timedOut: false});
        }
    };
    
    if (req['reqMethod'] == 'GET' || req['reqMethod'] == 'HEAD') {
        // According to XMLHttpRequest spec: https://xhr.spec.whatwg.org/#the-send()-method
        // the implementation should ignore body if method is GET/HEAD, but
        // apparently either iOS or react-native doesn't ignore it.
        xhr['send']();
    } else {
        xhr['send'](req['reqBody']);
    }
}
