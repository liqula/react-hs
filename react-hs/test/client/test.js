window['hsreact$log_when_updated'] = React['createClass']({
    'displayName': "log-when-updated",
    'componentWillUpdate': function() {
        var msg;
        if (this.ulId) {
            msg = "Update in ul " + this.ulId + ": " + this['props']['message'];
        } else {
            msg = "Update: " + this['props']['message'];
        }
        hsreact$log_message(msg);
        console.log(msg);
    },
    'render': function() {
        var that = this;
        return React['createElement']('span',
                                      { 'ref': function(r) {
                                          if (r) {
                                            var e = r.closest("ul");
                                            if (e) that.ulId = e.id;
                                          }
                                        }
                                      },
                                      this['props']['message']);
    }
});

function hsreact$log_message(m) {
  if (!window['test_client_output']) window['test_client_output'] = [];
  window['test_client_output']['push'](m);
}

window['hsreact$callback_wrapper'] = React['createClass']({
    'displayName':'callback wrapper',
    'render': function() {
        return React['createElement']('div', {}, [ React['createElement']('p', {'key': 'para'}, 'From Callback')
                                                 , React['createElement']('div', {'key': 'callback'},
                                                                          this['props']['foo'](5, 'Hello World'))
                                                 ]);
    }
});
