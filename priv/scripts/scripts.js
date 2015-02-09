// Generated by LiveScript 1.3.1
(function(){
  var _, Vue, jx, log, tee, prettify, getJSON, hsvToRgb, f, randomRgb, slice$ = [].slice;
  _ = require('prelude-ls');
  Vue = require('vue');
  jx = require('jx');
  log = bind$(console, 'log');
  tee = curry$(function(fun, v){
    fun(v);
    return v;
  });
  prettify = partialize$.apply(JSON, [JSON.stringify, [void 8, null, 2], [0]]);
  getJSON = function(url, callback){
    return jx.get(url).success(callback);
  };
  window.onload = function(){
    var sessionsToColorsMap, sock;
    sessionsToColorsMap = {};
    window.vm = new Vue({
      el: "body",
      data: {
        pings: [],
        filterExpr: "",
        history: false
      },
      computed: {
        count: function(){
          return this.filtered.length;
        },
        selectedRows: function(){
          return _.reverse(
          _.filter(function(it){
            return it.checked;
          })(
          this.filtered));
        },
        encodedJsonData: function(){
          return "data:text/plain;base64," + btoa(prettify(this.selectedRows));
        },
        filtered: function(){
          var expr, fun;
          expr = new RegExp(this.filterExpr.toLowerCase());
          fun = this.filterExpr
            ? function(it){
              return expr.test(it.sess) || expr.test(it.method);
            }
            : function(it){
              return it;
            };
          return _.filter(fun, this.pings);
        }
      },
      methods: {
        deselectAll: function(){
          return _.map(function(it){
            return it.checked = false;
          }, this.pings);
        },
        selectAll: function(){
          return _.map(function(it){
            return it.checked = true;
          }, this.pings);
        },
        toggleBody: function(it){
          return it.show_body = !it.show_body;
        },
        formatTime: function(time){
          return (parseFloat(time) * 1000).toFixed(2);
        },
        formatReq: function(req){
          var args, k, ref$, v;
          args = "";
          for (k in ref$ = JSON.parse(req.args)) {
            v = ref$[k];
            args += k + "=" + encodeURIComponent(v) + "&";
          }
          args = args.replace(/&$/, "");
          return req.method + " " + req.uri + "?" + args;
        },
        formatDate: function(timestamp){
          var date, hour, min, sec;
          date = new Date(timestamp * 1000);
          hour = date.getHours();
          min = date.getMinutes();
          sec = date.getSeconds();
          return hour + ":" + min + ":" + sec;
        },
        format: function(responseBody){
          if (/^{/.test(responseBody)) {
            return prettify(JSON.parse(responseBody));
          } else {
            return prettify(responseBody);
          }
        },
        colorize: function(session){
          if (!(session in sessionsToColorsMap)) {
            sessionsToColorsMap[session] = randomRgb();
          }
          return sessionsToColorsMap[session];
        },
        addEvent: function(data){
          var ex;
          try {
            data = JSON.parse(data);
          } catch (e$) {
            ex = e$;
          }
          data.show_body = false;
          data.checked = false;
          data.args = prettify(data.args);
          this.$data.count++;
          return this.$data.pings.unshift(data);
        }
      }
    });
    getJSON("/history", _.map(bind$(vm, 'addEvent')));
    sock = new WebSocket("ws://" + location.host + "/websocket/");
    sock.onopen = function(){
      log("Connecting socket...");
      return sock.send("init");
    };
    return sock.onmessage = function(it){
      var data;
      data = it.data;
      if (data === "ok") {
        return log("Socket connected!");
      } else {
        log("Event arrived!");
        return vm.addEvent(data);
      }
    };
  };
  hsvToRgb = function(h, s, v){
    var h_i, f, p, q, t, ref$, r, g, b;
    h_i = Math.round(h * 6);
    f = h * 6 - h_i;
    p = v * (1 - s);
    q = v * (1 - f * s);
    t = v * (1 - (1 - f) * s);
    ref$ = (function(){
      switch (h_i) {
      case 0:
        return [v, t, p];
      case 1:
        return [q, v, p];
      case 2:
        return [p, v, t];
      case 3:
        return [p, q, v];
      case 4:
        return [t, p, v];
      case 5:
        return [v, p, q];
      default:
        return [p, v, t];
      }
    }()), r = ref$[0], g = ref$[1], b = ref$[2];
    return [Math.round(r * 256), Math.round(g * 256), Math.round(b * 256)];
  };
  f = function(colors, target){
    var i$, ref$, len$, c, diff;
    for (i$ = 0, len$ = (ref$ = _.values(colors)).length; i$ < len$; ++i$) {
      c = ref$[i$];
      diff = _.map(fn$, _.zip(target, c));
      if (diff < 45) {
        return;
      }
    }
    function fn$(arg$){
      var a, b;
      a = arg$[0], b = arg$[1];
      return Math.abs(a - b);
    }
  };
  randomRgb = function(){
    var ref$, r, g, b;
    ref$ = hsvToRgb(Math.random(), 0.5, 0.45), r = ref$[0], g = ref$[1], b = ref$[2];
    return "rgb(" + r + ", " + g + ", " + b + ")";
  };
  function bind$(obj, key, target){
    return function(){ return (target || obj)[key].apply(obj, arguments) };
  }
  function curry$(f, bound){
    var context,
    _curry = function(args) {
      return f.length > 1 ? function(){
        var params = args ? args.concat() : [];
        context = bound ? context || this : this;
        return params.push.apply(params, arguments) <
            f.length && arguments.length ?
          _curry.call(context, params) : f.apply(context, params);
      } : f;
    };
    return _curry();
  }
  function partialize$(f, args, where){
    var context = this;
    return function(){
      var params = slice$.call(arguments), i,
          len = params.length, wlen = where.length,
          ta = args ? args.concat() : [], tw = where ? where.concat() : [];
      for(i = 0; i < len; ++i) { ta[tw[0]] = params[i]; tw.shift(); }
      return len < wlen && len ?
        partialize$.apply(context, [f, ta, tw]) : f.apply(context, ta);
    };
  }
}).call(this);
