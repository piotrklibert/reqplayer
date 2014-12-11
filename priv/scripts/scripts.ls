# ==============================================================================
## Format of incoming JSON data:
#
# id           = ngx.now(),
# ip           = ngx.var.remote_addr,
# uri          = ngx.var.uri,
# args         = ngx.req.get_uri_args(),
# sess         = sid,
# method       = ngx.var.request_method,
# time_taken   = ngx.ctx.time_taken,
# req = {
#  header=req_header,
#  body=req_body
# },
# resp = {
#  status=resp_status,
#  header=resp_header,
#  body=resp_body
# }
# ==============================================================================

_   = require "prelude-ls"
Vue = require "vue"


log = console~log
tee = (fun, v) --> fun(v); v
prettify = JSON.stringify _, null, 2


window.onload = ->
    sessions-to-colors-map = {}

    window.vm = new Vue do
        el: "body"
        data:
            pings: []
            filter-expr: ""


        computed:
            checked: ->
                _.filter (.checked), @pings

            encodedJsonData: ->
                checked-rows = _.filter (.checked), @pings
                "data:text/plain;base64," + btoa prettify checked-rows

            filtered: ->
                expr = new RegExp(@filter-expr)
                fun = if @filter-expr
                    -> expr.test(it.sess) or expr.test(it.method)
                else
                    -> it

                _.filter fun, @pings


        methods:
            deselect-all: -> _.map (.checked = false), @pings
            select-all: -> _.map (.checked = true), @pings
            toggle-body: -> it.show_body = not it.show_body

            formatTime: (time) ->
                (parseFloat(time) * 1000ms) .toFixed 2

            format-req: (req) ->
                args = ""
                for k,v of JSON.parse(req.args)
                    args += k + "=" + encodeURIComponent(v) + "&"
                args = args.replace(/&$/, "")

                "#{req.method} #{req.uri}?#{args}"

            formatDate: (timestamp) ->
                date = new Date(timestamp*1000ms)
                # year = date.getFullYear()
                # month = date.getMonth()  + 1
                # day = date.getDate()
                hour = date.getHours()
                min = date.getMinutes()
                sec = date.getSeconds()
                # "#day-#month-#year"
                "#hour:#min:#sec"

            format: (response-body) ->
                if /^{/.test response-body
                    prettify (JSON.parse response-body)
                else
                    prettify response-body

            colorize: (session) ->
                if session not of sessions-to-colors-map
                    sessions-to-colors-map[session] = random-rgb!

                sessions-to-colors-map[session]

            add-event: (data) ->
                data = JSON.parse data

                # add some properties (needed for keeping frontend state) before
                # passing data to Vue.js so that it can initialize Observables
                data.show_body = false
                data.checked = false

                data.args = prettify data.args
                this.$data.pings.unshift data


    sock = new WebSocket("ws://#{location.host}/websocket/")

    sock.onopen = ->
        log "Connecting socket..."
        sock.send "init"

    sock.onmessage = ->
        data = it.data
        if data == "ok"
            log "Socket connected!"
        else
            log "Event arrived!"
            vm.add-event data


hsv-to-rgb = (h, s, v) ->
    h_i = Math.round(h*6)
    f = h*6 - h_i
    p = v * (1 - s)
    q = v * (1 - f*s)
    t = v * (1 - (1 - f) * s)

    [r,g,b] = switch h_i
    | 0         => [v, t, p]
    | 1         => [q, v, p]
    | 2         => [p, v, t]
    | 3         => [p, q, v]
    | 4         => [t, p, v]
    | 5         => [v, p, q]
    | otherwise => [p, v, t]

    [Math.round(r*256), Math.round(g*256), Math.round(b*256)]


f = (colors, target) ->
    for c in _.values colors
        diff = _.map (([a,b]) -> Math.abs(a - b)), (_.zip target, c)
        if diff < 45
            return

random-rgb = ->
    [r,g,b] = hsv-to-rgb Math.random(), 0.5, 0.45
    "rgb(#r, #g, #b)"
