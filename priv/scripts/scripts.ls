_   = require "prelude-ls"
$   = require "jquery"
Vue = require "vue"
m   = require "mori"

# {debounce} = require "underscore"
# qajax      = require "qajax"

# id           = ngx.now(),
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

log = console~log
tee = (fun, v) --> fun(v); v

$ ->
    sessions-to-colors-map = {}

    window.vm = new Vue(
        el: "body"
        data:
            pings: []
            filter-expr: ""

        computed:
            checked: ->
                _.filter (.checked), @pings

            clicked: ->
                tmpl = "data:text/plain;base64,"
                _.filter (.checked), @pings
                    |> JSON.stringify _, null, 2
                    |> btoa
                    |> tee log
                    |> (tmpl +)

            filtered: ->
                expr = new RegExp(@filter-expr)
                fun = if @filter-expr
                    ->  expr.test(it.sess) or expr.test(it.method)
                else
                    -> it

                _.filter fun, @pings

        methods:
            deselect-all: ->
                for x in @pings
                    x.checked = false

            select-all: ->
                for x in @pings
                    x.checked = true

            formatTime: (time) ->
                (parseFloat(time) * 1000ms).toFixed 2

            format-resp: (response-body) ->
                if /^{/.test response-body
                    JSON.stringify (JSON.parse response-body), null, 2
                else
                    response-body

            format: ->
                try
                    parsed = JSON.parse it
                    JSON.stringify parsed, null, 2
                catch ex
                    JSON.stringify it,null, 2

            toggle-body: ->
                it.show_body = not it.show_body

            hide: ->
                it.hidden = not it.hidden

            colorize: (session) ->
                if session not of sessions-to-colors-map
                    sessions-to-colors-map[session] = random-rgb!

                sessions-to-colors-map[session]


    sock = new WebSocket("ws://#{location.host}/websocket/")

    sock.onopen = ->
        sock.send "init"

    sock.onmessage = ->
        try
            data = JSON.parse it.data
            data.show_body = false
            data.checked = false
            if data.args?
                data.args = JSON.stringify data.args
                vm.$data.pings.unshift data
            else
                console.log "handshake"
        catch
            console.log "Got malformed data from server", it


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

random-rgb = ->
    [r,g,b] = hsv-to-rgb Math.random(), 0.5, 0.95
    "rgb(#r, #g, #b)"
