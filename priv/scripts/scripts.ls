_          = require "prelude-ls"
$          = require "jquery"
Vue        = require "vue"

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

COLORS = <[ red blue green teal olive black ]>

$ ->
    sessions-to-colors-map = {}

    vm = new Vue(
        el: "body"
        data: {
            pings: []
        }
        methods: {
            formatTime: (time) ->
                (parseFloat(time) * 1000ms).toFixed 2
            keys: Object.keys

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

            colorize: ->
                if it not of sessions-to-colors-map
                    color = COLORS[ Math.floor (Math.random() * COLORS.length)]
                    COLORS.splice(COLORS.indexOf(color), 1)
                    sessions-to-colors-map[it] = color
                sessions-to-colors-map[it]

        }
    )

    sock = new WebSocket("ws://#{location.host}/websocket/")

    sock.onopen = ->
        sock.send "init"

    sock.onmessage = ->
        try
            data = JSON.parse it.data
            data.show_body = false
            console.log JSON.parse it.data
            if data.args?
                data.args = JSON.stringify data.args
                vm.$data.pings.unshift data
            else
                console.log "handshake"
        catch
            console.log "Got malformed data from server", it
