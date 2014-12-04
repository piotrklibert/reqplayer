_          = require "prelude-ls"
$          = require "jquery"
Vue        = require "vue"

# {debounce} = require "underscore"
# qajax      = require "qajax"


$ ->
    vm = new Vue(
        el: "body"
        data: {
            pings: []
        }
        methods: {
            formatTime: (time) ->
                (parseFloat(time) * 1000ms).toFixed 2
            keys: Object.keys
            format-trace-body: ->
                try
                    parsed = JSON.parse it
                    JSON.stringify parsed, null, 2
                catch ex
                    it

            toggle-body: ->
                it.show_body = not it.show_body
            hide: ->
                it.hidden = not it.hidden

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
