_          = require "prelude-ls"
$          = require "jquery"
Vue        = require "vue"

# {debounce} = require "underscore"
# qajax      = require "qajax"

dict = (...args)->
    obj = {}
    for x from 0 til args.length by 2
        obj[args[x]] = args[x+1]
    obj

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







# <div class="transaction" v-repeat="d: data" v-on="">
#     <h1>URI: {{d.uri}} STATUS: {{d.resp.status}}</h1>
#     <h3 v-on="click: hide(d.req)">Request</h3>
#     <table v-class="hidden: d.req.hidden">
#     <tr class="req" v-repeat="d.req.header">
#         <td>{{ $key }}</td>
#         <td>{{ $value }}</td>
#     </tr>
#     </table>
#     <h3 v-on="click: hide(d.resp)">Response</h3>
#     <table v-class="hidden: d.resp.hidden">
#         <tr class="resp" v-repeat="d.resp.header">
#             <td>{{ $key }}</td>
#             <td>{{ $value }}</td>
#         </tr>
#     </table>
#     <p class="title" v-text="keys(d.resp)">asd</p>
# </div>
