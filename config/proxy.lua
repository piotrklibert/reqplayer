local _     = require("fun")
local js    = require("cjson")
local redis = require("resty.redis")


TARGET_URI = "/_proxied"
SESSION_COOKIE_NAME = "sessionid"
DEFAULT_SESSION = "UNKNOWN_SESSION"


--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

-- strip leading and trailing whitespace from a string, return new string
function strip(str)
   str = string.gsub(str, "^%s+", "")
   str = string.gsub(str, "%s+$", "")
   return str
end


-- split string on a given delimiter, return a table with all the substrings
function split(str, sep)
   if not sep then sep = "&" end

   local parts = {}
   local start = 0

   for _it, n, char in _.enumerate(str) do
      if char == sep then
         table.insert(parts, strip(string.sub(str, start, n-1)))
         start = n+1
      end
   end

   table.insert(parts, strip(string.sub(str, start, -1)))

   return parts
end


-- helper function for unwrapping results of `split` if it only has two strings
function split2(str, sep)
   local res = split(str, sep)
   if #res < 2 then
      return nil, nil
   end
   return res[1], res[2]
end


function split_cookies(s)
    local result = {}
    for n,part in ipairs(split(s, ";")) do
       local cookie, val = split2(part, "=")
       -- cookie and name will be nil for a malformed cookie name and value
       if cookie and val then
          result[strip(cookie)] = strip(val)
       end
    end
   return result
end

--------------------------------------------------------------------------------
-- REQUEST/RESPONSE DATA GATHERING
--------------------------------------------------------------------------------

function copy_resp_headers (headers)
   -- Insert headers returned from sub-request into our response headers
   for key, val in pairs(headers) do ngx.header[key] = val end
end




-- Either return current session id extracted from a given cookie or nil if it's
-- not there.
function get_session_id()
   local cookies = ngx.req.get_headers()["Cookie"]

   if not cookies then
      return DEFAULT_SESSION
   end

   -- ngx.log(ngx.EMERG, "something in cookie" .. cookies)

   local cc = split_cookies(cookies)

   -- ngx.log(ngx.EMERG, "table in cookie!" .. js.encode(cc))

   local sid = cc[SESSION_COOKIE_NAME]
   if not sid then
      return DEFAULT_SESSION
   else
      return sid
   end
end


local function get_response()
   -- disable gzipping of the response so that we can inspect it if needed
   local orig_encoding = ngx.req.get_headers()["Accept-Encoding"]
   ngx.req.set_header("Accept-Encoding", "identity")

   local start_time = ngx.now()
   local result = ngx.location.capture(
      TARGET_URI .. ngx.var.uri,
      {
         method = ngx["HTTP_" .. ngx.var.request_method],
         args = ngx.req.get_uri_args()
      }
   )
   ngx.ctx.time_taken = ngx.now() - start_time

   -- restore content encoding which a client wanted
   ngx.req.set_header("Accept-Encoding", orig_encoding)

   return result.status, result.header, result.body
end



--------------------------------------------------------------------------------
-- REDIS INTERFACE
--------------------------------------------------------------------------------

function save_to_redis(sid, req_header, req_body, resp_header, resp_body, resp_status)
   local r = redis:new()
   r:connect("127.0.0.1", 6379)

   local trace_data = {
      id           = ngx.now(),
      uri          = ngx.var.uri,
      args         = ngx.req.get_uri_args(),
      sess         = sid,
      method       = ngx.var.request_method,
      time_taken   = ngx.ctx.time_taken,
      req = {
         header=req_header,
         body=req_body
      },
      resp = {
         status=resp_status,
         header=resp_header,
         body=resp_body
      }
   }

   -- append the data to a list associated with current session
   r:rpush(sid, js.encode(trace_data))
   r:publish("newreq", js.encode(trace_data))
end



--------------------------------------------------------------------------------
-- MAIN FUNCTION
--------------------------------------------------------------------------------

function create_response()
   -- make sure we have all of the request before proceeding
   ngx.req.read_body()

   local sid = get_session_id()
   local status, headers, body = get_response()

   save_to_redis(sid, ngx.req.get_headers(), ngx.req.get_body_data(),
                 headers, body, status)

   copy_resp_headers(headers)
   ngx.status = status
   ngx.send_headers()
   ngx.say(body)

   return ngx.exit(200)  -- this does not override ngx.status; it's here because
                         -- ngx.exit needs an argument to be called
end



return create_response()
