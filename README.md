A viewer part of ReqPlayer - ReqViewer

## Running

You can customize how RQ will communicate with your Redis in app.config.

**NOTE** for now only straight HTTP is supported; HTTPS support is going to be
  implemented later.


## Feature requests

We've got our first real feature request!

1. Add an ability to "fix" a value returned from an URI so that all subsequent
   requests do not touch the backend at all, and response is served directly
   from ReqPlayer cache in Redis. Thanks to Norbert S. for bringing this up.


## TODO

1. Record when user changes session id (possibly because of logging in/out)
2. ~~Color code duration and session id~~
3. ~~Show method and POST body if relevant~~
4. ~~Do some minimal CSS, because right now it's... really bad. To say the least.~~
5. Make interface only reachable via HTTPS if not running in a local network.
6. Loading history: don't colorize all the data on load (this takes years), do
   it on display.
