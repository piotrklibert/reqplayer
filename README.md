A viewer part of ReqPlayer - ReqViewer

## Installation

1. We need to compile Nginx and LuaJIT from source, so make sure you have
all the prerequisites installed. On Fedora you can do this with:

    sudo dnf builddep nginx luajit

2. Install Ansible, Erlang and Redis.
3. Go to `ansible` directory and run:

    ansible-playbook -i inventory.ini build.yml

(this will download, configure, compile and install the deps locally, in `resty`
directory)
4. Make sure you don't have

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
