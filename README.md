eeventd
=====

An OTP application for bridging a redis queue to server sent events.

Build
-----

    $ rebar3 compile


Config
------

The minimal configuration required is an ini-style config
file, containig the sections `[security]` and `[eventd]`,
providing a shared secret for authenticating web clients 
and the eventd configuration.

The app config is usually shared with the event producing
application, e.g. a django project.

### Example:
    
    [security]
    secret_key = 2342foobar

    [eventd]
    enabled = true
    redis_server = localhost:6379
    redis_db = 0

    redis_queue = eventd_events

    default_stream = notifications

    stream_url = http://localhost:2323/eventd/events
    listen = localhost:2323


## Running
    
In development mode using rebar, pass the 
configuration file via the `APP_CONFIG` environment variable:

    APP_CONFIG=/path/to/app/etc/app.conf rebar3 auto


