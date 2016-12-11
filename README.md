# crone

Cron for Erlang.

crone is a simple application to schedule repeatable tasks within the Erlang
VM. It is modelled on the Unix utility, `cron`.

It has no application dependencies other than `kernel` and `stdlib`.

## Usage
By default, the list of scheduled tasks is defined by the `crontab` variable
of the `crone` application. For example, the following sys.config defines a
single task that is scheduled to log a message every 15 minutes.

```erlang
[{crone, 
  [{crontab, {{[0,15,30,45],any,any,any},{error_logger,info_msg,["Hello world.~n"]}}}]}].
```

## CronTab format
The crontab format is a list of crontab entries. The general format of each
crontab entry is {TimePattern, MFA}. When the time pattern matches, the
specified action is invoked.

The TimePattern is made of 4 components - {Minute, Hour, Day, Month}. The
following table lists the valid numerical range of each component:

Component | Range
----------|-------
Minute    | 0 - 59
Hour      | 0 - 23
Day       | 1 - 31
Month     | 1 - 12

Additionally, each component may have the wildcard atom `any` which always
matches, and set of specific values may be expressed via a list.

## Build

    $ rebar3 compile
