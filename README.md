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
  [{crontab, [{{[0,15,30,45],any,any,any},{error_logger,info_msg,["Hello world.~n"]}}]}]}].
```

## CronTab format
The crontab format is a list of crontab entries. The general format of each
crontab entry is {TimePattern, MFA}. When the time pattern matches, the
specified action is invoked.

The TimePattern is made of 5 components - {Minute, Hour, Day, DayOfWeek, Month}. The
following table lists the valid numerical range of each component:

Component | Range
----------|---------
Minute    |   0 .. 59
Hour      |   0 .. 23
Day       | -31 .. -1 , 1 - 31
DayOfWeek |  -7 .. -1 , 1 -  7
Month     |   1 - 12

The day may be specified as a positive number in which case it represents
that day of the month. If specified as a non-positive number it represents
the offset from the last day of the month (so -1 represents the last day
of each month, and -2 represents the penultimate day of each month.)

As an alternative to specifying the numeric day of the mnth, day of the
week may be specified where 1 is Monday, 2 is Tuesday etc. Specifying a
negative number (-7 .. -1) means the last weekday of the month.

Additionally, each component may have the wildcard atom `any` which always
matches, and set of specific values may be expressed via a list.

## Build

    $ rebar3 compile
