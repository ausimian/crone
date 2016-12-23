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
  [{crontab, [{{[0,15,30,45],any,any,any,any},{error_logger,info_msg,["Hello world.~n"]}}]}]}].
```

## CronTab format
The crontab format is a list of crontab entries. The general format of each
crontab entry is {TimePattern, MFA}. When the time pattern matches, the
specified action is invoked.

The TimePattern is made of 5 components - {Minute, Hour, Day, DayOfWeek, Month}. The
following table lists the valid numerical range of each component:

Component | Range               | Remarks
----------|---------------------|----------------------
Minute    |             0 .. 59 |
Hour      |             0 .. 23 |
Day       | -31 .. -1 , 1 .. 31 | Negative values count backwards from the last day of the month i.e. -2 means 'penultimate day of the month'.
DayOfWeek |  -7 .. -1 , 1 ..  7 | 1 is Monday, 7 is Sunday, Negative values mean the last day of week in the month i.e. -3 means 'last Wednesday of the month'.
Month     |             1 .. 12 |

Additionally:

- Any component may have the wildcard atom `any` which always matches.
- Any component may be expressed as a list in which case the elements are considered alternatives.

## CronTab Sources
When the crone application is started it, by default, calls `crone:get_crontab/0` to retrieve the configuration. If an alternative source of configuration is required, override the `crontab_provider` environment variable, e.g.

```erlang
[{crone, 
  [{crontab_provider, {mymodule, getcrontab, []}}]}]
```
and ensure that the specified module exists and exports the required function. The function should return a tuple of the type `{ok, crone:crontab()}`.

### Dynamic Update
The configuration can be replaced dynamically by calling `crone:update/1`. The argument is a term of type `crone:crontab`. Providing an invalid configuration causes the caller to crash.


## Build and test

    $ rebar3 compile
    $ rebar3 proper
    
