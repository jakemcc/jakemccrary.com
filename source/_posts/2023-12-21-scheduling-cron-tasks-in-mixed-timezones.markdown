---
layout: post
title: "Scheduling cron tasks in mixed timezones"
date: 2023-12-21 09:26:42 -0600
comments: true
published: false
description: Ever want to schedule cron tasks in different timezones? Here is how.
keywords: 'cron, timezone, linux'
categories: 
- linux
- cron
---

Have you ever needed to schedule a repeating task on a Linux host?
If so, you've probably reached for cron.
cron is widely available and reliable; it is a great choice for scheduling tasks.

Sometimes you find yourself in a situation where the programs you need to run should really be scheduled in a different timezone than the timezone of the host running cron.
This is a common need if your programs are interacting with systems hosted in different areas of the world.
If one system you interact with starts up at 7 AM Europe/Berlin and another at 8 AM Australia/Syndey, it would be much better to schedule your program to run using times specified in those timezones.

Why is that prefered?
- If you schedule in your host timezone, you have to convert from the other timezone to your own. This is error prone.
- Different timezones have different Daylights savings shifts. Having to adjust your schedule when your host or target timezone shifts is error prone.

Luckily, you can do this with cron!
At least, with the cronie implementation.

You do this by specifying the timezone in the crontab with the `CRON_TZ` variable.
Any line scheduling a task after a `CRON_TZ` specification is scheduled in the specified timezone.
This persists until the next `CRON_TZ` value is specified.

Below is a sample crontab that schedules four tasks.
One is scheduled in the host timezone, two in Australia/Sydney, and one in Europe/Berlin.

```
0 7 * * * echo "run at 7 AM in the host timezone"

CRON_TZ=Australia/Sydney
0 7 * * * echo "Run at 7 AM Sydney"
10 7 * * * echo "Run at 7:10 AM Sydney"

CRON_TZ=Europe/Berlin
* 8 * * * edcho "Run at 8 AM Berlin"
```

The one gotcha with this is that cronic's behavior is unspecified if the scheduled time ends up in the daylights savings shift of the host machine[^1].
So make sure you don't do that.

[^1]: We have unit tests that confirm someone hasn't configured a task to run within one of these periods.

My team at work has been taking advantage of this feature for over a year now for scheduling all of our processes start and end times.
It has been working great.
Prior to figuring[^2] this out, the fall and spring time shifts were sources of issues as various countries shifted at different days.
That entire source of problem has been solved through scheduling tasks in the proper timezone.

[^2]: Figuring this out was a bit of a chore. Even the Linux experts I talked to weren't aware of being able to do this. Digging through the source of cronic was how I figured this out. Hopefully this article makes it easier for the next person.






