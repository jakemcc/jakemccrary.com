---
layout: post
title: "Scheduling cron tasks in mixed time zones"
date: 2023-12-21 09:26:42 -0600
comments: true
published: false
description: Ever want to schedule cron tasks in different time zones? Here is how.
keywords: 'cron, time zone, linux'
categories: 
- linux
- cron
---

Have you ever needed to schedule a repeating task on a Linux host?
If so, you've probably reached for cron.
cron is widely available and reliable; it is a great choice for scheduling tasks.

Sometimes you find yourself scheduling a task and, ideally, you'd be scheduling that task referencing a different time zone.
This is a common need if your programs are interacting with systems hosted in different areas of the world.
If one system you interact with starts up at 7 AM `Europe/London` and another at 8 AM `America/New_York`, it would be much better to schedule your program to run using times specified in those time zones.

Why is that preferred?

- If you schedule in your host time zone, you have to convert from the other time zone to your own. This is error prone.
- Different time zones have different Daylights savings shifts. Having to adjust your schedule when your host or target time zone shifts is error prone.

Luckily, you can do this with cron!
At least, with the cronie implementation.

You do this by specifying the time zone in the crontab with the `CRON_TZ` variable.
Any line after a `CRON_TZ` specification is scheduled in the specified time zone.
This persists until the next `CRON_TZ` value is specified.

Below is a sample crontab that schedules four tasks.
One is scheduled in the host time zone, two in `America/New_York`, and one in `Europe/London`.

```
0 7 * * * echo "run at 7 AM in the host time zone"

CRON_TZ=America/New_York
0 7 * * * echo "Run at 7 AM New York"
10 7 * * * echo "Run at 7:10 AM New York"

CRON_TZ=Europe/London
* 8 * * * echo "Run at 8 AM London"
```

The one gotcha with this is that cronie's behavior is unspecified if the scheduled time ends up in the daylights savings shift of the host machine[^1].
So make sure you don't do that.

[^1]: We have unit tests that confirm someone hasn't configured a task to run within one of these periods.

My team at work has been taking advantage of this feature since early 2023 for scheduling all of our processes start and end times.
It has been working great.
Prior to figuring[^2] this out, the fall and spring time shifts were sources of issues as various countries shifted on different days.
That entire source of problems has been solved through scheduling tasks in the proper time zone.

[^2]: Figuring this out was a bit of a chore. Even the Linux experts I talked to weren't aware of being able to do this. Digging through the source of cronie was how I figured this out. Hopefully this article makes it easier for the next person. Though, now that I know the `CRON_TZ` solution, it is pretty easy to search and find other folks talking about this.

