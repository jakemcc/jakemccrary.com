---
dated-url: true
layout: post
title: ! 'ErgoDox: Turn on an LED When Not on the Main Layer'
date: 2014-09-07 17:35
comments: true
published: true
categories:
- ErgoDox
- keyboard
---

The [ErgoDox](http://jakemccrary.com/blog/2014/07/27/building-the-ergodox-keyboard/) is a great keyboard.
One its appeals is that you can build your own firmware.
This makes it possible to rearrange the keys however you want and tweak other functionality.
The firmware is fairly advanced and allows you to have multiple layers to your keyboard.

Multiple layers allow the ErgoDox to have fewer physical keys than traditional keyboards.
How often do you use an F key?
If you are like me the answer is almost never.
Why bother having a dedicated key?

Another benefit of multiple layers is that your keyboard is multiple keyboards in one.
Do you use the Dvorak layout and your roommate use Qwerty?
Program both a Dvorak layer and a Qwerty layer into your keyboard and switch between them with the push of a button.

The only downside I've noticed of multiple layers is that I'll switch between them by accident.
This is frustrating as all of a sudden your keyboard works differently and there is no indication that you are on a different layer.

The ErgoDox has a [few LEDs](https://github.com/benblazak/ergodox-firmware/blob/513b82d585fdc7175db736163340af3ed6c6f38b/src/main.c#L124-L133) in it that I have never used.
I don't even have the needed keys as part of my keyboard layout (Caps lock?
Who uses caps lock?
I don't need to shout that often).
I decided to repurpose the num lock LED as an indicator that I'm off the main layer.

This was a straight forward change.
In the firmware there is a [variable](https://github.com/benblazak/ergodox-firmware/blob/513b82d585fdc7175db736163340af3ed6c6f38b/src/main.c#L171) that holds what keyboard layer is active.
All I had to do to get the num lock LED on when I changed layers was to move the `layers_head` variable higher in `main.c` and then change the conditional to turn on the num lock LED when `layers_head != 0`.
[This](https://github.com/jakemcc/ergodox-firmware/commit/383f16a3f091b4e2dd031d098007c4289cc1a261) is the commit that does this change.
It could have been done as a three line change.

I highly recommend making this change.
Now I just need to find a transparent DSA keycaps so I can see the LED easier.