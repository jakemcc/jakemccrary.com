---
layout: post
title: "ErgoDox: Turn on LED When Not on the Main Layer"
date: 2014-09-07 16:23
comments: true
published: false
categories:
- ergodox
---

The
[ErgoDox](http://jakemccrary.com/blog/2014/07/27/building-the-ergodox-keyboard/)
is a great keyboard. One of the appeals of it is that you can
build your own firmware. This makes it possible to rearrange the keys
however you want and tweak other functionality. They firmware is
fairly advanced and allows you to have multiple layers to your
keyboard.

Multiple layers allow the ErgoDox to have fewer keys than traditional
keyboards. How often do you use an F key? If you are like me the
answer is almost never. Why bother having a dedicated key?

Another benefit of multiple layers is your keyboard is basically
multiple keyboards in one. Do you use the Dvorak layout and your
roommate use Qwerty? Program a Dvorak layer and a Qwerty layer into
your keyboard and switch between them with the push of a button.

One downside of multiple layers is you will occasionally switch layers
by accident. This is frustrating since all of a sudden your keyboard
is working differently than you expect.

If you use the Massdrop ErgoDox configurator the
[base firmware](https://github.com/benblazak/ergodox-firmware) doesn't
help you know what layer you are using. The ErgoDox has a
[few LEDs](https://github.com/benblazak/ergodox-firmware/blob/513b82d585fdc7175db736163340af3ed6c6f38b/src/main.c#L124-L133)
in it that I have never used. I don't even have the needed keys as
part of my keyboard layout (Caps lock? Who still has a caps lock? I don't
need to shout that often). I decided to repurpose the num lock LED as
an indicator that I'm off the main layer.

This was a straight forward change. In the firmware there is a
[variable](https://github.com/benblazak/ergodox-firmware/blob/513b82d585fdc7175db736163340af3ed6c6f38b/src/main.c#L171)
that holds what keyboard layer is active. All I had to do to get the
num lock LED on when I changed layers was the `layers_head` variable
higher in `main.c` and then change the conditional to turn on the num
lock LED when `layers_head != 0`.
[This](https://github.com/jakemcc/ergodox-firmware/commit/383f16a3f091b4e2dd031d098007c4289cc1a261)
is the commit that does this change. It could have been done as a
three line change.
