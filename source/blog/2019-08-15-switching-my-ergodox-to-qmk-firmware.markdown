---
dated-url: true
layout: post
title: Switching my Ergodox to QMK firmware
date: 2019-08-15 19:07 -0500
comments: true
published: true
description: Here are some steps I followed for flashing my hand-built Ergodox to the QMK firmware.
keywords: ergodox, qmk, qmk firmware, ergodox ez
categories:
- ErgoDox
- hardware
- keyboard
---

Last fall I started to work in an office again. I've used a hand-built Ergodox for years now and really prefer working on it. This meant I needed another ergodox for the office. Luckily, now you don't have to build your own. I bought an [Ergodox EZ](https://ergodox-ez.com)[^1].

The Ergodox EZ uses the [QMK](https://github.com/qmk/qmk_firmware) firmware. This has a lot of fancier options than the firmware I had been using on my hand-built ergodox.

This mostly didn't matter and I just configured the Ergodox EZ to match my original Ergodox's layout. Then I started a new job and found myself programming in Scala using IntelliJ IDEA.

Shockingly, after not using IntelliJ for years, I still remembered many of the keyboard shortcuts. This was great! Unfortunately, in my years since last using IntelliJ, I created some conflicting keyboard shortcuts for managing my window layout. These were mostly shortcuts that involved holding Command + Alt and pushing an arrow key. Luckily, the QMK firmware supports a *Meh* key.

What is the *Meh* key? It is a key that presses Control + Alt + Shift all at the same time.

This is great for setting up shortcuts that don't conflict with ones found in most normal programs. This let me [change my window manger](https://github.com/jakemcc/cljs-phoenix/commit/fa2186589d99a4763c7bf79e1f795cb910063a4e) shortcuts to use the *Meh* key and I was then conflict free.

I can't handle having different shortcuts across different machines with the same OS, so I needed to needed to update my original Ergodox to use the QMK firmware so I could also have a *Meh* key at home. Luckily, the QMK firmware also works on it and, maybe even more luckily, the Ergodox EZ firmware just works with my original Ergodox.

This actually means I can simply take the compiled Ergodox EZ firmware and flash it straight to my Ergodox. Any time I've done this the keyboard keeps working.

Unfortunately, the LEDs in my original Ergodox are mostly hidden by non-translucent keys. These LEDs indicate when I'm not main layer and I find them really useful. I only have a single translucent keycap and would prefer only that LED to be used.

Here are the steps I took to make that change.

1. Use the [graphical QMK Configurator](https://config.qmk.fm/#/ergodox_ez/LAYOUT_ergodox) to visually configure my keyboard layout. In the **Keymap Name** field, put `jakemcc`.
1. Click the **Compile** button in the above configurator.
1. Download the full source.
1. Unzip the source and edit `qmk_firmware/keyboards/ergodox_ez/keymaps/jakemcc/keymap.c` to include snippet of code below this list.
1. In `qmk_firmware` run `make ergodox_ez:jakemcc`.
1. Find `ergodox_ez_jakemcc.hex` and flash my original Ergodox.

```c
uint32_t layer_state_set_user(uint32_t state) {
  if (biton32(state) == 0) {
    ergodox_right_led_1_off();
  } else {
    ergodox_right_led_1_on();
  }
  return state;
}
```

This snippet gets added to the bottom of the `keymap.c`. It only turns on led 1, which is the one under my translucent key, whenever my keyboard isn't on layer 0.

Now, I can use the fancy *Meh* key to be conflict free and easily tell when I'm not on my main layer. This is wonderful.
 

[^1]: I bought one with Cherry MX Clear switches. I've since switched them to Cherry MX Browns. The clears were too firm for me. I did not get Cherry MX Blues because I didn't want my fellow coworkers to be annoyed by the glorious clickty-clack of those switches.
 