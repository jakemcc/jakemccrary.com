---
layout: post
title: "Switching my Ergodox to QMK firmware"
date: 2019-08-09 18:21:38 -0500
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'csv, keywords, here'
categories: 
- ergodox
---

```org
* I needed another ergodox, I bought an ergodox ez.
* ergodox ez uses the QMK firmware, and has some fancy keys setup to hit a bunch of keys at once
* I switched to using those bunch-of-keys-at-once keys for controlling my windows using Phoenix so I wouldn't conflict with Intellij keyboard shortcuts
* I'd prefer to have similar keys on my old Ergodox so I switched it to the QMK firmware.
* Here is what I did
** Used the QMK configurator to make the same layout I used for my ergodox ez
*** (maybe could have just used the ergox ez still?, check)
** Downloaded the source from the QMK configurator, compiled it. Had to install some prereqs `brew tap`, `brew avr thing`
** Needed LED to come on with different layers selected, looked at docs and added this to the source
** Flashed using teensy app
```

```c
// If we are not on the default layer, then toggle the LED
uint32_t layer_state_set_user(uint32_t state) {
  if (biton32(state) == 0) {
    ergodox_right_led_1_off();
  } else {
    ergodox_right_led_1_on();
  }
  return state;
}
```
