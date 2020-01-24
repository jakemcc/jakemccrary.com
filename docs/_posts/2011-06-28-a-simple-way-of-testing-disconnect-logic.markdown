---
comments: true
layout: post
title: A simple way of testing disconnect logic
categories: [linux, testing, command-line, utilities]
---

I'm guessing that software you write connects to some other server. I'm also guessing that how it handles disconnects is tested (if ever tested) by either killing the process it connects to or by pulling out your network cable. I recently stumbled across a nifty Linux command line tool that makes causing disconnects significantly easier.

This tool is [tcpkill](http://linux.die.net/man/8/tcpkill). To use `tcpkill` you specify an interface and a [tcpdump](http://linux.die.net/man/8/tcpdump) style filter and it kills traffic on that interface that matches the filter.

For example, if your application has a connection to 192.168.1.130, then to force a disconnect you would execute `tcpkill -i eth0 host 192.168.1.130`.

`tcpkill` can be used for more than forcing disconnects. It can also be used as a simple website filter. If [Stack Overflow](http://stackoverflow.com) wastes too much of your time then you could simply leave `tcpkill -i eth0 host stackoverflow.com` running and enjoy your increased productivity.

`tcpkill` is a pretty useful tool. If you want to install it in Ubuntu it is found in the dsniff package (`apt-get install dsniff`).
