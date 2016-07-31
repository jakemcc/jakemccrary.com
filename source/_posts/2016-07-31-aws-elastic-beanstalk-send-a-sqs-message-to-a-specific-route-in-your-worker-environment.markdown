---
layout: post
title: 'AWS Elastic Beanstalk: Send a SQS message to a specific route in your worker
  environment'
date: 2016-07-31 12:16 -0500
comments: true
published: true
description: By default messages to an AWS EB Worker environment hit a single endpoint.
  Here is how you can vary the endpoint for each message.
keywords: aws, elastic beanstalk, sqs
categories:
- AWS
- Elastic Beanstalk
---

[Lumanu](https://lumanu.com) uses
[AWS Elastic Beanstalk](https://aws.amazon.com/elasticbeanstalk/). Elastic
Beanstalk (from now on abbreviated as EB) helps you provision and tie
together Amazon services to fairly easily get web applications and
services running with push button (or command line) deploys. We've
been using EB with a multi-container docker deploy for nearly a year
now and it pretty much just works.

EB has a concept of environment tiers and there are two different
types; a web tier and a worker tier. Web tier environments provide
the configuration and components necessary for serving HTTP requests
in a scalable
fashion. [Worker environments](http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features-managing-env-tiers.html)
are designed to run operations that you wouldn't want performed by
your front-end serving web application.

A major difference between the two environment tiers is that a worker
environment provisions a [SQS](https://aws.amazon.com/sqs/) queue and
provides a daemon that reads from this queue and POSTs messages to an
instance of your worker service. This daemon prevents your worker
service from having to connect to and manage a SQS queue. By default,
the daemon POSTs messages to `http://localhost/`. You can optionally
configure it to POST to a different route.

It is possible to have different messages POST to different
routes. You can do this by setting the `beanstalk.sqsd.path` attribute
on your SQS message. For example, if you want your worker service to
receive a message at `/trigger-email` you would set the
`beanstalk.sqsd.path` attribute to `/trigger-email`.
