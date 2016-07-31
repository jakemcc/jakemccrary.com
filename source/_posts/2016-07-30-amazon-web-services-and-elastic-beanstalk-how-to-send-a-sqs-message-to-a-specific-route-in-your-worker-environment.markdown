---
layout: post
title: "AWS Elastic Beanstalk: Send a SQS message to a specific route in your worker environment"
date: 2016-07-30 13:57:52 -0500
comments: true
published: false
description: By default messages to an AWS EB Worker environment hit a single endpoint. Here is how you can vary the endpoint for each message.
keywords: 'aws, elastic beanstalk, sqs'
categories: 
- AWS
- Elastic Beanstalk
---

[Lumanu](https://lumanu.com) runs on
[AWS Elastic Beanstalk](https://aws.amazon.com/elasticbeanstalk/). Elastic
Beanstalk, EB, helps you provision and tie together other Amazon
services to fairly easily get web applications and services running
with push button (or command line) deploys. We've been using EB with a
multi-container docker deploy for nearly a year now and it pretty much
just works.

EB has a concept of __environment tiers__ and there are two different
types; a web tier and a worker tier. Web tier environments are provide
the configuration and components necessary for serving HTTP requests
in a scalable
fashion. [Worker environments](http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features-managing-env-tiers.html)
are designed to run operations that you wouldn't want performed by
your front-end serving web application. An EB worker environment
provides tooling to pull messages off an SQS queue and POST them to an
instance of your worker service. This lets you ignore connecting and
managing a SQS queue from your worker service.

By default, your worker service receives messages POSTed to
`http://localhost/`. You can optionally configure EB to POST to a
different route. Every message placed on the worker environment's SQS
queue ends up being POSTed to the same route.

It is possible to have different messages POST to different routes. If
you set the `beanstalk.sqsd.path` attribute on your SQS message then
the message will POST to the value of that attribute. For example, if
you want your worker service to receive a message at `/trigger-email`
you would set the `beanstalk.sqsd.path` attribute to `/trigger-email`.
