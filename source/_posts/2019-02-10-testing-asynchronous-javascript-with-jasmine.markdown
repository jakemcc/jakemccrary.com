---
layout: post
title: "Testing asynchronous JavaScript with Jasmine"
date: 2019-02-10 17:15:25 -0600
comments: true
published: false
description: I recently struggled to get some asynchronous JavaScript code under test. Here is my solution.
keywords: javascript, jasmine, testing
categories: 
- javascript
- testing
---

I was recently adding a feature to an internal web UI that caught all unhandled JavaScript errors and reported them to the backend service. The implementation went smoothly with most of the effort spent figuring out how to test the code that was reporting the errors.

If the error reporting failed, I didn't want to trigger reporting another error or completely lose that error. I decided to log a reporting error to the console. I wanted to write a test showing that errors reporting errors were handled so that a future me, or another developer, didn't accidentally remove this special error handling and enable a never ending cycle of of reporting failed reporting attempts.

It took me a while to figure out how to do this. I searched the web and found various articles about using [Jasmine](https://jasmine.github.io/) to do async tests. They were helpful but I also wanted to mock out a function, `console.error`, and assert that it was called. None of the examples I found were explicit about doing something like this. I forget how many different approaches I tried, but it took a while to figure out the below solution.

Here is the code I wanted to test.

```javascript
function reportEvent(event) {
  return fetch('/report-event', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({name: 'ui name', ...event})
  }).catch(function(e) { console.error('Problem reporting event:', e)});
}
```

It takes an incoming `event` object and merges it with a default value and posts that to the backing service. `fetch` returns a Promise and the code handles errors by calling `catch` on it and logging.

Below is what I eventually came up with for testing the error handling feature of `reportEvent`.

```javascript
describe('reporting events', function() {
  it('logs errors', (done) => {
    spyOn(console, 'error').and.callFake(() => {
      expect(console.error).toHaveBeenCalled();
      done();
    });
    spyOn(window, 'fetch').and.returnValue(Promise.reject('error!'));
    reportEvent({level: 'WARN', msg: 'ERROR!'});
  });
});
```

This uses `spyOn` to mock out `fetch` and `console.error`. The `fetch` call is told to return a rejected Promise. The `console.error` spy is a bit more interesting.

The `console.error` spy is told to call a fake function. That function asserts that the `console.error` spy has been called. More importantly, it also calls a `done` function. That `done` function is a callback passed to your test by Jasmine. Calling `done` signals that your async work is completed.

If `done` is never called then Jasmine will fail the test after some amount of time. By calling `done` in our `console.error` fake, we're able to signal to Jasmine that we've handled the rejected promise.

You don't actually need the `expect(console.error).toHaveBeenCalled();` as `done` won't be called unless `console.error` has been called. If you don't have it though then Jasmine will complain there are no assertions in the test.

So there we have it, an example of using some of Jasmine's asynchronous test features with spies. I wish I had found an article like this when I started this task. Hopefully it saves you, and future me, some time.
