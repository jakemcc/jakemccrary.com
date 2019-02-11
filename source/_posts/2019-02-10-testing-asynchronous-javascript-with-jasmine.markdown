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

I was recently working on the web UI for an an internal tool and was adding a feature to capture all unhandled errors and report them to the backing service. The hardest part of this task was figuring out how to test the code that was reporting the errors.

If the error reporting failed, I didn't want to lose that error and I didn't want it to trigger reporting another error. I decided to log this type of error to the console. I wanted to make sure that a future me (or another dev) didn't accidentally remove the logging code and accidentally enable a loop of reporting failed reporting attempts.

It took me a while to figure out how to do this. I searched the web and found various articles about using [Jasmine](https://jasmine.github.io/) to do async tests. They were helpful but I also needed to mock out a function, `console.error`, and assert that it was called. None of the examples I found were explicit about doing something like this. I forget how many different approaches I tried, but it took a while to figure out the below solution.

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

It took me a while to come up with the below snippet for testing the error handling feature of `reportEvent`. Below is the test.

```javascript
describe("reporting events", function() {
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

The `console.error` spy sets is setup to call a fake function. That function asserts that the `console.error` spy has been called. It also calls a `done` function. That `done` function is a callback passed to your test by Jasmine. Calling it signals that your async work is completed.

If `done` is never called then Jasmine will fail the test after some amount of time. By calling `done` in our `console.error` fake, we're able to signal to Jasmine that we've handled the rejected promise.

You don't actually need the `expect(console.error).toHaveBeenCalled();` as `done` won't be called unless `console.error` has been called. If you don't have it though then Jasmine will complain there are no assertions in the test.

So there we have it, an example of using some of Jasmine's asynchronous test support with spies. Finding an article like this would have saved me some time. Hopefully it saves you, and future me, some time.
