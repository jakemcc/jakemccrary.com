---
layout: post
title: "Using only ChatGPT to write an activity tracker web app"
date: 2023-02-14 20:56:06 -0600
comments: true
published: false
description: PUT SUMMARY HERE 
keywords: 'chatgpt, ai, artifical intelligence, javascript, webapp'
categories: 
- chatgpt
---

[ChatGPT](https://openai.com/blog/chatgpt/) was released in late 2022 and it made a big splash.
For those who aren't aware, ChatGPT is an AI with a easy to use, conversational web interface.
The chat interface makes interacting with it intuitive and the model does a generally good job at coming up with responses.

For example, I asked ChatGPT to write an intro paragraph for this article and here is what it provided.

My prompt: "I'm writing a blog post where I show how I wrote a simple web app only using you, ChatGPT, to write the code. Could you help me write a good intro paragraph?"

ChatGPT's intro paragraph:
> In today's digital age, it's easier than ever to create a web application, even without any prior programming knowledge. Thanks to advances in natural language processing and machine learning, tools like ChatGPT can help you write code with just plain English. In this article, I'll show you how I created a simple web app using only ChatGPT to write the code. You'll see just how accessible programming has become and how powerful tools like ChatGPT can be. Let's dive in!

That's a pretty good paragraph, though I thought it was a misleading as I thought my experience programming was pretty useful when trying this exercise with ChatGPT.

Let's re-prompt it and see how that intro paragraph could be rewritten.

My prompt: "I've been programming for over twenty years and think having some experience programming was helpful in using ChatGPT to program. Could you revise the paragraph to reflect this?"

> In today's digital age, it's easier than ever to create a web application, even with only a basic understanding of programming. Thanks to advances in natural language processing and machine learning, tools like ChatGPT can help you write code with just plain English. As someone who has been programming for over twenty years, I found that my experience was helpful in using ChatGPT to program. In this article, I'll show you how I created a simple web app using ChatGPT to write the code. You'll see how even with a basic programming knowledge, tools like ChatGPT can be an accessible and powerful addition to your programming toolkit. Let's dive in!

Alright, that's better.

## The Goal

I wanted to try to write a simple web application only using ChatGPT.
In the spirit of [creating small programs](http://localhost:4000/blog/2020/10/03/go-create-silly-small-programs/) to solve a minor annoyance, I decided to try to make an activity tracker.

Features of the tracker:

- I could interact with it from a phone, computer, or tablet.
- It should have some number of buttons that, when clicked, record an activity was done
- Apperance doesn't matter
- I should be able to refresh the page and not lose state
- I don't want to manage any infrastructure

The goal was to create that tracker and do so without editing any of the source ChatGPT provided.
I planned on restricting myself to only "coding" by typing into ChatGPT and copy-and-pasting into an editor.

So, how did it go?

## Starting off

My prompt:
> I want to make a webapp I can use from my phone to track common activities. It should have buttons that when pressed record that a press happened. The buttons should be for Book, TV, Computer, Phone Game, VR Game.

ChatGPT:
> You can create a web app using HTML, CSS, and JavaScript for the frontend and a server-side language such as Node.js or Python for the backend. You can store the activities data in a database such as MongoDB or SQL and retrieve it when needed to display it on the frontend. You can also use JavaScript to record the activity each time a button is pressed. You may also want to consider using a mobile-responsive framework like Bootstrap to ensure your web app works well on mobile devices.

Alright, that is a pretty decent start.
I don't want to have to run any code on a server though.

My prompt:
> I don't really want to host my own backend and database. Are there any options?

ChatGPT:
> Yes, there are several options for hosting your web app without having to manage your own backend and database. Here are a few popular choices:

> 1. Firebase: A fully managed backend platform that includes real-time databases, authentication, and hosting. It integrates well with frontend frameworks like React and Angular.

> 2. AWS Amplify: A set of tools and services provided by Amazon Web Services that makes it easy to build scalable, secure and fast mobile and web applications.

> 3. Heroku: A cloud platform that makes it easy to deploy, run, and manage web apps. It supports multiple programming languages, including Node.js, Python, and Ruby.

> 4. Google Cloud Platform: A suite of cloud computing services offered by Google that includes hosting, databases, and backend services.

> Each of these platforms has its own set of features and pricing plans, so you can choose the one that best fits your needs and budget.

This reponse was ok.
Firebase is definitely an option that fulfills my ask and I'm not familiar enough with AWS Amplify to say it couldn't.
Heroku, at least the way I've used it before, isn't a good answer.
Google Cloud Platform might have options that keep me from running my own backend service, especially if you consider Firebase part of it, but I'm not familiar enough with all of their offerings to consider this a slam dunk.

Of the four options, I know Firebase will work.

## Let's write some files

I've resisted just taking screenshots of the conversation but here I'm going to need to.
The layout of the response is important and recreating it well is going to be too challenging.

{% img /images/chatgpt/directory-layout.png %}

This is pretty reasonable.
To nitpick it, ChatGPT suggested a `src/components/button.js` file but then didn't explain what it is, though it does explain what the containing directory is for.

Personally, I would not start off a project like this with `button.js`.
I'd just keep all the JavaScript in a single file to keep the project simpler.

ChatGPT has provided a pretty reasonable layout, let's try to get some files.

My prompt:
> What could the contents of index.html and script.js be?

Below is what it provided.
ChatGPT english in quote blocks and code in code blocks.

> Here's an example of what the contents of index.html could look like:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>My Activity Tracker</title>
    <link rel="stylesheet" href="styles.css">
  </head>
  <body>
    <h1>My Activity Tracker</h1>
    <div class="container">
      <button id="book-button">Book</button>
      <button id="tv-button">TV</button>
      <button id="computer-button">Computer</button>
      <button id="phone-game-button">Phone Game</button>
      <button id="vr-game-button">VR Game</button>
    </div>
    <script src="https://www.gstatic.com/firebasejs/7.21.0/firebase-app.js"></script>
    <script src="https://www.gstatic.com/firebasejs/7.21.0/firebase-database.js"></script>
    <script src="script.js"></script>
  </body>
</html>
```

> And here's an example of what `script.js` could look like:

```javascript
// Initialize Firebase
var firebaseConfig = {
  apiKey: "your-api-key",
  authDomain: "your-auth-domain",
  databaseURL: "your-database-url",
  projectId: "your-project-id",
  storageBucket: "your-storage-bucket",
  messagingSenderId: "your-sender-id",
  appId: "your-app-id"
};
firebase.initializeApp(firebaseConfig);

// Get a reference to the database
var database = firebase.database();

// Get a reference to the buttons
var bookButton = document.getElementById("book-button");
var tvButton = document.getElementById("tv-button");
var computerButton = document.getElementById("computer-button");
var phoneGameButton = document.getElementById("phone-game-button");
var vrGameButton = document.getElementById("vr-game-button");

// Add a click listener to each button
bookButton.addEventListener("click", function() {
  addActivity("book");
});

tvButton.addEventListener("click", function() {
  addActivity("tv");
});

computerButton.addEventListener("click", function() {
  addActivity("computer");
});

phoneGameButton.addEventListener("click", function() {
  addActivity("phone-game");
});

vrGameButton.addEventListener("click", function() {
  addActivity("vr-game");
});

// Function to add a new activity to the database
function addActivity(activity) {
  var timestamp = new Date().getTime();
  database.ref("activities/" + timestamp).set({
    activity: activity
  });
}
```

> This is a basic example of how you can use Firebase to record the activities when a button is pressed. In this example, Firebase is initialized with your Firebase configuration, and a reference to the database is obtained

This is a great response.
ChatGPT has us using the Firebase Realtime Database product as a persistent datastore, which is pretty reasonable.
I did not run this code but I have some familarity with Firebase and I'm pretty sure this would work (once the `firebaseConfig` was filled in).

I didn't run it because I wanted to see if I could prompt ChatGPT to use `const` instead of `var`.

{% img /images/chatgpt/update-to-const.png %}

Well, it started to do it and then just paused mid response.
So I asked it to continue.

{% img /images/chatgpt/update-to-const-rest.png %}

🤷‍♂️ It tried.
The code was updated to use `const` but it definitely failed formatting.
Not a big deal, I still replaced the previous contents of `script.js` with this code.

At this point, `button.js` isn't doing anything for us and doesn't seem like it will fit in with the already provided code.
So I asked about it.

## What's the deal with button.js?

{% img /images/chatgpt/first-button-js-response.png %}

This is an ok response.

The good parts of the response:

- The `Button` class is reasonable and is a valid approach for implementing this application.
- The second code block showing how to create new `Button` objects is fine.
- It even explains _why_ you might want to have the `Button` class.

Bad parts:

- No indication of changes to `script.js`
- No indication of how to include `button.js` in the project
- A hint of some changes in `index.html` but not the changes I'd expect given the previously provided file contents.

If I keep doing experiments like this, maybe I'll get better at prompting and will be able to get fuller responses.
I followed up with more questions.

{% img /images/chatgpt/second-button-response.png %}

This is nice, now we have an updated `script.js`.
I haven't written JavaScript that uses `import` before, so at this point I don't know what else needs to change or if this will just work.

So I asked if `index.html` needed to change.

{% img /images/chatgpt/third-button-response.png %}

Well, that response made me think (sortof, I was quite skeptical) that it might just all work.


## Trying it out

I now have all of the most recent versions of code saved to disk and want to try it out locally.
So I asked how[1].

[1]: I don't think someone without programming experience would think about this question.

{% img /images/chatgpt/how-to-test-locally.png %}

This is a pretty reasonable response.
It is partially wrong in that Firebase **can** work locally, though I still haven't configured it yet.
Recommending Firebase hosting as a solution is a pretty good recommendation.

I followed the steps and started up a python web server.
I substituted `python3` for `python` since I don't have a global `python` installed[2].

[2]: Definitely another moment where having experience writing software helped. If I had zero experience or didn't already have python3 installed, I would have had to go down a whole different path of prompting to figure out what was going on.

I opened my web browser and navigated to the correct location and was greated with a `Uncaught SyntaxError: Cannot use import statement outside of a module` in the JavaScript console[3].
I asked ChatGPT about the error.

[3]: Yet another moment where having experience doing web programming came in handy.

{% img /images/chatgpt/cannot-use-import-statement.png %}

One thing that is great about this response is that the bottom `html` code block tells us what line to change in `index.html`.
That is great.

As to the rest of it, at this point, I'm not entirely sure if this is a correct response or not.
I've used all the tools mentioned but, honestly, it's been a while and I don't know if this will solve the problem or not.
After installing `npm`[4], I gave it a shot!

[4]: This gives an idea of how often I'm in this ecosystem. I'm doing this work on a laptop I've owned since November 2022 and didn't already have npm installed.

This time, I ran into an error about `require` not being defined and once again I asked ChatCPT about it.
I'm not going to include the response because I don't reading it adds much value.
ChatGPT's response was just sort of meh here.
It suggested that the `require` is not supported in browsers and is used nodejs applications and recommended loading the Firebase SDK by using `<script>` tags in `index.html`.
Maybe that is right but I refused to open `script-compiled.js` or dig into what Babel produced.

## Changing approaches

At this point, I gave up on having a separate `button.js` and asked ChatGPT to inline `Button` into `script.js`

{% img /images/chatgpt/first-inlined-button-class.png %}

This was an interesting response.

It trimmed down the example `firebaseConfig`.
I did not dig into if the reduce `firebaseConfig` was the minimum needed or not.

It inlined `Button` and changed it so that it is adding `<button>` elements to the DOM instead of querying for the `<button>` by id.
How it persists to the Firebase database also changed to push a record with a timestamp into collections, where the collection is the activity being pushed.

Notice, that it is still using `const` instead of `var`.
ChatGPT seems to be remembering my preference.

Since `script.js` changed from querying for buttons to creating them, I knew `index.html` would need to be updated.
I asked ChatGPT for what `index.html` looks like with the updated `script.js` and it gave a good response.

{% img /images/chatgpt/updated-index-html-after-inline-button.png %}

This is pretty good.
The html is solid.

Weirdly, ChatGPT makes a reference to removing a reference to `button.js`.
Yes, this alleged reference would not be required but I hadn't seen a version of `index.html` that had a `button.js` reference in it.
Does this response imply that there existed an `index.html` that appropriately included `button.js` and, had I prompted appropriately, I could have access to it?

I try reloading my web browser with the updated code and of course it fails due to Firebase configuration not being setup.
This is completely expected and welcome!
No more JavaScript errors!

## Testing without Firebase

It was getting late and I didn't feel like setting up Firebase, so I asked ChatGPT to replace Firebase so I could go to bed successfully clicking buttons.

I prompted "For now, lets replace firebase with just printing to the javascript console so I can run this locally and get some feedback"

{% img /images/chatgpt/after-replacing-firebase-with-console.png %}

This is kind of an amazing response.
Sure, we're back to querying the DOM for pre-existing `<button>` elements but it did it.
Firebase was completely removed from `script.js`, `Button` was changed to mantain the total number of clicks, and it logs to the console.

Not worth sharing the exact response but I asked for the `index.html` that goes along with that `script.js` and got back a perfect response.
The HTML returned matched up with `script.js` and we were back to just loading `script.js` instead of `script-compiled.js`.
It also provided an explainer paragraph that repeated the JavaScript console would have log messages showing the button pressed and total number of prdesses for the button.

I refreshed the page, clicked some buttons, and saw the `console.log` messages.
🎉 Success!

At this point, I went to bed.
This was on a Tuesday.

## Persisting to Firebase

Four days later, I picked back up the project.

