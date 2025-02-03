---
dated-url: true
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
In the spirit of [creating small programs](/blog/2020/10/03/go-create-silly-small-programs/) to solve a minor annoyance, I decided to try to make an activity tracker.

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

![](/images/chatgpt/directory-layout.png)

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

![](/images/chatgpt/update-to-const.png)

Well, it started to do it and then just paused mid response.
So I asked it to continue.

![](/images/chatgpt/update-to-const-rest.png)

🤷‍♂️ It tried.
The code was updated to use `const` but it definitely failed formatting.
Not a big deal, I still replaced the previous contents of `script.js` with this code.

At this point, `button.js` isn't doing anything for us and doesn't seem like it will fit in with the already provided code.
So I asked about it.

## What's the deal with button.js?

![](/images/chatgpt/first-button-js-response.png)

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

![](/images/chatgpt/second-button-response.png)

This is nice, now we have an updated `script.js`.
I haven't written JavaScript that uses `import` before, so at this point I don't know what else needs to change or if this will just work.

So I asked if `index.html` needed to change.

![](/images/chatgpt/third-button-response.png)

Well, that response made me think (sortof, I was quite skeptical) that it might just all work.


## Trying it out

I now have all of the most recent versions of code saved to disk and want to try it out locally.
So I asked how[1].

[1]: I don't think someone without programming experience would think about this question.

![](/images/chatgpt/how-to-test-locally.png)

This is a pretty reasonable response.
It is partially wrong in that Firebase **can** work locally, though I still haven't configured it yet.
Recommending Firebase hosting as a solution is a pretty good recommendation.

I followed the steps and started up a python web server.
I substituted `python3` for `python` since I don't have a global `python` installed[2].

[2]: Definitely another moment where having experience writing software helped. If I had zero experience or didn't already have python3 installed, I would have had to go down a whole different path of prompting to figure out what was going on.

I opened my web browser and navigated to the correct location and was greated with a `Uncaught SyntaxError: Cannot use import statement outside of a module` in the JavaScript console[3].
I asked ChatGPT about the error.

[3]: Yet another moment where having experience doing web programming came in handy.

![](/images/chatgpt/cannot-use-import-statement.png)

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

![](/images/chatgpt/first-inlined-button-class.png)

This was an interesting response.

It trimmed down the example `firebaseConfig`.
I did not dig into if the reduce `firebaseConfig` was the minimum needed or not.

It inlined `Button` and changed it so that it is adding `<button>` elements to the DOM instead of querying for the `<button>` by id.
How it persists to the Firebase database also changed to push a record with a timestamp into collections, where the collection is the activity being pushed.

Notice, that it is still using `const` instead of `var`.
ChatGPT seems to be remembering my preference.

Since `script.js` changed from querying for buttons to creating them, I knew `index.html` would need to be updated.
I asked ChatGPT for what `index.html` looks like with the updated `script.js` and it gave a good response.

![](/images/chatgpt/updated-index-html-after-inline-button.png)

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

![](/images/chatgpt/after-replacing-firebase-with-console.png)

This is kind of an amazing response.
Sure, we're back to querying the DOM for pre-existing `<button>` elements but it did it.
Firebase was completely removed from `script.js`, `Button` was changed to mantain the total number of clicks, and it logs to the console.

Not worth sharing the exact response but I asked for the `index.html` that goes along with that `script.js` and got back a perfect response.
The HTML returned matched up with `script.js` and we were back to just loading `script.js` instead of `script-compiled.js`.
It also provided an explainer paragraph that repeated the JavaScript console would have log messages showing the button pressed and total number of prdesses for the button.

I refreshed the page, clicked some buttons, and saw the `console.log` messages.
🎉 Success!

At this point, I went to bed.

## Persisting to Firebase

Four days later, I picked back up the project and rejoined the conversation with ChatGPT.
I continued the conversation as if I had never left.

![](/images/chatgpt/restarting-the-conversation.png)

![](/images/chatgpt/first-instructions-for-readding-firebase.png)

This numbered list with a link to Firebase is a pretty solid response.
Step 6 and the code provided is a bit off as the previous code did not have a `recordActivity` function.

The code, and step 2, switches back to using the Firebase Firestore offering instead of the Realtime Database.
Probably either would be fine to use but at this point ChatGPT has flipped between them a couple times.

Given the time between when I had started this project and continued it, I had forgotten about the Firestore vs Realtime Database usage and just followed the instructions and setup a Firestore database through the Firebase console.

I next told ChatGPT that I didn't have a `recordActivity` function and asked if I should add it and where should I call it from.
It's response was good, saying ot add it to `script.js` and call it from the `onClick` handler.

I was still trying to not type any code myself, so I asked it to show me what that would look like.

![](/images/chatgpt/code-after-adding-record-activity.png)
![](/images/chatgpt/code-after-adding-record-activity-continued.png)

This response has good parts and bad parts.
We've switched back to use the Realtime Database and `import` statements have shown up again.
But the rest of it is pretty good and the explanation after it is useful.

I knew there would be problems but I saved the latest `script.js` to disk and refreshed my browser.
As expected, there were errors about the `import` statements.
So I told ChatGPT about them.

Me:
> I did this but now receive an error in my browser about the import statements and not being part of a module. Do I need to use the import statments?

It responded suggesting that my browser didn't support ES6 modules and said I could work around it by using script tags and then it displayed some updated HTML and javascript.

Updated `index.html`:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>My Activity Tracker</title>
  </head>
  <body>
    <h1>My Activity Tracker</h1>
    <div id="buttons">
      <button id="book-button">Book</button>
      <button id="tv-button">TV</button>
      <button id="computer-button">Computer</button>
      <button id="phone-game-button">Phone Game</button>
      <button id="vr-game-button">VR Game</button>
    </div>
    <script src="https://www.gstatic.com/firebasejs/7.19.0/firebase-app.js"></script>
    <script src="https://www.gstatic.com/firebasejs/7.19.0/firebase-database.js"></script>
    <script>
      // Your Firebase configuration
      const firebaseConfig = {
        apiKey: "YOUR_API_KEY",
        authDomain: "YOUR_AUTH_DOMAIN",
        datapbaseURL: "YOUR_DATABASE_URL",
        projectId: "YOUR_PROJECT_ID",
        storageBucket: "YOUR_STORAGE_BUCKET",
        messagingSenderId: "YOUR_MESSAGING_SENDER_ID",
        appId: "YOUR_APP_ID"
      };

      // Initialize Firebase
      firebase.initializeApp(firebaseConfig);

      // Get a reference to the database
      const database = firebase.database();
    </script>
    <script src="script.js"></script>
  </body>
</html>
```

The updated `script.js`:

```javascript
const buttons = document.querySelectorAll('button');

buttons.forEach(button => {
  button.addEventListener('click', event => {
    const buttonId = event.target.id;
    const activity = buttonId.split('-')[0];
    console.log(`Activity: ${activity}`);

    // Write the activity to the database
    database.ref().push({
      activity: activity,
      timestamp: Date.now()
    });
  });
});
```

Reviewing this while writing the article and it would have mostly worked.
One area that was slightly off is the `const activity = buttonId.split('-')[0];` line.
That would have worked fine for `book-button` but would not return the value I want for `vr-game-button`.

I also didn't want to inline JavaScript into the HTML, so I asked "Do I need to have the initialization in the html file? Can it be recombined with script.js?"

It replied that it was possible to combine the initialization with `script.js` and displayed an updated version of the JavaScript.

```javascript
const firebase = require("firebase/app");
require("firebase/database");

const firebaseConfig = {
  apiKey: "YOUR_API_KEY",
  authDomain: "YOUR_AUTH_DOMAIN",
  databaseURL: "YOUR_DATABASE_URL",
  projectId: "YOUR_PROJECT_ID",
  storageBucket: "YOUR_STORAGE_BUCKET",
  messagingSenderId: "YOUR_MESSAGING_SENDER_ID",
  appId: "YOUR_APP_ID",
};

firebase.initializeApp(firebaseConfig);
const database = firebase.database();

class Button {
  constructor(name) {
    this.name = name;
    this.count = 0;

    this.button = document.createElement("button");
    this.button.textContent = `${this.name}: 0`;
    this.button.addEventListener("click", () => {
      this.count += 1;
      this.button.textContent = `${this.name}: ${this.count}`;
      this.recordActivity();
    });
  }

  render() {
    return this.button;
  }

  recordActivity() {
    database.ref(this.name).set({
      count: this.count,
    });
    console.log(`${this.name} activity recorded: ${this.count}`);
  }
}

const buttons = [
  new Button("Book"),
  new Button("TV"),
  new Button("Computer"),
  new Button("Phone Game"),
  new Button("VR Game"),
];

buttons.forEach((button) => {
  document.body.appendChild(button.render());
});
```

This `script.js` brings back in `require` and moves back to using a `Button` class.

The main problem with this version of the script is that it stopped recording the timestamp of the event and instead just records the count.
I'm not upset that it stopped recording the timestamp.
While I do want to record a timestamp, I hadn't prompted ChatGPT to do so.

The actual bug here is that it is just setting the count in the Firebase Database to the in-memory count.
If I click a button ten times and then reload the page and click it again, the persisted count would be reset.
Generously, I suppose I hadn't specified that this shouldn't be the behavior so maybe that is on me.

At this point, I just wanted to get to a complete state so I stopped entirely replacing my code with the provided code blocks and just selectively made some of the suggested edits to the HTML file and fixed the `import` issue from the previously provided code.
This means I was still on a version of code that had a `recordActivity` that indirectly recorded a timestamp and did not lose data.
I had also filled in the in the `firebaseConfig` values in the `firebaseConfig`.
Below are my `index.html` and `script.js` looked like.

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Activity Tracker</title>
  </head>
  <body>
    <h1>Activity Tracker</h1>

    <button id="book-button">Book</button>
    <button id="tv-button">TV</button>
    <button id="computer-button">Computer</button>
    <button id="phone-game-button">Phone Game</button>
    <button id="vr-game-button">VR Game</button>

    <script src="https://www.gstatic.com/firebasejs/7.19.0/firebase-app.js"></script>
    <script src="https://www.gstatic.com/firebasejs/7.19.0/firebase-database.js"></script>
    <script src="script.js"></script>
  </body>
</html>
```

```javascript
const firebaseConfig = {
  apiKey: "AIzaSyBvjPwNyC3vpByghCK2oPJtZLdNJj-ZZY0",
  authDomain: "chatgpt-activity.firebaseapp.com",
  databaseURL: "https://chatgpt-activity.firebaseio.com",
  projectId: "chatgpt-activity",
  storageBucket: "chatgpt-activity.appspot.com",
  messagingSenderId: "302737905399",
  appId: "1:302737905399:web:a866ac2b8ab6e1b0f025f2"
};

firebase.initializeApp(firebaseConfig);

const database = firebase.database();

class Button {
  constructor(title, id) {
    this.title = title;
    this.id = id;
    this.element = document.createElement('button');
    this.element.textContent = this.title;
    this.element.id = this.id;
    this.element.addEventListener('click', () => this.onClick());
  }

  onClick() {
    console.log(`Button '${this.title}' clicked`);
    recordActivity(this.title);
  }

  appendTo(element) {
    element.appendChild(this.element);
  }
}

const recordActivity = (activity) => {
  database.ref('activities/' + Date.now()).set({
    activity: activity
  });
};

const buttons = [
  new Button('Book', 'book-button'),
  new Button('TV', 'tv-button'),
  new Button('Computer', 'computer-button'),
  new Button('Phone Game', 'phone-game-button'),
  new Button('VR Game', 'vr-game-button')
];

buttons.forEach((button) => button.appendTo(document.body));
```

I tried loading the page and received an error about the Firebase Realtime Database URL being wrong.
ChatGPT to the rescue!

![](/images/chatgpt/url-error.png)

This was a useful response.
I **hadn't** noticed that the earlier numbered instructions had me setup a Firebase Firestore and not a Realtime Database.
Following the instructions here made me realize the mistake.

I updated `script.js` to have the right URL and loaded the app.
It worked!
Kind of.

## Adding auth

Now I was getting a permission error.
But that was progress.

![](/images/chatgpt/open-permissions.png)

This is a good response and it even warns that it isn't a recommended configuration.
For this experiment, I could have just gone with the completely open permissions but I wanted to do better.

![](/images/chatgpt/some-permissions.png)

I updated the code and tried again.
Still getting permission issues because `script.js` isn't doing any sort of logging in.

I remembereed that the first `recordActivity` implementation had a line in it about signing in with email and password, so I asked ChatGPT about that.

![](/images/chatgpt/email-and-passord.png)

Not bad.
I setup a user in the Firebase console, copied the above code into `script.js`, and hard-coded an email and password[5].
I refreshed the browser and got a new error.
You know what I'm going to do next.

[5]: Not a good idea for deploying into the wild but good enough for running locally and this experiment.

![](/images/chatgpt/grab-auth.png)

Pretty near perfect response.
I added the suggested `<script>` tag, noticed the version was different from the other tags, and updated the version to match the others.

I refreshed the page and clicked some buttons.
No errors in the JavaScript console.
I pulled up the Firebase Realtime Database UI and can see activities! 🎉

## Displaying activities

We're in a great spot now; data is flowing.
But we really need to display the activities in our UI.

![](/images/chatgpt/first-display.png)

I updated `script.js` and `index.html` based off this response and now there was a list of activities on the page.
Unfortunately, `activity.time` did not exist since the current `recordActivity` was not recording time like that, so that part of the string just said `null`.
But this was still pretty good.

![](/images/chatgpt/persist-time.png)

Not a bad response but it switched us back to using Firestore and recorded the timestamp in `timestamp` field instead of `time`.
Time to try to switch back.

![](/images/chatgpt/back-to-realtime-database.png)

This worked!

## Skipping some parts

At this point, the UI was a little janky with the buttons being below the activity list.
So whenever I clicked a button, the list grew and the buttons would move.
This would be an easy fix to just make myself but I continued to work with ChatGPT on solving it.

I'm going to leave out some of the of the prompts and responses here.
Trust me that it was basically more of the same.

To try to fix the moving buttons, I prompted ChatGPT with the following:

> So, the clickable buttons are below the activity list so they keep moving when new activities are added. What needs to happen to keep them above the list?

ChatGPT suggested adding a fixed header to the HTML contain the buttons and provided a HTML snippet and CSS to add to a CSS file.
I recognized that `script.js` needed to be updated to work with the new HTML (the new HTML already had `<button>`s in it and the current `script.js` was adding them).

I asked ChatGPT how `script.js` needed to be changed to work with the suggested `index.html`.
The response it gave shows an example of using `document.getElementById` to grab a button, but it used entirely different ids[6].
The example would have probably been enough to have a novice go make the necessary edits.
But I still wanted to try to have it write the code.

[6]: It looked like these were ids for a timer: `start-button`, `stop-button`, `reset-button`.

So next, I asked "How would I change this block of code to work with the current index.html with regards to button clicks?" and I pasted a large snippet of code into the prompt (basically, everything to do with the class `Button` and adding the buttons to the DOM).
Its response was pretty good.

```javascript
class Button {
  constructor(title, id) {
    this.title = title;
    this.id = id;
    this.element = document.createElement('button');
    this.element.textContent = this.title;
    this.element.id = this.id;
    this.element.addEventListener('click', () => this.onClick());
  }

  onClick() {
    console.log(`Button '${this.title}' clicked`);
    recordActivity(this.title);
  }

  appendTo(element) {
    element.appendChild(this.element);
  }
}

const buttons = [
  new Button('Book', 'book-button'),
  new Button('TV', 'tv-button'),
  new Button('Computer', 'computer-button'),
  new Button('Phone Game', 'phone-game-button'),
  new Button('VR Game', 'vr-game-button')
];

buttons.forEach((button) => button.appendTo(document.getElementById('buttons')));
```

This is ok, except the previous HTML didn't use an id for `buttons`, it put that as the class.
It also ends up duplicating the buttons on the page since the JavaScript adds new buttons.

I did some more prompting and got a version of the HTML to exist without the `<button>` elements already being there and it had `<div>` with an id to query for adding the buttons from the JavaScript.
The only bit that was off now was the ids in the HTML were `buttons-container` and `activity-list` and the JavaScript was looking for `button-container` and `activities-list`.
I made those edits by hand and now the site works without moving the buttons around.

## More auth

The Firebase console was warning me about the Realtime Database rules being too permissive.
I asked ChatGPT for some stricter rules and it provided some to me that worked.

I also asked what other authentication methods were available and it provided a list of options, such as using Google or Twitter authentication.
I asked "how would I use Google authentication?" and its response seems pretty good.
I did not bother trying to do it though.

# Conclusion

This was a really interesting experiment to try.
I was able to limit the non-ChatGPT suggested editing I did to removing some `require`s, updating a version number (this probably wasn't strictly needed), and fixing a slight mismatch between the ids found in the HTML file and the ids the JavaScript needed.

Could I have done this faster on my own?
Yes.
But I've also written a handful of small ClojureScript and JavaScript tools/games that use Firebase Realtime Database as their persistent storage.

Would using ChatGPT for the scaffolding but allowing myself to make bigger edits have gone faster than doing it on my own?
I think so.

It was definitely useful to have experience writing these style of programs before.
Knowing the direction I wanted the code to go definitely helped me write prompts to get there.
I'd be really interested in observing a non-programmer attempt this same task.
