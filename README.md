ParseBird
=========

Parsers for Tweets

Wut?
---

There are a lot of libraries to parse parts of Tweets, including Twitter's own [twitter-text](https://github.com/twitter/twitter-text), but those libraries rely on regular expressions and are, generally, not type safe. ParseBird is an attempt to formalize the different parts of a tweet in a manner that would confirm with the generated JSON when creating a new tweet via Twitter. 

What kind of things can I parse?
---

Mentions, hashtags, etc. 