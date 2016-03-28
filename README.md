Cooperate
=========

> Wouldn't it be nice? :|

This is a simple app to help people share their beliefs, in hopes
of finding those that align with them, while avoiding those that
don't. By broadcasting the _beliefs_ you agree and disagree with,
and likewise the _people_ you appreciate or disdain, the public
can utilize _your_ opinion, proportially well to how aligned
they are with your beliefs, and how much you've been endorsed by
people that also align with their beliefs, or who they've direcly
endorsed (phew!).

-----

## How it works

Identity is ensured through 3rd party trusted credentials,
for instance Twitter or Facebook. This app __strictly__
disallows anonymity for the sake of spam, slander, and
other abuse. The way we see it, an online persona _is_
an identity, or really: the person (while logged into our
server) who is linking multiple accounts to this single
referenced identity, _is_ them.

From there, we can start tracking beliefs and endorsements.

### Beliefs

A belief, in our eyes, is the agreement or disagreement toward
a particular statement. So a statement like "global warming is
real" would be a prime example. We can then take some certain
scalar amount of agreement or disagreement with that. There
may be inverses that crop up, like "global warming is fake",
but what can you do (except moderate).

### Endorsement

Endorsement is a different matter, but still somewhat similar.
We let people publicly declare their opinions of each other,
if one feels compelled to do so. This can also be represented
by some scalar value. The search algorithm interprets this as
how much you trust, and care, about their judgement. Endorsements
can also have a _review_ too - to provide extra details.

Slander will be a crucial issue in this app, to the degree that
I see it as unavoidable. One way we get around it is the
the lack of anonymity - every review of a person will be
public. If a review spouts untrue information, a person
recieveing the review has the option to flag it as abuse
(i.e. incriminating claims without genuine evidence, stuff
like that). However, if it's minor and somewhat vague, like
"I just don't like how they treat me", that's fine and will
likely still be public. The app also allows people to validate a
review about them, too, as a means to establish more integrity
in a person's word.


### Warning

I strongly suggest you put on some kind of 24/7 surveilence
system or something if you fear that there may be people out
there that want to kill off people that share your beliefs.
I, myself, am going to use a really sweet stick I found to beat
the crap out of any jerk that tries to snipe me from their
sattelite or something.


## Installation

This web app is built with haskell and elm, so make sure you have
both stack and elm installed. Then, it's really simple:

To build the server, just issue

```bash
stack install --no-system-ghc --install-ghc
```

while inside the root directory of the repo. This will build
the executable, so you can run `stack exec cooperate` to spark
the server. Then, to build the frontend, just `cd frontend/`
first, then run

```bash
npm install && npm run watch 
```

To build both the less styles and the elm frontend. It
sparks a livereload process, I haven't build a oneshot
build script yet, sorry :\

## The Algorithm

The goal of this algorithm is to _sort_ the populus based on
coordination to particular beliefs that a _user_ searches for.

