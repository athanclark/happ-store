hApp-Store
==========

> A swanky little app store...age system for Hackage packages :D
>
> ...yeah...

Use this website to rate and give reviews on [Hackage](https://hackage.haskell.org)
packages, while also helping categorize them for great success! Very nice!

> **WARNING**: This friggin thing is in liek, toooootally pre-alpha right now,
> so if you have complaints, please take them and stick them right there up in
> your bum, okay? Unless it's an actual bug, I mean. Yeah sorry :\

## Building

The app has two components, the client and the server. There are also some dependencies,
but it's pretty easy getting it bootstrapped. First though, you'll need to clone the
code and jump into the directory to get yer bearings:

```bash
git clone https://github.com/athanclark/happ-store.git
cd happ-store
```

### Server

First and foremost, you will need the [libsodium](https://download.libsodium.org/doc/)
library; it's used for session authenticity, and the server won't build without it.
In Ubuntu, it's something like `apt-get install libsodium-dev` (not even sure if
you need development files), but generally you just need the library available.

Use [stack](http://docs.haskellstack.org/en/stable/README/) and the `stack.yaml`
included in the source code distribution:

```bash
stack build
```

This command should fetch all your dependencies and build an executable somewhere under
`./.stack-work/`. It's easiest to let `stack` find the executable for you, though.
If you're too lazy to build the project, but still want to check it out (and also
happen to be using linux on an x86_64 machine), just run the binary under
`./bin/happ-store`; it should "just work". It comes with a suite of options too -
just type `./bin/happ-store --help` to check them out.

> If you're building this on Windows and it didn't comple, please file an issue,
> I will repay you with many a kudo :D

### Client

Before you build the client, you should fetch any javascript dependencies so you can
run the server in "development mode" / locally, fetched through [bower](http://bower.io):

```bash
bower install
```

This should get all the javascripts and cascading stylesheets necessarry to make the
front-end work, and install them under `./static/vendor/`.

Next, jump into the `./frontend/` folder and build the client! The frontend code
is built entirely with [Elm](http://elm-lang.org), so make sure you have version
`0.17` installed! Then it's just a couple more commands to build the javascript app:

```bash
elm package install
elm make src/Main.elm --output dist/Main.js
```

## Linking Client to Server

Right now, I'm hardcoding the server to expect the frontend code to actually live in
`./frontend/dist/Main.min.js`. You can just copy-paste it in there if you don't want
to minify, but if you _do_, check out [Google Closure Compiler](https://developers.google.com/closure/compiler/):

```bash
java -jar ${HOME}/Downloads/compiler.jar dist/Main.js --js_output_file=dist/Main.min.js
```

> assuming GoogleCC lives in `~/Downloads`

## Client Demo File

If you look closely in `./frontend`, you'll see a `index-param.html` file that
has a funny syntax. This is a _parametric text file_, and can be used to build a
demo of the frontend without the need to build the server, using [ltext](http://ltext.github.io/). To plug the files in, run this command:

```bash
ltext "index-param.html dist/Main.js init.js style.css" > index.html
```

> **Note**: ltext expects each input to `index-param.html` to have a _blank line_
> as the first line of the text file. Strange, I know, but it's important.
> `init.js` and `style.css` both should be fine, and the default output of
> `elm make` should also have a blank first line, so you should be good there, but
> GoogleCC would remove that line, so be wary if you get a strange error from
> this command: you might need to break out your text editor and press enter a
> couple times in the correct order :x


## Running

Now that you have your server built and the frontend ready, you can invoke the
server in development mode with the following command:

```bash
stack exec happ-store
```

This will start the server in development mode, and will give some info to
the currently running environment. You should be able to view `localhost:3000`
to see the page. You can add additional options by plumbing them through
`stack` - note the first `--`:

```bash
stack exec happ-store -- --port=3005 --production
```

for instance.

> hApp-Store is also running an [ekg](http://hackage.haskell.org/package/ekg)
> server too, which is cool, and you can view it under `localhost:3001`
> by default.


## Contributing

If you have ideas or feature requests, please ping me on
[twitter](https://twitter.com/athan__) (warning, I'm kinda crazy),
or something along those lines. I am open to ideas, and because I made this I'm
likely going to be the one implementing the ideas, so it would be good to speak
1-on-1 to at least give insight to what is feasible. If however you would like the
discussion to be public and peer accessible, you can also file an issue. I
haven't gotten around to making a decent categorization of issues vs. feature requests
yet in GitHub's issue tracker, but one will be coming after it's up and running.

If you do find a bug, please report it in the GitHub issue tracker. I'll do my best
to solve them, however if I can't reproduce the issue or if the issue is moderately bogus,
I'll likely just kinda... delete it and uh... yeah. What mess?

If you would like to contribute code, that's friggin dope! Please do! Just follow the
traditional GitHub pull-request method for distributed collaboration so I can sync
into your changes. I'll try to maintain dutiful git tagging so stability can be referenced
easily, but I'm not a git wizard (gizzard?), and it's not like I squash or rebase or
whatever, so feel free to do what you want. Unless it's like a massive commit without
any comments or something, then I'll probably reject it and do the worm or something.
