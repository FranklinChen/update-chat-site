# Validate media links on a CHAT Web site

[![Build Status](https://travis-ci.org/TalkBank/update-chat-site.png)](https://travis-ci.org/TalkBank/update-chat-site)

This is used for the TalkBank project.

At CMU, we have three machines that serve Web sites of CHAT material:

- http://childes.talkbank.org/
- http://talkbank.org/
- http://homebank.talkbank.org/

Each CHAT transcript may contain directives indicating corresponding
media (video or audio or image) files. This program looks at all the
CHAT transcripts and prints a report of any transcript that is missing
an expected media file.

## Building with [Haskell Stack](https://github.com/commercialhaskell/stack)

First, `pcre` must be installed. On Mac OS X, this means

```console
$ brew update
$ brew install pcre
$ export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
```

```console
$ stack install
```

The resulting executable will be at
`~/.local/bin/update-chat-site` and will automatically be in
your path as `update-chat-site` if your `PATH` is already set
correctly to include `~/.local/bin`.

## Usage

The program can be run from anywhere since it uses hardcoded logic to
look for the required resources.

```console
$ update-chat-site
```
