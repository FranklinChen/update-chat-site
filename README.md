# Update a CHAT site

[![Build Status](https://travis-ci.org/TalkBank/update-chat-site.png)](https://travis-ci.org/TalkBank/update-chat-site)

This is used for the TalkBank project.

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

```console
$ update-chat-site
```
