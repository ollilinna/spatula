# Spatula

A bare-bones HTML scraper written in Haskell utilizing css selectors for filtering DOM element attributes. 


## Features

- Read local HTML documents or curl hosted content
- Print attributes to stdout
- Download documents to disk that are pointed to by attribute values that resolve to valid urls


## Installation

```
runhaskell Setup configure
runhaskell Setup build
(sudo) runhaskell Setup install
```


## Example Usage

```bash
runhaskell Main.hs -s SELECTOR -a ATTRIBUTE -p PATH/URL -d -f
```

where ``-d`` is _optional_ download flag and ``-f`` is an _optional_ flag which tells spatula to look for the path in the filesystem.

```bash
runhaskell Main.hs -s a -a href -p https://example.com/
```

Alternatively you may compile the file with

```bash
ghc -o spatula Main.hs
```

And run it with

```bash
./spatula -s a -a href -p https://example.com/
```


## External dependencies
- [curl](https://curl.haxx.se/)