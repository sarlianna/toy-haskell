I'm trying haskell, basically.

[explination/tutorial](http://www.arow.info/blog/posts/2015-07-10-servant-intro.html)
[servant docs/tutorials](http://haskell-servant.github.io/tutorial/)

Yeah, so.
This is the basics of an api in Haskell, which is like the ultimate/mother/hipster/mathematicians-only functional language.

All it does is use a json file as a value store, and serves objects found in that store.

####building

Once you have haskell, cabal, and stack installed, just run these commands:
```
$ stack build
$ stack exec haskell-api
```

For help installing haskell, cabal, and stack, see the following:

(Haskell installation)[https://www.haskell.org/platform/]
The above will also install cabal, but you'll want to run
`$ cabal update`

(stack installation)[https://github.com/commercialhaskell/stack/wiki/Downloads]
