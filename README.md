# VIM Installation

You'll need NeoBundle:

	mkdir -p ~/.vim/bundle
	git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim

# Haskell Installation

Assuming you have Cabal, you are going to need to download a bunch of things:

	cabal update && cabal install ghc-mod hoogle hdevtools hlint

If you want to use hoogle, you'll want to create its cache:

	hoogle data

# Using VIM + Haskell

Autocompletion still needs work :(

Otherwise support is pretty good.  Load up `ghci` in `tmux` and open a Haskell file in vim

In vim, type `ctrl-c ctrl-l` ; this will load the Haskell file into the ghci session

Now whenever you save, it will be reloaded in the ghci session; this is the most convenient way to REPL test Haskell code that I can think of (and is a copy of the emacs workflow)
