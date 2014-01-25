if has('vim_starting')
 set nocompatible
 set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc', {
\    'build' : {
\        'unix': 'make -f make_unix.mak',
\        'mac': 'make -f make_mac.mak',
\    },
\}

" Platform Neutral Plugins
NeoBundle 'Shougo/unite.vim'             " unite all the things!
NeoBundle 'tpope/vim-fugitive'     " git
NeoBundle 'sjl/gundo.vim'           " undo graph
NeoBundle 'mattn/webapi-vim'       " support for gist-vim
NeoBundle 'mattn/gist-vim'         " :Gist
NeoBundle 'Lokaltog/vim-powerline' " status info
NeoBundle 'kien/rainbow_parentheses.vim' " Rainbow parens!
NeoBundle 'tpope/vim-surround'           " Handy selection
NeoBundle 'benmills/vimux'               "tmux integration
NeoBundle 'altercation/vim-colors-solarized' " solarized colorscheme

" Tab to complete
NeoBundle 'ervandew/supertab'	
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"


NeoBundle 'tpope/vim-repeat'             " Repeat with .

" Generic Lisp Plugins
NeoBundle 'https://bitbucket.org/kovisoft/paredit'
"NeoBundle 'guns/vim-sexp'                " Motions over S-Expressions
"NeoBundle 'tpope/vim-sexp-mappings-for-regular-people' " Reworked mappings

" JVM Plugins
NeoBundle 'tpope/vim-classpath'          " mvn/lein classpath stuff

" Clojure/ClojureScript Plugins
NeoBundle 'guns/vim-clojure-static'      " syntax file
NeoBundle 'tpope/vim-fireplace'          " nREPL client
"NeoBundle 'guns/vim-slamhound'           " Slamhound via fireplace

" Hy Plugins
NeoBundle 'hylang/vim-hy'                " syntax file

" Python Plugins
"NeoBundle 'klen/python-mode'
"NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'ivanov/vim-ipython'
let g:ipy_perform_mappings = 1 " vim-ipython is weird

" XML Plugins

" HTML Plugins

" Javascript Plugins

filetype plugin indent on
set incsearch
set hlsearch
syntax on
autocmd FileType text setlocal textwidth=80

" Setup Global Rainbow Parentheses
" Fight the Heathens!

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces


