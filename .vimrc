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

let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

set number

function SetClojureOptions()
	set filetype=clojure
endfunction
autocmd BufNewFile,BufRead *.clj call SetClojureOptions()
 
""" not really a repl, but good enough for feedback
function SetClojurePyOptions()
	call SetClojureOptions()
	map <Leader>r :!clojurepy %<CR>
endfunction
 
 
function VimuxSlime()
	if !exists("g:VimuxRunnerIndex") || _VimuxHasRunner(g:VimuxRunnerIndex) == -1
		call VimuxOpenRunner()
	endif

	call VimuxSendText(@v)
	call VimuxSendKeys("Enter")
endfunction
""
let mapleader = ","
let maplocalleader = ",,"
map <Leader>vi :VimuxInspectRunner<CR>
map <Leader>vq :VimuxCloseRunner<CR>
map <Leader>vx :VimuxInterruptRunner<VR>
vmap <Leader>vs "vy :call VimuxSlime()<CR>
nmap <Leader>vs vip<Leader>vs<CR>
" eval the outermost form
nmap <LocalLeader>e vaF<Leader>vs<CR>
" eval the current form
nmap <LocalLeader>E va(<Leader>vs<CR>
" eval the buffer
nmap <LocalLeader>b ggVG<Leader>vs<CR>
""
 
autocmd BufRead,BufNewFile *.cljpy call SetClojurePyOptions()
autocmd BufRead,BufNewFile *.cljs call SetClojureOptions()
autocmd BufRead,BufNewFile *.cljx call SetClojureOptions()
