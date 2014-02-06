" NeoBundle
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

" My Bundles here:
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/unite.vim'             " unite all the things!
NeoBundle 'tpope/vim-fugitive'           " git
NeoBundle 'sjl/gundo.vim'                " undo graph
NeoBundle 'mattn/webapi-vim'             " support for gist-vim
NeoBundle 'mattn/gist-vim'               " :Gist
NeoBundle 'Lokaltog/vim-powerline'       " status info
NeoBundle 'kien/rainbow_parentheses.vim' " Rainbow parens!
NeoBundle 'https://bitbucket.org/kovisoft/paredit' " paredit for greatness
NeoBundle 'tpope/vim-surround'           " Handy selection
NeoBundle 'benmills/vimux'               " tmux integration
NeoBundle 'altercation/vim-colors-solarized' " solarized colorscheme
NeoBundle 'scrooloose/syntastic'         " Syntax checking
NeoBundle 'majutsushi/tagbar'            " tagbar
NeoBundle 'bitc/vim-hdevtools'           " hdevtools integration for Haskell
NeoBundle 'guns/vim-clojure-static'      " syntax file
NeoBundle 'tpope/vim-fireplace'          " nREPL client
NeoBundle 'hylang/vim-hy'                " syntax file
NeoBundle 'tpope/vim-classpath'          " mvn/lein classpath stuff

NeoBundle 'ivanov/vim-ipython'
let g:ipy_perform_mappings = 1 " vim-ipython is weird

" Tab to complete
NeoBundle 'ervandew/supertab'	
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"


 " If there are uninstalled bundles found on startup,
 " this will conveniently prompt you to install them.
NeoBundleCheck

" Set SuperTab
let g:SuperTabDefaultCompletionType = "context"

" Use VIM rather than VI keys
set nocompatible
set backspace=2

" Back up data
" Enable if paranoid ; slows everything down a lot!
"set backup
"set backupdir=~/.vim/backups
"set directory=/tmp

" For the love of God, never user <CR> for newlines
set fileformats=unix,mac,dos

" Other settings
"set showcmd             " display incomplete commands
set number              " line numbers
set incsearch           " do incremental searching
" Search highlight colors
hi Search cterm=NONE ctermfg=white ctermbg=darkred
" Regular select colors
hi Visual cterm=NONE ctermfg=white ctermbg=darkblue

" Turn on auto-indent
filetype plugin indent on

" 2013, and we are using VT100 technology
autocmd FileType text setlocal textwidth=80

" switch on syntax highlighting
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" VimTip 80: Restore cursor to file position in previous editing session
set viminfo='10,\"100,:20,%,n~/.viminfo
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen4'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen1'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['blue',        'SeaGreen2'],
    \ ['gray',        'RoyalBlue3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['darkgreen',   'RoyalBlue2'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ['Darkblue',    'RoyalBlue3'],
    \ ]

au VimEnter * RainbowParenthesesToggleAll

" Use tmux for slime
let g:slime_target = "tmux"
let g:slime_paste_file = tempname()

function SetHaskellOptions()
   set tabstop=8                   "A tab is 8 spaces
   set expandtab                   "Always uses spaces instead of tabs
   set softtabstop=4               "Insert 4 spaces when tab is pressed
   set shiftwidth=4                "An indent is 4 spaces
   set smarttab                    "Indent instead of tab at start of line
   set shiftround                  "Round spaces to nearest shiftwidth multiple
   set nojoinspaces                "Don't convert spaces to tabs
   
   " Slime stuff
   " Load the current file into the REPL
   nmap <c-c><c-l> :exec("SlimeSend1 :l " . expand('%:p'))<CR>
   " If we are connected to slime, reload files on write
   au BufWrite *.hs if exists('b:slime_config') | SlimeSend1 :r 
    
   " Type inspection
   " Hit F1 over something to see its type
   nmap <F1> :HdevtoolsType<CR>
   nmap <silent> <F2> :HdevtoolsClear<CR>
endfunction

autocmd BufNewFile,BufRead *.hs call SetHaskellOptions()

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
