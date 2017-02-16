" Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.vim/bundle/neobundle.vim/

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Note: You don't set neobundle setting in .gvimrc!

NeoBundle 'Shougo/unite.vim'                 " unite all the things!
NeoBundle 'tpope/vim-fugitive'               " git
NeoBundle 'sjl/gundo.vim'                    " undo graph
NeoBundle 'mattn/webapi-vim'                 " support for gist-vim
NeoBundle 'mattn/gist-vim'                   " :Gist
NeoBundle 'Lokaltog/vim-powerline'           " status info
NeoBundle 'kien/rainbow_parentheses.vim'     " Rainbow parens!
NeoBundle 'benmills/vimux'                   " tmux integration
NeoBundle 'altercation/vim-colors-solarized' " solarized colorscheme
NeoBundle 'scrooloose/syntastic'             " Syntax checking
NeoBundle 'majutsushi/tagbar'                " tagbar
NeoBundle 'ervandew/supertab'                " Tab to complete

call neobundle#end()

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck


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

" Rainbow Parens
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

" Set SuperTab
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"

