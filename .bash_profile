export BASH_PROFILE="READ" # Shitty C++ style approach to avoid include loops

# Make bash check its window size after a process completes
shopt -s checkwinsize

# Helpers for safely and non-redundantly adding directories to search paths
safe_add_PATH() {
	oIFS=$IFS
	IFS=':'
	t=(${PATH})
	unset IFS
	t=("$1" ${t[@]%%"$1"})
	# output the new array
	IFS=':'
	echo -n "${t[*]}"
	unset t
	IFS=$oIFS
}

safe_add_MANPATH() {
	oIFS=$IFS
	IFS=':'
	t=(${MANPATH})
	unset IFS
	t=("$1" ${t[@]%%"$1"})
	# output the new array
	IFS=':'
	echo -n "${t[*]}"
	unset t
	IFS=$oIFS
}

# Set up PATH for local/homebrew installs
export PATH=`safe_add_PATH /usr/local/bin`
export PATH=`safe_add_PATH /usr/local/sbin`
export PATH=`safe_add_PATH $(brew --prefix ruby)/bin`
export PATH=`safe_add_PATH /usr/local/share/npm/bin`
export PATH=`safe_add_PATH /Users/mpwd/.cabal/bin`
export PATH=`safe_add_PATH /Users/mpwd/.bin`

# Set the prompt
parse_git_branch() {
	git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/' -e 's/^/ /'
}

export PS1="[\t \w\\[$(tput setaf 2)\\]\$(parse_git_branch)\\[$(tput sgr0)\\]] $ "

# Color ls
alias ls="ls -G"

# Enable colors in grep
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'

# Use VI as our editor
if [ -x "`which vim`" ] ; then
	alias vi=vim
fi
export EDITOR=vi

# Fix PDFs
# This is for use cleaning up PDFs generated by Mathematica or OS X's native PDF generator, which are often corrupt and not viewable on other OSs
# More reliable solution uses pdftk (not available on OS X): http://superuser.com/a/536644
if [ -x "`which gs`" ] ; then
	pdffix() {
		gs -q -dNOPAUSE -dBATCH -dPDFSETTINGS=/prepress -sDEVICE=pdfwrite -sOutputFile=$2 -c .setpdfwrite -f $1
	}
fi

# SBCL
if [ -x "`which sbcl`" ] ; then 
	sbcl-compile() {
		fasl=`dirname $1`/`basename $1 .lisp`.fasl
		[ $fasl -ot $1 ] && sbcl --noinform --eval "(load \"$1\")" --eval "(compile-file \"$1\")" --eval "(quit)" > /dev/null
	}
	[ -x "`which rlwrap`" ] && alias sbcl="rlwrap sbcl --noinform"
fi
 
# When we are inside emacs, use emacs instead of shell things
if [ -n "$INSIDE_EMACS" ] ; then
       	alias vi=emacs
       	alias less=emacs
fi

# Bash completion
if [ -d /usr/local/etc/bash_completion.d/ ]; then
	for completion_file in /usr/local/etc/bash_completion.d/* ; do
		. $completion_file
	done
fi

# Terminal prompt
case "$TERM" in
	xterm*|rxvt*)
		export PROMPT_COMMAND='echo -ne "\033]0;${USER}@$(hostname -s): ${PWD}\007"'
		;;
	*)
		export PROMPT_COMMAND=""
		;;
esac

# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"

# Larger bash history (default is 500)
export HISTFILESIZE=10000
export HISTSIZE=$HISTFILESIZE


# Color LESS
# Use syntax highlight with less. Requires source-highlight.
#
# Install on Mac OS X via Homebrew:
#
#   $ brew install source-highlight
#
# On Ubuntu:
#
#   $ sudo apt-get install source-highlight
#
if [ -x "`which src-hilite-lesspipe.sh`" ] >/dev/null; then
	export LESSOPEN="| src-hilite-lesspipe.sh %s"
	export LESS="-R"
fi
