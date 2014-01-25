parse_git_branch() {
	    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
    }


PS1="[\t \w \\[$(tput setaf 2)\\] \$(parse_git_branch) \\[$(tput sgr0)\\]] $ "
