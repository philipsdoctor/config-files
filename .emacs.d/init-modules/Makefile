all: compiled-modules.elc bootstrap.elc

compile-all:
	emacs --batch --eval "\
	(let ((default-directory \"$(CURDIR)\")) \
	  (normal-top-level-add-to-load-path '(\"$(CURDIR)\")) \
	  (byte-recompile-directory (expand-file-name \"$(CURDIR)\") 0) \
        )\
	"

compiled-modules.el: $(shell ls init-*.el)
	cat $^ \
	| grep -v '(provide .*)$$' > $@
	echo "(provide '${@:.el=})" >> $@

%.elc : %.el
	emacs --batch --eval "\
	(let ((default-directory \"$(CURDIR)\")) \
	  (normal-top-level-add-to-load-path '(\"$(CURDIR)\")) \
	  (byte-compile-file \"$<\") \
        )\
	"

clean :
	rm -f compiled-modules.el *.elc
