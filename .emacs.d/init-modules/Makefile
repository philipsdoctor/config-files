all: bootstrap.elc compiled-modules.elc compiled-modules.el

compiled-modules.el: $(shell ls init-*.el)
	cat $^ | grep -v '(provide .*)$$' > $@
	echo "(provide '${@:.el=})" >> $@

%.elc : %.el
	rm -f $@
	emacs --script ../init.el
	emacs --batch --eval "\
	(let ((default-directory \"$(CURDIR)\")) \
	  (normal-top-level-add-to-load-path '(\"$(CURDIR)\")) \
	  (byte-compile-file \"$<\") \
        )\
	"

clean :
	rm -f compiled-modules.el *.elc *~
