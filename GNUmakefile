

uni-confusables.el: gen-confusables.el confusables.txt
	emacs --batch -l ./gen-confusables.el 		       	\
	      --eval '(gen-confusables-read "confusables.txt")' \
	      --eval '(gen-confusables-write "uni-confusables.el")'
