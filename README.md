Completion style using typo analysis
====================================

Find here the source for typo.el, an [Emacs] [completion style] based on "typo
analysis", that is to say it adds the suggestion to complete a prompt
to the [closest string] within a bounded number of edits.

[Emacs]:
	https://www.gnu.org/software/emacs/
[completion style]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
[closest string]:
	https://en.wikipedia.org/wiki/Levenshtein_distance

Installation
------------

Typo.el is avaliable from [GNU ELPA]. It can be installed by
invoking

	M-x package-install RET typo RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/typo.html

Usage
-----

Add `typo` to the end of `completion-styles` as follows:

    (add-to-list 'completion-styles 'typo t)

Adding it to the end is important, so as to make sure that typo
analysis (that can frequently be too eager) is only used as a last
resort.

Take a look at the `typo-level`, `typo-shrink` and `typo-expand` user
options if you find the completion to be too restrictive or invasive.

Contribute
----------

As typo.el is distribed as part of [GNU ELPA], and therefore requires
a [copyright assignment] to the [FSF], for all non-trivial code
contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

typo.el is developed on [SourceHut].

[SourceHut]:
	https://git.sr.ht/~pkal/typo/

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted to my [public
inbox].

[public inbox]:
	https://lists.sr.ht/~pkal/public-inbox

Distribution
------------

Typo.el and all other source files in this directory are distributed
under the [GNU Public License], Version 3 (like Emacs itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
