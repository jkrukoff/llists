#!/usr/bin/env sed
# Requires extended regular expresssions, run with -E
\~\(https?://[^\)]*\)~ {
	# A noop, but allows branch to work.
	s~\(https?://github.com/jkrukoff~&~
	# If link looks like it's internal to the project, do nothing.
	t
	# Otherwise, assume edown has rewritten any ".html" links to ".md" and
	# revert.
	s~(\(https?://.*)\.md\)~\1.html)~
}
