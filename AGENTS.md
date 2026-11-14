## General guidelines

See `HACKING.md`.

## Docs and comments

When you write docs in source code most of them must be a single sentence
that conveys the essence of the thing you are documenting. Do not be afraid
to use precise technical terms. Prefer them strongly to multi-word plain
English formulations. Each sentence after the first one needs justification
to be included—it must be important and non-trivial information.

Do not write line comments that explain what is going on inside the code,
unless the point they convey is truly important and non-trivial. Code should
dominate comments, not the other way around.

Instead of writing in Claudisms, try to write in a style that is already
used in the codebase.

Write lines that are no longer than 76 characters, if possible. The same
applies to commit messages and especially commit headers—they always have to
be under 76 characters.

## Checking your implementations

Before declaring something done, ask yourself the question: is this the
simplest and irreducible implementation of this logic? Are
simplifications/ablations possible that make the solution more elegant?

## Audits

When asked to perform an audit of the codebase it means that you should scan
it for redundant or obsolete fragments (both in code and documentation), as
well as potential bugs and design flaws. Fan out agents as necessary to
accomplish this.
