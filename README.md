# markdown-to-bbcode

markdown-to-bbcode is a simple tool to convert Markdown formatted text files into the corresponding BBcode.

## Compiling

To compile, run:

    mvn package

This will produce a jar with dependencies bundled in the target subdirectory. You'll need Scala 2.9.x to run it.

## Usage

    scala markdowntobbcode-1.2.jar <markdown-filename> [<output-filename>]

If the *output-filename* is omitted, it outputs to stdout.

Most standard Markdown syntax is supported. If a particular construct is not supported, a warning message is printed to stderr.
