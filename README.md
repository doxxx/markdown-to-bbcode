# markdown-to-bbcode

markdown-to-bbcode is a simple tool to convert Markdown formatted text files into the corresponding BBcode.

## Usage

    java -jar markdowntobbcode-1.0-jar-with-dependencies.jar <markdown-filename> [<output-filename>]

If the *output-filename* is omitted, it outputs to stdout.

Most standard Markdown syntax is supported. If a particular construct is not supported, a warning message is printed to stderr.