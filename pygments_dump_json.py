#!/usr/bin/env python
import sys
import json
from pygments.lexers import get_all_lexers
from pygments.formatters import get_all_formatters

def lexer_dict(name, aliases, filetypes, mimetypes):
    return { '_lexerName': name
           , '_lexerAliases': list(aliases)
           , '_lexerFileTypes': list(filetypes)
           , '_lexerMimeTypes': list(mimetypes)
           }

def formatter_dict(f):
    return { '_formatterName': f.name
           , '_formatterAliases': list(f.aliases)
           }

if len(sys.argv) < 2:
    print('Usage: {} (lexers|formatters)'.format(sys.argv[0]))
    sys.exit(1)

if sys.argv[1] == "lexers":
    lexers = [lexer_dict(*x) for x in get_all_lexers()]
    json.dump(lexers, sys.stdout)

if sys.argv[1] == "formatters":
    formatters = [formatter_dict(x) for x in get_all_formatters()]
    json.dump(formatters, sys.stdout)
