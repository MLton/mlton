# -*- coding: utf-8 -*-
"""
Standard ML Lexer for Pygments.
"""

import re

from pygments.lexer import RegexLexer, bygroups
from pygments.token import *


__all__ = ['StandardMLLexer']


class StandardMLLexer(RegexLexer):
    """
    A Standard ML lexer.
    """
    name = 'Standard ML'
    aliases = ['sml']
    filenames = ['*.sml','*.sig','*.fun','*.ML']
    mimetypes = ['text/x-standardml', 'application/x-standardml']

    flags = re.DOTALL | re.MULTILINE

    alphanumid_reserved = [
        ## Core
        'abstype', 'and', 'andalso', 'as', 'case', 'do', 'datatype', 'else',
        'end', 'exception', 'fn', 'fun', 'handle', 'if', 'in', 'infix',
        'infixr', 'let', 'local', 'nonfix', 'of', 'op', 'open', 'orelse',
        'raise', 'rec', 'then', 'type', 'val', 'with', 'withtype', 'while',
        ## Modules
        'eqtype', 'functor', 'include', 'sharing', 'sig',
        'signature', 'struct', 'structure', 'where'
    ]
    symbolicid_reserved = [
        ## Core
        ':', '|', '=', '=>', '->', '#',
        ## Modules
        ':>'
    ]
    nonid_reserved = [
        ## Core
        '(', ')', '[', ']', '{', '}', ',', ';', '...', '_'
        ## Modules
    ]

    alphanumid_re = r"[a-zA-Z][a-zA-Z0-9_']*"
    symbolicid_re = r"[!%&$#+\-/:<=>?@\\~`^|*]+"
    long_id_re = r"((%s\.)*)((%s)|(%s))" % (alphanumid_re, alphanumid_re, symbolicid_re)
    primed_alphanumid_re = r"'[a-zA-Z0-9_']*"

    def long_id_callback(self, match):
        strids = match.group(1)
        pos = 0
        for m in re.finditer(r'(%s)(\.)' % self.alphanumid_re, strids) :
            strid = m.group(1)
            if strid in self.alphanumid_reserved :
                token = Error
            else :
                token = Name
            yield pos, token, strid
            pos += len(strid)
            dot = m.group(2)
            yield pos, Punctuation, dot
            pos += len(dot)
        nqid = match.group(3)
        if strids == "" :
            if nqid in self.alphanumid_reserved :
                token = Keyword
            elif nqid in self.symbolicid_reserved :
                token = Punctuation
            else :
                token = Name
        else :
            if nqid in self.alphanumid_reserved :
                token = Error
            elif nqid in self.symbolicid_reserved :
                token = Error
            else :
                token = Name
        yield pos, token, nqid
        pos += len(nqid)

    printable_re = r'[^\x00-\x1F"\\\x7F]'
    escape_re = r'\\("|\\|a|b|t|n|v|f|r|^[@-_]|[0-9]{3}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})'

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'\(\*', Comment.Multiline, 'comment'),

            (r'~?[0-9]+\.[0-9]+((e|E)~?[0-9]+)?', Number.Float),
            (r'~?[0-9]+(e|E)~?[0-9]+', Number.Float),
            (r'0wx[0-9a-fA-F]+', Number.Hex),
            (r'~?0x[0-9a-fA-F]+', Number.Hex),
            (r'0w[0-9]+', Number.Integer),
            (r'~?[0-9]+', Number.Integer),

            (r'"', String, 'string'),
            (r'(#)(")', bygroups(Punctuation, String), 'string'),

            (long_id_re, long_id_callback),
            (r'(%s)' % '|'.join([re.escape(z) for z in nonid_reserved]), Punctuation),
            (primed_alphanumid_re, Name),

            (r'.', Error, 'error')
        ],
        'error': [
            (r'.', Error)
        ],
        'comment': [
            (r'\(\*', Comment.Multiline, '#push'),
            (r'\*\)', Comment.Multiline, '#pop'),
            (r'.', Comment.Multiline),
        ],
        'string': [
            (printable_re, String),
            (escape_re, String.Escape),
            (r'\\\s', String, 'gap'),
            (r'"', String, '#pop'),
            (r'.', Error),
        ],
        'gap': [
            (r'\s+', String),
            (r'\\', String, '#pop'),
            (r'.', Error),
        ],
    }
