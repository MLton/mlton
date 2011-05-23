# -*- coding: utf-8 -*-
"""
Standard ML Lexer for Pygments
"""
from setuptools import setup

setup(
    name='sml_lexer',
    version='1.0',
    author='Matthew Fluet',
    author_email='Matthew.Fluet@gmail.com',
    url='http://www.mlton.org/Pygments',
    description=__doc__,
    packages=['sml_lexer'],
    entry_points='''
    [pygments.lexers]
    StandardMLLexer = sml_lexer:StandardMLLexer
    '''
)
