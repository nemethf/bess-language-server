#!/usr/bin/env python
from setuptools import find_packages, setup
import versioneer

README = open('README.md', 'r').read()


setup(
    name='bess-language-server',

    # Versions should comply with PEP440.  For a discussion on single-sourcing
    # the version across setup.py and the project code, see
    # https://packaging.python.org/en/latest/single_source_version.html
    version=versioneer.get_version(),
    cmdclass=versioneer.get_cmdclass(),

    description='Bess Language Server for the Language Server Protocol',

    long_description=README,

    # The project's main homepage.
    url='https://github.com/nemethf/bess-language-server',

    author='Palantir Technologies, Inc.',

    # You can just specify the packages manually here if your project is
    # simple. Or you can use find_packages().
    packages=find_packages(exclude=['contrib', 'docs', 'test']),

    # List run-time dependencies here.  These will be installed by pip when
    # your project is installed. For an analysis of "install_requires" vs pip's
    # requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=[
        'configparser; python_version<"3.0"',
        'future>=0.14.0',
        'futures; python_version<"3.2"',
        'jedi>=0.12',
        'python-jsonrpc-server',
        'pluggy'
    ],

    # List additional groups of dependencies here (e.g. development
    # dependencies). You can install these using the following syntax,
    # for example:
    # $ pip install -e .[test]
    extras_require={
        'all': [
            'autopep8',
            'mccabe',
            'pycodestyle',
            'pydocstyle>=2.0.0',
            'pyflakes>=1.6.0',
            'rope>=0.10.5',
            'yapf',
        ],
        'autopep8': ['autopep8'],
        'mccabe': ['mccabe'],
        'pycodestyle': ['pycodestyle'],
        'pydocstyle': ['pydocstyle>=2.0.0'],
        'pyflakes': ['pyflakes>=1.6.0'],
        'rope': ['rope>0.10.5'],
        'yapf': ['yapf'],
        'test': ['tox', 'versioneer', 'pytest', 'mock', 'pytest-cov', 'coverage'],
    },

    # To provide executable scripts, use entry points in preference to the
    # "scripts" keyword. Entry points provide cross-platform support and allow
    # pip to create the appropriate form of executable for the target platform.
    entry_points={
        'console_scripts': [
            'bessls = bessls.__main__:main',
        ],
        'bessls': [
            'autopep8 = bessls.plugins.autopep8_format',
            'bess = bessls.plugins.bess',
            'jedi_completion = bessls.plugins.jedi_completion',
            'jedi_definition = bessls.plugins.definition',
            'jedi_hover = bessls.plugins.hover',
            'jedi_highlight = bessls.plugins.highlight',
            'jedi_references = bessls.plugins.references',
            'jedi_signature_help = bessls.plugins.signature',
            'jedi_symbols = bessls.plugins.symbols',
            'mccabe = bessls.plugins.mccabe_lint',
            'preload = bessls.plugins.preload_imports',
            'pycodestyle = bessls.plugins.pycodestyle_lint',
            'pydocstyle = bessls.plugins.pydocstyle_lint',
            'pyflakes = bessls.plugins.pyflakes_lint',
            'rope_completion = bessls.plugins.rope_completion',
            'rope_rename = bessls.plugins.rope_rename',
            'yapf = bessls.plugins.yapf_format',
        ]
    },

    package_data={
        'bessls': ['extra/*'],
    },
)
