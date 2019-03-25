# Copyright 2019 Felician Nemeth

# Bess specific funcionality

import collections
import functools
import gzip
import json
import logging
import os
import re
from pyls import hookimpl, uris
from pyls.config import config as pyls_config

from .bess_conf import BessConfig

log = logging.getLogger(__name__)

# Monkey patch :(
# But this way it's easier to sync with upstream, and
# config variable `bess.source_directory` can be used.
from pyls.workspace import Workspace
old_source_roots = Workspace.source_roots
def new_source_roots(self, document_path):
    path = [get_mpath()]

    log.debug('bess new_source_roots %s', self.bess_dir)
    if self.bess_dir:
        path.append(self.bess_dir)

    path.extend(old_source_roots(self, document_path))
    return path
Workspace.source_roots = new_source_roots

from pyls.workspace import Document
old_source = Document.source
@property
def new_source(self):
    src = old_source.fget(self)
    if self.filename.endswith('.bess'):
        if getattr(self, 'prepend_import_to_source', False):
            src = "from mclass import *\n" + src
        src = src.replace('->', '; ')
        src = src.replace('::', '= ')
        src = re.sub(r'\$\w(\w*)!', "'\\1'+", src)
    return src
Document.source = new_source

old_jedi_script = Document.jedi_script
def new_jedi_script(self, position=None):
    try:
        if self.filename.endswith('.bess'):
            self.prepend_import_to_source = True
            if position:
                new_position = position.copy()
                new_position['line'] += 1
        return old_jedi_script(self, new_position)
    finally:
        self.prepend_import_to_source = False
Document.jedi_script = new_jedi_script

###########################################################################

@hookimpl
def pyls_settings(config):
    default = pyls_config.DEFAULT_CONFIG_SOURCES
    sources = config._settings.get('configurationSources', default)
    sources.append('bess')
    config._settings['configurationSources'] = sources

    config._config_sources['bess'] = BessConfig(config._root_path)

    # We cannot set the defaults in config.py, because that
    # would overwrite user level configuration.  See:
    # ../config/config.py:107
    return {'plugins': {'bess': {}}}

@hookimpl
def pyls_initialize(config, workspace):
    workspace.bess_dir = get_spath(config)
    log.debug('pyls_initialize bess_dir: %s', workspace.bess_dir)

@hookimpl(hookwrapper=True)
def pyls_definitions(config, document, position):
    outcome = yield
    process_refs(config, document, 'definitions', outcome)

@hookimpl(hookwrapper=True)
def pyls_references(config, document, position, exclude_declaration=False):
    outcome = yield
    process_refs(config, document, 'references', outcome)

@hookimpl(hookwrapper=True)
def pyls_document_highlight(config, document, position):
    outcome = yield
    process_refs(config, document, 'highlight', outcome)

def fix_offset(d, document=None):
    if d.get('uri', document.uri).endswith('.bess'):
        d['range']['start']['line'] -= 1
        d['range']['end']['line'] -= 1
    return d

def process_refs(config, document, goto_kind, outcome):
    defs = []
    try:
        result = outcome.get_result()
    except Exception as e:
        log.warn("No results: %s, %s", e, outcome.excinfo)
        return

    for l in result:
        defs.extend( [fix_offset(d, document) for d in l] )

    if goto_kind != 'highlight':
        defs = insert_bess_refs(config, document, goto_kind, defs)

    outcome.force_result([defs])

def get_spath(config, document=None, filename=None):
    document_path = document and document.path
    settings = config.plugin_settings('bess', document_path=document_path)
    bess_dir = os.environ.get('BESS', '')
    bess_dir = settings.get('source_directory', bess_dir)
    bess_dir = os.path.abspath(bess_dir)
    if filename:
        return os.path.join(bess_dir, filename)
    return bess_dir

def get_mpath(filename=None):
    p = os.path
    path = p.realpath(p.join(p.dirname(__file__), '..', 'extra'))
    if filename:
        return p.join(path, filename)
    return path

db = {}
def get_mclass_db():
    global db
    if not db:
        with gzip.open(get_mpath('mclass.min.json.gz')) as f:
            db = json.load(f)
        msg_full = {m['fullName']: m for m in db['msg']}
        msg_short = {m['name']: m for m in db['msg']}
        db['msg'] = msg_short
        db['msg_full'] = msg_full
    return db

def get_ref_types(config, document, goto_kind):
    settings = config.plugin_settings('bess', document_path=document.path)
    ref_types = settings.get(goto_kind)
    if not ref_types:
        # Should keep this synchronized with
        # ../../vscode-client/package.json
        defaults = {
            'definitions': [
                "project",
                "cpp_definition",
            ],
            'references': [
                "project",
                "cpp_definition",
                "mclass",
                "protobuf",
                "examples",
            ],
        }
        ref_types = defaults.get(goto_kind, [])
    log.warn("Settings for '%s': %s", goto_kind, ref_types)
    return ref_types

def make_abs_bess_filename(config, document, filename):
    if type(filename) == int:
        db = get_mclass_db()
        if filename == 0:
            filename = get_mpath(db['files'][str(filename)])
        else:
            filename = db['files'][str(filename)]
    if os.path.isabs(filename):
        return filename
    return get_spath(config, document, filename)

def conv_loc(config, document, loc):
    if not loc:
        return
    path = make_abs_bess_filename(config, document, loc['file'])
    return {
        'uri': uris.uri_with(document.uri, path=path),
        'range': {
            'start': {'line': loc['line'] - 1, 'character': 0},
            'end': {'line': loc['line'] - 1, 'character': 0}
        }
    }

def insert_bess_refs(config, document, goto_kind, refs):
    ref_groups = collections.defaultdict(list)
    mclass_uri = uris.uri_with(document.uri,
                               path=get_mpath('mclass.py'))
    db = get_mclass_db()
    for ref in refs:
        if not (ref['uri'] == mclass_uri):
            ref_groups['project'].append(ref)
            continue
        ref_groups['mclass'].append(ref)
        mline = ref['range']['start']['line']
        for mclass in db['mclass']:
            for cmd in [mclass] + mclass['cmds']:
                if mline == cmd.get('line', 0) - 1:
                    break
            else:
                continue

            ref = conv_loc(config, document, cmd['definition'])
            ref_groups['cpp_definition'].append(ref)

            loc = db['msg'].get(cmd['arg'], {})
            ref = conv_loc(config, document, loc)
            ref_groups['protobuf'].append(ref)

            loc = db['msg_full'].get(cmd.get('return'))
            ref = conv_loc(config, document, loc)
            if ref and ref not in ref_groups['protobuf']:
                ref_groups['protobuf'].append(ref)

            for loc in cmd.get('examples', []):
                ref = conv_loc(config, document, loc)
                ref_groups['examples'].append(ref)

    refs = []
    for ref_type in get_ref_types(config, document, goto_kind):
        refs += ref_groups[ref_type]
    return refs
