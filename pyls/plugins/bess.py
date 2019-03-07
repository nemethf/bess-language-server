# Copyright 2019 Felician Nemeth

# Bess specific funcionality

import collections
import json
import logging
import os
from pyls import hookimpl, uris

log = logging.getLogger(__name__)

@hookimpl
def pyls_settings():
    # We cannot set the defaults in config.py, because that
    # would overwrite user level configuration.  See:
    # ../config/config.py:107
    return {'plugins': {'bess': {}}}

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
    for l in outcome.get_result():
        defs.extend( [fix_offset(d, document) for d in l] )

    if goto_kind != 'highlight':
        defs = insert_bess_refs(config, document, goto_kind, defs)

    outcome.force_result([defs])

db = {}
def get_mclass_db(mpath):
    global db
    if not db:
        with open(os.path.join(mpath, 'db.json')) as f:
            db = json.load(f)
    return db

def get_ref_types(config, goto_kind):
    settings = config.plugin_settings('bess')
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

def make_abs_bess_filename(filename):
    if os.path.isabs(filename):
        return filename
    return os.path.join(os.environ.get('BESS', ''), filename)

def conv_loc(document, loc):
    path = make_abs_bess_filename(loc['file'])
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
                               path=os.path.join(document.mpath, 'mclass.py'))
    db = get_mclass_db(document.mpath)
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

            ref = conv_loc(document, cmd['definition'])
            ref_groups['cpp_definition'].append(ref)

            proto_loc = db['msg'].get(cmd['arg'], {}).get('line')
            filename = 'protobuf/module_msg.proto'
            ref = conv_loc(document, {'file': filename, 'line': proto_loc})
            ref_groups['protobuf'].append(ref)

            for loc in cmd.get('examples', []):
                ref = conv_loc(document, loc)
                ref_groups['examples'].append(ref)

    refs = []
    for ref_type in get_ref_types(config, goto_kind):
        refs += ref_groups[ref_type]
    return refs
