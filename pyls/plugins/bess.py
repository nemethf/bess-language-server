# Copyright 2019 Felician Nemeth

# Bess specific funcionality

import json
import os
import logging
from pyls import hookimpl, uris

log = logging.getLogger(__name__)

@hookimpl
def pyls_settings():
    # We cannot set the defaults in config.py, because that
    # would overwrite user level configuration.  See:
    # ../config/config.py:107
    return {'plugins': {'bess': {}}}

def fix_offset(d):
    if d['uri'].endswith('.bess'):
        d['range']['start']['line'] -= 1
        d['range']['end']['line'] -= 1
    return d

def process_refs(config, document, goto_kind, outcome):
    defs = []
    for l in outcome.get_result():
        defs.extend( [fix_offset(d) for d in l] )

    defs = insert_bess_refs(config, document, goto_kind, defs)

    outcome.force_result([defs])

@hookimpl(hookwrapper=True)
def pyls_definitions(config, document, position):
    outcome = yield
    process_refs(config, document, 'definitions', outcome)

@hookimpl(hookwrapper=True)
def pyls_references(config, document, position, exclude_declaration=False):
    outcome = yield
    process_refs(config, document, 'references', outcome)

db = {}
def get_mclass_db(mpath):
    global db
    if not db:
        with open(os.path.join(mpath, 'db.json')) as f:
            db = json.load(f)
    return db

def get_ref_types(config, goto_kind):
    settings = config.plugin_settings('bess')
    ref_types = settings.get('definitions')
    if not ref_types:
        # Should keep this synchronized with
        # ../../vscode-client/package.json
        defaults = {
            'definitions': [
                "cpp_definition",
            ],
            'references': [
                "cpp_definition",
                "mclass",
                "protobuf",
                "examples",
            ],
        }
        ref_types = defaults.get(goto_kind, [])
    log.warn("Settings for '%s': %s", goto_kind, ref_types)
    return ref_types

def insert_bess_refs(config, document, goto_kind, refs):
    ref_types = get_ref_types(config, goto_kind)
    extra_refs = []
    mclass_uri = uris.uri_with(document.uri,
                               path=os.path.join(document.mpath, 'mclass.py'))
    db = get_mclass_db(document.mpath)
    orig_refs = []
    for ref in refs:
        if not (ref['uri'] == mclass_uri):
            orig_refs.append(ref)
            continue
        if 'mclass' in ref_types:
            orig_refs.append(ref)
        mline = ref['range']['start']['line']
        for mclass in db['mclass']:
            for cmd in [mclass] + mclass['cmds']:
                if mline == cmd.get('line', 0) - 1:
                    break
            else:
                continue
            locations = []
            if 'cpp_definition' in ref_types:
                locations = [cmd['definition']]
            proto_loc = db['msg'].get(cmd['arg'], {}).get('line')
            if 'protobuf' in ref_types and proto_loc:
                filename = 'protobuf/module_msg.proto'
                filename = document.make_abs_bess_filename(filename)
                locations.append({'file': filename, 'line': proto_loc})
            if 'examples' in ref_types:
                locations += (cmd.get('examples') or [])
            for loc in locations:
                path = document.make_abs_bess_filename(loc['file'])
                extra_refs.append({
                    'uri': uris.uri_with(document.uri, path=path),
                    'range': {
                        'start': {'line': loc['line'] - 1, 'character': 0},
                        'end': {'line': loc['line'] - 1, 'character': 0}
                        }
                })

    return orig_refs + extra_refs
