# Copyright 2017 Palantir Technologies, Inc.
import json
import logging
import os
from bessls import hookimpl, uris

log = logging.getLogger(__name__)

def offset(d):
    path = d.module_path if d.module_path else document.uri
    if path.endswith('.bess'):
        return 2
    return 1

db = {}
def get_mclass_db(mpath):
    global db
    if not db:
        with open(os.path.join(mpath, 'db.json')) as f:
            db = json.load(f)
    return db

@hookimpl
def pyls_references(document, position, exclude_declaration=False):
    # Note that usages is not that great in a lot of cases: https://github.com/davidhalter/jedi/issues/744
    usages = document.jedi_script(position).usages()

    if exclude_declaration:
        # Filter out if the usage is the actual declaration of the thing
        usages = [d for d in usages if not d.is_definition()]

    refs = [{
        'uri': uris.uri_with(document.uri, path=d.module_path) if d.module_path else document.uri,
        'range': {
            'start': {'line': d.line - offset(d), 'character': d.column},
            'end': {'line': d.line - offset(d), 'character': d.column + len(d.name)}
        }
    } for d in usages]

    extra_refs = []
    mclass_uri = uris.uri_with(document.uri,
                               path=os.path.join(document.mpath, 'mclass.py'))
    db = get_mclass_db(document.mpath)
    for ref in refs:
        if not (ref['uri'] == mclass_uri):
            continue
        mline = ref['range']['start']['line']
        for mclass in db['mclass']:
            for cmd in [mclass] + mclass['cmds']:
                if mline == cmd.get('line', 0) - 1:
                    break
            else:
                continue
            locations = [cmd['definition']]
            log.error('cmd: %s', cmd)
            proto_loc = db['msg'].get(cmd['arg'], {}).get('line')
            if proto_loc:
                filename = 'protobuf/module_msg.proto'
                filename = document.make_abs_bess_filename(filename)
                locations.append({'file': filename, 'line': proto_loc})
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

    return refs + extra_refs
