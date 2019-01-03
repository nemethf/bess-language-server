# Copyright 2017 Palantir Technologies, Inc.
import logging
from bessls import hookimpl, uris

log = logging.getLogger(__name__)


def offset(d):
    if d.module_path.endswith('.bess'):
        return 2
    return 1

@hookimpl
def pyls_definitions(config, document, position):
    settings = config.plugin_settings('jedi_definition')
    definitions = document.jedi_script(position).goto_assignments(
        follow_imports=settings.get('follow_imports', False),
        follow_builtin_imports=settings.get('follow_builtin_imports', False))

    definitions = [
        d for d in definitions
        if d.is_definition() and d.line is not None and d.column is not None and d.module_path is not None
    ]

    return [{
        'uri': uris.uri_with(document.uri, path=d.module_path),
        'range': {
            'start': {'line': d.line - offset(d), 'character': d.column},
            'end': {'line': d.line - offset(d), 'character': d.column + len(d.name)}
        }
    } for d in definitions]
