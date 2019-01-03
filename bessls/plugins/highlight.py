# Copyright 2017 Palantir Technologies, Inc.
import logging
from bessls import hookimpl, lsp, uris

log = logging.getLogger(__name__)


@hookimpl
def pyls_document_highlight(document, position):
    usages = document.jedi_script(position).usages()

    def is_valid(definition):
        return definition.line is not None and definition.column is not None

    def local_to_document(definition):
        return not definition.module_path or uris.uri_with(document.uri, path=definition.module_path) == document.uri

    if document.uri.endswith('.bess'):
        offset = 2
    else:
        offset = 1
    return [{
        'range': {
            'start': {'line': d.line - offset, 'character': d.column},
            'end': {'line': d.line - offset, 'character': d.column + len(d.name)}
        },
        'kind': lsp.DocumentHighlightKind.Write if d.is_definition() else lsp.DocumentHighlightKind.Read
    } for d in usages if is_valid(d) and local_to_document(d)]
