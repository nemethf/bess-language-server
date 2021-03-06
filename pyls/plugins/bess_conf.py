# Copyright 2017 Palantir Technologies, Inc.
import os

import pycodestyle
from pyls._utils import find_parents
from pyls.config import pycodestyle_conf
from pyls.config.source import ConfigSource

CONFIG_KEY = 'bess'
USER_CONFIGS = [pycodestyle.USER_CONFIG] if pycodestyle.USER_CONFIG else []
PROJECT_CONFIGS = ['pycodestyle.cfg', 'setup.cfg', 'tox.ini', '.bessls']

OPTIONS = [
    ('source_directory', 'plugins.bess.source_directory', str),
    ('definitions', 'plugins.bess.definitions', list),
    ('references', 'plugins.bess.references', list),
]


class BessConfig(pycodestyle_conf.PyCodeStyleConfig):

    def user_config(self):
        user_configs = USER_CONFIGS + [os.path.join(self.xdg_home, 'bessls')]
        config = self.read_config_from_files(user_configs)
        return self.parse_config(config, CONFIG_KEY, OPTIONS)

    def project_config(self, document_path):
        files = find_parents(self.root_path, document_path, PROJECT_CONFIGS)
        config = self.read_config_from_files(files)
        return self.parse_config(config, CONFIG_KEY, OPTIONS)
