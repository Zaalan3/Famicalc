"""
OS version definitions
"""


import tivars.tokens.scripts.parse as parse
from .model import *


class OsVersions(parse.OsVersions):
    """
    Namespace containing useful OS versions

    The maximal elements `OsVersions.INITIAL` and `OsVersions.LATEST` can be accessed via this namespace.
    """

    MATHPRINT = TI_84P.OS("2.53")

    ASMPRGM_DISABLED = TI_84PCE.OS("5.3.1")

    ASM_DISABLED_PCE = TI_83PCE.OS("5.5.1")
    ASM_DISABLED_CE = TI_84PCE.OS("5.6.0")


__all__ = ["OsVersions"]
