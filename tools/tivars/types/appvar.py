"""
Appvars
"""


from tivars.models import *
from tivars.var import SizedEntry


class TIAppVar(SizedEntry, register=True):
    """
    Parser for application variables (appvars)

    An appvar is a sized block of data, to be interpreted by the corresponding app as desired.
    Certain common formats, such as compiled Python modules, are denoted by a header at the start of the data block.
    """

    flash_only = True

    extensions = {
        None: "8xv",
        TI_83P: "8xv",
    }

    _type_id = 0x15

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "UNNAMED",
                 version: int = None, archived: bool = True,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def get_min_os(self, data: bytes = None) -> OsVersion:
        return TI_83P.OS()


__all__ = ["TIAppVar"]
