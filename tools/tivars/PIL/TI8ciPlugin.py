"""
Plugin for `.8ci` files (`TIPicture`)
"""


from tivars import TIPicture
from .common import *


class TI8ciImageFile(TIImageFile):
    """
    `ImageFile` handler for 8ci files (`TIPicture`)
    """

    _T = TIPicture

    format = "8ci"
    format_description = "TI (e)Z80 Color Picture Format"


class TI8ciEncoder(TIEncoder):
    """
    Encoder for 8ci files (`TIPicture`)
    """

    _T = TIPicture


register(TI8ciImageFile, TI8ciEncoder)

__all__ = ["TI8ciEncoder", "TI8ciImageFile"]
