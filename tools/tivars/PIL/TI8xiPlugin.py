"""
Plugin for `.8xi` files (`TIMonoPicture`)
"""


from tivars import TIMonoPicture
from .common import *


class TI8xiImageFile(TIImageFile):
    """
    `ImageFile` handler for 8xi files (`TIMonoPicture`)
    """

    _T = TIMonoPicture

    format = "8xi"
    format_description = "TI (e)Z80 Monochrome Picture Format"


class TI8xiEncoder(TIEncoder):
    """
    Encoder for 8xi files (`TIMonoPicture`)
    """

    _T = TIMonoPicture


register(TI8xiImageFile, TI8xiEncoder)

__all__ = ["TI8xiEncoder", "TI8xiImageFile"]
