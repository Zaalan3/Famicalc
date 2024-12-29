"""
Plugin for `.8ca` files (`TIImage`)
"""


from tivars import TIImage
from .common import *


class TI8caImageFile(TIImageFile):
    """
    `ImageFile` handler for 8ca files (`TIImage`)
    """

    _T = TIImage

    format = "8ca"
    format_description = "TI (e)Z80 Image Format"


class TI8caEncoder(TIEncoder):
    """
    Encoder for 8ca files (`TIImage`)
    """

    _T = TIImage


register(TI8caImageFile, TI8caEncoder)

__all__ = ["TI8caEncoder", "TI8caImageFile"]
