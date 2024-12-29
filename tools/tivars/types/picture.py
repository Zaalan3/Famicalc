"""
Pictures and images
"""


import re


from collections.abc import Iterator, Sequence
from warnings import warn

from tivars.data import *
from tivars.models import *
from tivars.tokenizer import TokenizedString
from tivars.var import SizedEntry

RGB = tuple[int, int, int]


class L1(Converter):
    """
    Converter for black-and-white pixels packed eight-per-byte
    """

    _T = tuple[int, ...]

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``tuple[int, int, int, int, int, int, int, int]``

        :param data: The raw bytes to convert
        :return: A tuple of eight integers in {0, 255} corresponding to the bits of ``data``
        """

        return tuple(255 * (1 - int(bit)) for bit in f"{data[0]:08b}")

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts  ``tuple[int, int, int, int, int, int, int, int]`` -> ``bytes``

        :param value: The value to convert
        :return: The values in ``value`` joined into a single byte
        """

        return bytes([int("".join("0" if bit else "1" for bit in value), 2)])


class RGBPalette(Converter):
    """
    Converter for color pixels indexed by the TI palette
    """

    _T = tuple[RGB, RGB]

    Blue = (0, 0, 255)
    Red = (255, 0, 0)
    Black = (0, 0, 0)
    Magenta = (255, 0, 255)
    Green = (0, 158, 0)
    Orange = (255, 142, 32)
    Brown = (182, 32, 0)
    Navy = (0, 0, 134)
    LtBlue = (0, 146, 255)
    Yellow = (255, 255, 0)
    White = (255, 255, 255)
    LtGray = (231, 227, 231)
    MedGray = (199, 195, 199)
    Gray = (142, 138, 142)
    DarkGray = (81, 85, 81)

    palette = [White, Blue, Red, Black, Magenta, Green, Orange, Brown, Navy, LtBlue, Yellow,
               White, LtGray, MedGray, Gray, DarkGray]

    @classmethod
    def nearest(cls, r: int, g: int, b: int) -> RGB:
        """
        Finds the nearest palette color to the given RGB components under the Euclidean metric

        :param r: The red component
        :param g: The green component
        :param b: The blue component
        :return: The RGB components of the nearest palette color
        """

        nearest = min(cls.palette, key=lambda x: (x[0] - r) ** 2 + (x[1] - g) ** 2 + (x[2] - b) ** 2)
        if nearest != (r, g, b):
            warn(f"The pixel {(r, g, b)} is not contained in the palette; using {nearest} as an approximation.",
                 UserWarning)

        return nearest

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``tuple[RGB, RGB]``

        :param data: The raw bytes to convert
        :return: The pair of RGB values from the palette indexed by the nibbles in ``data``
        """

        return cls.palette[data[0] >> 4], cls.palette[data[0] & 15]

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts  ``tuple[RGB, RBG]`` -> ``bytes``

        :param value: The value to convert
        :return: The byte formed by finding the indices of the colors nearest to the RGB values in ``value``
        """

        return bytes([(cls.palette.index(cls.nearest(*value[0])) << 4) + cls.palette.index(cls.nearest(*value[1]))])


class RGB565(Converter):
    """
    Converter for color pixels stored in RGB565 format
    """

    _T = RGB

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> `RGB`

        :param data: The raw bytes to convert
        :return: The RGB value stored in the two bytes of ``data``
        """

        return (
            (data[1] >> 3) * 255 // 31,
            (((data[1] & 7) << 3) | (data[0] >> 5)) * 255 // 63,
            (data[0] & 31) * 255 // 31
        )

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts `RGB` -> ``bytes``

        :param value: The value to convert
        :return: The bytes formed by concatenating the RGB components of ``value`` in 565 format
        """

        return bytes([((value[1] >> 2 & 7) << 5) | value[2] >> 3]) + bytes([(value[0] & ~7) | (value[1] >> 5)])


class PictureEntry(SizedEntry):
    """
    Base class for all picture types

    A picture or image is stored as a stream of pixels in some encoding format.
    """

    width = 0
    """
    The width of the picture
    """

    height = 0
    """
    The height of the picture
    """

    data_width = width
    """
    The width of the picture while stored as data
    """

    data_height = height
    """
    The height of the picture while stored as data
    """

    pil_mode = None
    """
    The mode used by PIL images for this image type
    """

    pixel_type = None
    """
    The type of a single pixel
    """

    np_shape = (height, width, 3)
    """
    The shape of this image as a NumPy array
    """

    has_color = True
    """
    Whether this picture has color
    """

    def __iter__(self) -> Iterator[pixel_type]:
        for row in self.array():
            for col in row:
                yield col

    @Loader[Sequence]
    def load_array(self, arr: Sequence[Sequence[pixel_type]]):
        """
        Loads a two-dimensional sequence of pixels into this picture

        :param arr: The array to load
        """

        raise NotImplementedError

    def array(self) -> list[list[pixel_type]]:
        """
        :return: A two-dimensional ``list`` of the pixels in this picture
        """

        raise NotImplementedError

    def coerce(self):
        match self.length + 2:
            case TIMonoPicture.min_data_length: self.__class__ = TIMonoPicture
            case TIPicture.min_data_length: self.__class__ = TIPicture
            case TIImage.min_data_length: self.__class__ = TIImage
            case _: warn(f"Picture has unexpected length ({self.length}).",
                         BytesWarning)


class TIMonoPicture(PictureEntry):
    """
    Parser for monochromatic pictures

    A `TIMonoPicture` is a 96 x 63 grid of black or white pixels, stored as 8 pixels per byte.
    """

    extensions = {
        None: "8xi",
        TI_83P: "8xi",
        TI_84PCSE: ""
    }

    min_data_length = 758

    width = 96
    height = 63

    data_width = width // 8
    data_height = height

    pil_mode = "L"
    pixel_type = int
    np_shape = (height, width)

    has_color = False

    leading_name_byte = b'\x60'

    _type_id = 0x07

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "Pic1",
                 version: int = None, archived: bool = True,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def get_min_os(self, data: bytes = None) -> OsVersion:
        return TI_83P.OS()

    @Loader[Sequence]
    def load_array(self, arr: Sequence[Sequence[pixel_type]]):
        self.data = b''.join(L1.set(entry) for row in arr for entry in zip(*[iter(row)] * 8, strict=True))

    def array(self) -> list[list[pixel_type]]:
        return [[bw for col in range(self.data_width)
                 for bw in L1.get(self.data[self.data_width * row + col:][:1])]
                for row in range(self.data_height)]


class TIPicture(PictureEntry, register=True):
    """
    Parser for color pictures

    A `TIPicture` is a 266 x 165 grid of pixels which can take on any of the 15 standard colors.
    Each pixel comprises a single nibble, which stores an (offset) index into the color palette.
    """

    flash_only = True

    versions = [0x0A]

    extensions = {
        None: "8ci",
        TI_84PCSE: "8ci"
    }

    min_data_length = 21947

    width = 266
    height = 165

    data_width = width // 2
    data_height = height

    pil_mode = "RGB"
    pixel_type = RGB
    np_shape = (height, width, 3)

    leading_name_byte = b'\x60'

    _type_id = 0x07

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "Pic1",
                 version: int = None, archived: bool = True,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def get_min_os(self, data: bytes = None) -> OsVersion:
        return TI_84PCSE.OS()

    @Loader[Sequence]
    def load_array(self, arr: Sequence[Sequence[pixel_type]]):
        self.data = b''.join(RGBPalette.set(entry) for row in arr for entry in zip(row[::2], row[1::2]))

    def array(self) -> list[list[pixel_type]]:
        return [[rgb for col in range(self.data_width)
                 for rgb in RGBPalette.get(self.data[self.data_width * row + col:][:1])]
                for row in range(self.data_height)]


# Workaround until the token sheets are updated
class ImageName(TokenizedString):
    """
    Converter for the name section of images

    Image names can be ``Image1`` - ``Image0``, but are stored using a font offset rather than tokens.
    """

    _T = str

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        return f"Image{data[1] + 1}"

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        if not re.fullmatch(r"(Image)?\d", value):
            warn(f"'{value}' is not a valid image name; defaulting to 'Image1'.",
                 BytesWarning)
            value = "Image1"

        return b"\x3C" + bytes([int(value[-1], 16) - 1])


class TIImage(PictureEntry, register=True):
    """
    Parser for color images

    A `TIImage` is a 133 x 83 grid of pixels which can take on any value in the full color space.
    Each pixel comprises two bytes, stored in RGB565 format; rows are stored backward.

    On-calc, each pixel is duplicated in both directions to fill the screen.
    """

    flash_only = True

    versions = [0x00]

    extensions = {
        None: "8ca",
        TI_84PCSE: "8ca",
    }

    min_data_length = 22247

    width = 133
    height = 83

    data_width = 2 * width + 2
    data_height = height

    pil_mode = "RGB"
    pixel_type = RGB
    np_shape = (height, width, 3)

    leading_name_byte = b'\x3C'
    leading_data_bytes = b'\x81'

    _type_id = 0x1A

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "Image1",
                 version: int = None, archived: bool = True,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    @Section(8, ImageName)
    def name(self) -> str:
        """
        The name of the entry

        Must be one of the image names: ``Image1`` - ``Image0``
        """

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Bytes)[2:3]
    def image_magic(self) -> bytes:
        """
        Magic identifying the file as an image

        This value is always ``0x81``.
        """

    @View(calc_data, SizedData)[3:]
    def data(self) -> bytes:
        pass

    def get_min_os(self, data: bytes = None) -> OsVersion:
        return TI_84PCSE.OS()

    @Loader[Sequence]
    def load_array(self, arr: Sequence[Sequence[pixel_type]]):
        self.data = b''.join(RGB565.set(entry) for row in arr[::-1] for entry in [*row, (0, 0, 0)])

    def array(self) -> list[list[pixel_type]]:
        return [[RGB565.get(self.data[self.data_width * row + col:][:2])
                 for col in range(0, self.data_width - 2, 2)]
                for row in range(self.data_height)][::-1]


__all__ = ["TIMonoPicture", "TIPicture", "TIImage",
           "L1", "RGBPalette", "RGB565"]
