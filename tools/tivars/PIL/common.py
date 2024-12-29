"""
Common import for PIL plugins
"""


import numpy as np
import warnings

from PIL import Image, ImageFile
from tivars import TIVar
from tivars.types.picture import PictureEntry


def accept(prefix):
    return prefix[:8] in (b"**TI82**", b"**TI83**", b"**TI83F*")


def register(file, encoder):
    Image.register_open(file.format, file, accept)
    Image.register_extension(file.format, "." + file.format)
    Image.register_decoder(file.format, TIDecoder)

    Image.register_save(file.format, file._save)
    Image.register_encoder(file.format, encoder)


class TIImageFile(ImageFile.ImageFile):
    """
    Base class for PIL plugin `ImageFile` types
    """

    _T = PictureEntry

    format = "8??"

    def _open(self):
        """
        Loads necessary image information from an opened file
        """

        with warnings.catch_warnings():
            warnings.simplefilter("error")

            var = TIVar()
            var.load_bytes(self.fp.read())

            img = var.entries[0]
            if not isinstance(img, self._T):
                raise TypeError(f"image is not in .{self.format} format")

            self._size = (img.width, img.height)
            self._mode = img.pil_mode

            self.tile = [(self.format, (0, 0) + self.size, 0, None)]

    @classmethod
    def _save(cls, im, fp, format=None, **params):
        """
        Saves an image to a file pointer

        This function is used to register formats with PIL and should not be called manually.

        :param im: The image to save
        :param fp: The file pointer
        :param format: The format to save with (defaults to the image's known format) (unused)
        :param params: Additional encoder parameters (empty)
        """

        ImageFile._save(im, fp, [(cls.format, (0, 0) + im.size, 0, im.mode)])


class TIDecoder(ImageFile.PyDecoder):
    """
    Base class for PIL plugin decoders
    """

    def decode(self, buffer):
        """
        Decodes an input buffer and sets the image to its contents

        :param buffer: The input bytestream
        :return: The number of bytes consumed and the error code (``-1, 0`` on success)
        """

        var = TIVar()
        var.load_bytes(buffer)
        self.set_as_raw(np.asarray(var.entries[0].array(), dtype=np.uint8))

        return -1, 0


class TIEncoder(ImageFile.PyEncoder):
    """
    Base class for PIL plugin encoders
    """

    _pushes_fd = True

    _T = PictureEntry

    def encode(self, bufsize):
        """
        Encodes the image to an output bytestream

        :param bufsize: The size of the output buffer (unused)
        :return: The length of the output, the error code (``0`` on success), and the output
        """

        img = self._T()
        img.load_array(np.asarray(self.im).reshape(img.np_shape).tolist())
        data = img.export().bytes()

        return len(data), 0, data
