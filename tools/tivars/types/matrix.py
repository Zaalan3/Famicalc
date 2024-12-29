"""
Matrices
"""


from collections.abc import Iterator, Sequence
from io import BytesIO
from warnings import warn

from tivars.data import *
from tivars.models import *
from tivars.var import TIEntry
from .real import RealEntry


class TIMatrix(TIEntry, register=True):
    """
    Parser for the matrix type

    A `TIMatrix` is a two-dimensional array of `RealEntry` elements.
    Exact types are supported, but complex numbers are not.
    """

    versions = [0x10, 0x0B, 0x00]

    extensions = {
        None: "8xm",
        TI_82: "82m",
        TI_83: "83m",
        TI_83P: "8xm"
    }

    min_data_length = 2

    leading_name_byte = b'\x5C'

    _type_id = 0x02

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "[A]",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def __format__(self, format_spec: str) -> str:
        if format_spec.endswith("t"):
            inner_sep, outer_sep = ",", ""

        else:
            inner_sep, outer_sep = ", ", ", "

        return "[" + outer_sep.join(f"[{inner_sep.join(format(entry, format_spec) for entry in row)}]"
                                    for row in self.matrix()) + "]"

    def __iter__(self) -> Iterator[RealEntry]:
        """
        :return: An iterator over this matrix's elements in row-major order
        """

        for row in self.matrix():
            for entry in row:
                yield entry

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[0:1]
    def width(self, value: int) -> int:
        """
        The number of columns in the matrix

        TI-OS imposes a limit of 99.
        Additionally, TI-OS imposes a limit of 400 total elements.
        """

        if value > 99:
            warn(f"The matrix is too wide ({value} > 99).",
                 UserWarning)

        if value * self.height > 400:
            warn(f"The matrix is too big ({value * self.height} > 400).",
                 UserWarning)

        return value

    @View(calc_data, Integer)[1:2]
    def height(self, value: int) -> int:
        """
        The number of rows in the matrix

        TI-OS imposes a limit of 99.
        Additionally, TI-OS imposes a limit of 400 total elements.
        """

        if value > 99:
            warn(f"The matrix is too tall ({value} > 99).",
                 UserWarning)

        if self.width * value > 400:
            warn(f"The matrix is too big ({self.width * value} > 400).",
                 UserWarning)

        return value

    @View(calc_data, Bytes)[2:]
    def data(self) -> bytes:
        pass

    @property
    def size(self) -> int:
        """
        :return: The number of elements in this matrix
        """

        return self.width * self.height

    def get_min_os(self, data: bytes = None) -> OsVersion:
        it = zip(*[iter(data or self.data)] * RealEntry.min_data_length)
        return max(map(RealEntry().get_min_os, it), default=OsVersions.INITIAL)

    def get_version(self, data: bytes = None) -> int:
        it = zip(*[iter(data or self.data)] * RealEntry.min_data_length)
        version = max(map(RealEntry().get_version, it), default=0x00)

        if version > 0x1B:
            return 0x10

        elif version == 0x1B:
            return 0x0B

        else:
            return 0x00

    def supported_by(self, model: TIModel) -> bool:
        return super().supported_by(model) and (self.get_version() <= 0x0B or model.has(TIFeature.ExactMath))

    @Loader[bytes, bytearray, BytesIO]
    def load_bytes(self, data: bytes | BytesIO):
        super().load_bytes(data)

        if self.calc_data_length // RealEntry.min_data_length != self.size:
            warn(f"The matrix has an unexpected size "
                 f"(expected {self.calc_data_length // RealEntry.min_data_length}, got {self.size}).",
                 BytesWarning)

    def load_data_section(self, data: BytesIO):
        width = int.from_bytes(width_byte := data.read(1), 'little')
        height = int.from_bytes(height_byte := data.read(1), 'little')
        self.raw.calc_data = bytearray(width_byte + height_byte + data.read(width * height))

    @Loader[Sequence]
    def load_matrix(self, matrix: Sequence[Sequence[RealEntry]]):
        """
        Loads a two-dimensional sequence into this matrix

        :param matrix: The matrix to load
        """

        if len({len(row) for row in matrix}) > 1:
            raise IndexError("matrix has uneven rows")

        self.width = len(matrix[0] if matrix else [])
        self.height = len(matrix)
        self.data = b''.join(entry.calc_data for row in matrix for entry in row)

    def matrix(self) -> list[list[RealEntry]]:
        """
        :return: A two-dimensional ``list`` of the elements in this matrix
        """

        it = zip(*[iter(self.data)] * RealEntry.min_data_length)
        return [[RealEntry(for_flash=self.meta_length > TIEntry.base_meta_length, data=data)
                 for data in row] for row in zip(*[it] * self.width)]

    @Loader[str]
    def load_string(self, string: str):
        matrix = []

        for string in ''.join(string.split())[1:-1].replace("],[", "][").split("]["):
            row = []
            for item in string.replace("[", "").replace("]", "").split(","):
                entry = RealEntry()
                entry.load_string(item)
                row.append(entry)

            matrix.append(row.copy())

        self.load_matrix(matrix)


__all__ = ["TIMatrix"]
