"""
Lists
"""


from collections.abc import Iterator, Sequence
from io import BytesIO
from warnings import warn

from tivars.data import *
from tivars.models import *
from tivars.tokenizer import *
from tivars.var import TIEntry
from .complex import ComplexEntry
from .real import RealEntry


class ListName(TokenizedString):
    """
    Converter for the name section of lists

    List names can be ``L1`` - ``L6`` or a string of five alphanumeric characters that do not start with a digit.
    The special name and token ``IDList`` is also used (but is planned to be relegated to a separate type).
    """

    _T = str

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``str`` as done by the memory viewer

        :param data: The raw bytes to convert
        :return: The list name contained in `data`
        """

        if data[0] == 0x5D:
            if data[1] < 6:
                return super().get(data)

            elif data[1] == 0x40:
                return "IDList"

            data = data[1:]

        return super().get(data)

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``str`` -> ``bytes`` to match appearance in the memory viewer

        :param value: The value to convert
        :return: The name encoding of ``value``
        """

        # TI-ASCII hack
        varname = value.upper().replace("]", "|L")

        if "IDList" in varname:
            return b'\x5D\x40'

        elif varname in ("L1", "L2", "L3", "L4", "L5", "L6"):
            return super().set(varname[:2])

        else:
            return super().set(varname[-5:])


class ListEntry(TIEntry):
    """
    Base class for all list entries

    A list entry is a one-dimensional array of `RealEntry` or `ComplexEntry` elements.
    Exact types are supported.
    """

    _E = TIEntry

    versions = [0x10, 0x0B, 0x00]

    min_data_length = 2

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "L1",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def __format__(self, format_spec: str) -> str:
        if format_spec.endswith("t"):
            return "{" + ",".join(format(entry, format_spec) for entry in self.list()) + "}"

        else:
            return "[" + ", ".join(format(entry, format_spec) for entry in self.list()) + "]"

    def __iter__(self) -> Iterator[_E]:
        return iter(self.list())

    @Section(8, ListName)
    def name(self) -> str:
        """
        The name of the entry

        Names must be 1 to 5 characters in length.
        The name can include any characters A-Z, 0-9, or θ.
        The name cannot start with a digit; for these lists, use ``L1`` - ``L6`` instead.
        """

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[0:2]
    def length(self, value) -> int:
        """
        The length of the list

        TI-OS imposes a limit of 999.
        """

        if value > 999:
            warn(f"The list is too long ({value} > 999).",
                 UserWarning)

        return value

    @View(calc_data, Bytes)[2:]
    def data(self) -> bytes:
        pass

    def get_min_os(self, data: bytes = None) -> OsVersion:
        it = zip(*[iter(data or self.data)] * self._E.min_data_length)
        return max(map(self._E().get_min_os, it), default=OsVersions.INITIAL)

    def get_version(self, data: bytes = None) -> int:
        it = zip(*[iter(data or self.data)] * self._E.min_data_length)
        version = max(map(self._E().get_version, it), default=0x00)

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

        if self.calc_data_length // self._E.min_data_length != self.length:
            warn(f"The list has an unexpected length "
                 f"(expected {self.calc_data_length // self._E.min_data_length}, got {self.length}).",
                 BytesWarning)

    @Loader[Sequence]
    def load_list(self, lst: Sequence[_E]):
        """
        Loads a sequence into this list

        :param lst: The list to load
        """

        self.length = len(lst)
        self.data = b''.join(entry.calc_data for entry in lst)

    def list(self) -> list[_E]:
        """
        :return: A ``list`` of the elements in this list
        """

        it = zip(*[iter(self.data)] * self._E.min_data_length)
        return [self._E(for_flash=self.meta_length > TIEntry.base_meta_length, data=data) for data in it]

    @Loader[str]
    def load_string(self, string: str):
        lst = []

        for string in ''.join(string.strip("[]{}").split()).split(","):
            entry = self._E()
            entry.load_string(string)
            lst.append(entry)

        self.load_list(lst)


class TIRealList(ListEntry, register=True):
    """
    Parser for lists of real numbers
    """

    _E = RealEntry

    extensions = {
        None: "8xl",
        TI_82: "82l",
        TI_83: "83l",
        TI_83P: "8xl",
    }

    _type_id = 0x01


class TIComplexList(ListEntry, register=True):
    """
    Parser for lists of complex numbers
    """

    _E = ComplexEntry

    extensions = {
        None: "8xl",
        TI_83: "83l",
        TI_83P: "8xl",
    }

    _type_id = 0x0D


__all__ = ["TIRealList", "TIComplexList"]
