"""
Groups
"""


from collections.abc import Sequence
from io import BytesIO
from warnings import warn

from tivars.data import *
from tivars.models import *
from tivars.var import TIEntry, SizedEntry
from .appvar import *
from .gdb import TIGraphedEquation
from .list import *
from .tokenized import *


class TIGroup(SizedEntry, register=True):
    """
    Parser for group objects

    A group is a collection of entries packaged together for easy transfer and saving in the archive.
    Each entry is stored with its entry in the VAT followed by its regular data.

    The VAT information can be safely ignored since it is redetermined when importing back onto a calculator.
    """

    _T = 'TIGroup'

    extensions = {
        None: "8xg",
        TI_82: "82g",
        TI_83: "83g",
        TI_83P: "8xg"
    }

    _type_id = 0x17

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "GROUP",
                 version: int = None, archived: bool = True,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    @staticmethod
    def group(entries: Sequence[TIEntry], *, name: str = "GROUP") -> 'TIGroup':
        """
        Creates a new `TIGroup` by packaging a sequence of entries using defaulted VAT data

        :param entries: The entries to group
        :param name: The name of the group (defaults to ``GROUP``)
        :return: A group containing ``entries``
        """

        if not entries:
            warn("Groups are expected to be non-empty.",
                 UserWarning)

            return TIGroup(name=name)

        elif len(entries) < 2:
            warn("Groups are expected to have at least two entries.",
                 UserWarning)

        group = TIGroup(for_flash=entries[0].meta_length > TIEntry.base_meta_length, name=name)

        for index, entry in enumerate(entries):
            name = entry.raw.name.rstrip(b'\x00')
            vat = bytearray([entry.type_id, 0, entry.version, 0, 0, entry.archived])

            if entry.archived:
                warn(f"Entry #{index} ({type(entry)}) is archived, which may lead to unexpected behavior on-calc.",
                     UserWarning)

            if isinstance(entry, TIGraphedEquation):
                vat[0] |= entry.raw.flags

            match entry.type_id:
                case TIProgram.type_id | TIProtectedProgram.type_id | TIAppVar.type_id | TIGroup.type_id:
                    vat += bytearray([len(name), *name])

                case TIRealList.type_id | TIComplexList.type_id:
                    vat += bytearray([len(name) + 1, *name, 0])

                case _:
                    vat += name.ljust(3, b'\x00')

            group.data += vat
            group.data += entry.calc_data

        return group

    def get_min_os(self, data: bytes = None) -> OsVersion:
        return max([entry.get_min_os() for entry in self.ungroup(data)], default=OsVersions.INITIAL)

    def get_version(self, data: bytes = None) -> int:
        return max([entry.get_version() for entry in self.ungroup(data)], default=0x00)

    def ungroup(self, data: bytes = None) -> list[TIEntry]:
        """
        Ungroups a group object into a ``list`` of its entries

        All VAT data is ignored.

        :param data: The data to ungroup (defaults to this group's data)
        :return: A ``list`` of entries stored in ``data``
        """

        data = BytesIO(data or self.data[:])
        entries = []

        index = 1
        while type_byte := data.read(1):
            _, version = data.read(2)

            match type_id := type_byte[0] & 63:
                case TIProgram.type_id | TIProtectedProgram.type_id | TIAppVar.type_id | TIGroup.type_id:
                    *_, page, length = data.read(4)

                    if length > 8:
                        warn(f"The name length of entry #{index} ({length}) exceeds eight.",
                             BytesWarning)

                    name = data.read(length)

                case TIRealList.type_id | TIComplexList.type_id:
                    *_, page, length = data.read(4)

                    if length > 7:
                        warn(f"The name length of entry #{index} ({length - 2}), a list, exceeds five.",
                             BytesWarning)

                    name = data.read(length - 1)
                    data.read(1)

                case _:
                    *_, page = data.read(3)
                    name = data.read(3)

            entry = TIEntry(for_flash=self.meta_length > TIEntry.base_meta_length, version=version, archived=page > 0)
            entry.type_id = type_id
            entry.coerce()

            entry.raw.name = name.ljust(8, b'\x00')
            entry.load_data_section(data)

            if isinstance(entry, TIGraphedEquation):
                entry.raw.flags = type_byte

            entries.append(entry)

        return entries

    @Loader[Sequence]
    def load_from_entries(self, entries: Sequence[TIEntry]):
        """
        Loads a sequence of entries into this group

        All VAT data is cleared.

        :param entries: The entries to group
        """

        self.data = self.group(entries).data
