"""
The fundamental flash file components
"""


from io import BytesIO
from sys import version_info
from typing import BinaryIO
from warnings import warn

from .data import *
from .flags import *
from .models import *
from .numeric import BCD


match version_info[:2]:
    case 3, 10:
        Self = 'TIFlashHeader'

    case _:
        from typing import Self


class DeviceType(Enum):
    """
    Enum of flash device types
    """

    TI_83P = 0x73
    TI_73 = 0x74
    TI_92 = 0x88
    TI_89 = 0x98

    _all = [TI_83P, TI_73, TI_92, TI_89]
    DEVICES = _all


class BCDDate(Converter):
    """
    Converter for dates stored in four byte BCD

    A date (dd, mm, yyyy) is stored in BCD as ``ddmmyyyy``.
    """

    _T = tuple[int, int, int]

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``tuple[int, int, int]``

        :param data: The raw bytes to convert
        :return: The date stored in ``data``
        """

        return BCD.get(data[0:1]), BCD.get(data[1:2]), BCD.get(data[2:4])

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``tuple[int, int, int]`` -> ``bytes``

        :param value: The value to convert
        :return: The BCD encoding of the date in ``value``
        """

        return BCD.set(value[0] * 100 ** 3 + value[1] * 100 ** 2 + value[2], **kwargs)


class BCDRevision(Converter):
    """
    Converter for revision numbers stored in two byte BCD

    A revision xx.yy is stored in BCD as ``xxyy``.
    """

    _T = str

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``str``

        :param data: The raw bytes to convert
        :return: The revision number stored in ``data``
        """

        return f"{BCD.get(data[0:1])}.{BCD.get(data[1:2])}"

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``str`` -> ``bytes``

        :param value: The value to convert
        :return: The BCD encoding of the revision number in ``value``
        """

        major, minor = value.split(".")
        return BCD.set(100 * int(major) + int(minor), **kwargs)


class FlashDevices(Converter):
    """
    Converter for the device field of a flash header

    The device field contains at least one device type and type ID pair (xx, yy), stored as ``xxyy``.
    A flash header usually contains only has one pair in this field; the remainder of the field is null-padded.
    The exception is a `TILicense`, which can hold licenses for multiple devices.
    """

    _T = list[tuple[int, int]]

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``list[tuple[int, int]]``

        :param data: The raw bytes to convert
        :return: The device tuples stored in ``data``
        """

        return [*zip(*[iter(data)] * 2)]

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``list[tuple[int, int]]`` -> ``bytes``

        :param value: The value to convert
        :return: The device field derived from ``value``
        """

        return bytes([item for pair in value for item in pair])


class TIFlashBlock(Dock):
    """
    Parser for Intel blocks

    The data section of a flash header with ``binary_flag == $01`` is composed of blocks stored in the Intel format.
    Each block contains some segment of data stored at an address, which may be relative or absolute.
    """

    class Raw:
        """
        Raw bytes container for `TIFlashBlock`

        Any class with a distinct byte format requires its own `Raw` class to contain its data sections.
        Each data section must have a corresponding slot in `Raw` in order to use `Converter` classes.

        The `Raw` class must also contain a `bytes()` method specifying the order and visibility of the data sections.
        Additional methods can also be included, but should be callable from the outer class.
        """

        __slots__ = "address", "block_type", "data"

        @property
        def checksum(self) -> bytes:
            """
            The checksum for this block

            This is equal to the lower byte of the sum of all bytes in this block.
            """

            record = self.size + self.address + self.block_type + self.data
            return f"{-sum(bytes.fromhex(record.decode())) & 0xFF:02X}".encode()

        @property
        def size(self) -> bytes:
            """
            The size of this block's data in characters
            """

            return f"{len(self.data) // 2:02X}".encode()

        def bytes(self) -> bytes:
            """
            :return: The bytes contained in this block
            """

            return b':' + self.size + self.address + self.block_type + self.data + self.checksum

    def __init__(self, init=None, *,
                 address: bytes = b'0000', block_type: bytes = b'00',
                 data: bytes = b''):
        """
        Creates an empty flash data block

        :param init: Values to initialize this block's data (defaults to ``None``)
        :param address: The address of this block (defaults to ``$0000``)
        :param block_type: The type of this block (defaults to ``$00``, data)
        :param data: This block's data (defaults to empty)
        """

        self.raw = self.Raw()

        self.address = address
        self.block_type = block_type

        if data:
            self.data = bytearray(data)

        elif init is not None:
            if hasattr(init, "bytes"):
                self.load_bytes(init.bytes())

            else:
                self.load(init)

    @property
    def size(self) -> int:
        """
        The size of the block data in characters
        """

        return int(self.raw.size.decode(), 16)

    @Section(4, Bytes)
    def address(self) -> bytes:
        """
        The address of the block
        """

    @Section(2, Bytes)
    def block_type(self) -> bytes:
        """
        The type of the block
        """

    @Section()
    def data(self) -> bytes:
        """
        The data in the block
        """

    @property
    def checksum(self) -> bytes:
        """
        The checksum for the flash block

        This is equal to the lower byte of the sum of all bytes in the block.
        The checksum is not present in end blocks.
        """

        return self.raw.checksum

    @Loader[bytes, bytearray, BytesIO]
    def load_bytes(self, data: bytes | BytesIO):
        """
        Loads a byte string or bytestream into this block

        :param data: The bytes to load
        """

        if hasattr(data, "read"):
            data = BytesIO(data.read())

        else:
            data = BytesIO(data)

        data.seek(1)
        size = 2 * int(data.read(2).decode(), 16)
        self.raw.address = data.read(4)

        # Read type
        self.raw.block_type = data.read(2)

        if self.block_type not in [b'00', b'01', b'02']:
            warn(f"The block type ({self.block_type}) is not recognized.",
                 BytesWarning)

        self.raw.data = data.read(size)
        if len(self.raw.data) != size:
            warn(f"The block data size is incorrect (expected {size}, got {len(self.raw.data)}.",
                 BytesWarning)

        # Check² sum
        checksum = data.read(2)

        if checksum != self.checksum:
            warn(f"The checksum is incorrect (expected {self.checksum}, got {checksum}).",
                 BytesWarning)

    def bytes(self) -> bytes:
        """
        :return: The bytes contained in this block
        """

        return self.raw.bytes()


class FlashData(Converter):
    """
    Converter to split flash data into blocks if stored in Intel format

    If ``binary_flag != $01``, this converter is a no-op on ``bytes``.
    Otherwise, this converter manipulates ``list[TIFlashBlock]``.
    """

    _T = bytes | list[TIFlashBlock]

    @classmethod
    def get(cls, data: bytes, *, instance=None) -> _T:
        """
        Converts ``bytes`` -> ``bytes | list[TIFlashBlock]``

        :param data: The raw bytes to convert
        :param instance: The instance which contains the data section
        :return: The blocks stored in ``data``
        """

        if instance is None or instance.binary_flag == 0x01:
            return list(map(TIFlashBlock, data.split(b'\r\n')))

        else:
            return data

    @classmethod
    def set(cls, value: _T, *, instance=None, **kwargs) -> bytes:
        """
        Converts ``bytes | list[TIFlashBlock]`` -> ``bytes``

        :param value: The value to convert
        :param instance: The instance which contains the data section
        :return: The concatenation of the blocks in ``value``
        """

        if instance is None or instance.binary_flag == 0x01:
            return b'\r\n'.join(block.bytes() for block in value)

        else:
            return value


class TIFlashHeader(Dock):
    """
    Parser for flash headers

    A flash file can contain up to three headers, though usually only one.
    """

    extensions = {None: "8ek"}
    """
    The file extension used for this header per-model
    """

    _type_id = None
    _type_ids = {}

    class Raw:
        """
        Raw bytes container for `TIFlashHeader`

        Any class with a distinct byte format requires its own `Raw` class to contain its data sections.
        Each data section must have a corresponding slot in `Raw` in order to use `Converter` classes.

        The `Raw` class must also contain a `bytes()` method specifying the order and visibility of the data sections.
        Additional methods can also be included, but should be callable from the outer class.
        """

        __slots__ = "magic", "revision", "binary_flag", "object_type", "date", "name", "devices", "product_id", \
            "calc_data"

        @property
        def calc_data_size(self) -> bytes:
            """
            The length of the data stored in this header, measured in chars
            """

            return int.to_bytes(len(self.calc_data), 4, 'little')

        @property
        def checksum(self) -> bytes:
            """
            The checksum for this header, which may not exist

            This is equal to the lower 2 bytes of the sum of all bytes in this header.
            """

            return int.to_bytes(sum(self.calc_data) & 0xFFFF, 2, 'little')

        @property
        def name_length(self) -> bytes:
            """
            The length of the name or basecode attached to this header
            """

            return bytes([len(self.name.rstrip(b'\x00'))])

        def bytes(self) -> bytes:
            """
            :return: The bytes contained in this header
            """

            return self.magic + self.revision + self.binary_flag + self.object_type + self.date + \
                self.name_length + self.name + bytes(23) + self.devices + bytes(23) + self.product_id + \
                self.calc_data_size + self.calc_data + self.checksum

    def __init__(self, init=None, *,
                 magic: str = "**TIFL**", revision: str = "0.0", binary_format: bool = False, object_type: int = 0x88,
                 date: tuple[int, int, int] = (0, 0, 0), name: str = "UNNAMED",
                 device_type: int = 0x73, product_id: int = 0x00,
                 data: bytes = b':00000001FF'):
        """
        Creates an empty flash header with specified meta and data values

        :param init: Values to initialize the header's data (defaults to ``None``)
        :param magic: File magic at the start of the header (defaults to ``**TIFL**``)
        :param revision: The header's revision number (defaults to ``0.0``)
        :param binary_format: Whether the header's data is stored in binary format (defaults to ``False``)
        :param object_type: The header's object type (defaults to ``$88``)
        :param date: The header's stored date as a tuple (dd, mm, yyyy) (defaults to null)
        :param name: The name of the headers (defaults to ``UNNAMED``)
        :param device_type: The device type of the header (defaults to ``$73``, the 83+ series)
        :param product_id: The targeted model's product ID (defaults to ``$00``)
        :param data: The header's data (defaults to empty)
        """

        self.raw = self.Raw()

        self.magic = magic
        self.revision = revision
        self.binary_flag = 0x00 if binary_format else 0x01
        self.object_type = object_type

        self.date = date
        self.name = name

        self.devices = [(device_type, self._type_id if self._type_id is not None else 0xFF)]
        self.product_id = product_id

        if data:
            self.calc_data = bytearray(data)

        elif init is not None:
            if hasattr(init, "bytes"):
                self.load_bytes(init.bytes())

            else:
                self.load(init)

        self._has_checksum = True

    def __init_subclass__(cls, /, register=False, override=None, **kwargs):
        super().__init_subclass__(**kwargs)

        if register:
            TIFlashHeader.register(cls, override)

    def __len__(self) -> int:
        """
        :return: The total length of this header's bytes
        """

        return 78 + self.calc_data_size + 2 * self._has_checksum

    @Section(8, String)
    def magic(self) -> str:
        """
        The file magic for the flash header
        """

    @Section(2, BCDRevision)
    def revision(self) -> int:
        """
        The revision of the flash header
        """

    @Section(1, Bits[:])
    def binary_flag(self) -> int:
        """
        Whether this flash header's data is in binary (0x00) or Intel (0x01) format
        """

    @Section(1, Bits[:])
    def object_type(self) -> int:
        """
        The object type of the flash header
        """

    @Section(4, BCDDate)
    def date(self) -> tuple[int, int, int]:
        """
        The date attached to the flash header as a 3-tuple
        """

    @property
    def name_length(self) -> int:
        """
        The length of the name or basecode attached to the flash header
        """

        return int.from_bytes(self.raw.name_length, 'little')

    @Section(31, String)
    def name(self) -> str:
        """
        The name or basecode attached to the flash header
        """

    @Section(None, FlashDevices)
    def devices(self) -> list[tuple[int, int]]:
        """
        The devices targeted by the flash header

        Each device is a (device_type, type_id) tuple. The type_id should be constant throughout.
        Only licenses may be expected to have more than one device.
        """

    @View(devices, DeviceType)[0:1]
    def device_type(self) -> int:
        """
        The (first) device targeted by the flash header
        """

    @View(devices, Bits[:])[1:2]
    def type_id(self) -> int:
        """
        The (first) type ID of the flash header
        """

    @Section(1, Bits[:])
    def product_id(self) -> int:
        """
        The product ID for the header

        While used to identify the model the var was created on, it has no actual functional ramifications.
        Furthermore, it does not constitute a 1-to-1 mapping to distinct models.
        """

    @property
    def calc_data_size(self) -> int:
        """
        The length of the data stored in the flash header, measured in chars
        """

        return int.from_bytes(self.raw.calc_data_size, 'little')

    @Section()
    def calc_data(self) -> bytes:
        """
        The data stored in the flash header
        """

    @View(calc_data, FlashData)[:]
    def data(self) -> bytes | list[TIFlashBlock]:
        """
        The data stored in the flash header as either raw binary or Intel blocks

        If ``binary_flag == $01``, the data is returned as ``list[TIFlashBlock]``.
        Otherwise, the data is returned as ``bytes``.
        """

    @property
    def checksum(self) -> bytes:
        """
        The checksum for the flash header

        This is equal to the lower 2 bytes of the sum of all bytes in the header.
        """

        return self.raw.checksum

    @classmethod
    def get_type(cls, type_id: int) -> type['TIFlashHeader'] | None:
        """
        Gets the subclass corresponding to a type ID if one is registered

        :param type_id: The type ID to search by
        :return: A subclass of `TIFlashHeader` with corresponding type ID or ``None``
        """

        return cls._type_ids.get(type_id, None)

    @staticmethod
    def next_header_length(stream: BinaryIO) -> int:
        """
        Helper function to determine the length of the next flash header in a bytestream

        :param stream: A bytestream
        :return: The length of the next header in the bytestream
        """

        stream.seek(74, 1)
        data_size = int.from_bytes(stream.read(4), 'little')

        stream.seek(data_size, 1)
        match remaining := stream.read(8):
            case b"":
                entry_length = 78 + data_size

            case b"**TIFL**":
                entry_length = 78 + data_size
                stream.seek(-8, 1)

            case _:
                entry_length = 78 + data_size + 2
                stream.seek(-len(remaining), 1)

        stream.seek(-78 - data_size, 1)
        return entry_length

    @classmethod
    def register(cls, var_type: type['TIFlashHeader'], override: int = None):
        """
        Registers a subtype with this class for coercion

        :param var_type: The `TIFlashHeader` subtype to register
        :param override: A type ID to use for registry that differs from that of the var type
        """

        cls._type_ids[var_type._type_id if override is None else override] = var_type

    def extension(self, model: TIModel = TI_84PCE) -> str:
        """
        Determines the header's file extension given a targeted model

        :param model: A model to target (defaults to ``TI_84PCE``)
        :return: The header's file extension for that model
        """

        if not model.has(TIFeature.Flash):
            warn(f"The {model} does not support flash files.",
                 UserWarning)

        extension = ""
        for min_model in reversed(TIModel.MODELS):
            if model in self.extensions and min_model <= model:
                extension = self.extensions[model]
                break

        if not extension:
            warn(f"The {model} does not support this var type.",
                 UserWarning)

            return self.extensions[None]

        return extension

    def filename(self, model: TIModel = TI_84PCE) -> str:
        """
        Determines the header's filename given a targeted model

        The filename is the concatenation of the header name and extension (see `TIFlashHeader.extension`).

        :param model: A model to target (defaults to ``TI_84PCE``)
        :return: The header's filename
        """

        return f"{self.name}.{self.extension(model)}"

    @Loader[bytes, bytearray, BytesIO]
    def load_bytes(self, data: bytes | BytesIO):
        """
        Loads a byte string or bytestream into this header

        :param data: The bytes to load
        """

        if hasattr(data, "read"):
            data = BytesIO(data.read())

        else:
            data = BytesIO(data)

        # Read magic
        self.raw.magic = data.read(8)

        if self.magic != "**TIFL**":
            warn(f"The header has signature '{self.magic}', expected '**TIFL**'.",
                 BytesWarning)

        self.raw.revision = data.read(2)
        self.raw.binary_flag = data.read(1)
        self.raw.object_type = data.read(1)

        self.raw.date = data.read(4)

        # Read name
        name_length = data.read(1)[0]
        self.raw.name = data.read(31).rstrip(b'\x00')

        if name_length != self.name_length:
            warn(f"The header name length ({name_length}) doesn't match the length of the name "
                 f"(|{self.name}| = {self.name_length}).",
                 BytesWarning)

        # Read types
        self.raw.devices = data.read(1)

        if self.device_type not in DeviceType.DEVICES:
            warn(f"The device type ({self.device_type}) is not recognized.",
                 BytesWarning)

        # Read and check type ID
        self.raw.devices += data.read(1)

        if self._type_id is not None and self.type_id != self._type_id:
            if subclass := TIFlashHeader.get_type(self.type_id):
                warn(f"The header type is incorrect (expected {type(self)}, got {subclass}).",
                     BytesWarning)

            else:
                warn(f"The header type is incorrect (expected {type(self)}, got an unknown type). "
                     f"Load the header into a TIFlashHeader instance if you don't know the header type.",
                     BytesWarning)

        device_bytes = data.read(23)
        self.raw.devices += device_bytes.rstrip(b'\x00')
        self.raw.product_id = data.read(1)

        # Read data
        data_size = int.from_bytes(data.read(4), 'little')
        self.raw.calc_data = data.read(data_size)

        if len(self.calc_data) != data_size:
            warn(f"The data section has an unexpected length (expected {data_size}, got {len(self.calc_data)}).",
                 BytesWarning)

        # Check² sum
        checksum = data.read(2)

        if checksum:
            if checksum != self.checksum:
                warn(f"The checksum is incorrect (expected {self.checksum}, got {checksum}).",
                     BytesWarning)

        else:
            self._has_checksum = False

        self.coerce()

    def bytes(self) -> bytes:
        """
        :return: The bytes contained in this header
        """

        return self.raw.bytes() if self._has_checksum else self.raw.bytes()[:-2]

    @Loader[BinaryIO]
    def load_from_file(self, file: BinaryIO, *, offset: int = 0):
        """
        Loads this header from a file given a file pointer and offset

        :param file: A binary file to read from
        :param offset: The offset of the header to read
        """

        # Seek to offset
        while offset:
            file.seek(self.next_header_length(file), 1)
            offset -= 1

        self.load_bytes(file.read(self.next_header_length(file)))

    @classmethod
    def open(cls, filename: str) -> Self:
        """
        Creates a new header from a file given a filename

        :param filename: A filename to open
        :return: The (first) header stored in the file
        """

        with open(filename, 'rb') as file:
            header = cls()
            header.load_bytes(file.read(cls.next_header_length(file)))

            if remaining := file.read():
                if remaining.startswith(b"**TIFL**"):
                    warn("The selected flash file contains multiple headers; only the first will be loaded. "
                         "Use load_from_file to select a particular header.",
                         UserWarning)

                else:
                    warn(f"The selected flash file contains unexpected additional data: {remaining}.",
                         BytesWarning)

        return header

    def save(self, filename: str = None, model: TIModel = TI_84PCE):
        """
        Saves this header to the current directory given a filename and targeted model

        :param filename: A filename to save to (defaults to the header's name and extension)
        :param model: A model to target (defaults to ``TI_84PCE``)
        """

        with open(filename or self.filename(model), 'wb+') as file:
            file.write(self.bytes())

    def coerce(self):
        """
        Coerces this header to a subclass if possible using the header's type ID

        Valid types must be registered to be considered for coercion.
        """

        if self._type_id is None:
            if subclass := self.get_type(self.type_id):
                self.__class__ = subclass
                self.coerce()

            elif self.type_id != 0xFF:
                warn(f"Type ID 0x{self.type_id:02x} is not recognized; header will not be coerced to a subclass.",
                     BytesWarning)

            else:
                warn("Type ID is 0xFF; no coercion will occur.",
                     UserWarning)


__all__ = ["DeviceType", "BCDDate", "BCDRevision", "TIFlashBlock", "TIFlashHeader"]
