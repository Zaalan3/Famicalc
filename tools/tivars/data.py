"""
Data interfaces for var objects

This module implements two primary means of data interface:

    -   The `Converter` system, which uses the descriptor protocol to treat var data sections as their canonical types.

        Each data section of a var, while stored as raw bytes, can be interpreted as some other useful type.
        Each `Converter` class implements a conversion to such a type, such as ``bytes`` <-> ``str`` using latin-1.
        A data section is declared as either a base `Section` or a `View` into another section and assigned a converter.
        The system allows a user to access data sections as regular variables without cumbersome getters or setters.

        See the `Converter` class and its children, and the `Section` and `View` classes for implementation details.

    -   The `Loader` system, which implements convenient object initialization using existing mutation methods.

        A `Dock` instance can declare `Loader` methods which accept some sets of input types, such as ``load_string``.
        Each loader is called by a generic ``load`` method should the input type be permissible for that loader.
        Var types call the generic loader during initialization should an initializer be passed.

        See the `Dock` and `Loader` classes for implementation details.
"""


import copy
import inspect

from collections.abc import Callable
from math import ceil
from typing import TypeVar
from warnings import warn


_T = TypeVar('_T')


class Converter:
    """
    Abstract base class for data section converters
    """

    _T = _T

    @classmethod
    def get(cls, data: bytes, *, instance=None) -> _T:
        """
        Converts ``bytes`` -> `_T`

        :param data: The raw bytes to convert
        :param instance: The instance which contains the data section
        :return: An instance of `_T`
        """

        raise NotImplementedError

    @classmethod
    def set(cls, value: _T, *, instance=None, length: int = None, current: bytes = None) -> bytes:
        """
        Converts  `_T` -> ``bytes``

        :param value: The value to convert
        :param instance: The instance which contains the data section
        :param length: The length of the data section
        :param current: The current value of the data section
        :return: A string of bytes
        """

        raise NotImplementedError


class Bytes(Converter):
    """
    No-op converter for data sections best interpreted as raw bytes
    """

    _T = bytes

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``bytes`` (no-op)

        :param data: The raw bytes to convert
        :return: The bytes in ``data``, unchanged
        """

        return bytes(data)

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``bytes`` -> ``bytes`` (no-op)

        :param value: The value to convert
        :return: The bytes in ``value``, unchanged
        """

        return bytes(value)


class Data(Bytes):
    """
    No-op converter for data sections with associated metadata

    The following metadata fields are automatically set by this converter:

            - Version
    """

    _T = bytes

    @classmethod
    def set(cls, value: _T, *, instance=None, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``bytes`` and updates metadata fields

        :param value: The value to convert
        :param instance: The instance which contains the data section
        :return: The bytes in ``value``, unchanged
        """

        if instance is not None:
            instance.version = instance.get_version(value)

        return super().set(value)


class SizedData(Data):
    """
    No-op converter for sized data sections with associated metadata

    The following metadata fields are automatically set by this converter:

            - Version
            - Length
    """

    _T = bytes

    @classmethod
    def set(cls, value: _T, *, instance=None, **kwargs) -> _T:
        if instance is not None:
            instance.length = len(value)

        return super().set(value, instance=instance)


class Boolean(Converter):
    """
    Converter for data sections best interpreted as boolean flags

    The data section is expected to have length one.
    """

    _T = bool

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``bool``, where any nonzero value is truthy

        :param data: The raw bytes to convert
        :return: Whether ``data`` is nonzero
        """

        return data != b'\x00'

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``bool`` -> ``bytes``, where ``b'\\x80'`` is truthy and ``b'\\x00'`` is falsy

        :param value: The value to convert
        :return: ``b'\\x80'`` if ``value`` is truthy else ``b'\\x00'``
        """

        return b'\x80' if value else b'\x00'


class Integer(Converter):
    """
    Converter for data sections best interpreted as integers

    Integers are always little-endian, unsigned, and at most two bytes.
    """

    _T = int

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts `bytes` -> ``int``

        :param data: The raw bytes to convert
        :return: The little-endian integer given by ``data``
        """

        return int.from_bytes(data, 'little')

    @classmethod
    def set(cls, value: _T, *, length: int = None, **kwargs) -> bytes:
        """
        Converts ``int`` -> ``bytes``

        :param value: The value to convert
        :param length: The length of the data section
        :return: The little-endian representation of ``value``
        """

        length = length if length is not None else 2
        try:
            return int.to_bytes(value, length, 'little')

        except OverflowError:
            raise OverflowError(f"{value} cannot fit in a section of width {length}")


class String(Converter):
    """
    Converter for data sections best interpreted as strings

    Strings are encoded in latin-1.
    """

    _T = str

    @classmethod
    def get(cls, data: bytes, **kwargs) -> _T:
        """
        Converts ``bytes`` -> ``str``

        :param data: The raw bytes to convert
        :return: The latin-1 decoding of ``data`` with trailing null bytes removed
        """

        return data.decode('latin-1').rstrip('\0')

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        """
        Converts ``str`` -> ``bytes``

        :param value: The value to convert
        :return: The latin-1 encoding of ``value``
        """

        return value.encode('latin-1')


class Bits:
    """
    Sliceable converter to extract and package a slice of bits within a byte

    Use like ``Bits[start:end:step]`` to view a slice of a byte.
    If the slice is empty, the entire byte will be targeted.
    """

    def __class_getitem__(cls, item: slice):
        indices = range(*item.indices(8))

        class BitSlice(Converter):
            """
            Converter to extract and package a slice of bits within a byte

            The data section is expected to have length one.
            """

            _T = int

            mask = sum(1 << i for i in indices)

            @classmethod
            def get(cls, data: bytes, **kwargs) -> _T:
                """
                Converts ``bytes`` -> ``int`` by concatenating bits in a slice

                :param data: The raw bytes to convert
                :return: The sliced bits in ``data`` joined without gaps as an integer
                """

                value = ""
                for index, bit in enumerate(f"{data[0]:08b}"[::-1]):
                    if index in indices:
                        value += bit

                return int(value[::-1], 2)

            @classmethod
            def set(cls, value: _T, *, current: bytes = None, **kwargs) -> bytes:
                """
                Converts ``int`` -> ``bytes`` by setting bits in a slice

                :param value: The value to convert
                :param current: The current value of the data section
                :return: The bytes in ``value`` fit into the section
                """

                data = 0
                bits = iter(f"{value % 256:08b}"[::-1])

                for index in range(8):
                    if index in indices:
                        data += int(next(bits)) << index

                    else:
                        data += current[0] & (1 << index)

                try:
                    while 1:
                        if next(bits) == "1":
                            warn(f"Value {value} has too many bits for this buffer.",
                                 BytesWarning)

                except StopIteration:
                    pass

                return bytes([data])

        return BitSlice


class Section:
    """
    Data section class which handles conversion between bytes and appropriate data types

    A data section is given by its length and type converter.
    Their primary function is to permit the user to read and write data sections as their natural data types.

    The sections are stored in the ``raw`` attribute of their class, which is an instance of a ``Raw`` container.
    A priori, a data section does not correspond to any one part of a var file.
    Individual sections are instead stitched together to yield a var file's contents via ``raw.bytes()``.

    Distinct data sections are entirely independent, which is useful for sections which may vary in length.
    To construct sections which point to a portion of another section, see the `View` class.

    Data sections can be declared by decorating methods:

    .. python::

        @Section(length, Converter)
        def data_section(self) -> _T:
            ...

    An optional second parameter can be passed, wherein the method is used as a pre-converter before `Converter.set`.
    """

    def __init__(self, length: int = None, converter: type[Converter] = None, *, class_attr: bool = False):
        """
        Define a new data section given a length and type converter

        :param length: The length of the section (defaults to ``None``, i.e. unbounded)
        :param converter: The type converter for the section (defaults to `Bytes`)
        :param class_attr: Whether the section should return a shadowed class attribute (defaults to ``False``)
        """

        self._converter = converter or Bytes
        self._get, self._set = self._converter.get, self._converter.set
        self._length = length
        self._class_attr = class_attr

    def __copy__(self) -> 'Section':
        cls = self.__class__
        new = cls.__new__(cls)
        new.__dict__.update(self.__dict__)
        return new

    def __deepcopy__(self, memo) -> 'Section':
        cls = self.__class__
        new = cls.__new__(cls)
        memo[id(self)] = new

        for k, v in self.__dict__.items():
            setattr(new, k, copy.deepcopy(v, memo))

        return new

    def __set_name__(self, owner, name: str):
        self._name = name

    def __get__(self, instance, owner: type = None) -> _T:
        if instance is None:
            return getattr(owner, f"_{self._name}") if self._class_attr else self

        try:
            return self._get(self._get_raw(instance), instance=instance)

        except IndexError:
            raise ValueError(f"data '{self._name}' is empty or missing")

    def __set__(self, instance, value: _T):
        setattr(instance.raw, self._name, self._set_raw(instance, value))

    def __call__(self, func: Callable) -> 'Section':
        new = copy.copy(self)
        new.__doc__ = func.__doc__

        signature = inspect.signature(func)
        match len(signature.parameters):
            case 1: pass
            case 2: new._set = lambda value, _set=self._set, *, instance=None, **kwargs:\
                _set(func(instance, value), instance=instance, **kwargs)
            case _: raise TypeError("Section and View function definitions can only take 1 or 2 parameters.")

        return new

    def _get_raw(self, instance) -> bytes:
        return getattr(instance.raw, self._name, None)

    def _set_raw(self, instance, value: _T) -> _T:
        value = self._set(value, instance=instance, length=self._length, current=self._get_raw(instance))

        if self._length is not None:
            if len(value) > self._length:
                warn(f"Value {value} is too wide for this buffer; truncating to {value[:self._length]}.",
                     BytesWarning)
                value = value[:self._length]

            value = value.ljust(self._length, b'\x00')

        return value

    @property
    def name(self) -> str:
        """
        :return: The ``Raw`` class attribute name that this section stores to
        """

        return self._name

    @property
    def length(self) -> int | None:
        """
        :return: The length of this section
        """

        return self._length


class View(Section):
    """
    Data view class which handles conversion between portions of data sections and appropriate data types

    A data view is given by its target data section, type converter, and indices within the target.
    Indices must be contiguous and forward-facing.

    Data views are effectively pointers to the data sections they view, converting data in and out as specified.
    Note that data views cannot target other data views; this is done for implementation simplicity.

    Data views can be declared by decorating methods:

    .. python::

        @View(section[slice], Converter)
        def data_view(self) -> _T:
            ...

    An optional second parameter can be passed, wherein the method is used as a pre-converter before `Converter.set`.
    """

    def __init__(self, target: Section, converter: type[Converter], indices: slice = slice(None)):
        """
        Define a new data view given a data section to watch, a type converter, and the portion of the section to view

        :param target: The data section to view
        :param converter: The type converter for the view (defaults to `Bytes`)
        :param indices: The slice of the data section to view (defaults to the entire section)
        """

        super().__init__(None, converter)

        self._target = target
        self._indices = indices

        if self._target.length is None:
            self._length = max(ceil(((self._indices.stop or 0) - (self._indices.start or 0))
                                    // (self._indices.step or 1)), 0)

            if (self._indices.step or 1) > 0:
                if (self._indices.start or 0) >= 0 and (self._indices.stop is None or self._indices.stop < 0):
                    self._length = None

            else:
                if (self._indices.stop or 0) >= 0 and (self._indices.start is None or self._indices.start < 0):
                    self._length = None

        else:
            self._length = len(range(*self._indices.indices(self._target.length)))

    def __set__(self, instance, value: _T):
        getattr(instance.raw, self._target.name)[self._indices] = self._set_raw(instance, value)

    def __getitem__(self, indices: slice) -> 'View':
        return self.__class__(self._target, self._converter, indices)

    def __index__(self) -> slice:
        return self.indices

    def _get_raw(self, instance) -> bytes:
        return getattr(instance.raw, self._target.name)[self._indices]

    @property
    def target(self) -> 'Section':
        return self._target

    @property
    def indices(self) -> slice:
        return self._indices


class Dock:
    """
    Base class to inherit to implement the loader system
    """

    loaders = {}

    def load(self, data):
        """
        Loads data into an instance by delegating to `Loader` methods based on the input's type

        :param data: Any type which the instance might accept
        """
        for loader_types, loader in self.loaders.items():
            if any(isinstance(data, loader_type) for loader_type in loader_types):
                try:
                    loader(self, data)
                    return

                except NotImplementedError:
                    continue

        raise TypeError(f"could not find valid loader for type {type(data)}")


class Loader:
    """
    Function decorator to identify methods as data loaders for `Dock` instances

    Specify the loader's accepted type(s) using brackets:

    .. python::

        @Loader[int]
        def load_int(self, data: int):
            ...
    """

    types = ()

    def __init__(self, func):
        self._func = func

    def __call__(self, *args, **kwargs):
        pass

    def __class_getitem__(cls, item: tuple[type, ...] | type) -> type:
        try:
            return type("Loader", (Loader,), {"types": tuple(item)})

        except TypeError:
            return type("Loader", (Loader,), {"types": (item,)})

    def __set_name__(self, owner, name: str):
        owner.loaders = owner.loaders | {self.types: self._func}
        setattr(owner, name, self._func)


__all__ = ["Section", "View", "Dock", "Loader",
           "Converter", "Bytes", "Data", "SizedData", "Boolean", "Integer", "String", "Bits"]
