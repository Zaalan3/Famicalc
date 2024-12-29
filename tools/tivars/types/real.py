"""
Real numeric types
"""


import copy
import re

from decimal import Decimal, localcontext
from fractions import Fraction
from warnings import warn

from tivars.data import *
from tivars.models import *
from tivars.numeric import *
from tivars.var import TIEntry


class RealEntry(TIEntry):
    """
    Base class for real numeric types

    This class handles floating-point types as well as the exact formats for the TI-83PCE and other newer models.
    The format for these types varies and is handled by the `subtype_id`, the first six bits of the first data byte.

    Two `RealEntry` types are used to form a single `ComplexEntry` corresponding to a complex number.
    """

    _T = 'RealEntry'

    extensions = {
        None: "8xn",
        TI_82: "82n",
        TI_83: "83n",
        TI_83P: "8xn"
    }

    min_data_length = 9

    min_exponent = 0x00
    """
    The smallest allowed floating point exponent
    """

    is_exact = False
    """
    Whether this numeric type is exact
    """

    imag_subtype_id = None
    """
    The subtype ID this type receives if used as an imaginary part
    """

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "A",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def __float__(self) -> float:
        return self.float()

    def __format__(self, format_spec: str) -> str:
        match format_spec:
            case "":
                if self.decimal() == 0:
                    return "0"

                else:
                    return f"{self.decimal():.14f}".rstrip("0").rstrip(".")

            case _:
                if format_spec.endswith("t"):
                    spec = "" if format_spec == "t" else format_spec[:-1] + "f"
                    return squash(re.sub(r"(√\d*)", r"sqrt(\1)", replacer(format(self, spec), {"-": "~", "/": "n/d"})))

                try:
                    return format(self.decimal(), format_spec)

                except (TypeError, ValueError):
                    return super().__format__(format_spec)

    def __int__(self) -> int:
        return int(self.float())

    def __neg__(self) -> 'RealEntry':
        negated = copy.copy(self)
        negated.sign_bit = 1 - negated.sign_bit

        return negated

    @Section(min_data_length)
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Bits[0:6])[0:1]
    def subtype_id(self) -> bytes:
        """
        The subtype ID of the number

        Differentiates the exact radical and fraction formats.
        """

    @View(calc_data, Bits[6:7])[0:1]
    def graph_bit(self) -> int:
        """
        Whether the entry is used during graphing
        """

    @View(calc_data, Bits[7:8])[0:1]
    def sign_bit(self) -> int:
        """
        The sign bit for the number

        If this bit is used, the number is negative when set.
        """

    @property
    def subtype(self) -> type['RealEntry']:
        """
        :return: The subtype of this real number
        """

        return self.get_type(self.subtype_id)

    @property
    def sign(self) -> int:
        """
        :return: The sign of this real number
        """

        return -1 if self.sign_bit else 1

    def clear(self):
        super().clear()

        if self._type_id is not None:
            self.subtype_id = self._type_id

    def get_min_os(self, data: bytes = None) -> OsVersion:
        data = data or self.data

        match data[0]:
            case 0x18 | 0x19:
                return TI_84P.OS("2.53")

            case _:
                return OsVersions.INITIAL

    def supported_by(self, model: TIModel) -> bool:
        return super().supported_by(model) and (self.subtype_id <= 0x19 or model.has(TIFeature.ExactMath))

    @Loader[Decimal]
    def load_decimal(self, decimal: Decimal):
        """
        Loads a ``decimal`` into this real number

        :param decimal: The decimal to load
        """

        raise NotImplementedError

    def decimal(self) -> Decimal:
        """
        :return: A ``decimal`` object corresponding to this real number
        """

        raise NotImplementedError

    @Loader[float, int]
    def load_float(self, decimal: float):
        """
        Loads a ``float`` into this real number, upcasting as necessary

        :param decimal: The float to load
        """

        self.load_decimal(Decimal(decimal))

    def json_number(self) -> float | str:
        """
        Encoder for JSON implementations with potentially low precision

        :return: A ``float`` or ``str`` depending on whether this real number can be stored in a single-precision float
        """

        if len(str(number := self.float())) <= 6:
            return number

        else:
            return str(number)

    def float(self) -> float:
        """
        :return: The ``float`` corresponding to this real number
        """

        return float(self.decimal())

    def coerce(self):
        if self._type_id is None:
            self.type_id = self.subtype_id

            super().coerce()


class GraphRealEntry(RealEntry):
    """
    Warning converter for real numeric types supported by graph parameters

    Values used for plotting (e.g. Xmin) will behave unexpectedly if set to a radical or π type.
    """

    _T = 'RealEntry'

    @classmethod
    def set(cls, value: _T, **kwargs) -> bytes:
        if type(value) not in (TIReal, TIUndefinedReal, TIRealFraction):
            warn(f"Graph parameters cannot store {type(value)} values correctly.",
                 UserWarning)

        return super().set(value)


class TIReal(RealEntry, register=True):
    """
    Parser for real floating point values

    A `TIReal` has 8 exponent bits and 14 decimal mantissa digits.

    A `TIReal` entry can be used to form `TIComplex` or `TIComplexPi` complex numbers.
    """

    min_data_length = 9

    imag_subtype_id = 0x0C

    _type_id = 0x00

    @Section(min_data_length)
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[1:2]
    def exponent(self) -> int:
        """
        The exponent of the real number

        The exponent has a bias of 0x80.
        """

    @View(calc_data, BCD)[2:9]
    def mantissa(self) -> int:
        """
        The mantissa of the real number

        The mantissa is 14 digits stored in BCD format, two digits per byte.
        """

    @Loader[Decimal]
    def load_decimal(self, decimal: Decimal):
        self.load_string(str(decimal))

    def decimal(self) -> Decimal:
        with localcontext() as ctx:
            ctx.prec = 14
            decimal = Decimal(self.sign * self.mantissa)
            decimal *= Decimal(10) ** (self.exponent - 0x80 - 13)

        return decimal

    @Loader[str]
    def load_string(self, string: str):
        if not string:
            self.mantissa, self.exponent, self.sign_bit = 0, 0x80, False
            return

        # Normalize string
        string = replacer(squash(string).lower(), {"~": "-", "|e": "e"})

        if "e" not in string:
            string += "e0"

        if "." not in string:
            string = string.replace("e", ".e")

        neg = string.startswith("-")
        string = string.strip("+-")

        # Obtain integer and decimal parts
        number, exponent = string.split("e")
        integer, decimal = number.split(".")
        integer, decimal = integer or "0", decimal or "0"

        if int(integer) == int(decimal) == 0:
            self.mantissa, self.exponent, self.sign_bit = 0, 0x80, neg
            return

        # Adjust exponent to make integer mantissa
        exponent = int(exponent or "0")
        while not 0 < (value := int(integer)) < 10:
            if value == 0:
                integer, decimal = decimal[0], decimal[1:]
                exponent -= 1

            else:
                integer, decimal = integer[:-1], integer[-1] + decimal
                exponent += 1

        self.mantissa, self.exponent, self.sign_bit = int((integer + decimal).ljust(14, "0")[:14]), exponent + 0x80, neg


class TIUndefinedReal(TIReal, register=True):
    """
    Parser for undefined real values

    A `TIUndefinedReal` is precisely a `TIReal` but marked as undefined for use in initial sequence values
    """

    _type_id = 0x0E


class TIRealFraction(TIReal, register=True):
    """
    Parser for real fractions

    A `TIRealFraction` has 8 exponent bits and 14 decimal mantissa digits.
    However, unlike a `TIReal`, the floating point value is automatically converted to an exact fraction on-calc.

    A `TIRealFraction` can be used to form `TIComplexFraction`, `TIComplexPi`, or `TIComplexPiFraction` complex numbers.
    """

    versions = [0x06]

    min_exponent = 0x7C

    is_exact = True

    imag_subtype_id = 0x1B

    _type_id = 0x18

    def __format__(self, format_spec: str) -> str:
        match format_spec:
            case "" | "#":
                return format(self.fraction(), format_spec)

            case _:
                return super().__format__(format_spec)

    @Loader[Fraction]
    def load_fraction(self, fraction: Fraction):
        with localcontext() as ctx:
            ctx.prec = 14
            decimal = Decimal(fraction.numerator) / fraction.denominator

        super().load_string(str(decimal))

    def fraction(self) -> Fraction:
        return Fraction(self.decimal()).limit_denominator(10000)

    @Loader[str]
    def load_string(self, string: str):
        self.load_fraction(Fraction(squash(string)))


class TIRealRadical(RealEntry, register=True):
    r"""
    Parser for real radicals

    A `TIRealRadical` is an exact sum of two square roots with rational scalars.
    Specifically, a `TIRealRadical` can represent numbers of the form ``(± a√b ± c√d) / e``.
    All values are non-negative, with signs tracked separately. Additionally, ``b > d ≥ 0`` and ``e > 0``.

    Each value is given three nibbles of storage in BCD format.
    Sign information for each radical is stored in an additional nibble.

    A `TIRealRadical` can be used to form `TIComplexRadical` complex numbers.
    """

    flash_only = True

    versions = [0x10]

    min_data_length = 9

    imag_subtype_id = 0x1D

    is_exact = True

    _type_id = 0x1C

    def __format__(self, format_spec: str) -> str:
        def reduce(part):
            match [*part]:
                case ["0", "√", *_]:
                    return ""

                case [*_, "√", "0"]:
                    return ""

                case [*_, "√", "1"]:
                    return part[:-2]

                case ["1", "√", *_]:
                    return part[1:]

                case ["-1", "√", *_]:
                    return part[2:]

                case _:
                    return part

        match format_spec:
            case "":
                left = reduce(f"{self.signed_left_scalar}√{self.left_radicand}")
                right = reduce(f"{self.signed_right_scalar}√{self.right_radicand}")

                match left, right, self.denominator:
                    case "", "", _:
                        string = "0"

                    case "", _, 1:
                        string = right

                    case _, "", 1:
                        string = left

                    case "", _, _:
                        string = f"{right}/{self.denominator}"

                    case _, "", _:
                        string = f"{left}/{self.denominator}"

                    case _:
                        string = f"({left}+{right})/{self.denominator}"

                return string.replace("+-", "-")

            case "#":
                left = f"{self.signed_left_scalar}√{self.left_radicand}"
                right = f"{self.signed_right_scalar}√{self.right_radicand}"

                return f"({left}+{right})/{self.denominator}".replace("+-", "-")

            case _:
                return super().__format__(format_spec)

    @Section(min_data_length)
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Bits[4:8])[1:2]
    def sign_type(self) -> int:
        """
        The sign type of the real radical

        If the sign type is odd (first bit is set), the left scalar is negative.
        If the sign type is greater than one (second bit is set), the right scalar is negative.
        """

    @View(calc_data, LeftNibbleBCD)[1:3]
    def denominator(self) -> int:
        """
        The denominator of the real radical
        """

    @View(calc_data, RightNibbleBCD)[3:5]
    def right_scalar(self) -> int:
        """
        The right scalar of the real radical
        """

    @View(calc_data, LeftNibbleBCD)[4:6]
    def left_scalar(self) -> int:
        """
        The left scalar of the real radical
        """

    @View(calc_data, RightNibbleBCD)[6:8]
    def right_radicand(self) -> int:
        """
        The right radicand of the real radical
        """

    @View(calc_data, LeftNibbleBCD)[7:9]
    def left_radicand(self) -> int:
        """
        The left radicand of the real radical
        """

    @property
    def sign(self) -> int:
        return -1 if self.decimal() < 0 else 1

    @property
    def signed_left_scalar(self) -> int:
        """
        The left scalar of the real radical, with sign attached
        """

        return self.left_scalar * (-1 if self.sign_type & 1 else 1)

    @property
    def signed_right_scalar(self) -> int:
        """
        The right scalar of the real radical, with sign attached
        """

        return self.right_scalar * (-1 if self.sign_type & 2 else 1)

    @Loader[Decimal]
    def load_decimal(self, decimal: Decimal):
        raise NotImplementedError("cannot determine exact representation from decimal approximation")

    def decimal(self) -> Decimal:
        return (self.signed_left_scalar * Decimal(self.left_radicand).sqrt() +
                self.signed_right_scalar * Decimal(self.right_radicand).sqrt()) \
            / self.denominator

    @Loader[str]
    def load_string(self, string: str):
        if not string:
            self.sign_type, self.denominator = 0, 1
            self.left_scalar, self.left_radicand = 0, 0
            self.right_scalar, self.right_radicand = 0, 0

        # Normalize string
        string = replacer(squash(string), {"(": "", ")": "", "*": "", "~": "-", "sqrt": "√", "-": "+-"}).lstrip("+")

        if "." in string:
            raise ValueError("radical type only accepts integer components")

        if "/" not in string:
            string += "/1"

        if "√" not in string:
            string = "0√1+" + string
            string = string.replace("/", "√1/")

        if "+" not in string:
            string = string.replace("/", "+0√1/")

        # Obtain left component, right component, and denominator
        top, bot = string.split("/")
        left, right = top.split("+")

        if "√" not in left:
            left += "√1"

        if "√" not in right:
            right += "√1"

        # Obtain scalars and radicands
        left_scalar, left_radicand = left.split("√")
        right_scalar, right_radicand = right.split("√")

        left_scalar = int(left_scalar if left_scalar.rstrip("+-") else left_scalar + "1")
        left_radicand = int(left_radicand)
        right_scalar = int(right_scalar if right_scalar.rstrip("+-") else right_scalar + "1")
        right_radicand = int(right_radicand)

        # Normalize radicands
        if left_radicand < 0 or right_radicand < 0:
            raise ValueError("square roots cannot be negative")

        if left_radicand < right_radicand:
            left_scalar, right_scalar = right_scalar, left_scalar
            left_radicand, right_radicand = right_radicand, left_radicand

        elif left_radicand == right_radicand:
            left_scalar += right_scalar
            right_scalar = right_radicand = 0

        denominator = int(bot.lstrip("+"))

        if denominator == 0:
            raise ValueError("denominator must be nonzero")

        # Obtain sign type
        left_scalar, right_scalar = left_scalar * sign(denominator), right_scalar * sign(denominator)
        match sign(left_scalar), sign(right_scalar):
            case 1, -1:
                self.sign_type = 2
            case 1, _:
                self.sign_type = 0
            case -1, 1:
                self.sign_type = 1
            case -1, _:
                self.sign_type = 3

            case _:
                raise ValueError(f"invalid sign type {(sign(left_scalar), sign(right_scalar))}")

        self.denominator = abs(denominator)
        self.left_scalar, self.right_scalar = abs(left_scalar), abs(right_scalar)
        self.left_radicand, self.right_radicand = left_radicand, right_radicand


class TIRealPi(TIReal, register=True):
    """
    Parser for real integer multiples of π

    A `TIRealPi` is simply a `TIReal` with an implicit factor of π.

    A `TIRealPi` can be used to form `TIComplexPi` or `TIComplexPiFraction` complex numbers.
    """

    flash_only = True

    versions = [0x10]

    min_exponent = 0x7C

    is_exact = True

    imag_subtype_id = 0x1E

    _type_id = 0x20

    def __format__(self, format_spec: str) -> str:
        match format_spec:
            case "":
                if self.decimal() == 0:
                    return "0"

                else:
                    return f"{super().decimal():.14f}".rstrip("0").rstrip(".") + "π"

            case _:
                return super().__format__(format_spec)

    @Loader[Decimal]
    def load_decimal(self, decimal: Decimal):
        raise NotImplementedError("decimal loading is ambiguous for pi types")

    def decimal(self) -> Decimal:
        with localcontext() as ctx:
            ctx.prec = 14

            return super().decimal() * pi

    @Loader[str]
    def load_string(self, string: str):
        string = replacer(string, {"pi": "π", "*": ""})

        if "π" not in string:
            raise ValueError("value must be a multiple of π")

        super().load_string(string.strip("π"))


class TIRealPiFraction(TIRealPi, TIRealFraction, register=True):
    """
    Parser for real fractional multiples of π

    A `TIRealPiFraction` is simply a `TIRealFraction` with an implicit factor of π.

    A `TIRealPiFraction` can be used to form `TIComplexPiFraction` complex numbers.
    """

    flash_only = True

    imag_subtype_id = 0x1F

    _type_id = 0x21

    def __format__(self, format_spec: str) -> str:
        match format_spec:
            case "" | "#":
                return format(self.fraction(), format_spec).replace("/", "π/")

            case _:
                return super().__format__(format_spec)

    def fraction(self) -> Fraction:
        return Fraction(self.decimal() / pi).limit_denominator(10000)

    @Loader[str]
    def load_string(self, string: str):
        replacer(string, {"pi": "π", "*": ""})

        if string != "0" and "π" not in string:
            raise ValueError("value must be a fractional multiple of π")

        super(TIRealPi, self).load_string(string.replace("π", ""))


__all__ = ["TIReal", "TIUndefinedReal", "TIRealFraction", "TIRealRadical", "TIRealPi", "TIRealPiFraction",
           "RealEntry", "GraphRealEntry"]
