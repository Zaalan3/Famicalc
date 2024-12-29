"""
GDBs and their settings
"""


import json
import os

from collections.abc import Iterator
from io import BytesIO
from warnings import catch_warnings, filterwarnings, warn

from tivars.flags import *
from tivars.data import *
from tivars.models import *
from tivars.tokenizer import decode
from tivars.var import TIEntry, SizedEntry
from .real import *
from .tokenized import TIEquation


class GraphMode(Flags):
    """
    Flags for GDB graph modes
    """

    Dot = {0: 1}
    Connected = {0: 0}
    Simul = {1: 1}
    Sequential = {1: 0}
    GridOn = {2: 1, 7: 0}
    GridOff = {2: 0, 7: 0}
    GridLine = {2: 1, 7: 1}
    GridDot = {2: 1, 7: 0}
    PolarGC = {3: 1}
    RectGC = {3: 0}
    CoordOn = {4: 0}
    CoordOff = {4: 1}
    AxesOff = {5: 1}
    AxesOn = {5: 0}
    LabelOn = {6: 1}
    LabelOff = {6: 0}

    ExprOff = {0: 1}
    ExprOn = {0: 0}
    SEQ_n = {1: 0, 2: 0}
    SEQ_np1 = {1: 1, 2: 0}
    SEQ_np2 = {1: 0, 2: 1}

    DetectAsymptotesOff = {0: 1}
    DetectAsymptotesOn = {0: 0}


class SeqMode(Flags):
    """
    Flags for sequential GDB plot modes
    """

    Time = {0: 0, 1: 0, 2: 0, 3: 0, 4: 0}
    Web = {0: 1, 1: 0, 2: 0, 3: 0, 4: 0}
    VertWeb = {0: 0, 1: 1, 2: 0, 3: 0, 4: 0}
    uv = {0: 0, 1: 0, 2: 1, 3: 0, 4: 0}
    vw = {0: 0, 1: 0, 2: 0, 3: 1, 4: 0}
    uw = {0: 0, 1: 0, 2: 0, 3: 0, 4: 1}


class GraphStyle(Enum):
    """
    Enum of GDB graph styles
    """

    SolidLine = 0x00
    ThickLine = 0x01
    ShadeAbove = 0x02
    ShadeBelow = 0x03
    Trace = 0x04
    Animate = 0x05
    DottedLine = 0x06

    _all = [SolidLine, ThickLine, ShadeAbove, ShadeBelow, Trace, Animate, DottedLine]
    STYLES = _all[:7]


class GraphColor(Enum):
    """
    Enum of GDB graph colors
    """

    Mono = 0x00
    Blue = 0x01
    Red = 0x02
    Black = 0x03
    Magenta = 0x04
    Green = 0x05
    Orange = 0x06
    Brown = 0x07
    Navy = 0x08
    LtBlue = 0x09
    Yellow = 0x0A
    White = 0x0B
    LtGray = 0x0C
    MedGray = 0x0D
    Gray = 0x0E
    DarkGray = 0x0F

    _all = [Mono, Blue, Red, Black, Magenta, Green, Orange, Brown, Navy,
            LtBlue, Yellow, White, LtGray, MedGray, Gray, DarkGray]
    COLORS = _all[1:]


class GlobalLineStyle(Enum):
    """
    Enum of global GDB line styles
    """

    Thick = 0x00
    DotThick = 0x01
    Thin = 0x02
    DotThin = 0x03

    _all = [Thick, Thin, DotThick, DotThin]
    STYLES = _all


class BorderColor(Enum):
    """
    Enum of GDB graph border colors
    """

    LtGray = 0x01
    Teal = 0x02
    LtBlue = 0x03
    White = 0x04

    _all = [LtGray, Teal, LtBlue, White]
    COLORS = _all


class EquationFlags(Flags):
    """
    Flags for equations stored in GDBs
    """

    Selected = {5: 1}
    Deselected = {5: 0}
    UsedForGraph = {6: 1}
    UnusedForGraph = {6: 0}
    LinkTransferSet = {7: 1}
    LinkTransferClear = {7: 0}


class TIGraphedEquation(TIEquation, register=True, override=0x23):
    """
    Parser for equations stored in GDBs

    A `TIGraphedEquation` is a `TIEquation` with an extra leading flag byte.
    The equation's style and color in its GDB is bundled for convenience.
    """

    min_data_length = 3

    class Raw(TIEntry.Raw):
        """
        Raw bytes container for `TIGraphedEquation`

        The ``Raw`` container adds ``style`` and ``color`` fields for convenient manipulation of graphed equations.
        These fields are stored in the container but do not contribute to the equation data section of a GDB.
        """

        __slots__ = "meta_length", "type_id", "name", "version", "archived", "style", "color", "calc_data"

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "Y1",
                 version: int = None, archived: bool = None,
                 data: bytes = None):
        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

        self.flags = EquationFlags({0: 1, 1: 1})
        self.style = 0x00
        self.color = 0x00

    def __class_getitem__(cls, index: int):
        index -= 1

        class IndexedEquationConverter(Converter):
            """
            Converter for equations within a GDB

            Since equations are stored contiguously within a GDB, their exact location is not static.
            This converter interfaces with the nth equation by counting up from the start of the data section.

            The ``n``th equation in a GDB is interfaced by the converter ``TIGraphedEquation[n]``.
            """

            _T = TIGraphedEquation

            @classmethod
            def get(cls, data: bytes, *, instance=None) -> _T:
                """
                Converts ``bytes`` -> `TIGraphedEquation` by finding the equation at ``index`` within a GDB

                :param data: The raw bytes to convert
                :param instance: The instance which contains the data section
                :return: The bytes in ``data``, unchanged
                """

                return instance.equations[index]

            @classmethod
            def set(cls, value: _T, *, instance=None, **kwargs) -> bytes:
                """
                Converts ``bytes`` -> `TIGraphedEquation` by modifying the equation at ``index`` within a GDB

                :param value: The value to convert
                :param instance: The instance which contains the data section
                :return: The bytes in ``value``, unchanged
                """

                # Set appropriate equation
                equations = list(instance.equations)
                equations[index] = value

                # Set styles
                data = instance.raw.calc_data[:instance.offset]
                for i in range(0, instance.num_equations, instance.num_equations // instance.num_styles):
                    data += equations[i].raw.style

                # Set data
                data += b''.join(equation.raw.calc_data for equation in equations)

                # Set colors (if they exist)
                if color := instance.get_color_data():
                    data += b'84C'
                    for i in range(0, instance.num_equations, instance.num_equations // instance.num_styles):
                        data += equations[i].raw.color

                    data += color[-5:]

                return data

        return IndexedEquationConverter

    def __iter__(self) -> Iterator:
        return iter(self.dict().items())

    @Section(1, GraphStyle)
    def style(self) -> bytes:
        """
        The style of the GDB equation

        This value is not intrinsically stored with the equation, but is bundled for convenience.
        """

    @Section(1, GraphColor)
    def color(self) -> bytes:
        """
        The color of the GDB equation

        This value is not intrinsically stored with the equation, but is bundled for convenience.
        """

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, EquationFlags)[0:1]
    def flags(self) -> EquationFlags:
        """
        The flags for the GDB equation

        The flags track whether the equation is selected, used for graphing, or is participating in a link transfer.
        """

    @View(calc_data, Integer)[1:3]
    def length(self) -> int:
        """
        The length of this entry's user data section
        """

    @property
    def json_name(self) -> str:
        """
        :return: The name of this equation used in the GDB JSON format
        """

        return decode(self.raw.name, mode="accessible")[0].strip("{}|")

    def load_data_section(self, data: BytesIO):
        flag_byte = data.read(1)
        data_length = int.from_bytes(length_bytes := data.read(2), 'little')
        self.raw.calc_data = bytearray(flag_byte + length_bytes + data.read(data_length))

    @Loader[dict]
    def load_dict(self, dct: dict):
        """
        Loads a JSON ``dict`` into this GDB equation

        :param dct: The dict to load
        """

        self.style = getattr(GraphStyle, dct["style"])
        self.color = getattr(GraphColor, dct["color"])

        self.load_equation(TIEquation(dct["expr"]))

        # Set flags
        flags = {"selected": False, "wasUsedForGraph": False, "linkTransfer": False} | dct["flags"]
        self.flags = EquationFlags({1: 1, 0: 1})

        self.flags |= EquationFlags.Selected if flags["selected"] else EquationFlags.Deselected
        self.flags |= EquationFlags.UsedForGraph if flags["wasUsedForGraph"] else EquationFlags.UnusedForGraph
        self.flags |= EquationFlags.LinkTransferSet if flags["linkTransfer"] else EquationFlags.LinkTransferClear

    def dict(self) -> dict:
        """
        :return: A ``dict`` representing this GDB equation in JSON format
        """

        dct = {"style": GraphStyle.get_name(self.style)}

        if self.color != GraphColor.Mono:
            dct["color"] = GraphColor.get_name(self.color)

        return dct | {
            "flags": {
                "selected": EquationFlags.Selected in self.flags,
                "wasUsedForGraph": EquationFlags.UsedForGraph in self.flags,
                "linkTransfer": EquationFlags.LinkTransferSet in self.flags
            },
            "expr": self.string()
        }

    @Loader[TIEquation]
    def load_equation(self, equation: TIEquation):
        """
        Loads a `TIEquation` into this GDB equation

        :param equation: The equation to load
        """

        self.raw.calc_data[1:] = equation.calc_data

    def equation(self) -> TIEquation:
        """
        :return: The `TIEquation` component of this GDB equation
        """

        # Kinda yucky ngl
        with catch_warnings():
            filterwarnings("ignore", "The data section length is incorrect")
            return TIEquation(self.bytes()[:-self.calc_data_length] + self.bytes()[-self.calc_data_length + 1:])

    @Loader[str]
    def load_string(self, string: str, *, model: TIModel = None):
        equation = TIEquation()
        equation.load_string(string, model=model)
        self.load_equation(equation)

    def coerce(self):
        if self.type_id == 0x23:
            self.flags |= EquationFlags.Selected
            self.type_id = 0x03


class TIMonoGDB(SizedEntry, register=True):
    """
    Base class for all GDB entries

    A GDB is a collection of equations and graph settings representing the state of one of the equation plotters.
    A GDB can correspond to one of the Function, Parametric, Polar, or Sequence plotting modes.
    GDBs for monochrome models are truncations of those for color models.
    """

    extensions = {
        None: "8xd",
        TI_82: "82d",
        TI_83: "83d",
        TI_83P: "8xd"
    }

    min_data_length = 61

    leading_name_byte = b'\x61'

    mode_byte = 0x00
    """
    The byte which identifies the GDB type
    """

    has_color = False
    """
    Whether this GDB type carries color information
    """

    num_equations = 0
    """
    The number of equations contained in this GDB type
    """

    num_parameters = 0
    """
    The number of graph parameters contained in this GDB type
    """

    num_styles = 0
    """
    The number of equation styles contained in this GDB type
    """

    equation_names = []
    """
    The names of the equations in this GDB type
    """

    _type_id = 0x08

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "GDB1",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    def __format__(self, format_spec: str) -> str:
        match format_spec:
            case "":
                return json.dumps(self.dict())

            case _:
                return super().__format__(format_spec)

    def __iter__(self) -> Iterator:
        return iter(self.dict().items())

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[3:4]
    def mode_id(self) -> int:
        """
        The mode ID for the GDB

        The ID is one of ``0x10`` (Function), ``0x20`` (Polar), ``0x40`` (Parametric), or ``0x80`` (Sequence).
        """

    @View(calc_data, GraphMode)[4:5]
    def mode_flags(self) -> GraphMode:
        """
        The flags for the mode settings

        Dot/Connected, Simul/Sequential, GridOn/Line/Dot/Off, PolarGC/RectGC, CoordOn/Off, AxesOff/On, and LabelOn/Off
        """

    @View(calc_data, GraphMode)[6:7]
    def extended_mode_flags(self) -> GraphMode:
        """
        The flags for the extended mode settings

        ExprOn/Off and sequence plot offsets for sequence mode
        """

    @View(calc_data, GraphRealEntry)[7:16]
    def Xmin(self) -> GraphRealEntry:
        """
        Xmin: the X-coordinate of the left edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[16:25]
    def Xmax(self) -> GraphRealEntry:
        """
        Xmax: the X-coordinate of the right edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[25:34]
    def Xscl(self) -> GraphRealEntry:
        """
        Xscl: the separation between ticks on the X-axis
        """

    @View(calc_data, GraphRealEntry)[34:43]
    def Ymin(self) -> GraphRealEntry:
        """
        Ymin: the Y-coordinate of the bottom edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[43:52]
    def Ymax(self) -> GraphRealEntry:
        """
        Ymax: the Y-coordinate of the top edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[52:61]
    def Yscl(self) -> GraphRealEntry:
        """
        Yscl: the separation between ticks on the Y-axis
        """

    @property
    def mode(self) -> str:
        """
        The mode for the GDB

        One of Function, Parametric, Polar, or Sequence
        """

        match self.mode_id:
            case TIMonoFuncGDB.mode_byte:
                return 'Function'
            case TIMonoParamGDB.mode_byte:
                return 'Parametric'
            case TIMonoPolarGDB.mode_byte:
                return 'Polar'
            case TIMonoSeqGDB.mode_byte:
                return 'Sequence'

            case _:
                warn(f"Graphing mode byte 0x{self.mode_id:x} not recognized.",
                     BytesWarning)

    @property
    def offset(self) -> int:
        """
        :return: The index of the start of the equation styles in this GDB
        """

        return TIMonoGDB.min_data_length + GraphRealEntry.min_data_length * self.num_parameters

    @property
    def equations(self) -> tuple[TIGraphedEquation, ...]:
        """
        :return: This GDB's stored equations
        """

        return self.get_equations()

    def get_color_data(self, data: bytes = None) -> bytes:
        """
        Finds the color portion of a GDB if it exists

        :param data: The data to find the color portion of (defaults to this GDB's data)
        :return: The color portion of ``data``, which may be empty
        """

        data = BytesIO(data or self.calc_data[self.offset + self.num_styles:])
        temp = TIGraphedEquation()
        for i in range(self.num_equations):
            temp.load_data_section(data)

        return data.read()

    def get_equations(self, data: bytes = None) -> tuple[TIGraphedEquation, ...]:
        """
        Extracts the equations stored in a GDB into a ``tuple``

        :param data: The data to extract equations from (defaults to this GDB's data)
        :return: A ``tuple`` of equations stored in ``data``
        """

        data = BytesIO(data or self.calc_data[self.offset:])
        equations = tuple(TIGraphedEquation(name=name) for name in self.equation_names)

        # Load styles
        for i in range(self.num_styles):
            style = data.read(1)
            for j in range(r := self.num_equations // self.num_styles):
                equations[r * i + j].raw.style = style

        # Load data sections
        for i in range(self.num_equations):
            equations[i].load_data_section(data)

        # Load colors (if they exist)
        if rest := data.read():
            data = BytesIO(rest)
            data.seek(3, 1)

            for i in range(self.num_styles):
                color = data.read(1)
                for j in range(r := self.num_equations // self.num_styles):
                    equations[r * i + j].raw.color = color

        return equations

    def get_min_os(self, data: bytes = None) -> OsVersion:
        return max([eq.get_min_os() for eq in self.get_equations(data)], default=OsVersions.INITIAL)

    def get_version(self, data: bytes = None) -> int:
        return max([eq.get_version() for eq in self.get_equations(data)], default=0x00)

    @Loader[dict]
    def load_dict(self, dct: dict):
        """
        Loads a JSON ``dict`` into this GDB

        :param dct: The dict to load
        """

        self.clear()
        self.raw.calc_data[3] = {
            'Function': 0x10,
            'Parametric': 0x40,
            'Polar': 0x20,
            'Sequence': 0x80
        }.get(mode := dct.get("graphMode", "Function"), 0x00)

        # Load formatSettings
        for setting in dct.get("formatSettings", []):
            try:
                self.mode_flags |= getattr(GraphMode, setting)

            except AttributeError:
                warn(f"Unrecognized format setting ({setting}).",
                     UserWarning)

        # Load extSettings
        ext_settings = dct.get("extSettings", {})
        if "showExpr" in ext_settings:
            self.extended_mode_flags |= GraphMode.ExprOn if ext_settings["showExpr"] else GraphMode.ExprOff

        if self.raw.calc_data[3] != 0x80:
            if "seqMode" in ext_settings or "seqSettings" in dct:
                warn(f"Sequence settings have been provided, but this GDB is for {mode.lower()} graphs.",
                     UserWarning)

        # Load globalWindowSettings
        for var, value in dct.get("globalWindowSettings", {}).items():
            if not hasattr(self, var):
                warn(f"Unrecognized window setting ({var}).",
                     UserWarning)
            else:
                setattr(self, var, TIReal(value))

        # Load specific data
        data = dct.get("specificData", {})
        for var, value in data.get("settings", {}).items():
            if not hasattr(self, var):
                warn(f"Unrecognized window setting ({var}).",
                     UserWarning)
            else:
                setattr(self, var, TIReal(value))

        # Load equations
        for name, equation in data.get("equations", {}).items():
            if name in self.equation_names:
                plotted = TIGraphedEquation(name=name)
                plotted.load_dict(equation)
                setattr(self, name, plotted)

            else:
                warn(f"Unrecognized equation ({name}).",
                     UserWarning)

        # Set type if color data exists so it can be loaded
        if "global84CSettings" in dct:
            self.__class__ = TIGDB

        self.coerce()
        self._load_dict(dct)

    def _load_dict(self, dct: dict):
        pass

    def dict(self) -> dict:
        """
        :return: A ``dict`` representing this GDB in JSON format
        """

        return {
            "graphMode": self.mode,
            "formatSettings": [
                "Dot" if GraphMode.Dot in self.mode_flags else "Connected",
                "Simul" if GraphMode.Simul in self.mode_flags else "Sequential",
                "GridOn" if GraphMode.GridOn in self.mode_flags else "GridOff",
                "PolarGC" if GraphMode.PolarGC in self.mode_flags else "RectGC",
                "CoordOff" if GraphMode.CoordOff in self.mode_flags else "CoordOn",
                "AxesOff" if GraphMode.AxesOff in self.mode_flags else "AxesOn",
                "LabelOn" if GraphMode.LabelOn in self.mode_flags else "LabelOff"
            ],
            "extSettings": {
                "showExpr": GraphMode.ExprOn in self.extended_mode_flags
            },
            "globalWindowSettings": {
                "Xmin": self.Xmin.json_number(),
                "Xmax": self.Xmax.json_number(),
                "Xscl": self.Xscl.json_number(),
                "Ymin": self.Ymin.json_number(),
                "Ymax": self.Ymax.json_number(),
                "Yscl": self.Yscl.json_number()
            }
        }

    @Loader[str]
    def load_string(self, string: str):
        self.load_dict(json.loads(string))

    def coerce(self):
        if self.get_color_data():
            self.__class__ = TIGDB
            self.coerce()
        else:
            match self.mode_id:
                case TIMonoFuncGDB.mode_byte:
                    self.__class__ = TIMonoFuncGDB
                case TIMonoParamGDB.mode_byte:
                    self.__class__ = TIMonoParamGDB
                case TIMonoPolarGDB.mode_byte:
                    self.__class__ = TIMonoPolarGDB
                case TIMonoSeqGDB.mode_byte:
                    self.__class__ = TIMonoSeqGDB

                case _:
                    warn(f"Graphing mode byte 0x{self.mode_id:x} not recognized.",
                         BytesWarning)


class TIGDB(TIMonoGDB):
    """
    Base class for all color GDB entries

    A GDB is a collection of equations and graph settings representing the state of one of the equation plotters.
    A GDB can correspond to one of the Function, Parametric, Polar, or Sequence plotting modes.
    """

    min_data_length = 66
    has_color = True

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "GDB1",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

        self.axes_color = GraphColor.Black
        self.grid_color = GraphColor.MedGray

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, GraphColor)[-5:-4]
    def grid_color(self) -> bytes:
        """
        The color of the grid
        """

    @View(calc_data, GraphColor)[-4:-3]
    def axes_color(self) -> bytes:
        """
        The color of the axes
        """

    @View(calc_data, GlobalLineStyle)[-3:-2]
    def global_line_style(self) -> bytes:
        """
        The line style for all plotted equations
        """

    @View(calc_data, BorderColor)[-2:-1]
    def border_color(self) -> bytes:
        """
        The color of the graph border
        """

    @View(calc_data, GraphMode)[-1:]
    def color_mode_flags(self) -> GraphMode:
        """
        The flags for extended color mode settings

        Only DetectAsymptotesOn/Off is stored here.
        """

    def _load_dict(self, dct: dict):
        self.color_magic = "84C"

        if colors := dct.get("global84CSettings", {}).get("colors", {}):
            if "grid" in colors:
                self.grid_color = getattr(GraphColor, colors["grid"])

            if "axes" in colors:
                self.axes_color = getattr(GraphColor, colors["axes"])

            if "border" in colors:
                self.border_color = BorderColor.COLORS[colors["border"] - 1]

        if other := dct.get("global84CSettings", {}).get("other", {}):
            if "globalLineStyle" in other:
                self.global_line_style = getattr(GlobalLineStyle, other["globalLineStyle"])

            if "detectAsymptotes" in other:
                self.color_mode_flags |= \
                    GraphMode.DetectAsymptotesOn if other["detectAsymptotes"] else GraphMode.DetectAsymptotesOff

    def dict(self) -> dict:
        return {
            "global84CSettings": {
                "colors": {
                    "grid": GraphColor.get_name(self.grid_color),
                    "axes": GraphColor.get_name(self.axes_color),
                    "border": self.border_color
                },
                "other": {
                    "globalLineStyle": GlobalLineStyle.get_name(self.global_line_style),
                    "detectAsymptotes": GraphMode.DetectAsymptotesOn in self.color_mode_flags
                }
            }
        }

    def coerce(self):
        match self.mode_id:
            case TIFuncGDB.mode_byte:
                self.__class__ = TIFuncGDB
            case TIParamGDB.mode_byte:
                self.__class__ = TIParamGDB
            case TIPolarGDB.mode_byte:
                self.__class__ = TIPolarGDB
            case TISeqGDB.mode_byte:
                self.__class__ = TISeqGDB

            case _:
                warn(f"Graphing mode byte 0x{self.mode_id:x} not recognized.",
                     BytesWarning)


class TIMonoFuncGDB(TIMonoGDB):
    """
    Parser for function GDBs
    """

    mode_byte = 0x10

    min_data_length = 110

    num_equations = 10
    num_parameters = 1
    num_styles = 10

    equation_names = ["Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y0"]

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[3:4]
    def mode_id(self) -> int:
        """
        The mode ID for the GDB - ``0x10``
        """

    @View(calc_data, GraphRealEntry)[61:70]
    def Xres(self, value: GraphRealEntry) -> GraphRealEntry:
        """
        Xres: The pixel separation of points in a function plot

        The value must be an integer in ``[1,8]``.
        """

        if int(value) != float(value) or not 1 <= int(value) <= 8:
            warn(f"Expected an integer between 1 and 8, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, TIGraphedEquation[1])
    def Y1(self) -> TIGraphedEquation:
        """
        Y1: The 1st equation in function mode
        """

    @View(calc_data, TIGraphedEquation[2])
    def Y2(self) -> TIGraphedEquation:
        """
        Y2: The 2nd equation in function mode
        """

    @View(calc_data, TIGraphedEquation[3])
    def Y3(self) -> TIGraphedEquation:
        """
        Y3: The 3rd equation in function mode
        """

    @View(calc_data, TIGraphedEquation[4])
    def Y4(self) -> TIGraphedEquation:
        """
        Y4: The 4th equation in function mode
        """

    @View(calc_data, TIGraphedEquation[5])
    def Y5(self) -> TIGraphedEquation:
        """
        Y5: The 5th equation in function mode
        """

    @View(calc_data, TIGraphedEquation[6])
    def Y6(self) -> TIGraphedEquation:
        """
        Y6: The 6th equation in function mode
        """

    @View(calc_data, TIGraphedEquation[7])
    def Y7(self) -> TIGraphedEquation:
        """
        Y7: The 7th equation in function mode
        """

    @View(calc_data, TIGraphedEquation[8])
    def Y8(self) -> TIGraphedEquation:
        """
        Y8: The 8th equation in function mode
        """

    @View(calc_data, TIGraphedEquation[9])
    def Y9(self) -> TIGraphedEquation:
        """
        Y9: The 9th equation in function mode
        """

    @View(calc_data, TIGraphedEquation[10])
    def Y0(self) -> TIGraphedEquation:
        """
        Y0: The 10th equation in function mode
        """

    @Loader[dict]
    def load_dict(self, dct: dict = None):
        if dct is None:
            with open(os.path.join(os.path.dirname(__file__), "json/func.default.json")) as file:
                dct = json.load(file)

        super().load_dict(dct)

    def dict(self) -> dict:
        return super().dict() | {
            "specificData": {
                "settings": {
                    "Xres": int(self.Xres)
                },
                "equations": {
                    equation.json_name: equation.dict() for equation in self.equations
                }
            }
        }


class TIFuncGDB(TIGDB, TIMonoFuncGDB):
    """
    Parser for function color GDBs
    """

    min_data_length = 128

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, String)[-18:-15]
    def color_magic(self) -> str:
        """
        Magic to identify the GDB as color-oriented

        This value is always ``84C``.
        """

    def mono(self) -> TIMonoFuncGDB:
        """
        :return: A copy of this GDB with all color data removed
        """

        return TIMonoFuncGDB(self.bytes()[:-18])

    def _load_dict(self, dct: dict):
        super(TIGDB, self)._load_dict(dct)
        super(TIFuncGDB, self)._load_dict(dct)

    def dict(self) -> dict:
        return super(TIGDB, self).dict() | super(TIFuncGDB, self).dict()


class TIMonoParamGDB(TIMonoGDB):
    """
    Parser for parametric GDBs
    """

    mode_byte = 0x40

    min_data_length = 130

    num_equations = 12
    num_parameters = 3
    num_styles = 6

    equation_names = ["X1T", "Y1T", "X2T", "Y2T", "X3T", "Y3T", "X4T", "Y4T", "X5T", "Y5T", "X6T", "Y6T"]

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[3:4]
    def mode_id(self) -> int:
        """
        The mode ID for the GDB - ``0x40``
        """

    @View(calc_data, GraphRealEntry)[61:70]
    def Tmin(self) -> GraphRealEntry:
        """
        Tmin: The initial time
        """

    @View(calc_data, GraphRealEntry)[70:79]
    def Tmax(self) -> GraphRealEntry:
        """
        Tmax: The final time
        """

    @View(calc_data, GraphRealEntry)[79:88]
    def Tstep(self) -> GraphRealEntry:
        """
        Tstep: The time increment
        """

    @View(calc_data, TIGraphedEquation[1])
    def X1T(self) -> TIGraphedEquation:
        """
        X1T: The 1st X-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[2])
    def Y1T(self) -> TIGraphedEquation:
        """
        Y1T: The 1st Y-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[3])
    def X2T(self) -> TIGraphedEquation:
        """
        X2T: The 2nd X-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[4])
    def Y2T(self) -> TIGraphedEquation:
        """
        Y2T: The 2nd Y-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[5])
    def X3T(self) -> TIGraphedEquation:
        """
        X3T: The 3rd X-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[6])
    def Y3T(self) -> TIGraphedEquation:
        """
        Y3T: The 3rd Y-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[7])
    def X4T(self) -> TIGraphedEquation:
        """
        X4T: The 4th X-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[8])
    def Y4T(self) -> TIGraphedEquation:
        """
        Y4T: The 4th Y-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[9])
    def X5T(self) -> TIGraphedEquation:
        """
        X5T: The 5th X-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[10])
    def Y5T(self) -> TIGraphedEquation:
        """
        Y5T: The 5th Y-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[11])
    def X6T(self) -> TIGraphedEquation:
        """
        X6T: The 6th X-component in parametric mode
        """

    @View(calc_data, TIGraphedEquation[12])
    def Y6T(self) -> TIGraphedEquation:
        """
        Y6T: The 6th Y-component in parametric mode
        """

    @Loader[dict]
    def load_dict(self, dct: dict = None):
        if dct is None:
            with open(os.path.join(os.path.dirname(__file__), "json/param.default.json")) as file:
                dct = json.load(file)

        super().load_dict(dct)

    def _load_dict(self, dct: dict):
        for i in range(1, self.num_styles + 1):
            if (x_style := getattr(self, f"X{i}T").style) != (y_style := getattr(self, f"Y{i}T").style):
                warn(f"X and Y component styles do not agree (X{i}T: {x_style}, Y{i}T: {y_style}).",
                     UserWarning)

            if (x_color := getattr(self, f"X{i}T").color) != (y_color := getattr(self, f"Y{i}T").color):
                warn(f"X and Y component colors do not agree (X{i}T: {x_color}, Y{i}T: {y_color}).",
                     UserWarning)

    def dict(self) -> dict:
        return super().dict() | {
            "specificData": {
                "settings": {
                    "Tmin": self.Tmin.json_number(),
                    "Tmax": self.Tmax.json_number(),
                    "Tstep": self.Tstep.json_number(),
                },
                "equations": {
                    equation.json_name: equation.dict() for equation in self.equations
                }
            }
        }


class TIParamGDB(TIGDB, TIMonoParamGDB):
    """
    Parser for parametric color GDBs
    """

    min_data_length = 144

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, String)[-14:-11]
    def color_magic(self) -> str:
        """
        Magic to identify the GDB as color-oriented

        This value is always ``84C``.
        """

    def mono(self) -> TIMonoParamGDB:
        """
        :return: A copy of this GDB with all color data removed
        """

        return TIMonoParamGDB(self.bytes()[:-14])

    def _load_dict(self, dct: dict):
        super(TIGDB, self)._load_dict(dct)
        super(TIParamGDB, self)._load_dict(dct)

    def dict(self) -> dict:
        return super(TIGDB, self).dict() | super(TIParamGDB, self).dict()


class TIMonoPolarGDB(TIMonoGDB):
    """
    Parser for polar GDBs
    """

    mode_byte = 0x20

    min_data_length = 112

    num_equations = 6
    num_parameters = 3
    num_styles = 6

    equation_names = ["r1", "r2", "r3", "r4", "r5", "r6"]

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[3:4]
    def mode_id(self) -> int:
        """
        The mode ID for the GDB - ``0x20``
        """

    @View(calc_data, GraphRealEntry)[61:70]
    def Thetamin(self) -> GraphRealEntry:
        """
        θmin: The initial angle
        """

    @View(calc_data, GraphRealEntry)[70:79]
    def Thetamax(self) -> GraphRealEntry:
        """
        θmax: The final angle
        """

    @View(calc_data, GraphRealEntry)[79:88]
    def Thetastep(self) -> GraphRealEntry:
        """
        θstep: The angle increment
        """

    @View(calc_data, TIGraphedEquation[1])
    def r1(self) -> TIGraphedEquation:
        """
        r1: The 1st equation in polar mode
        """

    @View(calc_data, TIGraphedEquation[2])
    def r2(self) -> TIGraphedEquation:
        """
        r1: The 2nd equation in polar mode
        """

    @View(calc_data, TIGraphedEquation[3])
    def r3(self) -> TIGraphedEquation:
        """
        r3: The 3rd equation in polar mode
        """

    @View(calc_data, TIGraphedEquation[4])
    def r4(self) -> TIGraphedEquation:
        """
        r4: The 4th equation in polar mode
        """

    @View(calc_data, TIGraphedEquation[5])
    def r5(self) -> TIGraphedEquation:
        """
        r5: The 5th equation in polar mode
        """

    @View(calc_data, TIGraphedEquation[6])
    def r6(self) -> TIGraphedEquation:
        """
        r6: The 6th equation in polar mode
        """

    @Loader[dict]
    def load_dict(self, dct: dict = None):
        if dct is None:
            with open(os.path.join(os.path.dirname(__file__), "json/polar.default.json")) as file:
                dct = json.load(file)

        super().load_dict(dct)

    def dict(self) -> dict:
        return super().dict() | {
            "specificData": {
                "settings": {
                    "Thetamin": self.Thetamin.json_number(),
                    "Thetamax": self.Thetamax.json_number(),
                    "Thetastep": self.Thetastep.json_number(),
                },
                "equations": {
                    equation.json_name: equation.dict() for equation in self.equations
                }
            }
        }


class TIPolarGDB(TIGDB, TIMonoPolarGDB):
    """
    Parser for polar color GDBs
    """

    min_data_length = 126

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, String)[-14:-11]
    def color_magic(self) -> str:
        """
        Magic to identify the GDB as color-oriented

        This value is always ``84C``.
        """

    def mono(self) -> TIMonoPolarGDB:
        """
        :return: A copy of this GDB with all color data removed
        """

        return TIMonoPolarGDB(self.bytes()[:-14])

    def _load_dict(self, dct: dict):
        super(TIGDB, self)._load_dict(dct)
        super(TIPolarGDB, self)._load_dict(dct)

    def dict(self) -> dict:
        return super(TIGDB, self).dict() | super(TIPolarGDB, self).dict()


class TIMonoSeqGDB(TIMonoGDB):
    """
    Parser for sequential GDBs
    """

    mode_byte = 0x80

    min_data_length = 163

    num_equations = 3
    num_parameters = 10
    num_styles = 3

    equation_names = ["u", "v", "w"]

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, Integer)[3:4]
    def mode_id(self) -> int:
        """
        The mode ID for the GDB - ``0x80``
        """

    @View(calc_data, GraphMode)[5:6]
    def sequence_flags(self) -> SeqMode:
        """
        The flags for the sequence mode settings
        """

    @View(calc_data, GraphRealEntry)[61:70]
    def PlotStart(self, value) -> GraphRealEntry:
        """
        PlotStart: The initial value of 𝑛 for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[70:79]
    def nMax(self, value) -> GraphRealEntry:
        """
        𝑛Max: The final value of 𝑛

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[79:88]
    def unMin(self) -> GraphRealEntry:
        """
        u(𝑛Min): The initial value of u at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[88:97]
    def vnMin(self) -> GraphRealEntry:
        """
        v(𝑛Min): The initial value of v at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[97:106]
    def nMin(self, value) -> GraphRealEntry:
        """
        nMin: the initial value of 𝑛

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[106:115]
    def unMinp1(self) -> GraphRealEntry:
        """
        u(𝑛Min+1): The initial value of u at 𝑛Min + 1
        """

    @View(calc_data, GraphRealEntry)[115:124]
    def vnMinp1(self) -> GraphRealEntry:
        """
        v(nMin+1): The initial value of v at 𝑛Min + 1
        """

    @View(calc_data, GraphRealEntry)[124:133]
    def wnMin(self) -> GraphRealEntry:
        """
        w(𝑛Min): The initial value of w at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[133:142]
    def PlotStep(self, value) -> GraphRealEntry:
        """
        PlotStep: The 𝑛 increment for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[142:151]
    def wnMinp1(self) -> GraphRealEntry:
        """
        w(𝑛Min+1): The initial value of w at 𝑛Min + 1
        """

    @View(calc_data, TIGraphedEquation[1])
    def u(self) -> TIGraphedEquation:
        """
        u: The 1st equation in sequence mode
        """

    @View(calc_data, TIGraphedEquation[2])
    def v(self) -> TIGraphedEquation:
        """
        v: The 2nd equation in sequence mode
        """

    @View(calc_data, TIGraphedEquation[3])
    def w(self) -> TIGraphedEquation:
        """
        w: The 3rd equation in sequence mode
        """

    @Loader[dict]
    def load_dict(self, dct: dict = None):
        if dct is None:
            with open(os.path.join(os.path.dirname(__file__), "json/seq.default.json")) as file:
                dct = json.load(file)

        super().load_dict(dct)

    def _load_dict(self, dct: dict):
        ext_settings = dct.get("extSettings", {})
        if "seqMode" in ext_settings:
            match ext_settings["seqMode"]:
                case "SEQ(n)":
                    self.extended_mode_flags |= GraphMode.SEQ_n
                case "SEQ(n+1)":
                    self.extended_mode_flags |= GraphMode.SEQ_np1
                case "SEQ(n+2)":
                    self.extended_mode_flags |= GraphMode.SEQ_np2

        if "seqSettings" in dct:
            try:
                self.sequence_flags |= getattr(SeqMode, dct["seqMode"])

            except AttributeError:
                warn(f"Unrecognized sequence mode ({dct['seqMode']}).",
                     UserWarning)

    def dict(self) -> dict:
        dct = super().dict()

        match self.extended_mode_flags:
            case _SEQ_n if GraphMode.SEQ_n in self.extended_mode_flags:
                dct["extSettings"]["seqMode"] = "SEQ(n)"
            case _SEQ_np1 if GraphMode.SEQ_np1 in self.extended_mode_flags:
                dct["extSettings"]["seqMode"] = "SEQ(n+1)"
            case _SEQ_np2 if GraphMode.SEQ_np2 in self.extended_mode_flags:
                dct["extSettings"]["seqMode"] = "SEQ(n+2)"

        match self.sequence_flags:
            case _Time if SeqMode.Time in self.sequence_flags:
                dct["seqSettings"] = {"mode": "Time"}
            case _Web if SeqMode.Web in self.sequence_flags:
                dct["seqSettings"] = {"mode": "Web"}
            case _VertWeb if SeqMode.VertWeb in self.sequence_flags:
                dct["seqSettings"] = {"mode": "VertWeb"}
            case _uv if SeqMode.uv in self.sequence_flags:
                dct["seqSettings"] = {"mode": "uv"}
            case _vw if SeqMode.vw in self.sequence_flags:
                dct["seqSettings"] = {"mode": "vw"}
            case _uw if SeqMode.uw in self.sequence_flags:
                dct["seqSettings"] = {"mode": "uw"}

        return dct | {
            "specificData": {
                "settings": {
                    "nMin": int(self.nMin),
                    "nMax": int(self.nMax),
                    "PlotStart": int(self.PlotStart),
                    "PlotStep": int(self.PlotStep),
                    "unMin": self.unMin.json_number(),
                    "unMinp1": self.unMinp1.json_number(),
                    "vnMin": self.vnMinp1.json_number(),
                    "vnMinp1": self.vnMinp1.json_number(),
                    "wnMin": self.wnMin.json_number(),
                    "wnMinp1": self.wnMinp1.json_number()
                },
                "equations": {
                    equation.json_name: equation.dict() for equation in self.equations
                }
            }
        }


class TISeqGDB(TIGDB, TIMonoSeqGDB):
    """
    Parser for sequential color GDBs
    """

    min_data_length = 174

    @Section()
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, String)[-11:-8]
    def color_magic(self) -> str:
        """
        Magic to identify the GDB as color-oriented

        This value is always ``84C``.
        """

    def mono(self) -> TIMonoSeqGDB:
        """
        :return: A copy of this GDB with all color data removed
        """

        return TIMonoSeqGDB(self.bytes()[:-11])

    def _load_dict(self, dct: dict):
        super(TIGDB, self)._load_dict(dct)
        super(TISeqGDB, self)._load_dict(dct)

    def dict(self) -> dict:
        return super(TIGDB, self).dict() | super(TISeqGDB, self).dict()


__all__ = ["TIMonoGDB", "TIMonoFuncGDB", "TIMonoParamGDB", "TIMonoPolarGDB", "TIMonoSeqGDB",
           "TIGDB", "TIFuncGDB", "TIParamGDB", "TIPolarGDB", "TISeqGDB",
           "TIGraphedEquation", "EquationFlags", "GraphMode", "GraphStyle", "GraphColor", "GlobalLineStyle"]
