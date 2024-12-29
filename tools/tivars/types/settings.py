"""
Graph window and table settings
"""


import json

from warnings import warn

from tivars.data import *
from tivars.models import *
from tivars.var import SizedEntry
from .real import GraphRealEntry


class SettingsEntry(SizedEntry):
    """
    Base class for settings entries

    A settings entry stores all parameters for the different plot windows or tables
    """

    min_data_length = 2

    def __format__(self, format_spec: str) -> str:
        match format_spec:
            case "":
                return json.dumps(self.dict())

            case _:
                return super().__format__(format_spec)

    @Loader[dict]
    def load_dict(self, dct: dict):
        """
        Loads a JSON ``dict`` into this settings entry

        :param dct: The dict to load
        """

        for var, value in dct:
            if not hasattr(self, var):
                warn(f"Unrecognized window setting ({var}).",
                     UserWarning)
            else:
                setattr(self, var, GraphRealEntry(value))

    @Loader[str]
    def load_string(self, string: str):
        self.load_dict(json.loads(string))


class TIWindowSettings(SettingsEntry, register=True):
    """
    Parser for window settings

    A `TIWindowSettings` stores all plot window parameters as a contiguous stream of `TIReal` values.
    """

    extensions = {
        None: "8xw",
        TI_82: "82w",
        TI_83: "83w",
        TI_83P: "8xw"
    }

    min_data_length = 210

    _type_id = 0x0F

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "Window",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    @Section(8, String)
    def name(self) -> str:
        """
        The name of the entry

        This value is always ``Window``.
        """

    @Section(min_data_length)
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, GraphRealEntry)[3:12]
    def Xmin(self) -> GraphRealEntry:
        """
        Xmin: the X-coordinate of the left edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[12:21]
    def Xmax(self) -> GraphRealEntry:
        """
        Xmax: the X-coordinate of the right edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[21:30]
    def Xscl(self) -> GraphRealEntry:
        """
        Xscl: the separation between ticks on the X-axis
        """

    @View(calc_data, GraphRealEntry)[30:39]
    def Ymin(self) -> GraphRealEntry:
        """
        Ymin: the Y-coordinate of the bottom edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[39:48]
    def Ymax(self) -> GraphRealEntry:
        """
        Ymax: the Y-coordinate of the top edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[48:57]
    def Yscl(self) -> GraphRealEntry:
        """
        Yscl: the separation between ticks on the Y-axis
        """

    @View(calc_data, GraphRealEntry)[57:66]
    def Thetamin(self) -> GraphRealEntry:
        """
        Θmin: the initial angle for polar plots
        """

    @View(calc_data, GraphRealEntry)[66:75]
    def Thetamax(self) -> GraphRealEntry:
        """
        Θmax: the final angle for polar plots
        """

    @View(calc_data, GraphRealEntry)[75:84]
    def Thetastep(self) -> GraphRealEntry:
        """
        Θstep: the angle increment for polar plots
        """

    @View(calc_data, GraphRealEntry)[84:93]
    def Tmin(self) -> GraphRealEntry:
        """
        Tmin: the initial time for parametric plots
        """

    @View(calc_data, GraphRealEntry)[93:102]
    def Tmax(self) -> GraphRealEntry:
        """
        Tmax: the final time for parametric plots
        """

    @View(calc_data, GraphRealEntry)[102:111]
    def Tstep(self) -> GraphRealEntry:
        """
        Tstep: the time increment for parametric plots
        """

    @View(calc_data, GraphRealEntry)[111:120]
    def PlotStart(self, value) -> GraphRealEntry:
        """
        PlotStart: the initial value of 𝑛 for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[120:129]
    def nMax(self, value) -> GraphRealEntry:
        """
        𝑛Max: the final value of 𝑛 for sequential equations and plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[129:138]
    def unMin0(self) -> GraphRealEntry:
        """
        u(𝑛Min): the initial value of u at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[138:147]
    def vnMin0(self) -> GraphRealEntry:
        """
        v(𝑛Min): the initial value of v at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[147:156]
    def nMin(self, value) -> GraphRealEntry:
        """
        𝑛Min: the initial value of 𝑛 for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[156:165]
    def unMin1(self) -> GraphRealEntry:
        """
        u(𝑛Min+1): the initial value of u at 𝑛Min + 1
        """

    @View(calc_data, GraphRealEntry)[165:174]
    def vnMin1(self) -> GraphRealEntry:
        """
        v(𝑛Min+1): the initial value of v at 𝑛Min + 1
        """

    @View(calc_data, GraphRealEntry)[174:183]
    def wnMin0(self) -> GraphRealEntry:
        """
        w(𝑛Min): the initial value of w at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[183:192]
    def PlotStep(self, value) -> GraphRealEntry:
        """
        PlotStep: the 𝑛 increment for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[192:201]
    def Xres(self, value) -> GraphRealEntry:
        """
        Xres: the pixel separation of points in a function plot

        The value must be an integer in ``[1,8]``.
        """

        if int(value) != float(value) or not 1 <= int(value) <= 8:
            warn(f"Expected an integer between 1 and 8, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[201:210]
    def wnMin1(self) -> GraphRealEntry:
        """
        w(𝑛Min+1): the initial value of w at 𝑛Min + 1
        """

    def dict(self) -> dict:
        return {
            "Xmin": self.Xmin.json_number(),
            "Xmax": self.Xmax.json_number(),
            "Xscl": self.Xscl.json_number(),
            "Ymin": self.Ymin.json_number(),
            "Ymax": self.Ymax.json_number(),
            "Yscl": self.Yscl.json_number(),
            "Thetamin": self.Thetamin.json_number(),
            "Thetamax": self.Thetamax.json_number(),
            "Thetastep": self.Thetastep.json_number(),
            "Tmin": self.Tmin.json_number(),
            "Tmax": self.Tmax.json_number(),
            "Tstep": self.Tstep.json_number(),
            "PlotStart": int(self.PlotStart),
            "nMax": int(self.nMax),
            "unMin0": self.unMin0.json_number(),
            "vnMin0": self.vnMin0.json_number(),
            "nMin": int(self.nMin),
            "unMin1": self.unMin1.json_number(),
            "vnMin1": self.vnMin1.json_number(),
            "wnMin0": self.wnMin0.json_number(),
            "PlotStep": int(self.PlotStep),
            "Xres": int(self.Xres),
            "wnMin1": self.wnMin1.json_number()
        }


class TIRecallWindow(SettingsEntry, register=True):
    """
    Parser for recalled windows

    A `TIRecallWindow` stores all plot window parameters as a contiguous stream of `TIReal` values.
    """

    extensions = {
        None: "8xz",
        TI_82: "82z",
        TI_83: "83z",
        TI_83P: "8xz"
    }

    min_data_length = 209

    _type_id = 0x10

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "RclWindw",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    @Section(8, String)
    def name(self) -> str:
        """
        The name of the entry

        This value is always ``RclWindw``.
        """

    @Section(min_data_length)
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, GraphRealEntry)[2:11]
    def Xmin(self) -> GraphRealEntry:
        """
        Xmin: the X-coordinate of the left edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[11:20]
    def Xmax(self) -> GraphRealEntry:
        """
        Xmax: the X-coordinate of the right edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[20:29]
    def Xscl(self) -> GraphRealEntry:
        """
        Xscl: the separation between ticks on the X-axis
        """

    @View(calc_data, GraphRealEntry)[29:38]
    def Ymin(self) -> GraphRealEntry:
        """
        Ymin: the Y-coordinate of the bottom edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[38:47]
    def Ymax(self) -> GraphRealEntry:
        """
        Ymax: the Y-coordinate of the top edge of the graphscreen
        """

    @View(calc_data, GraphRealEntry)[47:56]
    def Yscl(self) -> GraphRealEntry:
        """
        Yscl: the separation between ticks on the Y-axis
        """

    @View(calc_data, GraphRealEntry)[56:65]
    def Thetamin(self) -> GraphRealEntry:
        """
        Θmin: the initial angle for polar plots
        """

    @View(calc_data, GraphRealEntry)[65:74]
    def Thetamax(self) -> GraphRealEntry:
        """
        Θmax: the final angle for polar plots
        """

    @View(calc_data, GraphRealEntry)[74:83]
    def Thetastep(self) -> GraphRealEntry:
        """
        Θstep: the angle increment for polar plots
        """

    @View(calc_data, GraphRealEntry)[83:92]
    def Tmin(self) -> GraphRealEntry:
        """
        Tmin: the initial time for parametric plots
        """

    @View(calc_data, GraphRealEntry)[92:101]
    def Tmax(self) -> GraphRealEntry:
        """
        Tmax: the final time for parametric plots
        """

    @View(calc_data, GraphRealEntry)[101:110]
    def Tstep(self) -> GraphRealEntry:
        """
        Tstep: the time increment for parametric plots
        """

    @View(calc_data, GraphRealEntry)[110:119]
    def PlotStart(self, value) -> GraphRealEntry:
        """
        PlotStart: the initial value of 𝑛 for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[119:128]
    def nMax(self, value) -> GraphRealEntry:
        """
        𝑛Max: the final value of 𝑛 for sequential equations and plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[128:137]
    def unMin0(self) -> GraphRealEntry:
        """
        u(𝑛Min): the initial value of u at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[137:146]
    def vnMin0(self) -> GraphRealEntry:
        """
        v(𝑛Min): the initial value of v at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[146:155]
    def nMin(self, value) -> GraphRealEntry:
        """
        𝑛Min: the initial value of 𝑛 for sequential equations

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[155:164]
    def unMin1(self) -> GraphRealEntry:
        """
        u(𝑛Min + 1): the initial value of u at 𝑛Min + 1
        """

    @View(calc_data, GraphRealEntry)[164:173]
    def vnMin1(self) -> GraphRealEntry:
        """
        v(𝑛Min + 1): the initial value of v at 𝑛Min + 1
        """

    @View(calc_data, GraphRealEntry)[173:182]
    def wnMin0(self) -> GraphRealEntry:
        """
        w(𝑛Min): the initial value of w at 𝑛Min
        """

    @View(calc_data, GraphRealEntry)[182:191]
    def PlotStep(self, value) -> GraphRealEntry:
        """
        PlotStep: the 𝑛 increment for sequential plots

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[191:200]
    def Xres(self, value) -> GraphRealEntry:
        """
        Xres: the pixel separation of points in a function plot

        The value must be an integer in ``[1,8]``.
        """

        if int(value) != float(value) or not 1 <= int(value) <= 8:
            warn(f"Expected an integer between 1 and 8, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[200:209]
    def wnMin1(self) -> GraphRealEntry:
        """
        w(𝑛Min + 1): the initial value of w at 𝑛Min + 1
        """

    def dict(self) -> dict:
        return {
            "Xmin": self.Xmin.json_number(),
            "Xmax": self.Xmax.json_number(),
            "Xscl": self.Xscl.json_number(),
            "Ymin": self.Ymin.json_number(),
            "Ymax": self.Ymax.json_number(),
            "Yscl": self.Yscl.json_number(),
            "Thetamin": self.Thetamin.json_number(),
            "Thetamax": self.Thetamax.json_number(),
            "Thetastep": self.Thetastep.json_number(),
            "Tmin": self.Tmin.json_number(),
            "Tmax": self.Tmax.json_number(),
            "Tstep": self.Tstep.json_number(),
            "PlotStart": int(self.PlotStart),
            "nMax": int(self.nMax),
            "unMin0": self.unMin0.json_number(),
            "vnMin0": self.vnMin0.json_number(),
            "nMin": int(self.nMin),
            "unMin1": self.unMin1.json_number(),
            "vnMin1": self.vnMin1.json_number(),
            "wnMin0": self.wnMin0.json_number(),
            "PlotStep": int(self.PlotStep),
            "Xres": int(self.Xres),
            "wnMin1": self.wnMin1.json_number()
        }


class TITableSettings(SettingsEntry, register=True):
    """
    Parser for table settings

    A `TITableSettings` stores all plot table parameters as a contiguous stream of `TIReal` values.
    """

    extensions = {
        None: "8xt",
        TI_82: "82t",
        TI_83: "83t",
        TI_83P: "8xt"
    }

    min_data_length = 20

    _type_id = 0x11

    def __init__(self, init=None, *,
                 for_flash: bool = True, name: str = "TblSet",
                 version: int = None, archived: bool = None,
                 data: bytes = None):

        super().__init__(init, for_flash=for_flash, name=name, version=version, archived=archived, data=data)

    @Section(8, String)
    def name(self) -> str:
        """
        The name of the entry

        This value is always ``TblSet``.
        """

    @Section(min_data_length)
    def calc_data(self) -> bytes:
        pass

    @View(calc_data, GraphRealEntry)[2:11]
    def TblMin(self, value) -> GraphRealEntry:
        """
        TblMin: the initial value for the table

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer for TblMin, got {float(value)}.",
                 UserWarning)

        return value

    @View(calc_data, GraphRealEntry)[11:20]
    def DeltaTbl(self, value) -> GraphRealEntry:
        """
        ΔTbl: the increment for the table

        The value must be an integer.
        """

        if int(value) != float(value):
            warn(f"Expected an integer for ΔTbl, got {float(value)}.",
                 UserWarning)

        return value

    def dict(self) -> dict:
        return {
            "TblMin": int(self.TblMin),
            "DeltaTbl": int(self.DeltaTbl)
        }


__all__ = ["TIWindowSettings", "TIRecallWindow", "TITableSettings"]
