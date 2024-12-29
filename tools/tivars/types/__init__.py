"""
Parsers for the TI var types
"""


# Makes pydoctor happy
from typing import TYPE_CHECKING

from .appvar import *
from .complex import *
from .flash import *
from .gdb import *
from .group import *
from .list import *
from .matrix import *
from .real import *
from .picture import *
from .settings import *
from .tokenized import *

from ..flash import TIFlashHeader, DeviceType
from ..var import TIEntry


if TYPE_CHECKING:
    __all__ = []

else:
    __all__ = ["TIEntry", "TIFlashHeader",
               "TIReal",
               "TIRealList", "TIMatrix",
               "TIEquation", "TIString",
               "TIProgram", "TIAsmProgram", "TIProtectedProgram", "TIProtectedAsmProgram",
               "TIPicture", "TIMonoPicture",
               "TIMonoGDB", "TIGDB", "TIGraphedEquation",
               "TIMonoFuncGDB", "TIMonoParamGDB", "TIMonoPolarGDB", "TIMonoSeqGDB",
               "TIFuncGDB", "TIParamGDB", "TIPolarGDB", "TISeqGDB",
               "EquationFlags", "GraphMode", "GraphStyle", "GraphColor", "GlobalLineStyle",
               "TIComplex", "TIComplexList",
               "TIUndefinedReal",
               "TIWindowSettings", "TIRecallWindow", "TITableSettings",
               "TIAppVar",
               "TIGroup",
               "TIRealFraction",
               "TIImage",
               "TIComplexFraction",
               "TIRealRadical", "TIComplexRadical",
               "TIComplexPi", "TIComplexPiFraction",
               "TIRealPi", "TIRealPiFraction",
               "TIOperatingSystem", "TIApp", "TICertificate", "TILicense",
               "DeviceType"
               ]
