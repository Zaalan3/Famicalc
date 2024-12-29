"""
Flash types
"""


from tivars.data import *
from tivars.flash import TIFlashHeader
from tivars.models import *


class TIOperatingSystem(TIFlashHeader, register=True):
    """
    Parser for operating systems (OSes)
    """

    extensions = {
        None: "8eu",
        TI_82A: "82u",
        TI_84PCSE: "8cu",
        TI_84PCE: "8eu",
        TI_83PCE: "8pu",
        TI_82AEP: "8yu"
    }

    _type_id = 0x23


class TIApp(TIFlashHeader, register=True):
    """
    Parser for apps
    """

    extensions = {
        None: "8ek",
        TI_83P: "8xk",
        TI_84PCSE: "8ck",
        TI_84PCE: "8ek"
    }

    _type_id = 0x24


class TICertificate(TIFlashHeader, register=True):
    """
    Parser for certificate files

    To date, no external certificate files have been found in the wild.
    """

    extensions = {
        None: "8eq",
        TI_83P: "8xq",
        TI_84PCSE: "8cq",
        TI_84PCE: "8eq"
    }

    _type_id = 0x25


class TILicense(TIFlashHeader, register=True):
    """
    Parser for licenses

    A license is simply a string containing the TI license agreement, possibly spanning multiple devices and languages.
    """

    extensions = {
        None: "8eu",
        TI_82A: "82u",
        TI_84PCSE: "8cu",
        TI_84PCE: "8eu",
        TI_83PCE: "8pu",
        TI_82AEP: "8yu"
    }

    _type_id = 0x3E

    @Section()
    def calc_data(self) -> bytes:
        """
        The data stored in the flash header
        """

    @View(calc_data, String)[:]
    def license(self) -> str:
        """
        The license stored in the header as a string
        """


__all__ = ["TIOperatingSystem", "TIApp", "TICertificate", "TILicense"]
