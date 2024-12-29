"""
Utilities for managing calc models and OS versions
"""


from .model import *
from .versions import *

__all__ = ["TI_82",
           "TI_83", "TI_82ST", "TI_82ST_fr", "TI_76_fr",
           "TI_83P", "TI_83PSE", "TI_83P_fr", "TI_82P",
           "TI_84P", "TI_84PSE", "TI_83P_fr_USB", "TI_84P_fr", "TI_84PPSE",
           "TI_84PT", "TI_82A",
           "TI_84PCSE",
           "TI_84PCE", "TI_84PCET", "TI_83PCE", "TI_83PCEEP", "TI_84PCEPY", "TI_84PCETPE", "TI_82AEP",
           "TIFeature", "TIModel", "OsVersion", "OsVersions"]
