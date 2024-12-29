"""
Model and feature definitions
"""


import os

from functools import total_ordering

from tivars.flags import *
from tivars.tokens.scripts import OsVersion, Tokens, TokenTrie
from tivars.tokens.scripts.parse import MODEL_ORDER


@total_ordering
class TIModel:
    """
    Data type for all model constants

    Every 83-series model is included in this module as a constant to use in code.
    Each model holds its (abbreviated) name, features, file magic, product ID, and native language.

    Models can also be used to obtain token maps and tries for tokenization and OS versions for compatibility checks.
    """

    MODELS = []
    """
    A list of all models
    """

    def __init__(self, name: str, features: 'TIFeature', magic: str, product_id: int, lang: str):
        self._name = name
        self._features = TIFeature(features)
        self._magic = magic
        self._product_id = product_id
        self._lang = lang

        with open(os.path.join(os.path.dirname(__file__), "../tokens/8X.xml"), encoding="UTF-8") as file:
            self._tokens = Tokens.from_xml_string(file.read(), self.OS("latest"))

        self._trie = {}
        for lang in self._tokens.langs:
            self._trie[lang] = TokenTrie.from_tokens(self._tokens, lang)

        self._trie[None] = self._trie["en"]

    def __eq__(self, other):
        return str(self) == str(other)

    def __ge__(self, other):
        return self.order >= other.order

    def __hash__(self):
        return hash(self.name)

    def __str__(self):
        return self.name

    @property
    def features(self) -> 'TIFeature':
        """
        :return: This model's features
        """

        return self._features

    @property
    def lang(self) -> str:
        """
        :return: This model's native language
        """

        return self._lang

    @property
    def magic(self) -> str:
        """
        :return: This model's file magic
        """

        return self._magic

    @property
    def name(self) -> str:
        """
        :return: This model's (abbreviated) name
        """

        return self._name

    @property
    def order(self) -> int:
        """
        :return: This model's order within the chronology used by the token sheets
        """

        return MODEL_ORDER[self._name]

    @property
    def product_id(self) -> int:
        """
        :return: This model's product ID
        """

        return self._product_id

    @property
    def tokens(self) -> Tokens:
        """
        :return: The tokens supported by this model
        """

        return self._tokens

    def get_trie(self, lang: str = None) -> TokenTrie:
        """
        Gets the token trie for this model corresponding to a given language

        :param lang: A language code (defaults to English, ``en``)
        :return: The token trie corresponding to ``lang``
        """

        return self._trie[lang]

    def has(self, feature: 'TIFeature'):
        """
        Whether this model has a given feature

        :param feature: The feature to check
        :return: Whether this model has ``feature``
        """

        return feature in self._features

    def OS(self, version: str = "") -> OsVersion:
        """
        An `OsVersion` with this model as its model and a supplied version

        :param version: An OS version number (defaults to the model's earliest OS)
        :return: An `OsVersion` for this model and ``version``
        """

        return OsVersion(self.name, version)


class TIFeature(Flags):
    """
    Flags representing all calculator features
    """

    Complex = {0: 1}
    """
    Whether the model supports complex numbers
    """

    Flash = {1: 1}
    """
    Whether the model has a flash chip
    """

    Apps = {2: 1}
    """
    Whether the model supports apps
    """

    Clock = {3: 1}
    """
    Whether the model has a real-time clock
    """

    Color = {4: 1}
    """
    Whether the model has a color display
    """

    eZ80 = {5: 1}
    """
    Whether the model has an eZ80 chip
    """

    ExactMath = {6: 1}
    """
    Whether the model supports exact math for fractions and radicals
    """

    Python = {7: 1}
    """
    Whether the model supports Python
    """


features82 = {}
features83 = features82 | TIFeature.Complex
features82a = features83 | TIFeature.Flash
features83p = features82a | TIFeature.Apps
features84p = features83p | TIFeature.Clock
features84pcse = features84p | TIFeature.Color
features84pce = features84pcse | TIFeature.eZ80
features83pce = features84pce | TIFeature.ExactMath
features83pceep = features83pce | TIFeature.Python
features84pcepy = features84pce | TIFeature.Python
features82aep = features83pceep | {2: 0}

it = iter(MODEL_ORDER)
next(it)

TIModel.MODELS = [
    TI_82 := TIModel(next(it), features82, "**TI82**", 0x00, "en"),

    TI_83 := TIModel(next(it), features83, "**TI83**", 0x00, "en"),
    TI_82ST := TIModel(next(it), features83, "**TI83**", 0x00, "en"),
    TI_82ST_fr := TIModel(next(it), features83, "**TI83**", 0x00, "fr"),
    TI_76_fr := TIModel(next(it), features83, "**TI83**", 0x00, "fr"),

    TI_83P := TIModel(next(it), features83p, "**TI83F*", 0x04, "en"),
    TI_83PSE := TIModel(next(it), features83p, "**TI83F*", 0x04, "en"),
    TI_83P_fr := TIModel(next(it), features83p, "**TI83F*", 0x04, "fr"),
    TI_82P := TIModel(next(it), features83p, "**TI83F*", 0x04, "fr"),

    TI_84P := TIModel(next(it), features84p, "**TI83F*", 0x0A, "en"),
    TI_84PSE := TIModel(next(it), features84p, "**TI83F*", 0x0A, "en"),
    TI_83P_fr_USB := TIModel(next(it), features84p, "**TI83F*", 0x0A, "fr"),
    TI_84P_fr := TIModel(next(it), features84p, "**TI83F*", 0x0A, "fr"),
    TI_84PPSE := TIModel(next(it), features84p, "**TI83F*", 0x0A, "en"),

    TI_82A := TIModel(next(it), features82a, "**TI83F*", 0x0B, "fr"),
    TI_84PT := TIModel(next(it), features84p, "**TI83F*", 0x1B, "en"),

    TI_84PCSE := TIModel(next(it), features84pcse, "**TI83F*", 0x0F, "en"),

    TI_84PCE := TIModel(next(it), features84pce, "**TI83F*", 0x13, "en"),
    TI_84PCET := TIModel(next(it), features84pce, "**TI83F*", 0x13, "en"),
    TI_83PCE := TIModel(next(it), features83pce, "**TI83F*", 0x13, "fr"),
    TI_83PCEEP := TIModel(next(it), features83pceep, "**TI83F*", 0x13, "fr"),
    TI_84PCEPY := TIModel(next(it), features84pcepy, "**TI83F*", 0x13, "en"),
    TI_84PCETPE := TIModel(next(it), features84pcepy, "**TI83F*", 0x13, "en"),
    TI_82AEP := TIModel(next(it), features82aep, "**TI83F*", 0x00, "fr"),
]

__all__ = ["TI_82",
           "TI_83", "TI_82ST", "TI_82ST_fr", "TI_76_fr",
           "TI_83P", "TI_83PSE", "TI_83P_fr", "TI_82P",
           "TI_84P", "TI_84PSE", "TI_83P_fr_USB", "TI_84P_fr", "TI_84PPSE",
           "TI_84PT", "TI_82A",
           "TI_84PCSE",
           "TI_84PCE", "TI_84PCET", "TI_83PCE", "TI_83PCEEP", "TI_84PCEPY", "TI_84PCETPE", "TI_82AEP",
           "TIFeature", "TIModel", "OsVersion"]
