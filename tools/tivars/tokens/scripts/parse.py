import functools
import xml.etree.ElementTree as ET
from dataclasses import dataclass

# Models ordered such that models earlier in the list are earlier in the evolution of the token tables
MODEL_ORDER = {
    "": 0,

    "TI-82": 10,

    "TI-83": 20,
    "TI-82ST": 20,
    "TI-82ST.fr": 20,
    "TI-76.fr": 20,

    "TI-83+": 30,
    "TI-83+SE": 30,
    "TI-83+.fr": 30,
    "TI-82+": 30,

    "TI-84+": 40,
    "TI-84+SE": 40,
    "TI-83+.fr:USB": 40,
    "TI-84P.fr": 40,
    "TI-84+PSE": 40,

    "TI-82A": 45,
    "TI-84+T": 45,

    "TI-84+CSE": 50,

    "TI-84+CE": 60,
    "TI-84+CET": 60,
    "TI-83PCE": 60,
    "TI-83PCEEP": 60,
    "TI-84+CEPY": 60,
    "TI-84+CETPE": 60,
    "TI-82AEP": 60,

    "latest": 9999999
}


@functools.total_ordering
@dataclass(frozen=True)
class OsVersion:
    """
    Data class for defining and comparing OS versions

    An OS version is defined by its model and version number.
    The model name must appear in the MODEL_ORDER map above.
    The version number must be of the form "x1.x2.....xn", where each xi is an integer.

    Both the model and version can also be empty or "latest".
    An empty model/version is always first in version ordering, while "latest" is always last.
    """
    
    model: str
    version: str

    def __lt__(self, other):
        order1 = MODEL_ORDER[self.model]
        order2 = MODEL_ORDER[other.model]

        if order1 < order2:
            return True
        elif order1 > order2:
            return False
        else:
            if self.version == "latest":
                return False
            elif other.version == "latest":
                return True
            elif other.version == "":
                return False
            elif self.version == "":
                return True
            else:
                return self.version.split(".") < other.version.split(".")

    def __eq__(self, other):
        return MODEL_ORDER[self.model] == MODEL_ORDER[other.model] and self.version == other.version

    @staticmethod
    def from_element(element: ET.Element) -> 'OsVersion':
        """
        Constructs an instance from an XML element in a token sheet

        :param element: An XML element
        :return: An OS version corresponding to the element
        """
        
        model = ""
        version = ""

        for child in element:
            if child.tag == "model":
                model = child.text
            elif child.tag == "os-version":
                version = child.text
            else:
                raise ValueError("Unrecognized tag in " + element.tag + ": " + child.tag)

        if version == "":
            raise ValueError("<" + element.tag + "> has a missing or empty <os-version> tag.")
        elif model == "":
            raise ValueError("<" + element.tag + "> has a missing or empty <model> tag.")

        if model not in MODEL_ORDER or model == "latest":  # "latest" is for user convenience, not the sheet itself
            raise ValueError("Unrecognized <model>: " + model)

        if any([c != '.' and not c.isnumeric() for c in version]):
            raise ValueError(
                "Invalid <version> string \"" + version + "\", must be a sequence of numbers separated by periods.")

        return OsVersion(model, version)


class OsVersions:
    """
    Enum class to contain useful OS version constants

    This class can be extended with useful versions for other applications.
    """
    
    INITIAL = OsVersion("", "")
    LATEST = OsVersion("latest", "latest")


class Translation:
    """
    Data class for managing the different decodings of tokens

    A token translations stores the following:
        - The TI ASCII representation, i.e. the sequence of font bytes used on-calc
        - The displayed name, a Unicode string intended to best emulate on-calc display
        - The accessible name, an ASCII string intended to be easy to type
        - Any variant names; such may be derived from their use in other tokenization tools
    """
    
    def __init__(self, ti_ascii: bytes, display: str, accessible: str, variants: list[str]):
        self.ti_ascii = ti_ascii
        self.display = display
        self.accessible = accessible
        self.variants = variants

    def names(self) -> list[str]:
        """
        :return: A list of all names in this translation used for tokenization
        """
        
        return [self.accessible, *self.variants]

    @staticmethod
    def from_element(element: ET.Element) -> (str, 'Translation'):
        """
        Constructs an instance and its key from an XML element in a token sheet

        :param element: An XML element
        :return: A tuple of a string key and a token translation corresponding to the element
        """
        
        code = element.attrib["code"]

        ti_ascii = bytes.fromhex(element.attrib["ti-ascii"])
        display = element.attrib["display"]

        accessible = ""
        variants = []

        for child in element:
            match child.tag:
                case "accessible":
                    accessible = child.text
                case "variant":
                    variants.append(child.text)

        return code, Translation(ti_ascii, display, accessible, variants)


class Token:
    """
    Data class for storing all information about a single token

    A token stores the following:
        - The bytes for this token on-calc
        - The translations of this token in all supported languages
        - The earliest OS version supporting this token
        - The latest OS version supporting this token
        - Any additional attributes stored in the token sheets
    """
    
    def __init__(self, bits: bytes, langs: dict[str, Translation], attrs: dict[str, str] = None,
                 since: OsVersion = OsVersions.INITIAL,
                 until: OsVersion = OsVersions.LATEST):
        self.bits = bits
        self.langs = langs
        self.attrs = attrs
        self.since = since
        self.until = until

    @staticmethod
    def from_element(element: ET.Element, bits: bytes, version: OsVersion = OsVersions.LATEST):
        """
        Constructs an instance from an XML element in the token sheets

        :param element: An XML element
        :param bits: The token's bytes
        :param version: A minimum OS version (defaults to latest)
        :return: A token corresponding to the element and bits
        """

        since, until = OsVersions.INITIAL, OsVersions.LATEST
        langs: dict[str, Translation] = {}

        for version_elem in element:
            below, above = False, True

            for child in version_elem:
                match child.tag:
                    case "since":
                        version_since = OsVersion.from_element(child)
                        if version_since < version:
                            below = True
                            since = version_since

                    case "until":
                        version_until = OsVersion.from_element(child)
                        if version_until < version:
                            above = False

                        else:
                            until = version_until

                    case "lang":
                        if above and below:
                            code, translation = Translation.from_element(child)
                            langs[code] = translation

        return Token(bits, langs, attrs=element.attrib, since=since, until=until)


class Tokens:
    """
    Data class for storing maps between text and tokens

    Two maps are stored: bytes -> token and string -> string -> bytes.
    
    The byte map is indexed by bytes and yields token objects.
    The lang map is indexed by language code, then token name, and yields token bytes.
    """
    
    def __init__(self, byte_map: dict[bytes, Token], lang_map: dict[str, dict[str, bytes]]):
        self.bytes = byte_map
        self.langs = lang_map

    @staticmethod
    def from_xml_string(xml_str: str, version: OsVersion = OsVersions.LATEST):
        """
        Constructs an instance from an XML string

        :param xml_str: An XML string
        :param version: A minimum OS version (defaults to latest)
        :return: Token maps corresponding to the string
        """
        
        return Tokens.from_element(ET.fromstring(xml_str), version=version)

    @staticmethod
    def from_element(root: ET.Element, version: OsVersion = OsVersions.LATEST):
        """
        Constructs an instance from an XML element in the token sheets

        :param root: An XML element, which must be the root element of the sheet
        :param version: A minimum OS version (defaults to latest)
        :return: Token maps corresponding to the root element
        """
        
        if root.tag != "tokens":
            raise ValueError("Not a tokens xml.")

        all_bytes: dict[bytes, Token] = {}
        all_langs: dict[str, dict[str, bytes]] = {}

        def parse_page(element: ET.Element, bits: bytes = b""):
            nonlocal all_bytes
            nonlocal all_langs

            if element.tag == "token":
                token_bits = bits + bytes.fromhex(element.attrib["value"][1:])
                token = Token.from_element(element, token_bits, version=version)

                if token.langs:
                    all_bytes[token_bits] = token
                    for lang, translation in token.langs.items():
                        if lang not in all_langs:
                            all_langs[lang] = {}

                        for name in translation.names():
                            all_langs[lang][name] = token_bits

            for child in element:
                if child.tag == "two-byte":
                    parse_page(child, bits=bits + bytes.fromhex(child.attrib["value"][1:]))
                else:
                    parse_page(child, bits=bits)

        parse_page(root)

        return Tokens(all_bytes, all_langs)


# with open("../8X.xml", encoding="UTF-8") as file:
#   Tokens.from_xml_string(file.read())
