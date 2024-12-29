import os
import xml.etree.ElementTree as ET

from .parse import Tokens, OsVersion, OsVersions


COMMENT = """<!--
TokenIDE-compatible token file generated using the TI-Toolkit token sheets:
https://github.com/TI-Toolkit/tokens

TokenIDE created by
Shaun McFall, Merthsoft Creations
-->"""


class TokenIDESheet:
    """
    Data class representing the contents of a TokenIDE token file

    The sheet is a dictionary with two elements:
        - tokens:   a recursing dictionary of tokens, indexed by byte
        - meta:     global metadata for TokenIDE concerning styling and grouping

    If an existing TokenIDE token file is not used a base, no metadata is present.
    """

    NAMESPACE = "http://merthsoft.com/Tokens"

    STARTERS = [b'\x2A']
    TERMINATORS = [b'\x04', b'\x2A', b'\x3F']

    def __init__(self, sheet: dict[str] = None):
        self.sheet = sheet or {"tokens": {}, "meta": []}
        self.sheet["tokens"] |= {"$00": {"string": "", "variants": set(), "attrib": {}, "tokens": {}}}

    @staticmethod
    def from_xml_string(xml_str: str) -> 'TokenIDESheet':
        """
        Constructs an instance from an XML string

        :param xml_str: An XML string
        :return: A TokenIDESheet corresponding to the string
        """

        return TokenIDESheet.from_element(ET.fromstring(xml_str))

    @staticmethod
    def from_element(root: ET.Element) -> 'TokenIDESheet':
        """
        Constructs an instance from an XML element in a TokenIDE token file

        :param root: An XML element, which must be the root element of the file
        :return: A TokenIDESheet corresponding to the root element
        """

        if root.tag != f"{{{TokenIDESheet.NAMESPACE}}}Tokens":
            raise ValueError("Not a TokenIDE xml.")

        sheet: dict[str] = {"tokens": {}, "meta": []}

        def parse_page(element: ET.Element, dct: dict):
            match element.tag.removeprefix(f"{{{TokenIDESheet.NAMESPACE}}}"):
                case "Token":
                    attrib = element.attrib

                    dct["tokens"][attrib.pop("byte")] = dct = {"string": attrib.pop("string", None), "variants": set(),
                                                               "attrib": attrib, "tokens": {}}

                case "Alt":
                    dct["variants"].add(element.attrib["string"])

                case "Groups" | "Styles":
                    sheet["meta"].append(element)

            for child in element:
                parse_page(child, dct)

        parse_page(root, sheet)
        return TokenIDESheet(sheet)

    def to_xml_string(self) -> str:
        """
        :return: This sheet as an indented XML string
        """

        element = self.to_element()
        ET.indent(element, "  ")

        # ET does not provide a method to insert a header comment
        string = ET.tostring(element, encoding="utf8").decode()
        string = string.replace("utf8", "utf-8")
        return string.replace("?>", "?>\n" + COMMENT)

    def to_element(self) -> ET.Element:
        """
        :return: This sheet as an XML element
        """

        sheet = ET.Element(f"{{{TokenIDESheet.NAMESPACE}}}Tokens",
                           {"xmlns:xsi": "http://www.w3.org/2001/XMLSchema-instance",
                            "xmlns:xsd": "http://www.w3.org/2001/XMLSchema"})

        sheet.extend(self.sheet["meta"] or [ET.Element("Groups"), ET.Element("Styles")])

        def build_page(element: ET.Element, byte: str, dct: dict, depth: int = 0):
            # Special case for newline
            if byte == "$3F" and depth == 1:
                element = ET.SubElement(element, "Token", byte=byte, string=r"\n", stringTerminator="true")

            elif byte:
                element = ET.SubElement(element, "Token", byte=byte,
                                        **({"string": dct["string"]} if dct.get("string", None) is not None else {}),
                                        **dct.get("attrib", {}))

                for name in dct.get("variants", set()):
                    element.append(ET.Element("Alt", string=name))

            for child_byte, child_dct in sorted(dct.get("tokens", {}).items()):
                build_page(element, child_byte, child_dct, depth + 1)

        build_page(sheet, "", self.sheet)
        return sheet

    def for_version(self, *, version: OsVersion = None, lang: str = 'en') -> 'TokenIDESheet':
        """
        Constructs a copy of this sheet tailored to a given OS version

        If a token is entirely absent, its accessible name is used as its string value.
        Metadata is always preserved.

        :param version: The OS version to target (defaults to latest)
        :param lang: A language code (defaults to "en")
        :return: A TokenIDESheet supporting the given version with this sheet's metadata attached
        """

        sheet = {"meta": self.sheet["meta"].copy(), "tokens": {}}

        with open(os.path.join(os.path.dirname(__file__), "../8X.xml"), encoding="UTF-8") as file:
            tokens = Tokens.from_xml_string(file.read(), version or OsVersions.LATEST)

        all_bytes = tokens.bytes
        all_names = [name for token in all_bytes.values()
                     for name in {*token.langs.get(lang, "en").names(), token.langs.get(lang, "en").display}]

        for byte, token in all_bytes.items():
            if version is not None and token.since > version:
                continue

            leading, trailing = byte[:1], byte[1:]

            new = sheet["tokens"]
            attrib = self.sheet["tokens"].copy()
            value = f"${leading.hex().upper()}"

            if value not in new:
                new[value] = {"string": None, "variants": set(), "attrib": {}, "tokens": {}}

            if trailing:
                attrib = attrib[value]["tokens"]
                new = new[value]["tokens"]
                value = f"${trailing.hex().upper()}"

                new[value] = {"string": None, "variants": set(), "attrib": {}, "tokens": {}}

            translation = token.langs.get(lang, "en")
            display = translation.display

            new[value]["string"] = display if all_names.count(display) == 1 else translation.accessible
            new[value]["variants"] |= {*translation.names()}
            new[value]["variants"] -= {new[value]["string"]}

            new[value]["attrib"] |= attrib.get(value, {}).get("attrib", {})
            if byte in TokenIDESheet.STARTERS:
                new[value]["attrib"]["stringStarter"] = "true"

            if byte in TokenIDESheet.TERMINATORS:
                new[value]["attrib"]["stringTerminator"] = "true"

        return TokenIDESheet(sheet)


ET.register_namespace("", TokenIDESheet.NAMESPACE)
__all__ = ["TokenIDESheet"]
