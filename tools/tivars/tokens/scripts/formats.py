import json
import re
import xml.etree.ElementTree as ET

from collections import defaultdict

from .parse import OsVersion, OsVersions


def validate(root: ET.Element) -> int:
    """
    Validates a token sheet, raising an error if an invalid component is found

    :param root: An XML element, which must be the root element of the sheet
    :return: The number of tokens in the sheet
    """

    if root.tag != "tokens":
        raise ValueError("not a token sheet")

    all_tokens = set()
    all_names = {}

    version = None

    def visit(element: ET.Element, byte: str = "", lang: str = ""):
        nonlocal version

        byte += element.attrib.get("value", "").lstrip("$")
        lang += element.attrib.get("code", "")

        class ValidationError(ValueError):
            __qualname__ = "ValidationError"

            def __init__(self, message: str):
                super().__init__((f"token 0x{byte}: " if byte else "root: ") + message)

        # Require attributes matching regexes
        def attributes(attrs: dict[str, str]):
            attrib = element.attrib.copy()

            for attr, regex in attrs.items():
                if attr not in attrib:
                    raise ValidationError(f"<{element.tag}> does not have attribute {attr}")

                if not re.fullmatch(regex, value := attrib.pop(attr), flags=re.DOTALL):
                    raise ValidationError(f"<{element.tag}> {attr} '{value}' does not match r'{regex}'")

            if attrib:
                raise ValidationError(f"<{element.tag}> has unexpected attribute {[*attrib.values()][0]}")

        # Require child tags to match regex when appended in order
        def children(regex: str):
            if not re.fullmatch(regex, "".join(f"<{child.tag}>" for child in element)):
                raise ValidationError(f"children of <{element.tag}> do not match r'{regex}'")

        # Require text to match regex
        def text(regex: str):
            if not re.fullmatch(regex, element.text, flags=re.DOTALL):
                raise ValidationError(f"<{element.tag}> text '{element.text}' does not match r'{regex}'")

        # Check requirements for each tag
        match element.tag:
            case "tokens":
                children(r"(<token>|<two-byte>)+")

            case "two-byte":
                attributes({"value": r"\$[0-9A-F]{2}"})
                children(r"(<token>)+")

            case "token":
                attributes({"value": r"\$[0-9A-F]{2}"})
                children(r"(<version>)+")

                if byte in all_tokens:
                    raise ValidationError("token byte must be unique")

                all_tokens.add(byte)

            case "version":
                version = OsVersions.INITIAL
                children(r"<since>(<until>)?(<lang>)+")

            case "since":
                if (this_version := OsVersion.from_element(element)) < version:
                    raise ValidationError(f"version {this_version} overlaps with {version}")

                version = this_version
                
                # Workaround for nested defaultdict
                all_names[version] = all_names.get(version, defaultdict(set))

                children(r"<model><os-version>")

            case "until":
                children(r"<model><os-version>")

            case "lang":
                attributes({"code": r"[a-z]{2}", "ti-ascii": r"([0-9A-F]{2})+", "display": r".+"})
                children(r"<accessible>(<variant>)*")

            case "accessible":
                text(r"[\u0000-\u00FF]+")

                if element.text in all_names[version][lang]:
                    raise ValidationError(f"{lang} accessible name '{element.text}' is not unique within {version}")

                all_names[version][lang].add(element.text)

            case "variant":
                text(r".+")

                if element.text in all_names[version][lang]:
                    raise ValidationError(f"{lang} variant name '{element.text}' is not unique within {version}")

                all_names[version][lang].add(element.text)

            case "model":
                text(r"TI-\d\d.*")

            case "os-version":
                text(r"(\d+\.)+\d+")

            case _:
                raise ValidationError(f"unrecognized tag <{element.tag}>")

        # Visit children
        for child in element:
            visit(child, byte, lang)

    visit(root)
    return len(all_tokens)


def to_json(element: ET.Element):
    """
    Converts a token sheet to an equivalent JSON representation

    :param element: An XML element; call on the root element to convert the entire sheet
    :return: The element and all its descendants as JSON
    """

    match element.tag:
        case "tokens" | "two-byte":
            return {child.attrib["value"]: to_json(child) for child in element}

        case "token":
            return [to_json(child) for child in element]

        case "version":
            dct = {}
            langs = {}

            for child in element:
                if child.tag == "lang":
                    langs[child.attrib["code"]] = to_json(child)

                else:
                    dct[child.tag] = to_json(child)

            return dct | {"langs": langs}

        case "lang":
            dct = {"ti-ascii": element.attrib["ti-ascii"], "display": element.attrib["display"]}
            variants = []

            for child in element:
                if child.tag == "variant":
                    variants.append(child.text)

                else:
                    dct[child.tag] = child.text

            if variants:
                return dct | {"variants": variants}

            else:
                return dct

        case _:
            if list(element):
                return {child.tag: to_json(child) for child in element}
            else:
                return element.text


# with open("../8X.xml", encoding="UTF-8") as file:
#   json.dumps(to_json(ET.fromstring(file.read())), indent=2)


__all__ = ["to_json", "validate"]
