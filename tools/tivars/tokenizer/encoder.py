"""
Context-aware text encoder
"""


import re
import unicodedata

from tivars.models import *
from tivars.tokens.scripts import *
from .state import *


def encode(string: str, *,
           trie: TokenTrie = None, mode: str = None, normalize: bool = True) -> tuple[bytes, OsVersion]:
    """
    Encodes a string of tokens represented as text into a byte stream and its minimum supported OS version

    Tokenization is performed using one of three procedures, dictated by ``mode``:
        - ``max``: Always munch maximally, i.e. consume the most input possible to produce a token
        - ``smart``: Munch maximally or minimally depending on context
        - ``string``: Always munch minimally (equivalent to ``smart`` string context)

    The ``smart`` tokenization mode uses the following contexts, munching maximally otherwise:
        - Strings: munch minimally, except when interpolating using ``Send(``
        - Program names: munch minimally up to 8 tokens
        - List names: munch minimally up to 5 tokens

    For reference, here are the tokenization modes utilized by popular IDEs and other software:
        - SourceCoder: ``max``
        - TokenIDE: ``max``
        - TI Connect CE: ``smart``
        - TI-Planet Project Builder: ``smart``
        - tivars_lib_cpp: ``smart``

    All tokenization modes respect token glyphs for substituting Unicode symbols.

    :param string: The text string to encode
    :param trie: The `TokenTrie` object to use for tokenization (defaults to the TI-84+CE trie)
    :param mode: The tokenization mode to use (defaults to ``smart``)
    :param normalize: Whether to apply NFC normalization to the input before encoding (defaults to ``true``)
    :return: A tuple of a stream of token bytes and a minimum `OsVersion`
    """

    string = _normalize(string) if normalize else string
    trie = trie or TI_84PCE.get_trie()
    mode = mode or "smart"

    data = b''
    since = OsVersions.INITIAL
    index = 0

    match mode:
        case "max":
            stack = [MaxMode()]

        case "smart":
            stack = [SmartMode()]

        case "string" | "min":
            stack = [MinMode()]

        case _:
            raise ValueError(f"unrecognized tokenization mode: '{mode}'")

    while string:
        try:
            token, remainder, contexts = stack.pop().munch(string, trie)
            stack += contexts

        except ValueError:
            raise ValueError(f"could not tokenize input at position {index}: '{string[:12]}'")

        except IndexError:
            raise ValueError(f"stack consumed at position {index}: '{string[:12]}'")

        data += token.bits
        since = max(token.since, since)

        index += len(string) - len(remainder)
        string = remainder

    return data, since


def normalize(string: str):
    """
    Applies NFC normalization to a given string to ensure recognition of certain Unicode characters used as token names

    :param string: The text to normalize
    :return: The text in ``string`` normalized
    """

    return re.sub("[\u0398\u03F4\u1DBF]", "θ", unicodedata.normalize("NFC", string))


# Yucky scope nonsense to avoid a globals() call
_normalize = normalize


__all__ = ["encode", "normalize"]
