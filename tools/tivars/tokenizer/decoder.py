"""
Token stream decoder
"""


from warnings import warn

from tivars.models import *
from tivars.tokens.scripts import *


def decode(bytestream: bytes, *,
           tokens: Tokens = None, lang: str = "en",
           mode: str = "display") -> tuple[str | bytes, OsVersion]:
    """
    Decodes a byte stream into a string of tokens and its minimum supported OS version

    Each token is represented using one of three different representations formats, dictated by ``mode``:
        - ``display``: Represents the tokens with Unicode characters matching the calculator's display
        - ``accessible``: Represents the tokens with ASCII-only equivalents, often requiring multi-character glyphs
        - ``ti_ascii``: Represents the tokens with their internal font indices (returns a ``bytes`` object)

    :param bytestream: The token bytes to decode
    :param tokens: The `Tokens` object to use for decoding (defaults to the TI-84+CE tokens)
    :param lang: The language used in ``string`` (defaults to English, ``en``)
    :param mode: The form of token representation to use for output (defaults to ``display``)
    :return: A tuple of a string of token representations and a minimum `OsVersion`
    """

    tokens = tokens or TI_84PCE.tokens

    out = []
    since = OsVersions.INITIAL

    byte_attr = mode == "ti_ascii"

    index = 0
    curr_bytes = b''
    while index < len(bytestream):
        curr_bytes += bytestream[index:][:1]
        curr_hex = curr_bytes.hex()

        if curr_bytes[0]:
            if curr_bytes in tokens.bytes:
                try:
                    out.append(getattr(tokens.bytes[curr_bytes].langs[lang], mode))

                except AttributeError:
                    raise ValueError(f"'{mode}' is not a recognized token representation")

                except KeyError:
                    raise ValueError(f"'{lang}' is not a recognized language")

                since = max(tokens.bytes[curr_bytes].since, since)

                curr_bytes = b''

            elif len(curr_bytes) >= 2:
                if not any(key.startswith(curr_bytes[:1]) for key in tokens.bytes):
                    warn(f"Unrecognized byte '0x{curr_hex}' at position {index}.",
                         BytesWarning)

                    out.append(b'?' if byte_attr else rf"\x{curr_hex}")

                else:
                    warn(f"Unrecognized bytes '0x{curr_hex}' at position {index}.",
                         BytesWarning)

                    out.append(b'?' if byte_attr else rf"\u{curr_hex}")

                curr_bytes = b''

        elif any(curr_bytes):
            count = 0
            while not curr_bytes[0]:
                curr_bytes = curr_bytes[1:]
                count += 1
                out.append(b'?' if byte_attr else r"\x00")

            warn(f"There are {count} unexpected null bytes at position {index}." if count > 1 else
                 f"There is an unexpected null byte at position {index}.",
                 BytesWarning)

            index -= 1

        index += 1

    return b''.join(out) if byte_attr else "".join(out), since


__all__ = ["decode"]
