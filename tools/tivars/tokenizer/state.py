"""
Encoder states
"""


from string import punctuation

from tivars.tokens.scripts import *


class EncoderState:
    """
    Base class for encoder states

    Each state represents some encoding context which affects tokenization.
    """

    mode = 0
    """
    Whether to munch maximally (``0``) or minimally (``-1``)
    """

    max_length = None
    """
    The maximum number of tokens to emit before leaving this state
    """

    def __init__(self, length: int = 0):
        self.length = length

    def munch(self, string: str, trie: TokenTrie) -> tuple[Token, str, list['EncoderState']]:
        """
        Munch the input string and determine the resulting token, encoder state, and remainder of the string

        :param string: The text string to tokenize
        :param trie: The `TokenTrie` object to use for tokenization
        :return: A tuple of the output `Token`, the remainder of ``string``, and a list of states to add to the stack
        """

        # Is this a byte literal?
        if string.startswith(r"\x") or string.startswith(r"\u"):
            length = 4 if string.startswith(r"\x") else 6
            string, remainder = string[:length], string[length:]
            token = Token(bytes.fromhex(string.lstrip(r"\ux")),
                          {"en": Translation(b'?', string, string, [])},
                          {"illegal": "true"})

            return token, remainder, self.next(token)

        tokens = trie.get_tokens(string)
        if not tokens:
            raise ValueError("no tokenization options exist")

        # Is this a glyph?
        if string[0] in punctuation and len(tokens) > 1:
            tokens.pop()

        token, remainder = tokens[self.mode]

        # Are we out of tokens?
        if self.length == self.max_length:
            return token, remainder, []

        return token, remainder, self.next(token)

    def next(self, token: Token) -> list['EncoderState']:
        """
        Determines the next encode state given a token

        The current state is popped from the stack, and the states returned by this method are pushed.

        If the list of returned states is...
            - empty, then the encoder is exiting the current state.
            - length one, then the encoder's current state is being replaced by a new state.
            - length two, then the encoder is entering a new state, able to exit back to this one.

        :param token: The current token
        :return: A list of encoder states to add to the stack
        """

        return [type(self)(self.length + 1)]


class MaxMode(EncoderState):
    """
    Maximal munching mode
    """

    mode = 0


class MinMode(EncoderState):
    """
    Minimal munching mode
    """

    mode = -1


class Line(EncoderState):
    """
    Encoder state which is always exited after a line break
    """

    def next(self, token: Token) -> list[EncoderState]:
        match token.bits:
            case b'\x04' | b'\x3F':
                return []

            case _:
                return super().next(token)


class Name(Line):
    """
    Valid var identifiers
    """

    mode = -1

    def next(self, token: Token) -> list[EncoderState]:
        #  Digits                              Uppercase letters (and theta)
        if b'\x30' <= token.bits <= b'\x39' or b'\x41' <= token.bits <= b'\x5B':
            return super().next(token)

        else:
            return []


class ListName(Name):
    """
    List names
    """

    max_length = 5


class ProgramName(Name):
    """
    Program names
    """

    max_length = 8


class String(Line):
    """
    Strings
    """

    mode = -1

    def next(self, token: Token) -> list[EncoderState]:
        match token.bits:
            case b'\x2A':
                return []

            case _:
                return super().next(token)


class InterpolatedString(String):
    """
    Strings interpolated via ``Send(``
    """

    mode = 0


class InterpolationStart(Line):
    """
    Encoder state to initialize `InterpolatedString`

    If any token besides ``"`` is encountered, this state is immediately exited to avoid cluttering the stack.
    """

    mode = 0

    def next(self, token: Token) -> list[EncoderState]:
        match token.bits:
            case b'\x2A':
                return [InterpolatedString()]

            case _:
                return []


class SmartMode(EncoderState):
    """
    Smart tokenization mode
    """

    mode = 0

    def next(self, token: Token) -> list[EncoderState]:
        match token.bits:
            case b'\x2A':
                return [self, String()]

            #    prgm
            case b'\x5F':
                return [self, ProgramName()]

            #    Send(
            case b'\xE7':
                return [self, InterpolationStart()]

            #    |L
            case b'\xEB':
                return [self, ListName()]

            case _:
                return super().next(token)


__all__ = ["EncoderState", "MaxMode", "MinMode", "SmartMode",
           "Line", "Name", "ListName", "ProgramName",
           "String", "InterpolatedString", "InterpolationStart"]
