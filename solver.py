#!/usr/bin/env python
"""
"""

# --------------------------------------------------------------------------- #

from __future__ import absolute_import, division, with_statement

import logging
import optparse

logging.basicConfig(format='[%(asctime)s '
                    '%(funcName)s:%(lineno)s %(levelname)-5s] '
                    '%(message)s')

WORDS_FN = 'sowpods.txt'

# --------------------------------------------------------------------------- #

def main():
    (opts, args) = getopts()
    words = Words(WORDS_FN)
    tiles = sorted(Tiles.from_str(a) for a in args)
    print Wordiest.find_two_best(words, tiles)

def getopts():
    parser = optparse.OptionParser()
    parser.add_option('--verbose',  action='store_true')
    (opts, args) = parser.parse_args()
    if opts.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    return (opts, args)

# --------------------------------------------------------------------------- #

class Words(object):
    def __init__(self, filename=WORDS_FN):
        self._data = None
        if filename is not None:
            self.read(filename)

    def read(self, filename):
        data = {}
        with open(filename) as fd:
            for line in fd:
                # consume the header
                if line.startswith("SOWPODS "):
                    continue
                line = line.strip()
                # skip empty lines
                if line == "":
                    continue
                data.setdefault(self.make_key(line), []).append(line)
        self._data = data

    @classmethod
    def make_key(cls, string):
        return ''.join(sorted(string.strip().lower()))

    def __iter__(self):
        return iter(self._data)

    def get_words(self, key):
        return self._data.get(key, [])

    def find(self, word):
        return self.get_words(self.make_key(word))

# --------------------------------------------------------------------------- #

class FieldMixin(object):
    """
    This Mixin applies to classes that have a few key attributes that
    determine all the other behavior.  Those attributes have to be
    settable as keyword arguments in the __init__ function.  In that
    case, if you put the names of those attributes in _flds, then this
    Mixin will provide repr, cmp, and hash functions.

    """
    @property
    def _flds(self):
        raise NotImplementedError

    @staticmethod
    def _vals(obj):
        return tuple(getattr(obj, fld) for fld in obj._flds)

    def __repr__(self):
        """
        Must satisfy:
          obj == eval(repr(obj))
        for any obj
        """
        cn = self.__class__.__name__
        fmt = ('{cn}(' +
               ',\n{pd} '.join('{0} = {{self.{0}!r}}'.format(fld)
                               for fld in (self._flds)) +
               ')')
        return fmt.format(self=self, cn=cn, pd=' '*len(cn))

    def __cmp__(self, other):
        tcmp = cmp(type(self), type(other))
        if tcmp == 0:
            return cmp(self._vals(self), self._vals(other))
        else:
            return tcmp

    def __hash__(self):
        return hash(self._vals(self))

class Tile(FieldMixin):
    LETTER_VALUES = {
        'a': 1,
        'b': 4,
        'c': 3,
        'd': 2,
        'e': 1,
        'f': 4,
        'g': 3,
        'h': 3,
        'i': 1,
        'j': 10,
        'k': 5,
        'l': 2,
        'm': 4,
        'n': 1,
        'o': 1,
        'p': 3,
        'q': 10,
        'r': 1,
        's': 1,
        't': 1,
        'u': 1,
        'v': 4,
        'w': 4,
        'x': 8,
        'y': 4,
        'z': 10,
    }

    @property
    def _flds(self):
        return ('letter', 'word_mult', 'letter_mult', 'value')

    def __init__(self, letter, letter_mult=1, word_mult=1, lvalue=None):
        self.letter = letter.lower()
        self.letter_mult = letter_mult
        self.word_mult = word_mult
        self.lvalue = self.LETTER_VALUES[self.letter] if lvalue is None else lvalue

    @property
    def value(self):
        return self.lvalue * self.letter_mult

    def __cmp__(self, other):
        if isinstance(other, str):
            return cmp(self.letter, other)
        tcmp = cmp(type(self), type(other))
        if tcmp != 0:
            return tcmp
        wcmp = cmp(other.word_mult, self.word_mult)
        if wcmp != 0:
            return wcmp
        vcmp = cmp(other.value, self.value)
        if vcmp != 0:
            return vcmp
        return cmp(self.letter, other.letter)

    @classmethod
    def from_str(cls, string):
        if len(string) == 1:
            return cls(string)
        if len(string) > 1:
            typ = string[-1].lower()
            if typ == 'w':
                return cls(string[0], word_mult=int(string[1:-1]))
            if typ == 'l':
                return cls(string[0], letter_mult=int(string[1:-1]))
        raise ValueError("Can't make Letter from string {0}".format(string))

# --------------------------------------------------------------------------- #

class Wordiest(object):
    @classmethod
    def make_tiles(cls, strings):
        return sorted(Tile.from_str(a) for a in strings)

    @classmethod
    def contains(cls, word, tiles):
        """
        @return (match, used, remaining)
        """
        length = len(word)
        ntiles = len(tiles)
        if length == 0:
            return (True, [], tiles)
        if ntiles == 0:
            return (False, [], tiles)
        try:
            idx = tiles.index(word[0])
            if length == 1:
                return (True, [tiles[idx]], tiles[:idx] + tiles[idx+1:])
            if ntiles == 1:
                return (False, [], tiles)
            (val, used, remaining) = cls.contains(word[1:], tiles[:idx] + tiles[idx+1:])
            return (val, [tiles[idx]] + used, remaining)
        except ValueError:
            return (False, [], tiles)

    @classmethod
    def subset(cls, words, tiles):
        for word in words:
            (match, used, remaining) = cls.contains(word, tiles)
            if match:
                yield (word, used, remaining)

    @classmethod
    def score_word(cls, word, tiles):
        (match, used, remaining) = cls.contains(word, tiles)
        if not match:
            return 0
        return cls.score(used)

    @classmethod
    def score(cls, used):
        raw = sum(u.value for u in used)
        for u in used:
            raw *= u.word_mult
        return raw

    @classmethod
    def find_best(cls, words, tiles):
        best_score = 0
        best_words = []
        for (word, used, remaining) in cls.subset(words, tiles):
            score = cls.score(used)
            logging.debug("%s %s", word, score)
            if score > best_score:
                best_score = score
                best_words = [(word, remaining)]
            elif score == best_score:
                best_words.append((word, remaining))
        return (best_score, best_words)

    @classmethod
    def find_two_best(cls, words, tiles):
        (best_score, best_words) = cls.find_best(words, tiles)
        (word, remaining) = best_words[0]
        (next_score, next_words) = cls.find_best(words, remaining)
        return (best_score+next_score,
                words.get_words(word)[0],
                words.get_words(next_words[0][0])[0],
                next_words[0][1])

# --------------------------------------------------------------------------- #

if __name__ == "__main__":
    main()

# --------------------------------------------------------------------------- #
