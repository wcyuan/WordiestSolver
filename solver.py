#!/usr/bin/env python
"""
A script that solves

https://play.google.com/store/apps/details?id=com.concreterose.wordiest

$ python solver.py --spin m i s2l o3w g3w y n t r2l a4l e n e n
(214, 'ornamenting', 'yes', [])
$ python solver.py --spin o2l d4w a e m e3w o2l e i k h f e2l g
(237, 'geekdom', 'foh', [Tile(letter = 'a',
     word_mult = 1,
     letter_mult = 1,
     value = 1), Tile(letter = 'e',
     word_mult = 1,
     letter_mult = 1,
     value = 1), Tile(letter = 'e',
     word_mult = 1,
     letter_mult = 1,
     value = 1), Tile(letter = 'i',
     word_mult = 1,
     letter_mult = 1,
     value = 1)])
"""

# --------------------------------------------------------------------------- #

from __future__ import absolute_import, division, with_statement

import inspect
import logging
import optparse
import os

logging.basicConfig(format='[%(asctime)s '
                    '%(funcName)s:%(lineno)s %(levelname)-5s] '
                    '%(message)s')

# http://stackoverflow.com/questions/714063/python-importing-modules-from-parent-folder
CURRENT_DIR = os.path.dirname(
    os.path.abspath(inspect.getfile(inspect.currentframe())))

SOWPODS_FN = 'sowpods.txt'
WORDS_FN = 'twl06.txt'
DEFAULT_TOPN = 10

# --------------------------------------------------------------------------- #

def main():
    (opts, args) = getopts()
    if opts.test:
        test()
    else:
        if opts.sowpods:
            words = Words(os.path.join(CURRENT_DIR, SOWPODS_FN))
        else:
            words = Words(os.path.join(CURRENT_DIR, WORDS_FN))
        tiles = sorted(Tile.from_str(a) for a in args)
        for data in Wordiest.find_two_best(words, tiles, opts.topn):
            print data
    if opts.spin:
        print "Control-C to exit"
        while True:
            pass

def getopts():
    parser = optparse.OptionParser()
    parser.add_option('--verbose',  action='store_true')
    parser.add_option('--spin',  action='store_true')
    parser.add_option('--sowpods',  action='store_true')
    parser.add_option('--test',  action='store_true')
    parser.add_option('-n', '--topn',  type=int, default=DEFAULT_TOPN)
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
        'n': 2,
        'o': 1,
        'p': 3,
        'q': 10,
        'r': 1,
        's': 1,
        't': 1,
        'u': 2,
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

class TopNQueue(object):
    def __init__(self, topn=DEFAULT_TOPN, by_value=False):
        self.topn = topn
        self.by_value = by_value
        self.data = []

    def insert(self, value, data):
        """
        >>> topn = TopNQueue(by_value=True)
        >>> topn.insert(3, 'asdf')
        >>> topn.insert(5, 'five')
        >>> topn.insert(5, 'cinq')
        >>> topn.data
        [(5, ['five', 'cinq']), (3, ['asdf'])]
        >>> topn = TopNQueue()
        >>> topn.insert(3, 'asdf')
        >>> topn.insert(5, 'five')
        >>> topn.insert(5, 'cinq')
        >>> topn.data
        [(5, 'cinq'), (5, 'five'), (3, 'asdf')]
        """
        ii = 0
        length = len(self.data)
        while ii < length and value < self.data[ii][0]:
            ii += 1
        if ii >= self.topn:
            return
        if self.by_value:
            if ii < length and self.data[ii][0] == value:
                self.data[ii][1].append(data)
            else:
                self.data.insert(ii, (value, [data]))
        else:
            self.data.insert(ii, (value, data))
        if len(self.data) > self.topn:
            self.data = self.data[:self.topn]

    def __iter__(self):
        if self.by_value:
            for (value, data_list) in self.data:
                for data in data_list:
                    yield (value, data)
        else:
            for d in self.data:
                yield d

    def __getitem__(self, ii):
        return self.data[ii]

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
    def find_best(cls, words, tiles, topn=DEFAULT_TOPN):
        best = TopNQueue(topn)
        for (word, used, remaining) in cls.subset(words, tiles):
            score = cls.score(used)
            logging.debug("%s %s", word, score)
            best.insert(score, (word, remaining))
        return best

    @classmethod
    def find_two_best(cls, words, tiles, topn=DEFAULT_TOPN):
        best = cls.find_best(words, tiles, topn)
        for (score, data) in best:
            (word, remaining) = data
            next_best = cls.find_best(words, remaining, topn=2)
            for (next_score, next_data) in next_best:
                (next_word, next_remaining) = next_data
                yield (score+next_score,
                       words.get_words(word),
                       words.get_words(next_word),
                       ''.join(r.letter for r in next_remaining))

# --------------------------------------------------------------------------- #

def test():
    import doctest
    doctest.testmod(verbose=True)

# --------------------------------------------------------------------------- #

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        import traceback
        traceback.print_exc()
        while True:
            pass

# --------------------------------------------------------------------------- #
