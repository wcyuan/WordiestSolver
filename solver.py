#!/usr/bin/env python
"""
A script that solves

https://play.google.com/store/apps/details?id=com.concreterose.wordiest

$ solver.py m i s2l o3w g3w y n t r2l a4l e n e n
(214, ['ornamenting'], ['yes'], '')
(212, ['ornamenting'], ['ye'], 's')
(206, ['omniranges'], ['tyne'], '')
(205, ['omniranges'], ['yen'], 't')
(206, ['nonmigrant'], ['eyes'], '')
(205, ['nonmigrant'], ['yes'], 'e')
(205, ['magnetrons'], ['yin'], 'e')
(205, ['magnetrons'], ['yen'], 'i')
(196, ['mangosteen'], ['yin'], 'r')
(194, ['mangosteen'], ['rin'], 'y')
(198, ['ransoming'], ['teeny', 'yente'], '')
(197, ['ransoming'], ['eyen', 'eyne'], 't')
(196, ['morganites'], ['yen'], 'n')
(194, ['morganites'], ['ye'], 'nn')
(197, ['strongman'], ['eyen', 'eyne'], 'i')
(196, ['strongman'], ['yin'], 'ee')
(198, ['agrimony'], ['sennet'], '')
(197, ['agrimony'], ['nenes'], 't')

(takes about 13s on my machine)

$ solver.py o2l d4w a e m e3w o2l e i k h f e2l g
(237, ['geekdom'], ['foh'], 'aeei')
(236, ['geekdom'], ['feh'], 'oaei')
(204, ['homaged'], ['kief'], 'oee')
(204, ['homaged'], ['keef'], 'oei')
(191, ['hooked'], ['feme'], 'gaei')
(191, ['hooked'], ['image'], 'fee')
(180, ['haemoid'], ['keef'], 'goe')
(179, ['haemoid'], ['kef'], 'goee')
(180, ['defame'], ['hook'], 'geei')
(180, ['defame'], ['gook'], 'heei')
(179, ['geeked'], ['homie'], 'foa')
(179, ['geeked'], ['homo'], 'faei')
(180, ['hoofed'], ['mike'], 'gaee')
(180, ['hoofed'], ['kame', 'make'], 'geei')
(184, ['goofed'], ['hakeem'], 'ei')
(182, ['goofed'], ['hakim'], 'eee')
(180, ['defoam', 'foamed'], ['hoke', 'okeh'], 'geei')
(179, ['defoam', 'foamed'], ['hike'], 'goee')
(168, ['hoked'], ['fogie'], 'maee')
(168, ['hoked'], ['omega'], 'feei')

(takes about 21s on my machine)

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
        # When we run on Windows using PortablePython, the window
        # closes as soon as the script is done executing.  So, use the
        # --spin option so that after it runs, the script will hang so
        # we can read the output before closing the window
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
    """
    Words holds the word list.  For each word it reads, it sorts the
    letters and stores the words based on that key, so it's easy to
    find anagrams.

    Of course, a Trie would be better, but it's a bit of a hassle to
    implement and this is good enough.
    """
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
    """
    A Tile is a letter with a score and an optional multiplier (either
    a letter multiplier or a word multiplier)

    A Tile knows how to compare itself to other Tiles and how to
    create itself from a string representation.

    """
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
        'v': 6,
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
        self.lvalue = (self.LETTER_VALUES[self.letter]
                       if lvalue is None else lvalue)
        super(Tile, self).__init__()

    @property
    def value(self):
        return self.lvalue * self.letter_mult

    def __cmp__(self, other):
        """
        When we compare two Tiles, we are trying to pick the tile which we
        would prefer to use.  For example, any Tile with a word
        multiplier comes before any Tile without a word multiplier.
        """
        if isinstance(other, str):
            return cmp(self.letter, other)
        tcmp = cmp(type(self), type(other))
        # If these aren't the same type, just return abitrarily (but
        # consistently) based on the type name
        if tcmp != 0:
            return tcmp
        wcmp = cmp(other.word_mult, self.word_mult)
        # If they have word multipliers, compare based on that
        if wcmp != 0:
            return wcmp
        vcmp = cmp(other.value, self.value)
        # Otherwise, compare based on value
        if vcmp != 0:
            return vcmp
        # If the value and multipliers are the same, return
        # arbitrarily (but consistently) based on letter.
        return cmp(self.letter, other.letter)

    @classmethod
    def from_str(cls, string):
        """
        Example strings include:

        "s2l" = the letter S with a letter_mult of 2

        "a3w" = the letter A with a word_mult of 3

        "i" = the letter I with no letter or word mult

        """
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
    """
    A TopNQueue is a structure which holds data elements.  Each
    element has a value.  You can add a bunch of elements (with their
    corresponding values), and later you can ask for the elements with
    the top N highest values (in order of value).  Any elements after
    the top N will be thrown away.

    By default, if two elements have the same value, they take up two
    spots in the top N.  But if by_value is true, then it's really the
    top-N values, not the top-N elements, so if multiple elements have
    the same value, they only take up 1 spot out of the top-N spots.

    When you iterate through the structure, you get back tuples of
    (value, element).  If by_value is true, then you get back tuples
    of (value, [element, element, ...]).

    """
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
        # Walk through the data to see where this element should go
        while ii < length and value < self.data[ii][0]:
            ii += 1
        # If this element is outside the top N elements, skip it
        if ii >= self.topn:
            return
        # Otherwise, insert the element, depending on how we are
        # storing the data
        if self.by_value:
            if ii < length and self.data[ii][0] == value:
                self.data[ii][1].append(data)
            else:
                self.data.insert(ii, (value, [data]))
        else:
            self.data.insert(ii, (value, data))
        # If we now have too many elements, throw away everything
        # outside the top N
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
        """
        Given a list of tiles as strings, create Tile objects.  Returns
        them sorted in the order in which we would prefer to use them
        (i.e. high value tiles come first).

        """
        return sorted(Tile.from_str(a) for a in strings)

    @classmethod
    def contains(cls, word, tiles):
        """
        Can we make this word using only the given tiles (doesn't have to
        use all the tiles)?

        @return (match, used, remaining)

        match is a boolean which is True if we have the tiles to make this word

        used are the tiles that were used

        remaining are the tiles that were not used

        This works by checking to see if we have a tile for the first
        letter in the word, then calling ourselves recursively on the
        rest of the word without that tile.

        This function guarantees that it will use the Tiles in the
        order of the Tile list -- As long as the Tile list is in order
        of the preference in which we want to use the Tiles, the used
        list we return will pick the best tiles to use for this word.

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
            (val, used, remaining) = cls.contains(word[1:],
                                                  tiles[:idx] + tiles[idx+1:])
            return (val, [tiles[idx]] + used, remaining)
        except ValueError:
            return (False, [], tiles)

    @classmethod
    def subset(cls, words, tiles):
        """
        Given a list of words and tiles, returns an iterator of all the
        words that can be made with these tiles.  Returns them in the
        order of the wordlist.
        """
        for word in words:
            (match, used, remaining) = cls.contains(word, tiles)
            if match:
                yield (word, used, remaining)

    @classmethod
    def score_word(cls, word, tiles):
        """
        Given a word and a set of tiles, compute the best score you can
        have by creating that word with those tiles.
        """
        (match, used, remaining) = cls.contains(word, tiles)
        if not match:
            return 0
        return cls.score(used)

    @classmethod
    def score(cls, used):
        """
        Given a list of tiles, computes the score you get from using all
        those tiles.
        """
        raw = sum(u.value for u in used)
        for u in used:
            raw *= u.word_mult
        return raw

    @classmethod
    def find_best(cls, words, tiles, topn=DEFAULT_TOPN):
        """
        Given a word list and a set of tiles, find the best N
        highest-scoring words that you can make with those tiles
        (don't have to use all the tiles)

        Returns a TopNQueue where each element in the Queue is:

          (word, remaining_tiles)

        """
        best = TopNQueue(topn)
        for (word, used, remaining) in cls.subset(words, tiles):
            score = cls.score(used)
            logging.debug("%s %s", word, score)
            best.insert(score, (word, remaining))
        return best

    @classmethod
    def find_two_best(cls, words, tiles, topn=DEFAULT_TOPN):
        """
        Given a word list and a set of tiles, find the best N
        highest-scoring set of two words that you can make with those
        tiles (don't have to use all the tiles)

        Returns an iterator where each element is a tuple:
          (score, list-of-first-words, list-of-second-words,
           string-of-remaining-letters)
        """
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
    """
    Runs all the doctests in this module
    """
    import doctest
    doctest.testmod(verbose=True)

# --------------------------------------------------------------------------- #

if __name__ == "__main__":
    """
    When we run on Windows using PortablePython, if there is an
    exception, it will close the window and we won't see the error.
    Instead, catch exceptions, print the stack trace, and spin forever
    so we can read the error.
    """
    try:
        main()
    except Exception as e:
        import traceback
        traceback.print_exc()
        while True:
            pass

# --------------------------------------------------------------------------- #
