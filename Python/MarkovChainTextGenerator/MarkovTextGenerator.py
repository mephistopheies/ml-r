# coding=utf-8
from optparse import OptionParser
import urllib2
from nltk.tokenize import RegexpTokenizer
from collections import deque
import cPickle
import random
import chardet
import sys

class TrieNode:
    childs = None
    value = None
    parent = None

    def __init__(self):
        self.childs = {}
        self.value = 0
        pass


class Trie:
    root = None

    def __init__(self):
        self.root = TrieNode()
        self.root.value = 1

    def GetNode(self, key, addIfNotExist=False):
        cur_node = self.root
        for w in key:
            if not cur_node.childs.has_key(w):
                if not addIfNotExist:
                    return None
                cur_node.childs[w] = TrieNode()
                cur_node.childs[w].parent = cur_node
            cur_node = cur_node.childs[w]
        return cur_node


class MarkovTextGenerator:
    _order = 1
    _trie = None
    _isTrained = False

    def __init__(self, n):
        if n < 1:
            n = 1
        self._order = n
        self._trie = Trie()

    def Train(self, urls):
        for url in urls:
            req = None
            try:
                req = urllib2.urlopen(url)
            except:
                print "ERROR: in processing " + url
            if req == None:
                continue
            q_tokens = deque()
            q_ngram = deque()
            rt = RegexpTokenizer(r'\w+')
            for line in req:
                q_tokens.extend(
                    map(
                        lambda s: s.lower(),
                        rt.tokenize(line.decode(chardet.detect(line)['encoding']))
                    )
                )
                if len(q_tokens) == 0:
                    continue
                while len(q_ngram) < self._order - 1 and len(q_tokens) > 0:
                    q_ngram.append(q_tokens.popleft())
                if len(q_ngram) < self._order - 1:
                    continue
                while len(q_tokens) > 0:
                    q_ngram.append(q_tokens.popleft())
                    ngram = tuple(q_ngram)
                    q_ngram.popleft()
                    self._trie.GetNode(ngram, True).value += 1
                    if self._order > 1:
                        self._trie.GetNode(ngram[:(self._order - 1)]).value += 1
            req.close()
        if len(self._trie.root.childs) == 0:
            print "ERROR: no data collected"
            return
        q_nodes = [self._trie.root]
        cur_node = None
        if self._order == 1:
            self._trie.root.value = sum([node.value for node in self._trie.root.childs.itervalues()])
        while len(q_nodes) > 0:
            cur_node = q_nodes.pop()
            if len(cur_node.childs) > 0:
                q_nodes.extend(cur_node.childs.values())
                continue
            cur_node.value = float(cur_node.value)/cur_node.parent.value
        self._isTrained = True

    def ConvertTokensToString(self, tokens):
        return reduce(lambda s1, s2: s1 + ' ' + s2, tokens)

    def Serialize(self, fname):
        if not self._isTrained:
            print "ERROR: can't save not trained model"
            return
        try:
            with open(fname, "wb") as writer:
                cPickle.dump(self, writer)
                writer.close()
        except Exception as e:
            print "ERROR: while serialization: " + e.message

    @staticmethod
    def Deserialize(fname):
        try:
            with open(fname, 'rb') as reader:
                obj = cPickle.load(reader)
                reader.close()
                return obj
        except Exception as e:
            print "ERROR: while deserialization: " + e.message
            return None

    def Sample(self, d):
        r = random.random()
        cum_sum = 0
        for w in d.iterkeys():
            cum_sum += d[w].value
            if r < cum_sum:
                return w

    def SequenceGenerator(self, sequence_head, n):
        if len(sequence_head) >= self._order - 1:
            tail = deque()
            if self._order == 1:
                tail.extend(sequence_head[-1:])
            else:
                tail.extend(sequence_head[-(self._order - 1):])
            for i in range(n):
                node = self._trie.root if self._order == 1 else self._trie.GetNode(tuple(tail))
                tail.popleft()
                if node == None:
                    break
                tail.append(self.Sample(node.childs))
                yield tail[len(tail) - 1]



if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option('-m', '--mode', action="store", choices=['train', 'generate'], dest="mode",
                      help="mode: train or generate")
    parser.add_option('-o', '--order', action="store", type="int", dest="order",
                      help="order of Markov chain (only in train mode)")
    parser.add_option('-f', '--file', action="store", type="string", dest="file", default='dump.cPickle',
                      help="file name to store trained chain in train mode or restore in generate mode")
    parser.add_option('-u', '--url', action="append", type="string", dest="urls",
                      help="url which contains text, can be set multiple times (only in train mode)")

    parser.add_option('-w', '--word', action="append", type="string", dest="words",
                      help="beginning of sentence (only in generate mode)")
    parser.add_option('-s', '--size', action="store", type="int", dest="size",
                      help="size of generated sequence (only in generate mode)")

    (options, arguments) = parser.parse_args()
    if options.mode is None:
        print "ERROR: mode is not set"
        sys.exit(0)
    if options.mode.lower() == "train":
        if options.order is None or options.urls is None or options.file is None:
            print "ERROR: not all parameters defined"
            sys.exit(0)
        m = MarkovTextGenerator(options.order)
        m.Train(options.urls)
        m.Serialize(options.file)
    elif options.mode.lower() == "generate":
        if options.size is None or options.words is None:
            print "ERROR: not all parameters defined"
            sys.exit(0)
        m = MarkovTextGenerator.Deserialize(options.file)
        if m is not None:
            start = map(lambda w: w.decode(chardet.detect(w)['encoding']), options.words)
            if options.size < 1:
                options.size = 10
            s = m.SequenceGenerator(start, options.size)
            for w in s:
                start.append(w)
            print m.ConvertTokensToString(start)
    else:
        print "ERROR: incorrect mode"
    print 'done!'
