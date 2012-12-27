#!/usr/bin/env python
# -*- python -*-
# Time-stamp: <2007-04-30 19:47:25 rozen>



"""
Python dot expression completion using Pymacs.

This almost certainly needs work, but if you add

    (require 'pycomplete)

to your .xemacs/init.el file (untried w/ GNU Emacs so far) and have Pymacs
installed, when you hit M-TAB it will try to complete the dot expression
before point.  For example, given this import at the top of the file:

    import time

typing "time.cl" then hitting M-TAB should complete "time.clock".


This is unlikely to be done the way Emacs completion ought to be done, but
it's a start.  Perhaps someone with more Emacs mojo can take this stuff and
do it right.

This has been modified to use patches I found on the web. I then further
modified it to put "()" after the completion if it is a callable object. Rozen.

See pycomplete.el for the Emacs Lisp side of things.

This is typical of software, almost no documentation. Rozen
"""

import os
import os.path
import string
import re

from Pymacs import lisp

def get_all_completions(s, imports=None):
    """Return contextual completion of s (string of >= zero chars).

    If given, imports is a list of import statements to be executed first.
    """

    locald = {}
    if imports is not None:
        for stmt in imports:
            try:
                #exec stmt in globals(), locald
                exec stmt
            except TypeError:
                raise TypeError, "invalid type: %s" % stmt

    dots = s.split(".")

    if not s or len(dots) == 1:
        # I think that this is the code executied when either s is the
        # null string or when we are interested in builtins, i.e., no
        # periods in the string s.  Basically, I don't understand this
        # code. This may also be the place where 'import x as y' is
        # handled because then name are like builtins.
        keys = set()
        keys.update(locald.keys())
        keys.update(globals().keys())
        import __builtin__
        keys.update(dir(__builtin__))
        keys = list(keys)
        keys.sort()
        if s:
            obj_name = ''
            list_of_completions = [k for k in keys if k.startswith(s)]
            list_c = []
            for c in list_of_completions:
                if callable(eval(obj_name+c)):
                    list_c.append(c + '()')
                else:
                    list_c.append(c)
            return list_c

        else:
            return keys

    # This seems to be the more interesting case.
    sym = None
    for i in range(1, len(dots)):
        s = ".".join(dots[:i])
        try:
            # Essentially this determines whether the module named has been
            # imported.
            #sym = eval(s, globals(), locald)
            sym = eval(s)
        except NameError:
            print "in NameError"
            try:
                sym = __import__(s, globals(), locald, [])
            except ImportError:
                print 'spot b'
                return []

    if sym is not None:
        s = dots[-1]
        # I need this for checking if it is callable.  This constructs the
        # obj_name even in the case of 'os.path', i.e, multiple path qualifiers.
        obj_name = string.join(dots[:-1],'.')+'.'
        # dir(sym) returns the attributs of the sym object and the k.startwith
        # selects those attributs beginning with specified leading characters.
        list_of_completions = [k for k in dir(sym) if k.startswith(s)]
        list_c = []
        for c in list_of_completions:
            if callable(eval(obj_name+c)):
                list_c.append(c + '()')
            else:
                list_c.append(c)
        return list_c

def pycomplete(s, imports=None):
##     print 'pycomplete: os.getcwd() =', os.getcwd()    # rozen   pyp
##     '''There seems to be problem when one has an import of the form:
##        'import x as y'.
##        Will have to also check things like
##        'from x import y'.
##        In fact that may be the same situation.
##        But
##        'from x import *'
##        will give me fits.
##        Also what to do about
##        'import x, y' ?  My way of treating this stuff is to build a
##        dictionary between the name used and the name in the import
##        statement. This in needed in the import x as y case.  To make
##        it all encompassing, in most cases the name and the key will be
##        the same. I still don\'t know what to do about the \'from x
##        import *\'.'''
##     dict = {}
##     if imports:
##         import_as_patt = r'import[ ]*([a-zA-Z0-9_.]*)[ ]*as[ ]*([a-zA-Z0-9_.]*)'
##         import_from_patt = r'from[ ]*([a-zA-Z0-9_.]*)[ ]*import[ ]*([a-zA-Z0-9_.]*)'
##         import_patt = r'import[ ]*([a-zA-Z0-9_.]*$)'
##         import_many_patt = r'import[ ]*([a-zA-Z0-9_., ]+)'
##         import_as_r = re.compile(import_as_patt)
##         import_from_r = re.compile(import_from_patt)
##         import_r = re.compile(import_patt)
##         import_many_r = re.compile(import_many_patt)
##         for imp in imports:
##             # import x as y
##             mo = import_as_r.search(imp)
##             if mo != None:
##                 dict[mo.group(2)] = mo.group(1)
##                 continue
##             # from x import y
##             mo = import_from_r.search(imp)
##             if mo != None:
##                 # NEEDS WORK
##                 dict[mo.group(2)] =  mo.group(1) + '.' +mo.group(2)
##                 continue
##             #import x
##             mo = import_r.search(imp)
##             if mo != None:
##                 dict[mo.group(1)] = mo.group(1)
##             # import x,y
##             mo = import_many_r.search(imp)
##             if mo != None:
##                 imp_list = mo.group(1).split()
##                 for name in imp_list:
##                     dict[name] = name

##     completions = get_all_completions(s, dict, imports)
    completions = get_all_completions(s, imports)
    if len(completions) == 0:
        return None
    dots = s.split(".")
    prefix = os.path.commonprefix([k for k in completions])
    if len(completions)==1 or len(prefix)>len(dots[-1]):
        return [prefix[len(dots[-1]):]]

    return completions


if __name__ == "__main__":
    '''For debugging try "setenv PYTHONPATH /home/rozen/python/pycomplete/"
    '''

    import sys
    print "<empty> ->\n", pycomplete("")
    print
    print "sys.get ->\n", pycomplete("sys.get")
    print
    print "sy ->\n", pycomplete("sy")
    print
    print "sy (sys in context) ->\n", pycomplete("sy", imports=["import sys"])
    print
    print "foo. ->\n", pycomplete("foo.")
    print
    print "Env (email * imported) ->\n",
    print
    print pycomplete("Env", imports=["from email import *"])
    print
    print "E (email * imported) ->\n",
    print
    print pycomplete("E", imports=["from email import *"])

    print "Enc ->\n", pycomplete("Enc")
    print "E ->\n", pycomplete("E")
    print
    print "sys. ->\n", pycomplete('sys.')
    print
    print "ope ->\n", pycomplete('ope')
    print
    print "os.path. ->\n", pycomplete('os.path.')
    print
    print "os.path.is ->\n", pycomplete('os.path.is')
    print
    print "re.c ->\n", pycomplete('re.c', ['import re'])
    print
    print "re. ->\n", pycomplete('re.', ['import re'])
    print
    print "re. ->\n", pycomplete('re.', ['import sys', 'import re'])
    print
    print "re. ->\n", pycomplete('re.', ['import re'])
    print
    print "re.c->\n", pycomplete ("re.c", ["import sys", 'import re'])
    print
    try:
        print "re.c->\n", pycomplete ("re.c", ["import sys"])
    except:
        print "Example fails because 're' has not been imported."
        pass
    print
    print "new_chart.dr->\n", pycomplete ("new_chart.dr", ["import os",
         "import sys", "import time", "import new_chart",
          "import new_chart as chart", "import utils"])
    print
    # Worry about this.  It seem that
    print "chart.dr->\n", pycomplete ("chart.dr", ["import os",
         "import sys", "import time", "import new_chart",
          "import new_chart as chart", "import utils"])
    print
    print "re.comp->\n", pycomplete("re.comp", ["import sys", "import os, re"])
    print
    print "colors.->\n", pycomplete("colors.", ["import sys", "import os, re",
                                            "from reportlab.lib import colors"])
# Local Variables :
# pymacs-auto-reload : t
# End :
