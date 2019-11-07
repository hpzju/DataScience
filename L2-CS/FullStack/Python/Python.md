---
id: Python
title: Python a programming language.
sidebar_label: Python
---

## Overview

- Bytecode compiled interpreted language
  - `.py` to `.pyc`
  - `.pyo` optimized bytecode file
- Dynamic Typing
- OOP
- FP
- PEP20
  - beautiful is better than ugly
  - explicit is better than implicit
  - simple is better than complex
  - complex is better than complicated
  - readability counts

### Features

- Open source
- Platform independent
- Modular
- Flexible

### Applicable Scenarios

- Game Dev
- Data Science
  - RDMS Interface
  - Scientific Computation
- Sys DevOps
  - Environment variables
  - File System
    - zip, XML, JSON, CVS
  - Network socket
  - Process and Ehread
  - RE
  - Pipeline
  - CLI
- Web DevOps
  - Web Scraping
  - Web Page Parsing
  - Web Devs
    - Djongo, Flask
- System Glues
  - C/C++ integration

### Python Interpreter Implementation

- Python
  - PyPy, JIT python interpreter implementation
- C
  - CPython, the default interpreter
- Java
  - Jython
- .NET
  - IronPython

### Data Model

- Types Hierachy

  - Numbers
    - Intergers: bool, int
    - float
    - Complex
    - Decimal
    - Fraction
  - Collections
    - Sequences
      - Mutable
        - list
        - bytearray
      - Immutable
        - string
        - bytes
        - tuple
    - Sets
      - set
      - frozenset
    - Mappings/Hash Tables
      - dict
  - Callables
    - function
      - built-in
      - customized
    - generator
    - class
      - method
        - bound
        - unbound
      - class instance(`__call__()`)
  - Misc
    - Module
    - Instance
    - File
    - None
    - View
  - Internals
    - Type
    - Code
    - Frame
    - Traceback
  - Singletons
    - interning
      - [-5, 256] integer
    - string interning
      - variable names
      - function names
      - class names
      - some string literal
        - similar to identifier, `hello_world`
    - None
    - True/False
    - NotImplemented
    - Ellipsis `...`

- Object

  - everything in python is an object
  - First-Class Object
    - can be used as argument
    - can be returned from function
    - can be assigned to variable
    - can be stored in a data structure

- Variables and Memory Management

  - variables
    - variables are memory address space symbolic representation
    - variables memory space is heap
    - `id(var)` returns variable's memory space reference.
  - reference counting
    - `sys.getrefcount(var)` function arg passing increase counter
    - `ctypes.c_long.from_address(ADDRESS).value`
    - circular reference
      - gc module resolve it python>3.4
  - garbage collection
    - `import gc`
  - dynamic/static typing
    - type info is in reference object, not in variable
  - mutable/immutable
    - object's internal state can be changed or not
    - immutable object's address id and space cannot be changed.
    - mutable object's address id cannot be change, but address sapce can be changed.
  - shared reference
    - more than one variables reference to the same object in memeory
    - immutable can use shared reference
    - mutables don't use shared reference
    - copy and deepcopy
      - `import copy; copy.copy(obj); copy.deepcopy(obj)`
  - variable equality
    - identity:
      - `a is b`, point to same object in memory.
    - data equality
      - `a == b`, variables' address spaces(or partial of address space) bits/bytes are the same

- Optimizatioin

  - interning
  - Peephole
    - `myfunc.__code__.co_consts`
    - Constant expression pre calculated.
      - numeric calculation
      - short strings
    - Membership tests: `in, not in`
      - mutable changed to immutable
  - constant folding
    - constant expressions are compiled before excution.

### Execution Model

- Source Code
- Byte code
- PVM Runtime
  - eval(), exec() built-in

---

---

## Python Language Basic

<!-- ### Building Blocks -->

### Variables, Namespace and Scope

- Variables Identifier

  - `/[a-zA-Z_][a-zA-Z0-9_]*/`
  - keywords
    - `import keyword; print(keyword.kwlist)`
  - naming convension
    - `_var` interal use
    - `__var` name mangled
    - `__var__` python language use
    - `package` lowercase, short, no underscore
    - `module` lowercase, short, no underscore
    - `Classes` camelcase
    - `my_func` snakecase, underscore to seperate words
    - `my_var` snakecase
    - `CONSTANT` uppercase, uderscore to seperate words

- Variable Declaration

  - Python has no varaible decalaration
  - Variables were created when new identifier was assigned
  - `id()` identifies variables
  - `A is B` tests two varaibles' equality of identity.

- Namespace

  - a collection of names, maintains identifier and it's value mapping relation.
  - list namespace variables
    - `dir(NAME_SPACE)`
  - namespace created within:
    - built-in namespace
    - moudle global namespace
    - block local namespace
      - function local namespace
      - class local namespace
        - `Class.__dict__`
      - instance local namespace

- Scope

  - an execution context, determins which namespaces and identifiers are available in current context.
  - Lexical scoping name resolution LEGB rule
    - names find by local -> enclosing -> Global -> Built-in order.
    - not applicable for name's new binding.
  - local scope
    - varaibles inside a function local scope are not created until function called.
    - a new scope created every time a function called.
    - `locals()`
    - `UnboundLocalError`
      - within local scope, try to assign value to a name in global namespace
    - global variable reference statement
      - `global variable_list`
  - nonlocal/enclosing scope
    - it will not look global scope
    - innner funnction access outer local variables scope chain for variable reference
      - `nonlocal variable_list`
      - `global variable_list`
  - global scope
    - `globals()`
  - built-in scope
    - `__builtins__`
    - `import builtins; dir(builtins)`
  - list current scope names
    - `dir()`
    - `import builtins; pylookup = ChainMap(locals(), globals(), vars(builtins))`

---

### Types

#### Types Introduction

- Reference

  - refer by reference
  - type lives with object/value, not variable

- Mutability

  - mutable
  - imutable

- type() function

  - return Type Object
    - `NoneType`
    - `bool`
    - `int, float, complex, decimal.Decimal, fractions.Fraction`
    - `str, bytes, bytearray`
    - `list, tuple`
    - `set, frozenset`
    - `dict`
    - `function, generator, range`
    - `type`
    - `__main__.MyObject`
    - `module`

- type conversion/coercion

  - explicit type constructor conversion
  - no implicit type coercion, except boolean expression

- type hint

  - `def addnum(a: int, b: int) -> int:`

- duck type
  - type was determined at runtime.

---

#### None

- `None`
- `type(None)` returns `NoneType`

#### Boolean

- Introduction

  - immutable type
  - refer elements by value
    - reference by counter implicitly
  - Falsey values
    - `0, 0.0, 0j, None, "", [], (), {}`
  - logical operators
    - `not, and, or`

- Practices

  ```python
    """ boolean literal """
    b = True
    b = False

    """ boolean explicit/implicit constructor """
    b = bool(value)
  ```

---

#### Numbers

- Introduction

  - immutable type
  - refer by value
    - very small interger reference by counter
  - categories
    - `int, float, complex, fractions.Fraction, decimal.Decima`
  - arithmetic operators
    - `+, -, *, /, //, %, **`
  - comparison operators
    - `>, >=, <, <=, ==, !=`
  - functions
    - `math, cmath module functions`

- Practices

  ```python
    import numbers
    numbers.Number
    numbers.Integral
    numbers.Rational
    numbers.Real
    numbers.Complex

    """int"""
    # int literal
    i = 1

    # int explicit/implicit constructor constructor
    i = int(val)
    '0x', '0o', '0b'
    hex(i), oct(i), bin(i)

    """float"""
    # float literal
    f = 1.0

    # float explicit/implicit constructor constructor
    f = float(val)

    """complex"""
    # complex literal
    c = 1+0J
    c = 5J

    # complex explicit/implicit constructor constructor
    c = complex(val)
    c = complex('1+2J') # no space required
    c = complex(re, im)

    """Fraction"""
    import * from fractions
    # Fraction literal

    # Fraction explicit/implicit constructor constructor
    """
      class fractions.Fraction(numerator=0, denominator=1)
      class fractions.Fraction(other_fraction)
      class fractions.Fraction(float)
      class fractions.Fraction(decimal)
      class fractions.Fraction(string)
    """

    r = Fraction()
    r = Fraction(16, 3)
    r = Fraction(2.25)
    r = Fraction(Decimal('1.1'))
    r = Fraction('3/7')

    # Fraction Class methods/properties
    r.numerator
    r.denominator

    """Decimal"""
    import * from decimal
    getcontext().prec = 12

    # Decima literal

    # Decima explicit/implicit constructor constructor
    """
      class decimal.Decimal(value="0", context=None)
      value can be an integer, string, tuple, float, or another Decimal object
      tuple: a sign (0 for positive or 1 for negative), a tuple of digits, and an integer exponent
    """
    d = Decima(3)
    d = Decima('3.14')
    d = Decima(3.14)
    d = Decima((0, (3, 1, 4), -2))
    d = Decima(Decimal('3.14'))

    from decimal import localcontext

    with localcontext() as ctx:
      ctx.prec = 6
      do_decimal_computation
  ```

---

#### Strings: unicode text

- Introduction

  - unicode string type: `str`
  - immutable
  - unicode code points array data structure
  - formatting
    - f-string formatting
      - computing value
      - function call value
    - float formatting
      - {[value-ref]:[width].[precision][f-flag]}
      - `[<>^]?width[,]?(.precision)?[ef]?`
    - [alignment formatting](https://www.python.org/dev/peps/pep-3101/#standard-format-specifiers)
      - {[value-ref]:[padding][align-flag][width]}
      - align-flag
        - `<`: left
        - `^`: center
        - `>`: right
    - [[fill]align][sign][#][0][width][,][.precision][typecode]

- Practices

  ```python
    # string literal
    s = "string literal"
    s = 'string literal'
    s = ''' string literal with
            (indented)multiple lines'''
    s = """ string literal with
    (no indentation)multiple lines"""
    s = r"C:\number\nums"
    s = "C:\\number\\nums

    # string explicit/implicit constructor
    """
      class str(object='')
      class str(object=b'', encoding='utf-8', errors='strict')
      return object.__str__(), if no __str__() method, then str() falls back to returning repr(object)
      if encoding specified, object must be a bytes object.
    """
    s = str(s1)
    s = str(b"bytes literal", encoding='utf-8')

    # string Class methods/properties
    # formatting
    """formatting pattern: /:[<|>][/d]*[b|o|x]/"""
    "{0} placeholder {1}".format(var1, var2)
    "{name} is {age} years old".format(age = 10, name = "Jane")
    "position tag: {0} and {1}, named tag: {name}".format(2, 5, name = "Jane")
    "{:<{len}} align left with width {len}".format("string", len=10)
    "{:>{len}} align right with width {len}".format("string", len=10)
    "{:>{len}b} binary align right with width {len}".format(32, len=10)
    "{:>{len}o} binary align right with width {len}".format(32, len=10)
    "{:>{len}x} hex align right with width {len}".format(32, len=10)

    # f-str __str__()
    f"Hello, {name}. You are {age}."
    f"{name.lower()} is funny."
    f"The comedian is {comedian['name']}, aged {comedian['age']}."
    message = f"""
    Hi {name}.
    You are a {profession}.
    You were in {affiliation}.
    """
    # f-str __repr__()
    f"{new_comedian!r}"

    dir(s)

    # indexing/slicing
    newS = s[index]
    newS = s[-1]
    newS = s[start:end:step]
    newS = s[:end]
    newS = s[start:]

    # string manipulation
    index = str.find(substr, startPos)
    bool = str.startwith(substr, start, end)
    bool = str.endwith(substr, start, end)

    newS = str.strip(charColl)
    newS = str.lstrip(charColl)
    newS = str.rstrip(charColl)
    newS = str.center(width)
    newS = str.ljust()
    newS = str.rjust()

    newS = str.replace(targetsubstr,replacersubstr)
    strarr = str.split(splitCharCol)
    strarr = str.splitlines()
    strTuple = str.partition(partstr)
    newS = jointChars.join(str_seq)

    newS = str.capitalize()
    newS = str.lower()
    newS = str.upper()
    newS = str.title()
    newS = str.swapcase()

    # char manipulation
    bool = str.isalpha()
    bool = str.isalnum()
    bool = str.isdigit()
    bool = str.islower()
    bool = str.isnumeric()
    bool = str.istitle()
    bool = str.isspace()
    str.index(substr)
    number = str.count(substr)

    # transform to bytes
    b = str.encode("utf-8")

    # external str manipulation function/operator
    len(s)
    asciistr = ascii(s)
    codepoint =  ord(char)
    char = chr(codepoint)
    s = "s1" + " " + "s2"
    s = "repeate"*2
    char in s
    substr in str
    for char in s:
    eval(str_code)
    str(obj)
    repr(obj)

    import string

    string.ascii_lowercase
    string.whitespace
  ```

---

#### Strings: asciii binary

- Introduction

  - ascii string type: `bytes, bytearray`
    - value range: [0, 255]
  - immutable
    - `bytes`
  - mutable
    - `bytearray`
  - ascii code points array data structure

- Practices

  ```python
    # bytes literal
    s = b"bytes literal"
    s = b'bytes literal'
    s = b''' bytes literal with
            (indented)multiple lines'''
    s = b""" bytes literal with
    (no indentation)multiple lines"""
    s = rb"C:\number\nums"
    s = b"C:\\number\\nums

    # bytes explicit/implicit constructor
    """
      class bytes([source[, encoding[, errors]]]
      A zero-filled bytes object of a specified length: bytes(10)
      From an iterable of integers: bytes(range(20))
      Copying existing binary data via the buffer protocol: bytes(obj)
    """
    b = bytes(10)
    b = bytes(range(20))
    b = bytes(obj)

    # bytearray explicit/implicit constructor
    """
      class bytearray([source[, encoding[, errors]]]
      A zero-filled bytes object of a specified length: bytes(10)
      From an iterable of integers: bytes(range(20))
      Copying existing binary data via the buffer protocol: bytes(obj)
    """
    ba = bytearray()
    ba = bytearray(10)
    ba = bytearray(range(20))
    ba = bytearray(obj)

    # bytes and bytearray Class methods/properties
    # similar to str object
    # https://docs.python.org/3/library/stdtypes.html#bytes

    # transform to str
    str = b.decode("utf-8")

    # external str manipulation function/operator
    len(b)
    b = b"s1" + b" " + b"s2"
    b = b"repeate"*2
    char in b
    for char in b:
  ```

---

#### List

- Introduction

  - mutable type
  - array data structure
  - `==` compares contents of lists

- Practices

  ```python
    # list literal
    li = [1, 2, 4, "lsit"]
    li = []

    # list comprehension
    li = [x**2 for x in range(10)]
    li = [x**2 for x in range(10) if x%2 == 0]
    li = [x**2 if x > 5 else x**4 for x in range(10)]
    li = [(i, j) for i in range(10) for j in range(i)]

    # list constructor explicit/implicit
    li = list(range(1,10,2))
    li = list("string char list")

    # element manipulation methods
    li.insert(index, value)
    li.append(value)
    value = li.pop(index)
    li.remove(value)
    li.count(value)
    li.index(value)

    # __getitem__()
    # __setitem__()
    # __slice__()
    #__del__()
    li[index]
    li[-1]
    li[start:end:step]
    li[start:]
    li[:end]
    li[start::step]
    del li[index]
    del li[start:end:step]

    # list manipulation method
    li.sort()
    li.reverse()
    li.extend(iterable)
    li.clear()

    # external functions/operators
    newLi = li + li2
    newLi = li * 2
    newLi = sorted(li)
    item in li
    max(li)
    min(li)
    sorted(li)
    for item in li:
      do_staff_with_item

    # destructuring/unpacking
    a, b, c = [1, 3, 4]
    a, *rest = [1, 3, 4]

  ```

---

#### Tuple

- Introduction

  - immutable type
  - array data structure
  - packing and uppacking
  - named tuples

- Practices

  ```python
    # tuple literal
    t = (1, 2, 4, "lsit", )
    t = (3, )
    t = ()

    # tuple constructor explicit/implicit
    t = tuple(range(1,10,2))
    t = tuple([1,4,9,"er"])
    t = tuple("string char tuple")

    # tuple methods
    t.index(value)
    t.count(value)

    # __getitem__()
    # __slice__()
    t[index]
    t[-1]
    t[start:end:step]
    t[start:]
    t[:end]
    t[start::step]

    # element operation
    elem in t

    # external functions/operators
    newT = t * 2
    newT = t1 + t2
    item in t
    max(t)
    min(t)
    for item in t:
      do_staff_with_item

    # destructuring/unpacking
    a, b, c = (1, 3, 4)
    a, *rest = (1, 3, 4)
    a, b = b, a

  ```

---

#### Set/Frozenset

- Introduction

  - set is mutable type
  - frozenset is immutable type
  - Disjoint set data structure
  - no order
  - no duplication

- Practices

  ```python
    # set literal
    s = {1,2,3,4,"set",}

    # set comprehension
    s = {x**2 for x in range(10)}
    s = {x**2 if x > 5 else x**4 for x in range(10)}

    # set constructor explicit/implicit
    s = set()
    s = set([1, 2, 4, "lsit"])
    s = set(range(1,10,2))
    s = s1.copy()

    # set class methods/properties
    s.add(elem)
    s.update(iterable)
    s.clear()
    elem = s.pop()
    s.discard(elem)
    s.remove(elem) # KeyError if elem doesn't exist.

    # set operation
    s.clear()
    newS = s1.union(s2)
    newS = s1 | s2
    newS = s1.intersection(s2)
    newS = s1 & s2
    newS = s1.difference(s2)
    newS = s1 - s2
    newS = s1.symmetric_difference(s2)
    newS = s1 ^ s2
    newS = s1.union(s2) - s1.intersection(s2)

    bool = elem in s1
    bool = elem not in s1
    bool = s.issubset(s1)
    bool = s.issuperset(s1)

    # external functions/operators
    len(s)
    max(s)
    min(s)
    sum(s)
    sorted(s)
    for elem in S:
      do_stuff

    # frozenset
    fs = frozenset(s)

  ```

---

#### Dictionary

- Introduction

  - mutable type
  - orderless
  - RuntimeError if change dict object during iteration
  - key-value data structure
    - key must be immutable type
  - custome key missing behavior
    - `__missing__()`
    - memoization
      - transform a dict subclass into a function
  - dictionary comprehension
  - dictionary views
    - dict.keys(), dict.values(), dict.items()
    - set-like object

- Practices

```python
  # dict literal
  d = {'key' : 3, "lsit": [1,2,3]}

  # dictionary comprehension
  d = {x: x**2 for x in range(10)}
  d = {x: x**2 if x > 5 else x**4 for x in range(10)}

  # dict constructor explicit/implicit
  d = dict()
  d = dict(key1 = value1, key2 = "value2") # key1 will be "key1" string key
  d = dict([("key1", 1),("key2", 2),(3, "three")])

  # dict build method
  d = dict.fromkeys(key_iterable, value_iterable)
  d = d1.copy()

  # dict manipulation methods
  d.update(d1)
  d = d1.copy() # shallow copy, key copied, but value referenced.
  d.clear()

  # key-value manipulation methods
  keysObj = d.keys()
  valueObj = d.values()
  keyvalueObj = d.items()
  keyvalue_tuple = d.popitem()
  value = d[key]
  del d[key]
  value = d.get(key, default_value)
  value = d.setdefault(key, default_value)
  value = d.pop(key, default_value)

  # external functions/operators
  len(d)

  if key in d:
    do_stuff

  for key in d:
    do_stuff

  for value in d.values():
    do_stuff

  for k,v in d.items():
    do_stuff
```

---

### Function and FP

#### Functions

- Introduction

  - function decorator
  - function declaration
    - function docstring
      - `func.__doc__`
      - a string, at first line of code block.
    - function anotation/type hint
      - `def func(arg: <expr>, arg: <expr>,...) -> <expr>:`
      - `func.__anotations__`
    - function name
      - `func.__name__`
    - parameters and orders
      - parameter term used in function definition context
      - argument term used in function calling context
      - python arguments passed by reference
      - positional arguments
      - keyword/named arguments
        - args with default value, default value should be immutable, or side effect will hold
        - default arg evaluate only once when `def` func first run
        - when use mutable default value, cautious about each function call
        - use immutable default value is recommanded.
      - `def func(pos_arg, pos_arg_option = default_val, *args, kwarg_mandate, kwarg = val, **kwargs)`
        - after \*args, no positional args allowed, only keyword arguments allowed.
        - after \*\*kwargs, no arguments allowed.
      - argurments destructuring/unpacking
        - `func(*sequences)`
        - `func(**dicts)`
    - return value
      - return `None` by default
  - function scope
    - Built-in
    - Global
    - Enclosing/Nonlocal
    - Local
  - nested function and closure
    - inner functions can access outer scope chain variables.
    - `func.__closure__`
    - later-binding
      - enclosing variables's values passed to neseted function when it's been invoked.
  - lambda function
    - no assignment in lambda expression
    - no return statement in lambda expression
    - no annotation in lambda
  - introspection

    - `from inspect import Parameter, Signature`

    ```python
    import inspect
    dir(func)
    func.__name__
    func.__code__
        func.__code__.co_varnames
        func.__code__.co_argcount
        func.__code__.freevars
    func.__closure__
    func.__annotation__
    def func(a: 'spam' = 4, b: (1, 10) = 5, c: float = 6) -> int:
    ```

  - operators
    - `import operator`

- Practices

  ```python
  # function declarition
  def funcname(arg: 'annotation for arg', defarg1 = defvalue : int,
              *args: tuple, kwarg1: 'kw mandatory arg', **kwargs: 'keyword args') -> "return value annotation":
    """function description docstring with
    multiple lines"""
    func_body
    return expression

  #lambda function
  lambda comma_express_argslist: function_body_expression
  named = lambda comma_express_argslist: function_body_expression

  # introspection
  import inspect
  inspect.isfunction(obj)
  inspect.ismethod(obj)
  inspect.isroutine(obj)
  code = inspect.getsource(myfunc)
  comment = inspect.getcomment(myfunc)
  inspect.signature(myfunc)

  # operator
  import operator
  add(a, b)
  mul(a, b)
  pow(a, b)
  mod(a, b)
  floordiv(a, b)
  neg(a)
  lt(a, b)
  le(a, b)
  gt(a, b)
  ge(a, b)
  eq(a, b)
  ne(a, b)
  is_(a, b)
  is_not(a, b)
  truth(a)
  not_(a, b)
  and_(a, b)
  or_(a, b)
  concat(s1, s2)
  contains(s, val)
  countOf(s, val)
  getitem(s, i)
  setitem(s, i, val)
  delitem(s, i)
  f = itemgetter(i, i2, ...)
  f(s)
  f = attrgetter(prop, prop2,...)
  f(obj)
  f = methodcaller(meth, args_kwargs)
  f(obj)

  ```

#### Function Programming

- Introduction

  - Paradigm
    - Functional Programming vs OOP
    - Pure Function vs Impure Function
  - Features
    - Formal provability
    - Modularity
    - Composability
    - Lazy evaluation
    - high-order function
    - no flow-control statements
    - support OOP encasulation, inheritance, polymophism, abstraction
  - Terminology
    - function signature/function prototype
    - purely functional
      - no side effect on input
    - First Order Function
      - stored as variable
      - passed as parameter
      - returned as result
      - created at runtime

- Functors

  - Decorator

    - a type of HOF, high order function
    - take a function as argument, return a closure, the closure calls the original function.
    - wrap a function into decorator, simplify interface design and lower the exposure surface.
    - use self-defined wrap function will change original function's name and docstring
      - use decorator `functools.wraps` will solve
      - @wraps copies metadata: name, docstring, function attributes.
    - categories
      - nested decorators
      - parameterized decorators
      - stacked decorators
      - class decorators
        - metaclass
      - function decorator
        - metafunction
        - delegation design patter
      - decorator classed

    ```python
    import functools

    # function decorator
    def mydecorator(func):
      @functools.wraps(func)
      def mywrapper(*args, **kwargs)
        value = func(*args, **kwargs)
        manipulate_value_accroding_design_purpose
        return something_unified_format
      return mywarpper

    # decorator syntax
    @mydecorator
    def myfunc(*args, **kwargs):
      func_body_statements
      return value

    # Class decorator
    class MyClassDecorator(func):
      def __init__(self, func)
        self._f = func
        self._states = []

      def __call__(self, *args, **kwargs)
        value = self._f(*args, **kwargs)
        manipulate_value_accroding_design_purpose
        update_states_if_neccessary
        return something_unified_format

    # decorator syntac
    @MyClassDecorator
    def myfunc(*args, **kwargs):
      func_body_statements
      return value

    # Instance decorator
    class MyObjDecorator():
      def __call__(self, func, *args, **kwargs)
        @functools.wraps(func)
        def mywrapper(*args, **kwargs)
          value = func(*args, **kwargs)
          manipulate_value_accroding_design_purpose
          return something_unified_format
        return mywarpper

    # decorator syntac
    myobjdecorator = MyObjDecorator()
    @myobjdecorator
    def myfunc(*args, **kwargs):
      func_body_statements
      return value

    # multiple decorator syntac
    """order matters"""
    @decorator1
    @decorator2
    @decorator2
    def myfunc(*args, **kwargs):
      func_body_statements
      return value
    ```

  - Iterator

    - iterator protocol requires `__iter__()` to return a special iterator object that implements a `__next__()` method to carry out the actual iteration
    - traverse a collection with simple iterator protocal.
    - cosumable, once exhausted, it's disposed.
    - two categories of iterator objects
      - a sequence iterator, works with an arbitrary sequence supporting the `__getitem__()` method.
      - a callable object and a sentinel value, calling the callable for each item in the sequence, and ending the iteration when the sentinel value is returned
    - iterable object, iteration context and iterator
      - iteration context has next protocal, `val = next(iter_ctx); iter_ctx.__next__()`
      - iterable, collection of data with iterable protocol, `iterator = obj.__iter__()`
      - iterator, traverse over iterable data with iterator protocol, `__iter__(), __next__()`
      - iterator is iterable, bad practice to combine iterator with iterable into one object
    - Lazy evaluation
    - single/multiple pass iterator

    ```python
    # iterator protocol: for-loop, comprehensions
    iterator = iterator.__iter__()
    iterator.__next__()
        raise StopIteration()
    value = next(iterator)
    selfiter = iter(iterator)
    # or with if no __iter__()
    iterator.__len__()
    iterator.__getitem__(index)
        raise IndexError()
    value = next(iterator)
    iterator = iter(iterator)

    # iterable object protocol
    iterator = itrerableObj.__iter__()

    # iterable functor protocol
    iterator = iter(functor, sentinel)

    # iterable statements
    for item in iterator:
      do_stuff_with_item

    timestamps = iter(datetime.datatiem.now(), None)

    ```

  - Generator

    - generate an iterable object
      - `iter(gen) is gen`
      - yield-next pattern
    - ranger
      - a number generator
      - `range(start, stop, step)`
    - generator FP
      - `yield statement`
      - generator is lazy binding.

    ```python
    # Generator wrapper function
    def gernerator_wrapper(sequence)
      for item in sequence:
        yield item

    def gen(n):
      for i in range(n):
        yield i

    for i in gen(10):
      do_stuff_with_i

    # Generator comprehension
    gen = (i**4 for i in range(100))
    next(gen)

    # generator protocal
    generator.__next__()
    generator.send(value)
    generator.throw(*EXCEPTION)
    generator.close()

    ```

  - Dispatcher

  - Coroutine Generator

    - states: `inspect.getgeneratorstate(gen)`
      - GEN_CREATED
        - genobj = genfunc() generator function called
      - GEN_RUNNING
        - inside genfunc code running
      - GEN_SUSPENDED
        - yield statement returned in genfunc
      - GEN_CLOSED
        - genfunc returns implicitly or explicitly

    ```python
    # a type of generator
    ```

  - Filter
    - `filter()`
  - Mapper
    - `map()`
  - Reducer
    - `functools.reduce()`
  - Invoker/Caller/Applier
    - `apply()`

- Practices

  ```python
  import reduce from functools
  import * from itertools

  # functors
  # lazy evaluation
  iterator = map(func, *iterable)
  iterator = filter(func, iterable)
  accum = functools.reduce(func, iterable, initializer)
  iterator = zip(*iterable1)
  bool = all(iterable)
  bool = any(iterable)
  iterator = sorted(iterable, key = sorter, reverse=False)
  iterator = reversed(iterable)
  max(iterable, key = sorter)
  min(iterable, key = sorter)
  len(iterable_sequence)

  #iterator tools
  iterator = chain(iterable1, iterable2)
  iterator = count(start, step)
  iterator = cycle(iterable)
  iterator = ifilter(func, iterable)
  iterator = imap(func, iterable)
  iterator = islice(iterable, start, stop, step)
  iterable = izip(iterable1, iterable2, ...iterables)

  ```

---

#### Built-in Functions

- Type and type conversion
  - `bool(v)`
  - `int(sn, base=10), bin(i), oct(i), hex(i), float(sn), complex(n1, n2)`
  - `chr(code), ord(char), ascii(s), bytearray(), bytes(), str()`
  - `list(iter), tuple(iter), dict(iter), enumerate(iter), set(iter), frozenset(iter)`
  - `type()`
- Object
  - `isinstance(obj, Class), issubclass(SubClass, BaseClass),`
  - `repr(o), hash(o), id(o), callable(o),`
  - `@classmethod, @staticmethod, super(), object()`
  - `delattr(), hasattr(), getattr(), setattr(), property()`
- Math Calc
  - `abs(x), round(), floor(), ceil()`
  - `max(), min(), divmod(n1, n2), power(x, y)`
  - `len(x)`
- I/O
  - `format(), input(), print(data, sep=' ', end='\n')`
  - `open(file)`
- Functors
  - `all(iter), any(iter)`
  - `range(start, end, step), slice(start, end, step)`
  - `max(iter), min(iter),sorted(iter), sum(iter), zip(*iters)`
  - `iter(), next(), reversed(iter)`
  - `map(f, iter)`
  - `filter(f, iter)`
- System and Modules
  - `dir(), vars(), locals(), globals() __import__()`
  - `complie(), memoryview()`
  - `exec()`
  - `eval()`
- [Refs](https://docs.python.org/3/library/functions.html)

---

### Class and OOP

#### Classes

- Class, Instance and Object

  - Class Type declaration
  - Class Docstrings
    - `cls.__doc__`
  - Constructor
    - `__new__(cls, *args, **kwargs)`
    - `__init__(self, *args, **kwargs)`
  - Destroctor
    - `__del__(self)`
  - Attributes
    - Class Attributes
      - not defined in every instance
      - only init once when class was created
      - shared by all instances
    - Instance Attributes
    - attributes loopup
      - instance -> class -> parents classes
    - get attributes of class
      - `dir(Obj)`
    - dunder and name mangling
      - `ClassName.name`
        - for public use.
      - `ClassName._name`
        - symanticly, for internal use, but syntaxly, can be access directly.
      - `ClassName.__name`
        - name mangled into `_ClassName__name`, so can not be access directly.
        - sub classes can have the same name after mangled.
      - `ClassName.__name__`
        - python special purpose names.
  - Methods
    - method docstrings
      - `cls.method.__doc__`
    - bound method
      - has `self` bound to instance
      - `f=instance.boundM; f(*args)`
    - unbound method
      - no `self`
      - `f=cls.unboundM; f(instance, *args)`
    - instance method
    - class method
      - `@classmethod` decorator
      - with implicity `cls` argument
      - can update class/object status
      - can be used as a constructor with different init values
    - static method
      - `@staticmethod` decorator
      - can not update class/object status
      - pure utility function
    - getter/setter/deleter/docstring
      - `myprop = property(fget=None, fset=None, fdel=None, doc=None)`
      - getter `@property`
      - setter `@myprop.setter`
      - deleter `@myprop.deleter`
      - accessing using getter/setter
        - `print(obj.myporp)`
        - `obj.myprop = VALUE`
  - Slots
    - `__slots__` a replacement of `__dict__` for RAM optimization
    - attributed defined in `__slots__()`, not listed in `__dict__`, and it can suppress `__dict__`
    - explicit define `__slots__` will replace `__dict__`
    - limit attributes defined, adding `__dict__` to `__slots__` can solve this limitation
    - inherit a unsloted superclass will break subclass's slot rule, and vice versa.
      - slots in dict VS dict in slots
    - slots prevent class level attributes
  - Instance
    - `instance.__class__, type(intance`
  - sumary
    - 4 attribute accessor tools
      - `__getattr__, __getattribute__, properties, descriptors`
    - 4 kinds of methods
      - `Instance, static, class, metaclass`
    - 2 attribute storage systems
      - Dictionaries, slots
    - 2 superclass dispatch protocols
      - Direct calls, super + MRO
    - 4 state retention options
      - Classes, closures, function attributes, mutables
  - Operator Override and dunders

    - `__new__()`
    - `__init__()`
    - `__del__()`

      - constructor, initiator and destructor

    - `__add__(), __radd__(), __iadd__()`
    - `__mul__(), __rmul__(), __imul__()`
    - `__sub__()`

      - `+, +=` operator override
      - `*, *=` operator override

    - `__repr__()`

      - provides unambiguous string representation of object
      - more detail information about object
      - `repr(obj)` interface

    - `__str__()`
    - `__format__()`

      - stringify object
      - human-readable information
      - print call str if it's presence
      - `str(obj)` interface
      - `format(obj` interface

    - `__enter__()`
    - `__exit__()`

      - context manager `with obj as obj_enter_return: do_struff`

    - `__iter__()`
    - `__next__()`

    - `__copy__()`
    - `__deepcopy_-()`

    - `__format__()`

      - `"{}".format(obj)` interface
      - `"{!s}".format(obj)` force str() interface call
      - `"{!r}".format(obj)` force repr() interface call

    - `__contains__()`

      - `in` operator override

    - `__getattr__()`
    - `__setattr__()`
    - `__delattr__()`

      - missing attributes fallback call
      - properties getter, setter, deleter

    - `__getattribute__()`

      - preemptive attributes access call

    - `__get__()`
    - `__set__()`
    - `__delete__()`

      - descriptor core methods

    - `__eq__()`
    - `__ne__()`
    - `__lt__()`
    - `__gt__()`
    - `__le__()`
    - `__ge__()`
    - `__or__()`

      - `==` operator override
      - `!=` operator override
      - `<` operator override
      - `>` operator override

    - `__index__()`

      - integer value for an instance when needed, like, `hex(obj)`

    - `__hash__()`

      - auto hashable using id(CLASS) if `__eq__()` not defined
      - not hashable if `CLASS.__hash__ = None`
      - custome hashable using `__hash__()`
        - returns an integer
        - if obja == objb, hash(obja) == hash(objb)

    - `__len__()`
    - `__getitem__(index|slice)`
    - `__setitem__(key|slice, value)`
    - `__delitem__(key|slice)`
    - `__reversed__()`

      - `len(obj)` interface
      - `obj[index]` operator override
      - `obj[start:end:step]` operator override

    - `__call__(*args, **kwargs)`

      - `obj(*args, **kwargs)` callable operator override

    - `__bool__()`

      - `bool(obj)` interface, boolean test call bool interface first, if no exist, call len interface.

    - `__reduce__()`
    - `__bases__`
    - `__mro__`
    - `__class__`
    - `__slots__`
    - `__name__`

      - tuple of base classes

      - method resolution order list, same as `obj.mro()`

- Abstract Class

  - can't instantiated
  - subclasses must implement/provide abastract method to instantiation.

  ```python
  from abc import ABC, abstractmethod

  class Shape(ABC):
    @abstractmethod
    def area(self):
      pass

    @abstractmethod
    def perimeter(self):
      pass

  class Triangle(Shape):
    def __init(self, a, b, c):
      self.a = a
      self.b = b
      self.c = c

    def area(self):
      p = self.perimeter()/2
      return math.sqrt(p(p-a)(p-b)(p-c))

    def perimeter(self):
      return self.a + self.b + self.c

  ```

- OOP

  - Inheritance
    - subclass is-a parent class relation
    - parents' constructor must called explicitly using `super().__init__()`
    - Inheritance Chain
      - linearization of C
    - Multiple Inheritance
      - Method Resolution Order
        - `L[C(B1 ... BN)] = C + merge(L[B1], ... L[BN], [B1, B2, ... BN])`
        - `L[object] = object`
        - merge: go through all lists, took the first non-tailed head class append to C's linearization list, if can't merge, raise TypeError
    - `super(base, argv = subclass/instance) proxy`
      - find the MRO og subclass/instance, locate base class in MRO
      - return the first class after base class has target method
      - all-or-nothing model
      - class-bound super() proxy
        - super(BaseClass, SubClass)
      - instance-bound super() proxy
        - super(BaseClass, subInstance)
    - inheritance principles
      - super
        - proxy super class methods
      - inheritor
        - do nothing
      - Replacer
        - overide super class methods
      - extender
        - overide and call back super class method combined
      - provider
        - implement abstract method defined by super class
    - Abstract Base classes
      - abc module
        - decorators
          - `@abc.abstractclassmethod`
          - `@abc.abstractstaticmethod`
          - `@abc.abstractproperty`
          - `@abc.abstractmethod`
  - Encapsulation
    - no strict public/private attributes/methods in python
    - conventional `_private, __internal`
    - dunder name mangling
      - `__x` mangled to `_clsname__x`
  - Polymophism
    - Method Overwriten
  - Isolation
  - Composition
    - class has-a component class
    - delegation/proxy pattern

- Python Object Model

  - Object in Python
    - Identity
    - Type
    - Value
  - Memory Reference Model
    - object assigned by reference
    - reference count reduce to 0, then object will be destroied by GC.
    - reference count increment
      - create new object and assigned
      - assigned to other identifier
      - passed to a function as argument
      - referenced by container object as an element.
    - reference count decrement
      - local reference out of scope
      - `del` operator explicitly destroy alias
      - alias reassigned to different object
      - object removed from container by container's operation
      - container was destroied
    - pass by reference
      - assignment is alias, no copy
        - `objx = objy # objx is objy; objx == objy`
      - shallow copy increase reference count for elements
        - `objx = shallow_copy(objy) # objx is not objy; objx == objy`
      - deep copy create new object for each level of objects
        - `objx = deep_copy(objy) # objx is not objy; objx == objy`
  - Categories
    - stroage model
      - literal object
        - numbers, str
      - container object
        - list, tuple, dict, set
    - update model
      - mutable
      - immutable
    - access model
      - direct access
      - sequence
      - hashing

- Practices

  ```python
  #Object Declaration
  class MyObject(PARENT1, PARENT2, PATENT3, ...):
    """MyObject Class Docstring"""
    def __init__(self, arg, *args, **kwargs):
      constructor_body_block

  # Object declarition
  class Shape(object):
    def __init__(self):
      pass

    @classmethod
    def BuildShape(cls, name):
      return cls(name)

    @staticmethod
    def PI():
      return math.pi

    def calcArea(self):
      pass

  class Circle(Shape):
    def __init__(self, r):
      Shape.__init__(self)
      self.radius = r

    def calcCircumference(self):
      return 2*math.pi*self.radius

    def calcDiameter(self):
      return 2*self.radius

    def calcArea(self):
      return math.pi*self.radius**2

  # Instantiation
  c = Circle()

  #ABC
  from abc import ABC, ABCMeta

  class MyABC(ABC):
      pass

  class MyABC(metaclass=ABCMeta):
    pass

  #Dynamic Class Type
  NewClass = type('NewClass', (object,), {})

  ```

---

#### Built-in Classess

##### Custome Context Manager

- Context Manager

  - implement cm protocol
  - do init and cleanup jobs implicitly, no matter what happend in the block.
  - in `with statement`, CM object must return by expression.
  - `ALIAS` is bound to `CM.__enter__()` return value, not expression
  - `CM.__exit__()` returns a bool value, True indicates supress exception sillently, Fasle indicates propergate exception.
  - no new scope created
  - context manager use case:
    - start-stop block
      - start-stop timer
    - open-close
      - open-close file
      - open-close socket
      - open-close database
    - lock-release
    - change-reset
      - change-reset decimal context
      - change-reset stdin-stdout context
    - html-tagging
    - format-indentation

- Practice

  ```python
  # CM underline
  with CM() as obj:
    CM_BLOCK

  # equals to
  obj = CM().__enter__()
  try:
    CM_BLOCK
  finally:
    CM().__exit__(exception_type, exception_value, exception_traceback))

  # CM class
  Class myCM():
    def __enter__(self):
      pass
    def __exit__(self, exception_type, exception_value, exception_traceback)
      pass

  # contextlib context manager with generator
  import contextlib
  @contextlib.contextmanager
  def myCM():
    # enter_blcok
    try:
      yield ALIAS_value
      # normal_exit_block
    except Exception as e:
      # except_exit_block

  # nested CM
  with myCM1() as cm1, myCM2() as cm2:
    do_stuff_block

  with myCM1() as cm1:
    with myCM2() as cm2:
      do_stuff_block

  # decimal CM
  with decimal.localcontext() as ctx:
    ctx.prec = 6
    DECIMAL_ARITHMETIC

  # timer CM
  with Timer() as timer:
    PERFORMANCE_MON
  print(timer)

  class Timer(object):
    def __init__(self):
      self._elapsed = 0

    def __repr__(self):
      return f"elapsed: {self._elapsed}"

    def __enter__(self):
      self._start = time.perf_counter()
      return self

    def __exit__(self, e_type, e_value, e_traceback):
      self._stop = time.perf_counter()
      self._elapsed = self._stop - self._start
      return False

  # IO redirect
  with OutRedirect('logfile'):
      PRINT_TO_STDOUT_REDIRECT_TO_FILE_OBJECT

  class OutRediret(object):
    def __init(self, filename):
      self._fname = filename
      self._pre_stdout = sys.stdout

    def __enter__(self):
      self._fo = open(self._fname, 'a+')
      sys.stdout = self._fo

    def __exit__(self):
      sys.stdout = self._pre_stdout
      self._fo.close()
      return False

  ```

---

##### Exception

- Exception Hierarchy

- Customer Exception
- `raise MyException` statement will invoke constructor, then `MyException.__str__()`
- BaseException -> Exception
- [Ref](https://docs.python.org/3/library/exceptions.html)

- Exception
- SyntaxError
- NameError
- ValueError
- TypeError
- ArithmeticError
- ZeroDivisionError
- OverflowError
- FloatPointError
- IndexError
- KeyError
- IOError
- FileNotFoundError
- OSError
- AttributeError
- AssertionError
- UnboundLocalError
- StopIteration

---

##### File IO

- Introduction

- file is a contiguous sequence of bytes
  - text file
  - binary file
- EOL
- standard files
- stdin
  - `input()`
  - `sys.stdin`
- stdout
  - `print()`
  - `sys.stdout()`
- stderr
  - `sys.stderr`
- text/binary IO objects
  - `fo = open("path/to/file", "MODE")`
- pipeline
  - `pipe = os.popen('ls -al'); pipe.read()`
- check built-in [io module](#io)

- Practices

```python
# get file/io object
fo = open("./data/test.txt", "r+w")

#file object methods/properties
fo.name
fo.mode
fo.closed
fo.encoding
fo.newlines

fo.fileno()
alltext = fo.read()
fo.read(size)
linetext = fo.readline()
filetext = fo.readlines()
fo.write(string)
fo.writelines(string_list)
fo.seek(offset, start)
fo.next()
fo.tell()
fo.truncate(size)
fo.flush()
fo.close()

with open("file.txt", "+") as fo:
for line in fo:
  print(line, end="")
print("done")

# external module/functions
import os
os.rename(oldfilename, newfilename)
os.remove(filename)

```

---

### Operators

- Grouping Operators

  - `(group)`

- Generator send protocal Operator

  - `yeild val`

- Anonymouse function Operator

  - `lambda x: expr_x`

- Indexing/Accessing Operator

  - `__getitem__(), __setitem__(), and __delitem__()`
  - `sequence[index]`
  - `collect[member]`

- Slice Operator

  - `__getslice__(), __setslice__() and __delslice__()`
  - `sequence[start:end:step]`

- String/List Concatenation Operator

  - `__concat__()`
  - `s + s2`

- String/List Repeatition Operator

  - `__mul__()`
  - `s * 3`

- del Operator

  - `del arr[i]`

- in, not in Membership Operator

  - `elem in COLLECTION`
  - `elem not in COLLECTION`

- Bitwise Operators

  - `&, |, ^, ~`
  - `<<, >>`
  - `~x & ((1<<x.big_length)-1)`

- Tenary Operator

  - `true_expression if condition_express else false_expression`

- Arithmetic Operators

  - `+, -`
  - `*, /, //, %, **`

- Comparison Operators

  - `==, !=`
  - `>, >=, <, <=`

- is, is not Identity Operator

  - `is`
  - `is not`

- Assignment Operator

  - `=`
  - `+=, -=, *=, /=, //=, %=, **=`

- Condition/Logical Operators

  - `not, and, or`

- tuple/dict destructuring/unpacking Operators

  - `func(*args)`
  - `func(**kwargs)`

- Callable Operator

  - `val(...)`

- Generator Operator

  - `(gen_expr)`

- Comprehension Operator

  - `[list_expr]`
  - `{dict_expr}`
  - `{set_expr}`

- Callable Operator

  - `val(...)`

- f-string Operator

  - `f"{var}"`

- Comma Operator

  - `a, b, c`

---

### Expressions

- assignment expression

  - `a = 1`
  - multiple-target assignment
    - `a = b = 1`
  - `a = 1, b = 2`
  - sequence assignment
    - `a, b, c = 1, 2, 3`
  - list, tuple destructuring/sequence unpacking assignment
    - `a, *b = [1, 2, 3]`
  - augmentaed assignemnt
    - `+=, -=...`

- condition expression

- generator expression

  - `(expr for iter in interable)`

- lambda expression

- yield expression
  - yield
  - yield
  - yield from

### Statements

- Comments

  - shebang
    - `#!/usr/bin/env python3`
    - `# -*- coding: utf_8 -*-`
  - inline comment
    - `# inline comment`
  - block comment

    - `''' block comment '''`
    - `""" block comment """`

- Importing/Exporting

  - `import MODULE`

- Global variable reference statement

  - `global variable_list`

- Nonlocal variable reference statement

  - `nonlocal variable_list`

- Docstring statement

  - docstring is a string literal that occurs as the first statement in a module, function, class, or method definition.
  - docstring becomes the `__doc__` special attribute of that object

- Decorator statement

  - `@decorator`

- Pass Statement

  - `pass`

- Assert Statement

  - `assert condition, MSG`

- Function Statements

  - function declaration
  - lambda function

- Return Statement

- Class Statements

  - class declaration

- Flow Control Statements

  - condition statements
    - if-elif-else
  - context blocking statement
    - with-as
  - loop statements
    - for-in-else
    - while-else
    - break
    - continue
  - exception statements
    - try-except-else-finally
    - raise

### Flow Control

#### Condition Branching

- if
- if-else
- if-elif-else

```python
# if
if condition_expression :
  true_blocks

# if-else
if condition_expression :
  true_blocks
else :
  false_blocks

# if-elif-else
if condition_expression1 :
  condition1_true_blocks
elif condition_expression2:
  condition1_false
  condition2_true_blocks
else :
  all_above_condition_expression
  false_block
```

#### Context Blocking

- Context Manager Protocol

  - context manager interface `__enter__()`, `__exit__()`
  - return value from enter will assign to as clause variable
  - exit interface `__exit__(type, value, traceback)`

- with-as

```python
  # nested with-as
  with CM1() as cm1, CM2() as cm2:
    do_stuff
  # equivalence
  with CM1() as cm1:
    with CM2() as cm2:
      do_stuff

  # file open and close
  with open("file.txt", "+") as fh:
    for line in fh:
      print(line)
  print("done")

  # class __enter__(), __exit__()
  class MyClass():
    def __init__(self):
      pass

    def __enter__(self):
      print("enter 'with' block.")
      print("do some init work.")
      return self

    def __exit__(self, type, value, traceback):
      if type is None:
        print("exit 'with' block.")
        print("do some cleanup work.")
      else:
        print(f"exception: {type}")
        return False

    def showinfo()
      print("hello MyClass.")

  with MyClass() as myclass:
    myclass.showinfo()
  print("done")

```

---

#### Looping

- for-in
- for-in-else
- while
- while-else
- break
- continue

```python
  # for-in
  for i in  range(100) :
    loop_with_indexing_block

  for char in  STR :
    loop_with_char_block

  for iter in  ITERABLE :
    loop_with_iter_block

  for iter in  ITERABLE :
    loop_with_iter_block
  else:
    loop_over_iter_block
    no_run_if_break

  while condition_expression :
    loop_block
    update_condition
    or_if_break_condition

  while condition_expression :
    loop_block
    update_condition
    or_break_condition
  else:
    loop_over_block
    no_run_if_break
```

---

#### Exception/Error Handling

- Introduction

  - Error handling
  - Event notification
  - Special-case handling
  - Termination action
  - Unusual flow control
    - like a jump by design
  - exceptions will propergate to next level try-except block if no except matches.
  - `sys.exec_info()`

- patterns
  - try-except
  - try-raise-except
  - try-except-else-finally

```python
  # try-except
  try :
    try_block_with
    nomal_process_logic
  except Exception as e:
    exception_handle_block

  # try-raise-except
  try :
    try_block_with
    nomal_process_logic
    throw_if_error
    raise Exception(msg)
  except Exception as e:
    exception_handle_block
  else:
    no_matched_exception_block
  finally:
    clean_up_block

  # except
  except Exception:
  except name:
  except name as value:
  except (name1, name2,...):
  except (name1, name2,...) as value:

  # raise
  raise E
  raise E from V

  # assert
  assert test, data
  assert False, 'cannot ingored'

  #customized exception
  class MyException(Exception):
      def __str__(self):
          return 'MyException Message:'

```

---

---

## Builtin Moudles and Packages

### Importing/Exporting

- Module

  - `globals()`
  - `locals()`
  - module

  ```python
  import MODULE
  import types
  import importlib
  type(MODULE)
  types.ModuleType
  onthefly_module = types.ModuleType('MODULENAME', 'DOCSTRING')
  module_namespace = dir(onthefly_module)
  onthefly_module.__dict__
  onthefly_module.prop = VALUE
  id(MODULE)
  importlib.reload(MODULE)
  globals()
  locals()

  import sys
  sys.modules
  id(sys.modules[MODULE])

  # python execution file location
  sys.prefix

  # python module path
  sys.path

  ```

- import/export/reload

  - import module process
    - check sys.modules for cached module existance, if exist, use reference.
    - if no cache, creates new types.ModuleType object
    - locate new module's source file location by sys.path
    - add new module entry to sys.modules
    - compile and execute source code.
  - import module from path: `. ; PYTHONPATH`
    - config `sys.path`
  - import statment excution module codes only once
  - import statements
    - `import MODULE`
    - `import MODULE as ALIAS`
    - `from MODULE import NAMES`
    - `from MODULE import NAME as ALIAS`
    - `from MODULE import *`
  - relative import
    - apply to from statement only
    - apply to within package from statement
    - module must exist in package by relative import
    - `from . import grok`
    - `from ..B import bar`
  - exclude codes for import excution
    - `if __name__ == "__main__":`
    - `excluded_module_block`
    - `_name`
    - `__all__`
  - show module namespace
    - `dir(MODULE)`
  - program entry point
    - `__name__ == "__main__"`
  - reload
    - `import importlib`
    - reload affect future from statements

- importlib

  - import statement is syntactic sugar of `importlib.__import__()` function

  ```python
  import importlib
  as_alias = importlib.import_module(MYMODULE)
  my_module = importlib.import_module(MYMODULE)
  importlib.reload(MYMODULE)
  importlib.util.find_spec(MYMODULE)

  sys.meta_path
  sys.modules

  #custom modules
  """
  root
    |--module1.py
    |--module2.py
    |--module3.py
    |--main.py

  main module can import module1, module2, and module3 using import statements.
  modules can also import with each other.
  """

  #external modules
  """
  pip install PACKAGE
  python will take care of PATH and module finding problem.
  external modules can be imported just like built-in modules.
  """
  ```

- packages

  - package find path:
    - `sys.path`, this value init with `PYTHONPATH` env variable
  - using `__init__.py` in package root directory
  - export minimal interface in `__init__.py` to reduce exposure surface
  - excutable directory
    - has `__main__.py` as programe entry point
  - implicite namespace package
    - a logical package, modules are spreaded in different directories.
    - no `__init__.py` entry.
    - `pkg.__path__` to search all components
    - sys.path set to includes all directories
  - special dunders

    ```python
    """ Package dir structure
    entry_path_in_sys.path
        |--Package root
            |--__init__.py
            |--funtional_module.py
            |--Sub package root
                |--__init__.py
                |--sub_funtional_module.py
    """

    dir(PACKAGE)
    # package name
    PACKAGE.__name__
    # package __init__.py
    PACKAGE.__file__
    # package path
    PACKAGE.__path__
    # package namespace exposed by import *
    PACKAGE.__all__
    ```

---

### Math

- Moduels

  - numbers
  - decimal
    - accounting applications
  - fractions
    - rational number arithmetic
  - math
    - real/rational/integer number arithmetic
  - cmath
    - complex number arithmetic
  - random
    - `random.choice(indexable)`
    - `random.shuffle(iterable)`
    - `random.randrange(start, stop, step)`
    - `random.randint(lower, upper)`
    - `random.random()`
  - statistics

- Practices

  ```python
  import numbers
  """
    numbers module defines numeric abstract base classes.
    numbers.Complex, numbers.Real, numbers.Rational, numbers.Integral
    all abstract classes can't be instantiated.
  """

  from decimal import *
  """
    decimal module provides precisely rounded float numbers.
    and arithmetic operations carries the precision.
  """
  # Context Object
  getcontext()
  setcontext()

  # Decimal Object
  d = Decimal(value="0", context=None)

  with decimal.localcontext() as ctx:
      ctx.prec = 2
      do_decimal_arithmetic

  from fractions import Fraction
  """
    fractions module provides rational number arithmetic.
  """
  # Fraction Object
  r = Fraction(16, -10)
  r.numerator
  r.denominator

  import math
  math.ceil()
  math.floor()
  math.pi
  math.e
  math.nan
  math.inf

  ```

- Ref
  - [General Decimal Arithmetic Specification](http://speleotrove.com/decimal/decarith.html)

---

### Data Processing and Persistance

- pickle

  - `pickle.dump(data, filehandler)`
  - `data = pickle.load(filehandler)`
    - load meaning full object, the class of object must defined before load().

- json

  - `json.dump(data, filehandler, indent=2, sepeators=(',', ':'))`
  - `data = json.load(filehandler)`
    - load meaning full object, the class of object must defined before load().

- yaml

  - `pip install pyyaml`
  - `yaml.dump(data, filehander)`
  - `data = yaml.load(filehandler)`
    - load meaning full object, the class of object must defined before load().

- shelve

  - `with shelve.open('path/to/file') as db:`
  - `do_stuff_with_db_like_a_dict_type`
  - `db.close()`

- csv

---

### Python Language Services

- keyword
  - `keyword.kwlist`
- reprlib
- pdb
  - `pdb.set_trace()`
    - `l for list, n for next, s for step in`
- logging

  ```python
  import logging
  logger = logging.getLogger(NAME.SUBLOGGER: str)

  logging.basicConfig(level=logging.INFO,
                      format='%(asctime)s %(levelname)s:%(message)s',
                      datefmt='%Y-%m-%d $H:$M:%S',
                      file='log.txt')

  logger.debug(DEBUG_MSG)
  logger.info(INFO_MSG)
  logger.warning(WARNING_MSG)
  logger.error(MSG)
  logger.critical(MSG)
  ```

  - [Ref](https://docs.python.org/3/library/logging.html)

- pytest

  - `pip install pytest`
  - testing script
    - `touch test_MYSCRIPT.py`
    - editing test cases for `MYSCRIPT`
  - testing cases
    - `import MYSCRIPT`
    - `def test_MYSCRITP_FUNC1():`
    - `assert assert_statements`
  - run test
    - in CLI, run `py.test`

- pydoc
- docstrings

  ```python
  python -m pydoc -b
  ```

---

### FP

- itertools
- functools
- operator

---

### OOP

- abc

---

### Algorithms and Data Structure

- bisect

  ```python
  import bisect

  # binary search tree, left ordered
  index = bisect.bisect_left(arr, searchvalue, start, end)

  # binary search tree, right ordered
  index = bisect.bisect_right(arr, searchvalue, start, end)

  # binary search tree, left insert
  bisect.insort_left(arr, insertvalue, start, end)

  # binary search tree, right ordered
  bisect.insort_right(arr, insertvalue, start, end)

  ```

- collections

  ```python
  from collection import *

  # Counter
  num = Counter(ITERABLE)

  # defaultdict
  dd = defaultdict(TYPE_FACTORY)
  dd.default_factory = None

  # OrderDict
  od = OrderDict()
  od.move_to_end(KEY)
  od.move_to_end(KEY,last = False)
  od.popitem()

  #namedtuple
  NT = namedtuple(NAME, ITERABLE_NAME)
  var = NT(ITERABLE_VALUE)
  var.NAME

  #deque
  dq = deque(ITERABLE)
  dq.append(VALUE)
  dq.appendleft(VALUE)
  dq.pop()
  dq.popleft()

  ```

- heapq

  ```python
  heapq.heapify()
  heapq.heappush()
  heapq.heapreplace()
  heapq.nlargest()
  heapq.heappop()
  heapq.heappushpop()
  heapq.merge()
  heapq.nsmallest()
  ```

---

### Web

- webbrowser

  ```python
    import webbrowser as wb
    url = 'http://docs.python.org/'
    wb.open(url)
    wb.open_new(url)
    wb.open_new_tab(url)
  ```

- urllib

  ```python
    import urllib
    url = 'http://docs.python.org/'

    # urllib.request
    requestObj = urllib.request.urlopen(url)
    namedtuple = urllib.parse.urlparse(url)
    rp = urllib.robotparser.RobotFileParser(url)
  ```

---

### IO Texting and Streaming

#### re

- Introduction

  - regular expression
  - pattern
  - match object
  - [re Ref](https://docs.python.org/3/library/re.html)

- Practices

  ```python
  import re
  # python regexp pattern
  """
  Metacharacters:
    .         a single character, exclude newlines if DOTALL 're.S' flag is not set.
    \         escaping character
    \0        a null char
    \t        a tab char
    \v        a vertical tab
    \r        a carriage return
    \n        a newline char
    \uXXXX    a unicode char

  Boundaries
    ^     begin of text, if MULTILINE 're.M' flag is not set, else, begin of each line.
    \A    begin on text, no matter re.M flag
    $     end of text, if MULTILINE 're.M' flag is not set, else end of each line.
    \Z    end of text, no matter re.M flag
    \b    word boundary
    \B    not word boundary

  Char Classes
    \s       a whitespace, like [ \t\r\n\f\v]
    \S       a non-whitespace character, like [^ \t\r\n\f\v]
    \d       a digit, like [0-9]
    \D       a non-digit, like [^0-9]
    \w       a world character, like [a-zA-Z0-9_]
    \W       a non-world character, like [^a-zA-Z0-9_]

  Bracket and Ranges
    [...]    Any one character between the brackets, Brackets escapes special chars implicitly, but exclude '-, ^, \, ], '.
    [^...]   Any one character not between the brackets.
    [0-9]    one digit char
    [a-z]    one lowercase char
    [A-Z]    one uppercase char
    [a-Z]    one char case-insensitive
    [^\s]    matches any non-empty char

  Quantifier, Greedniess and Laziness
    p+      p repeated 1 or more, greedniess
    p+?     p repeated 1 or more, laziness
    p*      p repeated 0 or more, greedniess
    p*?     p repeated 0 or more, laziness
    .*      matches any sequence of char, except newline if flag 's' is not set.
    p?      p repeated atmost 1, greedniess
    p??     p repeated atmost 1, laziness
    p{N}    p repeated N, greedniess
    p{N}?   p repeated N, laziness
    p{L,}   p repeated more then L, greedniess
    p{L,U}  p repeated between [L,U], greedniess
    p{L,U}?  p repeated between [L,U], laziness

  Alternation
    p1|p2   p1 or p2 pattern

  Grouping
    (p)               group sub patterns
    (?:...)           group without reference later
    (?P<name>)       named group
    (?P=name)         reference a named group
    (?#...)           comments, ingored
    \g<N>             group N reference
    \g<name>          named group reference

  Assertion Positive/Negative lookahead/lookbehind
    (?=...)           positive lookahead
        'Isaac (?=Asimov)'  matches 'Isaac ' only if it’s followed by 'Asimov'
    (?!...)           negative lookahead
        'Isaac (?!Asimov)'  matches 'Isaac ' only if it’s not followed by 'Asimov'.
    (?<=...)          postive lookbehind
        '(?<=abc)def'       matches 'def' only if it's preceded by 'abc'
    (?<!...)          negative lookbehind
        '(?<!abc)def'       matches 'def' only if it's not preceded by 'abc'
    (?(id/name)yes-pattern|no-pattern)
        given id or name exists, try yes-pattern, if not, try no-pattern

  Flags
    re.I
    re.M
    re.S
    re.L
    re.X
    re.A

  """

  # re mothod
  matchObj = re.match(pattern, text, flags)
  matchObj = re.search(pattern, text, flags)
  matchResult = re.findall(pattern, text, flags)
  substitutestr = re.sub(pattern, text, flags)

  # match object
  mo.group()
  mo.groups()
  ```

---

#### io

- Introduction

  - text I/O
  - binary I/O
  - raw I/O
  - [io Ref](https://docs.python.org/3/library/io.html#module-io)

- Practices

  ```python
  import io
  # open mode
  """
  'r'     open for reading (default)
  'w'     open for writing, truncating the file first
  'x'     open for exclusive creation, failing if the file already exists
  'a'     open for writing, appending to the end of the file if it exists
  'b'     binary mode
  't'     text mode (default)
  '+'     open a disk file for updating (reading and writing)
  """

  # text I/O
  f = open("myfile.txt", "r", encoding="utf-8")
  f = io.StringIO("some initial text data")
  f.read()
  f.read(cursorLocation)
  f.readline()
  f.readlines()
  f.seek(cursorLocation)
  cursorLocation = f.tell()
  f.write(string)
  f.writelines(text)
  f.append(string)
  f.close()

  #binary I/O
  f = open("myfile.jpg", "rb")
  f = io.BytesIO(b"some initial binary data: \x00\x01")

  #raw I/O
  f = open("myfile.jpg", "rb", buffering=0)

  ```

---

### OS and System Services

#### Date and Time

- time

  ```python
  improt time

  #get time
  timeObj = time.gmtime(0)
  timeObj = tiem.localtime()
  slapsed = time.time()

  t.hour
  t.minute
  t.second`
  t.microsecond
  t.isoformat()
  t.strftime(FORMATER)

  time
  monotonic
  #include sleep time
  time.perf_counter
  #exclude sleep time
  time.process_time

  ```

- datetime

  ```python
  improt datetime

  #get datetime
  d = datetime.date.today()
  d = datetime.date.now()
  d = datetime.date.utcnow()
  d.year
  d.month
  d.day
  d.weekday()
  d.isoweekday()
  d.isoformat()
  d.strftime(FORMATER)
  datetime.tzinfo
  datetime.timezone
  dt = datatime.datatime.combine(dt,t)

  ```

- calendar

  ```python
  improt time

  #get time
  timeObj = time.gmtime(0)
  timeObj = tiem.localtime()
  slapsed = time.time()

  ```

- pytz

  - `pip intsll pytz`
  - Olson timezones database

  ```python
  import pytz
  from pytz import timezone
  from datetime import datetime, timedelta

  #get time
  china = 'Asia/Shanghai'
  tzdisplay = pytz.timezone(country)
  local_time = datetime.datetime.now(tz = tzdisplay)
  utc+time = datetime.datetime.utcnow(tz = tzdisplay)
  ```

- timeit

  ```python
  improt timeit
  """
  timeit.timeit(stmt='pass', setup='pass', timer=<default timer>, number=1000000, globals=None)
  timeit.repeat(stmt='pass', setup='pass', timer=<default timer>, repeat=5, number=1000000, globals=None)
  """
  timeit.timeit('"-".join(str(n) for n in range(100))', number=10000)
  timeit.timeit('char in text', setup='text = "sample string"; char = "g"')
  timeit.timeit(lambda: "-".join(map(str, range(100))), number=10000)

  """
  timeit.Timer(stmt='pass', setup='pass', timer=<timer function>, globals=None)
  """
  timer = timeit.Timer('char in text', setup='text = "sample string"; char = "g"')
  elapsed = tiemr.timeit(number=1000000)
  repeat = timer.repeat(repeat=5, number=1000000)
  ```

#### Concurent Computing

- concurrent
- threading
- subprocess
- multiprocessing

  ```python
  import os, sys, subprocess, multiprocessing, threading concurrent

  # no exception, just exit
  os._exit()

  # exit by exception: SystemExit
  sys.exit()
  ```

- contextlib
  - `@contextlib.contextmanager`

```python
import subprocess
subprocess.call("echo 'hello world!'")
pipe = subprocess.Popen('python helloshell.py', stdout=subprocess.PIPE)
pipe.communicate()[0]
pipe.stdout.read()
pipe.wait()
pipe = Popen('python hello-in.py', stdin=PIPE)
pipe.stdin.write(b'Pokey\n')
pipe.stdin.close()
pipe.wait()
```

#### Cli

- argparse
- cmd

---

#### SysEnv

- os, sys

  ```python
  import os, sys

  sys.setdefaultencoding("utf-8")

  env_dict = os.environ
  for key, val in env_dict.items():
    print(key, val)
  os.putenv()
  os.getenv()

  os.open()
  os.read()
  os.write()
  os.lseek()
  ```

#### File System

- os
- glob

  ```python
  """
  os.walk(top, topdown=True, onerror=None, followlinks=False)
  """
  import os

  os.getcwd()
  os.listdir()
  bool = os.access("path/to/file/dir", os.F_OK|os.R_OK|os.W_OK|os.X_OK)
  for dirs, subdir, files in os.walk('.'):

  #Shell variables
  os.environ

  #Running programs
  os.system('echo "Hello World"')
  os.popen('echo "hello world!"').read(), os.execv, os.spawnv
  os.startfile(filename)

  #Spawning processes
  os.pipe, os.waitpid, os.kill

  #Descriptor files, locks
  os.open, os.read, os.write, os.lseek, os.stat

  #File processing
  os.remove, os.rename, os.mkdir, os.rmdir

  #Administrative tools
  os.getcwd, os.chdir, os.chmod, os.getpid, os.listdir, os.access
  os.path.abspath('.')

  #Portability tools
  os.sep, os.pathsep, os.curdir, os.path.split, os.path.join

  #Pathname tools
  os.path.exists('path')
  os.path.isdir('path')
  os.path.getsize('path')
  os.path.isfile('filename')
  os.path.split(r'C:\temp\data.txt')
  os.path.join(r'C:\temp', 'output.txt')
  os.path.splitext(r'C:\PP4thEd\Examples\PP4E\PyDemos.pyw')
  os.path.dirname(filepath), os.path.basename(filepath)
  os.path.abspath(os.sep)

  import glob
  glob.glob('*')
  ```

- gzip
- bz2

---

### Misc Modules

- Documentation

  - doctest

---

---

## Design Patterns

### FP Patterns

- lambda function

  ```python
  # lambda anounymous function
  lambda arglist: func_body_expression
  namedlambda = lambda arglist: func_body_expression

  # IEFE
  (lambda arglist: func_body_expression)(arglist)
  ```

- recursion

  - Tail Recursion
    - a recursion function returns a recursive function call or return a value pattern

  ```python

  ```

- closure

  - `func.__closure__`

- curryinng

```python
def curry(func):
    print(f"decorating: {func.__qualname__}")

    def wrapper_func(*args, **kwargs):
        if len(args) + len(kwargs) >= func.__code__.co_argcount:
            print(f"evaluating: {args}, {kwargs}")
            return func(*args, **kwargs)
        print(f"currying: {args}, {kwargs}")
        return (lambda *args2, **kwargs2:
                wrapper_func(*(args + args2), **dict(kwargs, **kwargs2)))

    return wrapper_func
```

- higher order function

  - return a function
  - function factory

- memoization

  ```python
  # dict subclass functionization
  class memoiFunc(dict):
    def __init__(func):
      self.__caller = func

    def __missing__(self, key):
      return self.__caller(key)

  d = memoiFunc(print)
  d[1,2,3,4]
  ```

### Collection Class Pattern

- Python Collection Classes

  - list, range, tuple, set, str, bytes, bytearray, dictionary

- Protocol for Collection

  ```python
  # container
  elem in | not in Coll

  # sized
  len(Coll)

  # iterable
  for elem in iter(Coll)

  # sequence
  elem = Coll[index]
  index = Coll.index(elem)
  Coll.count(elem)
  Coll.reverse()

  # set

  #mutable/immutable

  ```

---

### Singleton

- Module as singleton

  ```python

  ```

---

### Synthetication and Monkey Patching

- Function Synthetic

  - dictionary based hacking

    ```python
    d = {}
    code = '''\
    def synthatic_hello():
      print("hello world")
    '''
    exec(code, d)
    d.keys()
        dict_keys(['__builtins__', 'synthatic_hello'])
    d['synthatic_hello']()

    ```

- Class Synthetic

  - class creation process

    ```python
    class A(B, C):
      def __call__(): pass
      def __new__(): pass
      def __init__(): pass

    # class creation process without metacalss
    # step 1: isolate class name, bases, metaclass and body code
    clsname = 'A'
    bases = (B, C)
    metaclass = Meta
    body = 'code after class declaration line'

    # step 2: create clsdict with type and update it with exec
    clsdict = type.__prepare__(clsname, bases)
    exec(body, globals(), clsdict)

    # step 3: class created by type
    A = type(clsname, bases, clsdict)

    ```

  - class based hacking

    ```python
    class A: pass
    class B: pass

    def ham(): pass
    def spam(): pass

    attrd = {'ham': ham, 'spam': spam}

    MySyntheticClass = type('MySyntheticClass', (A, B), attrd)
    ```

- Module Synthetic

  ```python
  #syntheticating moduels
  import sys, types
  mymodule = types.ModuleType('mysynthetic', 'docstring')
  class A: pass
  def ham(): pass
  mymodule.A = A
  mymodule.ham = ham

  sys.modules['mysynthetic'] = mymodule

  #import at somewhere
  import mysynthetic
  mysynthetic.ham()

  ```

### Factories

- Function Factory

- Class Factory

### Metaprogramming

- Intro
  
  - Code that manipunates code: read, modify, output
  - DRY: Dont's Repeate Yourself
  - metaprogramming is about creating functions and classes whose main goal is to manipulate code (e.g., modifying, generating, or wrapping existing code)
  - direct means: decorators, class decorators, and metaclasses
  - indirect means: signature objects, execution of code with exec(), and inspecting the internals of functions and classes

- User cases
  - Decorator
    - call proxy
    - interface proxy
  - Metaclass
  - Descriptor

#### metafunction

- first-class function:
  - can be assinged to a variable
  - can be called upon by names
  - can be passed as argument
  - can be returned as value

- closure
  - in function, inner scope can access outer scope's namespace, even the function returned.
    - outer scopes's names serve inner scope as free variables.
  - a decorator is a kind of closure

- function's signitures

  ```python
  from inspect import Signature, Parameter

  def func(a, b=42, *args, d=None, **kwargs): pass
  sig = inspect.signature(func)

  parms = [
      Parameter('a', Parameter.POSITIONAL_OR_KEYWORD),
      Parameter('b', Parameter.POSITIONAL_OR_KEYWORD, default=42),
      Parameter('args', Parameter.VAR_POSITIONAL),
      Parameter('d', Parameter.KEYWORD_ONLY, default=None),
      Parameter('kwargs', Parameter.VAR_KEYWORD)]

  sig2 = Signature(parms)
  ```

- function's metadata: `@functools.wraps(func)`

  - `func.__qualname__`
  - `func.__doc__`
  - `func.__annotations__`
  - `func.__wrapped__`

- functional decorator
  - decrates callable object
  - a callable return a callable
  - class implementation
  - function implementation
  - decorator status pattern
    - global variable
    - enclosing scope
    - class with attributes
    - wrap function attributes

  ```python
  @decrator
  def func(): pass

  func = decrator(func)

  # class based function decrator by utilize __call__
  class tracer:
      def __init__(self, func):
          self.calls = 0
          self.func  = func

      def __call__(self, *args):
          self.calls += 1
          print(f'call {self.calls} to {self.func.__name__}')
          return self.func(*args)

  # function based functioin decrator
  def tracer(func):
      calls = 0
      def oncall(*args):
          nonlocal calls
          calls += 1
          print(f'call {calls} to {func.__name__}')
          return func(*args)
      return oncall

  @tracer
  def spam(a, b, c):
      return a + b + c

  # decorator with arguments
  from functools import wraps, partial
  def debug(func=None, *, prefix=''):
      if func is None:
          return partial(debug, prefix=prefix)
      msg = prefix + func.__qualname__
      @wraps(func)
      def wrapper(*args, **kwargs):
          print(msg)
          return func(*args, **kwargs)
      return wrapper

  # class decorator
  def debugmethods(cls):
      for name, val in vars(cls).items():
          if callable(val):
              setattr(cls, name, debug(val))
      return cls

  #vars(cls) returns instane method only
  # cls.__getattributes__ hacking
  def debugattr(cls):
      orig_getattribute = cls.__getattribute__
      def __getattribute__(self, name):
          print('Get:', name)
          return orig_getattribute(self, name)
      cls.__getattribute__ = __getattribute__
      return cls
  ```

- built-in decorators

  ```python
  #built-in decorators
  @classmethod
  @staticmethod
  @property
  @prop.setter
  @prop.deleter
  ```

#### metaclass

- class deep dive

  - class attributes and methods

    - method
      - instance method, has `self` refer to instance namespace
      - class method, has `cls` refer to class namespace
      - static method, just like plain function
      - methods are in class's namespace `cls.__dict__`
    - attributes
      - class attributes
      - instance attributes
      - [attributes lookup deep dive](http://www.betterprogramming.com/object-attribute-lookup-in-python.html)
      - [Graphic Display](https://github.com/mzohaibqc/python-attribute-lookup)

  - metaclass is about Level of manipulation and redundancy reduction
  - Level of Manipulation
    - `super()`
      - lookup MRO, find the first match.

    - Instance Manipulation

      - creation process

        - `class.__new__(cls, *args, **args)`

          - must return obj, static method.
          - manipulate class level namespace, like methods, attributes
          - not inheritable if not called by subclass
          - if superclass’s `super().__new__` is called, `class.__init__` will also be called

        - `class.__init__(self, *args, **args)`

          - manipulate instance level namespace or class level namespace's value like attributes init
          - not inheritable if not called by subclass
          - non-static method.

      - attributes

        - decorator manipulation

          - getter/setter/deleter
          - `__getattribute__`
          - `__getattr__, __setattr__, __delattr__`

        - discriptor manipulation

          - assign attribute to discriptor
          - `__get__, __set__, __delete__`

      - callable

        - `instance.__call__(slef,*args, **kwargs)`
        - `class.__call__(cls, *args, **kwargs)`
        - `metaclass.__call__(*args, **kwargs)`

    - Class Manipulation

      - type function hacking

        - `ClassA = type(clsname, bases, clsdict)`
        - creates class type dynamiclly.

      - metaclass

        - class is an instance of type
        - inheritable
        - manipulate class definition, then can passed to instance and subclasses
        - `class A: pass` is `class A(object, metaclass=type)`

      - class underline

        - metaclass creates namespace dict by `type.__prepare__()`
          - `clsdict = type.__prepare__(clsname, bases)`
        - execute class body code to update clsdict
          - `exec(code, globals(), clsdict)`
        - metaclass creates class(instanciate class type)
          - `metaclass.__new__(mcs, clsname, bases, clsdict)`
          - `metaclass.__init__(clsname, bases, clsdict)`
          - default `metaclass` is `type`
        - return class type object for instance object use.

          ```python
          class A: pass

          # interpreted like this:
          clsname = 'A'
          metaclass=type
          bases = ()
          kwargs = {} #come from class statement, can be manipulated
          clsdict = metaclass.__prepare__(clsname, bases, **kwargs)
          A = metaclass.__new__(mcs, clsname, bases, clsdict, **kwargs)
          metaclss.__init__(A, clsname, bases, clsdict, **kwargs)
          ```

      - callable
        - `class.__call__(cls, *args, **kwargs)`
        - `metaclass.__call__(*args, **kwargs)`

    - Metaclass `__call__`

      ```python
      class TracingMeta(type):
          @classmethod
          def __prepare__(mcs, clsname, bases, **kwargs):
              clsdict = super().__prepare__(clsname, bases)
              return clsdict

          def __new__(mcs, clsname, bases, clsdict, **kwargs):
              cls = super().__new__(mcs, clsname, bases, clsdict, **kwargs)
              return cls

          def __init__(cls, clsname, bases, clsdict, **kwargs):
              super().__init__(name, bases, namespace)

          def metamethod(cls):
              pass

          def __call__(cls, *args, **kwargs):
              obj = super().__call__(*args, **kwargs)
              return obj

      class TracingClass(metaclass=TracingMeta):

          def __new__(cls, *args, **kwargs):
              obj = super().__new__(cls)
              return obj

          def __init__(self, *args, **kwargs):
              pass

          def instmethod(self):
              pass

          @classmethod
          def __call__(cls, *args, **kwargs):
              pass

      class TC2(TracingClass):

          def __init__(self, *args, **kwargs):
              pass

          def __call__(self, *args, **kwargs):
              pass

      # Module Load:
      TrackingClass definition:
          TracingMeta.__prepare__
            type.__prepare__
          return clsdict

          TracingMeta.__new__
            type.__new__
          return cls

          TracingMeta.__init__
            type.__init__

      TC2 definition:
          TracingMeta.__prepare__
            type.__prepare__
          return clsdict

          TracingMeta.__new__
            type.__new__
          return cls

          TracingMeta.__init__
            type.__init__

      def main():
          a = TracingClass(1, '1', msg="TracingClass")
            TracingMeta.__call__
                type.__call__
                    TracingClass.__new__ with arg
                    TracingClass.__init__ with arg
            return TracingClass instance

          b = TC2(2, '2', msg='tc2')
            TracingMeta.__call__
                type.__call__
                    TracingClass.__new__ with arg
                    TC2.__init__ with arg
            return TC2 instance

          b(3, msg='TC2 instance called')
            TC2.__call__
              TracingClass.__call__

          b.instmethod()
            b.__dict__, TC2.__dict__, TracingClass.__dict__

          TC2.metamethod()
            TC2.__dict__. TC2.__class__.__dict__, TracingClass.__dict__

      ```

- class decorator decrates class object

  - class implementation
  - function implementation
  - method decorate need special attention

  ```python
  # function based class decrator
  def ProxyClass(cls):
      class Proxy:
          def __init__(self, *args):
              self.wrapped = cls(*args)
          def __getattr__(self, name):
              return getattr(self.wrapped, name)
      return Proxy

  @ProxyClass
  class C:
      pass

  X = C()
  X.attr
  ```

- class descriptor

  - a class that implements the three core attribute access operations (get, set, and delete) in the form of `__get__(), __set__(), and __delete__()` special methods.
  - capture the core instance operations (get, set, delete) at a very low level and completely customize what they do
  - descriptors is that they can only be defined at the class level, not on a per-instance basis
  - Data Descripter
    - `__get__, __set__, __delete__`
  - Non-Data Descripter
    - `__get__, __delete__`

  ```python
  attribut = property(fget, fset, fdel, docstr)
  class Descriptor:
    "docstring goes here"
    def __get__(self, instance, owner): pass
    def __set__(self, instance, value): pass
    def __delete__(self, instance): pass

  class A:
    attr = Descriptor()

  x = A()
  x.attr
  # x.attr  ->  Descriptor.__get__(Subject.attr, x, Subject)

  ```

- Gochas

  - metaclass calling process

    - prepare stage
      - `__prepare__()` method is called first and used to create the class namespace.
    - creating stage
      - `__new__()` method is used to instantiate the resulting type object. It is called after the class body has been fully executed.
      - invoked prior to class creation and is typically used when a metaclass wants to alter the class definition in some way by alter class namespace.
    - init stage
      - `__init__()` method is called last and used to perform any additional initialization steps fully executed.
      - inspect the class dictionary, base classes, and more

  - metaclass with arguments
    - `class Spam(metaclass=MyMeta, debug=True, synchronize=True)`
    - in metaclass `__prepare__(), __new__(), and __init__()` must match this signature

- metaclass

  - `class` is instance of type
  - `type` is class of type()
  - `types.new_class(name, bases=(), kwds=None, exec_body=None)`
    - manipulate class type has metaclass, create new metaclass type object.
  - `type` a is class, `metaclass` is a subclass of `type`

    - `type(object) is type; type(myclass) is type`
    - `type(<name>, <bases>, <dct>)` creates a new class

      - `<name>` specifies the class name. This becomes the `__name__` attribute of the class.
      - `<bases>` specifies a tuple of the base classes from which the class inherits. This becomes the `__bases__` attribute of the class.
      - `<dct>` specifies a namespace dictionary containing definitions for the class body. This becomes the `__dict__` attribute of the class.

      ```python
      # class statemetns undertable
      class classname(*bases):
          def __init__(): pass
          def att(): pass

      #actual process:
      class = type(classname, bases, attributedict)
      type.__call__
        type.__new__(mcs, classname, bases, attributedict)
        type.__init__(classname, bases, attributedict)

      # class statement with metaclass undertable
      # class statemetns undertable
      class classname(*bases, metaclass=Meta):
          def __init__(): pass
          def att(): pass

      #actual process:
      class = Meta(classname, bases, attributedict)
      Meta __call__ methods overaded by invoke:
        Meta.__new__(mcs, classname, bases, attributedict)
        Meta.__init__(mcs, classname, bases, attributedict)
      ```

  - metaclass inheritance

    - Metaclasses inherit from the type class (usually)
    - Metaclass declarations are inherited by subclasses
    - Metaclass attributes are not inherited by class instances
    - Metaclass attributes are acquired by classes

  - User case
    - Introspection attributes and tools
    - Operator override methods
    - Attribution interception methods
    - Class properties
    - Class attributes descriptors
    - Function/Class decorators
    - Metaclass

  ```python
  # metaclass convention
  class MyMeta(type):
    def __new__(mcs, clsname, bases, clsdict):
        # clsname is name of class being defined
        # bases is tuple of base classes
        # clsdict is class dictionary
        do_init_manipulation_stuff()
        return super().__new__(mcs, clsname, bases, clsdict)

    # if no __new__, alternatively using __init__
    def __init__(mcs, clsname, bases, clsdict):
        super().__init__(clsname, bases, clsdict)
        # clsname is name of class being defined
        # bases is tuple of base classes
        # clsdict is class dictionary
        do_init_manipulation_stuff()

  class Root(metaclass=MyMeta):
      pass

  class A(Root):
      pass

  class C(A):
      pass

  X = C()
  X.attr

  from inspect import Parameter, Signatur

  def make_signature(names):
    return Signature(Parameter(name,Parameter.POSITIONAL_OR_KEYWORD) for name in names)

  class StructMeta(type):
    def __new__(cls, name, bases, clsdict):
      clsobj = super().__new__(cls, name,bases, clsdict)
      sig = make_signature(clsobj._fields)
      setattr(clsobj, '__signature__', sig)
      return clsobj

  class Structure(metaclass=StructMeta):
    _fields = []
    def __init__(self, *args, **kwargs):
      bound = self.__signature__.bind(*args, **kwargs)
      for name, val in bound.arguments.items():
        setattr(self, name, val)

  class Stock(Structure):
    _fields = ['name', 'shares', 'price']

  class Point(Structure):
     _fields = ['x', 'y']

  class Host(Structure):
    _fields = ['address', 'port']

  ```

#### Reflection

- Intro
  - The abilty of a program to view and modify its own stucture at runtime
  - structures in Python
    - code
    - globals
    - locals
    - call stack
    - runtime??

- Practice

```python
globals()
locals()
exec(code, global_dict, local_dict)
eval()
compile()
inspect:
  inspect.getsourcelines(obj)
  inspect.getouterframes(frame)
sys.settrace(func)
  frame
    frame.f_globals
    frame.f_locals
    frame.f_code
      'co_argcount',
      'co_cellvars',
      'co_code',
      'co_consts',
      'co_filename',
      'co_firstlineno',
      'co_flags',
      'co_freevars',
      'co_kwonlyargcount',
      'co_lnotab',
      'co_name',
      'co_names',
      'co_nlocals',
      'co_stacksize',
      'co_varnames
```

---

---

## Best Practice

### Install and Initialize python

- install python

  - install python package
  - config python path if necessary
  - setup virtualenv

- install anaconda

### pip Package Manager

- pip list

  - `pip list`
  - `pip list --outdated`
  - `pip show SomePackage`
  - `pip search "keyword"`

- pip intall

  - `python -m pip install -U pip`
  - `pip install -U pip`
  - `pip install SomePackage`
  - `pip install SomePackage==1.0.4`
  - `pip install 'SomePackage>=1.0.4'`
  - `pip install --upgrade SomePackage`

- pip requirements.txt

  - `pip install -r requirements.txt`
  - `pip freeze > requirements.txt`
    - `pkg3>=1.0,<=2.0`
    - `ProjectB<1.3`

- pip unintall
  - `pip uninstall SomePackage`

### Anaconda Package Manager

- conda info

  - `conda --version`
  - `conda info`

- virtual env

  - `conda info --envs`
  - `onda create -n MYENV`
  - `conda create -n MYENV --clone OLDENV`
  - `conda create -n MYENV python=3.6.0`
  - `conda activate MYENV`
  - `conda deactivate MYENV`
  - `conda remove -n MYENV --all`
  - `conda env export --name MYENV > myenv.yml`
  - `conda env create --file myenv.yml`
  - `conda list --explicit > pkgs.txt`
  - `conda create --name NEWENV --file pkgs.txt`

- conda update

  - `conda update conda`
  - `conda update -n base conda`
  - `conda update anaconda`
  - `conda update --all`
  - `conda update PACKAGE`

- package management

  - `conda list`
  - `conda search PACKAGE`
  - `conda install PACKAGE`
  - `conda install PACKAGE=M.N.P`
  - `conda install "PACKAGE>1.11"`
  - `conda install "PACKAGE=1.11.1|1.11.3"`
  - `conda install "PACKAGE>=1.8,<2"`

### virtualenv Install and Setup

- `pip install virtualenv`
- `cd MYENV_ROOT`
- `virtualenv MYENV`
  - `virtualenv --python=python3.7.5 MYENV`
- `source MYENV_ROOT/MYENV/bin/activate`
- `which python`
- `deactivate`

### pyinstaller Executable Package Tool

- `pip install pyinstaller`
- `pyinstaller main.py`
- `pyinstaller --onfile main.py`

### inno Setup Install Wizard Tool

- GUI

### Testing

#### Terms

- Level of test

  - Unit Test
  - Integration Test
  - System Test

- Acceptance/Regression Test

- Test-driven Development

  - Approval Test

- Test Double
  - Dummy Object
  - Mock Object
  - Fake Object
  - Test Stub
  - Test Spy

#### Unit Test

- doctest

  - start run

    - `python -m doctest -v doctestDemo.py`
    - `python doctestDemo.py -v`
    - add doctest module
      - `import docktest; doctest.testmod()`
      - `python doctestDemo.py`

  - practice

    ```python
    def doctestDemo(a, b):
        """
        >>> doctestDemo(3, 1)
        7

        >>> doctestDemo(3, 4)
        7

        >>> doctestDemo("str1", "str2")
        'str1str2'

        >>> doctestDemo(3, "3")
        Traceback (most recent call last):
            ...
        TypeError: unsupported operand type(s) for +: 'int' and 'str'
        """
        return a + b

    if __name__ == "__main__":
        print(doctestDemo(1, 8))
    ```

- unittest

  - terms

    - test case
      - test case name
      - arrange
      - act
      - assert
    - test suite
    - test runner
    - test fixture
      - setUp
      - tearDown

  - pattern

    - setup
    - run test
    - teardown

  - start run

    - `python -m unittest -q test_unittest_demo.py`
    - `python -m unittest`

  - practice

    ```python
    # unittestDemo.py
    def adder(a, b):
        return a + b
    def adder10(a):
        return adder(a, 10)
    class Pet(object):
        __counter = 0
        def __init__(self, name):
            Pet.__counter += 1
            self._name = name
        @property
        def name(self):
            return self._name
        @name.setter
        def name(self, value):
            self._name = value
        @classmethod
        def counts(cls):
            return cls.__counter

    # test_unittestDemo.py
    import unittest
    from unittestDemo import adder, adder10, Pet

    class unittestDemoTester(unittest.TestCase):
        def test_adder(self):
            """testing adder()"""
            self.assertEqual(adder(1, 5), 6)
            self.assertNotEqual(adder(10, 10), 10)

        def test_adder10(self):
            """testing adder10()"""
            self.assertEqual(adder10(5), 15)
            self.assertEqual(adder10(5), adder(5, 10))
            self.assertNotEqual(adder10(20), 15)

        def test_Pet(self):
            """testing Pet Class"""
            cat = Pet("cat")
            dog = Pet("dog")
            self.assertEqual(cat.name, "cat")
            self.assertEqual(dog.name, "dog")
            dog.name = "DDDDog"
            self.assertEqual(dog.name, "DDDDog")
            self.assertEqual(Pet.counts(), 2)

    if __name__ == "__main__":
        print(f"Run {__file__} : ")
        unittest.main()

    ```

- pytest

  - terms:

    - test case
      - test case name
      - arrange
      - act
      - assert
    - test suite
    - test runner
    - test fixture
      - scoping setUp/tearDown
        - module scope
        - class scope
        - function scope
        - method scope

  - pattern

  - start run

    ```bash
    pip install pytest
    python -m pytest
    python -m pytest --doctest-modules
    pytest dir/
    pytest test_file.py
    pytest --fixtures
    ```

  - practice

  ```python

  @pytest.fixture
  def resource():
      arrange_resources()
      yield resources
      cleanup_resources()

  def test_cases_tobe_tested_by_business_module():
      pytest.skip('skip this test')
      arrangement_for_test)()
      action_on_arrangement()
      assert True!=False
      with pytest.raises(ExceptionType):
          exception_raising_actions()
  
  # Monckeypatching
  monkeypatch.setattr(obj, name, value, raising=True)
  monkeypatch.delattr(obj, name, raising=True)
  monkeypatch.setitem(mapping, name, value)
  monkeypatch.delitem(obj, name, raising=True)
  monkeypatch.setenv(name, value, prepend=False)
  monkeypatch.delenv(name, raising=True)
  monkeypatch.syspath_prepend(path)
  monkeypatch.chdir(path)

  #tmpdir
  tmp_path
  tmpdir.mkdir("sub").join("hello.txt")
  fn = tmpdir_factory.mktemp("data").join("img.png")

  #skip
  @pytest.mark.skip(reason="no way of currently testing this")
  @pytest.mark.skipif(sys.version_info < (3, 6), reason="requires python3.6 or higher")
  pytest.skip('no way of currently testing this')

  #xfail
  @pytest.mark.xfail
  pytest.xfail("failing configuration (but should work)")

  @pytest.mark.parametrize("i", range(50))
  def test_num(i):
    if i in (17, 25):
      pytest.fail("bad luck")
  ```

---

---

## Misc

### Learning Resources

- [Python Website](https://www.python.org/)
- [Python Docs](https://docs.python.org/3.7/)
- [Python Language Ref](https://docs.python.org/3/reference/index.html)
- [Python Standard Libs](https://docs.python.org/3/library/index.html)
- [Python Modules By Example](https://pymotw.com/3/#)
