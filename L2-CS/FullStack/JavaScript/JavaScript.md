---
id: JavaScript
title: JavaScript, A Web Oriented Programming Language
sidebar_label: JavaScript
---

## Overview

### Features

- lightweight interpreted scripting language
- dynamic type language
- prototype-based OOP
- functional programming
- Web Oriented, client-side prone
- server-side nodejs

---

### Applicable Scenarios

- Web Apps: server/client side

---

### JavaScript engine(V8)

- Implements just-in-time compilation ECMAScript
- Implements WebAssembly
- Supported by Linux with x64, IA-32, ARM, or MIPS
- Written in C++
- Used by chrome, Node.js
- Single threaded execution engine
- Proccessing:
  - compile
  - execute
  - Handling call stack
  - Handling event loop
  - memory allocation
  - garbage collects
- V8 engine Components:
  ![Alt](/img/JS-V8-engine-components.png "engine components")
- V8 engine pipeline:
  ![Alt](/img/JS-V8-engine-pipeline.svg "engine pipeline")
- V8 Optimization: [V8 Website](https://v8.dev/)
  - hidden classes optimization
  - number, 31-bit interger using tagged values
  - array, double numbers using unboxing
  - array, dense with fast elements linear store, sparse with dictionary element hash store
  - monomorphic is better than polymorphic
  - optimizing compiler using type feedback
  - IC: inline cacheing
  - minimize try-catch block
- Other Engines
  - Firefox has SpiderMonkey
  - Safari has JavaScriptCore (also called Nitro)
  - Edge has Chakra

---

### JavaScript Runtime

- Node.js
- Browsers, Chrome, Firefox, Edge
- Runtime Diagram:
  ![Alt](/img/JS-runtime.png "JavaScript Runtime")

---

---

## JavaScript Language Basic

---

<!-- ### Building Blocks -->

### Variable and Scope

- Variable Identifier:

  - `/(Starter)(Repeater)*/`:
    - Starter
      - `/[$_A-Za-z]/`
    - Repeater
      - `/(starter)|\d/`
      - `/[$_A-Za-z0-9]/`

- Reserved Words:

  - `await, break, case, catch, class, const, continue, debugger, default, delete, do, else, export, extends, finally, for, function, if, import, in, instanceof, new, return, super, switch, this, throw, try, typeof, var, void, while, with, yield`
  - `let, static`
  - `enum, implements, package, protected, interface, private, public`

- Excution Context

  - Global Excution Context
  - Current Excution Context
  - Excution Context Object
    - variable object, VO
    - scope chain, SC
    - `this` context
  - excution phases:
    - creation phase
      - init VO
        - arguments creation for current EC
        - function declaration scanning, function objects init pointer to coresponding function declaration.
        - variable declaration scanning, variable objects init value to `undefined`
      - init SC
        - function creates new Scope
        - current scope VO appends to scope chain
        - lexical scoping
          - variable searching in VO from inner to outer scope chain.
      - init `this`
        - assigned only when function is called.
        - regular function `this` points to global EC
        - arrow function `this` points to outer EC
        - method function `this` points to the Object binds this method.
    - excution phase
      - evaluation variables

- Namespacing and scoping:

  - Global Scope
    - `'use strict';` at begining to put JS in strict mode.
    - variables declared outside any function, block, or module scope have global scope.
  - Block Scope
    - variable is defined inside a block `{}` using the let or const keywords
  - Function Scope
    - `'use strict';` at start of function body to put function statements in strict mode
    - variables declared inside a function
  - Local Scope
    - current execution context scope
  - Hoisting
    - no matter where functions/variables are declared, during code parse time, they are registered to coresponding scope buckets(variable object) for refference.
  - Name Shadowing
    - inner scope varaible shadows outer scope chain name
  - Scope Chain
    - used to resolve variables,from inner to outer.
  - Closures
    - A closure can also access the variables of its outer function even after the function has returned. This allows the returned function to maintain access to all the resources of the outer function.
    - Memery efficiency
    - Encapsulatio Pattern
  - Module

    - `import name from '/modules/square.js';`
    - `import { name, draw } from '/modules/square.js';`
    - `import {default as randomSquare} from '/modules/square.js';`
    - `import { newFunctionName as function1 } from '/modules/square.js';`
    - `import * as Module from '/modules/module.js';`
    - `export default function () {};`
    - `export default function name() {};`
    - `export default expression;`
    - `export function name() {}`
    - `export name; // where name is const`
    - `export { name, draw };`
    - `export { function1 as newFunctionName }`

  - Ref: [Understanding Scope in JavaScript](https://scotch.io/tutorials/understanding-scope-in-javascript)

- let, const, var

  - `const` means that the identifier can’t be reassigned.
  - `let` used only in the block it’s defined in
  - `var`
    - declarations are globally scoped or function scoped,
    - variables can be re-declared and updated
    - variables can be used before declare by hoisting mechanism.

- Block: `{ ... }`

  - if, else if, else statement
  - switch statement
  - for statement
  - do statement
  - while statement

- Variable Lifecycle

  - Global excution context
  - undeclared, doesn't exist.
  - undefined, exists, but no value.

    - uninitialized

    - TDZ: temporary dead zone

  - Phases

    - Declaration
    - Initialization
    - Assignment

  - `var` lifecycle

    - compilation
      - declaration
      - initialization
    - execution
      - assginment

  - `undecalared` lifecycle

    - compilation
    - execution
      - declaration
      - initialization
      - assginment

  - `let` lifecycle

    - compilation
      - declaration
    - execution
      - initialization
      - assginment

  - `const` lifecycle

    - compilation
      - declaration
    - execution
      - initialization, Assginment

  - `function` lifecycle
    - compilation
      - declaration
      - initialization
      - assginment
    - execution

---

### Primitive Types

- Primitive Types
  - string, number, bigint, boolean, null, undefined, symbol, object
- Primitive wrapper objects
  - String, Number, BigIn, Boolean, Symbol, Object
- Variable VS Value
  - in JS, varaible has no type, value has type.
- Coercion, aka type conversion
  - string hint, `toString()`.
  - number hint, `valueOf()`
  - toPrimitive()
    - toString()
    - toNumber()
      - `[] == false; //true`
      - `!![]; //true`
    - toBoolean()
  - Boxing
    - primitive to Object implicite conversion
- typeof operator

  - retrun string
  - `"string", "number", "bigint", "boolean", "null", "undefined", "symbol", "object", "function"`

  ```javascript
  // Numbers
  typeof 37 === "number";
  typeof 3.14 === "number";
  typeof 42 === "number";
  typeof Math.LN2 === "number";
  typeof Infinity === "number";
  typeof NaN === "number"; // Despite being "Not-A-Number"
  typeof Number("1") === "number"; // Number tries to parse things into numbers
  typeof 42n === "bigint";

  // Strings
  typeof "" === "string";
  typeof "bla" === "string";
  typeof `template literal` === "string";
  typeof "1" === "string"; // note that a number within a string is still typeof string
  typeof typeof 1 === "string"; // typeof always returns a string
  typeof String(1) === "string"; // String converts anything into a string, safer than toString

  // Booleans
  typeof true === "boolean";
  typeof false === "boolean";
  typeof Boolean(1) === "boolean"; // Boolean() will convert values based on if they're truthy or falsy
  typeof !!1 === "boolean"; // two calls of the ! (logical NOT) operator are equivalent to Boolean()

  // Symbols
  typeof Symbol() === "symbol";
  typeof Symbol("foo") === "symbol";
  typeof Symbol.iterator === "symbol";

  // Undefined
  typeof undefined === "undefined";
  typeof declaredButUndefinedVariable === "undefined";
  typeof undeclaredVariable === "undefined";

  //Null
  typeof null === "object";

  // Objects
  typeof { a: 1 } === "object";

  // use Array.isArray or Object.prototype.toString.call
  // to differentiate regular objects from arrays
  typeof [1, 2, 4] === "object";

  typeof new Date() === "object";
  typeof /regex/ === "object"; // See Regular expressions section for historical results

  // The following are confusing, dangerous, and wasteful. Avoid them.
  typeof new Boolean(true) === "object";
  typeof new Number(1) === "object";
  typeof new String("abc") === "object";
  typeof new Function() === "function";

  // Functions
  typeof function() {} === "function";
  typeof class C {} === "function";
  typeof Math.sin === "function";
  ```

#### string/String Wrapper Object

- string literal

  - single quote: `'string'`
  - double quote: `"string"`
  - escaping char: `'string\'s length'`
  - string template literals

    ```javascript
    `hello world!` ...
    `hello ${myVal} world` ...
    ```

  - Long literal

    ```javascript
    let longString =
      "This is a very long string which needs " +
      "to wrap across multiple lines because " +
      "otherwise my code is unreadable.";
    let longString =
      "This is a very long string which needs \
    to wrap across multiple lines because \
    otherwise my code is unreadable.";
    ```

- Properties:

  - `str.length`
  - `String.prototype`
    - `str.__proto__`

- Static methods

  - `String.fromCharCode(189, 43, 190, 61)`
  - `String.fromCodePoint(9731, 9733, 9842, 0x2F804)`
  - `` String.raw`templateString`; ``
  - `String.raw() method is a tag function of template literals`

- Methods

  - `character = str.charAt(index)`
  - `newStr = str.toLowerCase()`
  - `newStr = str.toLocaleLowerCase([locale, locale, ...])`
  - `newStr = str.toUpperCase()`
  - `newStr = str.toLocaleUpperCase([locale, locale, ...])`

  - `charCode = str.charCodeAt(index)`
  - `codePoint = str.codePointAt(index)`
  - `str.normalize([form])`

  - `newStr = str.concat(string2[, string3, ..., stringN])`

    - `+, += operator is prefered.`

  - `bool = str.endsWith(searchString[, searchBackwardPos])`
  - `bool = str.startsWith(searchString[, searchForwardPos])`
  - `bool = str.includes(searchString[, searchForwardPos])`
  - `index = str.indexOf(searchString[, searchForwardPos]) //-1 returns if not found`
  - `index = str.lastIndexOf(searchString[, searchBackwardPos]) //-1 returns if not found`
    - `'abab'.lastIndexOf('ab', 2)` will return 2 and not 0, as fromIndex limits only the beginning of the match.
  - `bool = str.localeCompare(compareString[, locales[, options]])`

  - `matchArr = str.match(regexp)`
  - `matchIterator = str.matchAll(regexp)`
  - `index = str.search(regexp)`
  - `newStr = str.replace(regexp|substr, newSubstr|function)`

  - `strArr = str.split([separator[, limit]]) //separator can be a RegExp`
  - `newStr = str.slice(beginIndex[, endIndex])`
  - `newStr = str.substring(indexStart[, indexEnd])`

  - `newStr = str.padEnd(targetLength [, padString])`
  - `newStr = str.padStart(targetLength [, padString])`
  - `newStr = str.repeat(count);`

  - `newStr = str.trim()`
  - `newStr = str.trimEnd()`
  - `newStr = str.trimStart()`

  - `str = StringObj.toString()`
  - `str = StringObj.valueOf()`

  - `strIterator = str[Symbol.iterator]()`
  - `for (char of str) { ... }`

- Global built-in
  - `int = parseInt(string, radix)`
  - `float = parseFloat(string)`
  - `value = eval(string)`
  - `encodeURI(uriString), encodeURIComponent(uriString)`
  - `decodeURI(encodedURI), decodeURIComponent(encodedURI)`

#### number/Number Wrapper Object

- number literal

  - int: `let i = 5`
  - float: `let f = 5.0`
  - negative: `let n = -4`
    - `-0`
  - NaN: `NaN`
    - `NaN` has no equal value to it, `NaN === NaN; //return false`.
  - Infinity: `Infinity`

- Properties:

  - `Number​.EPSILON`
  - `Number​.MAX_SAFE_INTEGER`
    - 2^53-1
  - `Number​.MAX_VALUE`
  - `Number​.MIN_SAFE_INTEGER`
    - -(2^53-1)
  - `Number​.MIN_VALUE`
  - `Number​.NEGATIVE_INFINITY`
  - `Number​.NaN`
  - `Number​.POSITIVE_INFINITY`
  - `Number​.prototype`
    - `num.__proto__`

- Static methods

  - `bool = Number​.isFinite( val )`
  - `bool = Number​.isNaN( val )`
  - `num = Number​.parse​Float( string )`
  - `num = Number.parseInt(string,[ radix])`
  - `bool = Number​.isInteger( val )`
    - `Number.isInteger(Number.MAX_VALUE+4); //true`
    - `Number.isInteger(99999999999999999999999); // true`
    - `Number.isInteger(5.0); // true`
    - `Number.isInteger(5.000000000000001); // false`
    - `Number.isInteger(5.0000000000000001); // true`
  - `bool = Number​.isSafe​Integer( val )`

- Methods

  - `str = num.toExponential([fractionDigits])`
  - `str = num.toFixed([fractionDigits])`
  - `str = num.toPrecision([precision])`
  - `str = num.toString([radix])`

    - `radix ranges [2, 36]`

  - `num = numObj.valueOf()`

- Global built-in

  - `parseInt(string,[ radix])`
  - `parseFloat(string)`
  - `isFinite()`
  - `isNaN`

#### bitint/BigInt Wrapper Object

- bitint literal

  - `let bn = 223n`

#### boolean/Boolean Wrapper Object

- boolean literal

  - `let b = true`
  - `let b = false`

- Truthy objects

  - `[], {}, new Function()...`

- Falsy values
  - `null, undefined, '', "", 0, -0, NaN`

#### Symbol Wrapper Object

- Symbol constructor

  - `let sym = Symbol([description string])`
  - `let sym = Symbol()`
  - `let sym = Symbol("a symbal")`

- Usage
  - a symbol value is created by invoking the function `Symbol()`, which dynamically produces an anonymous, unique value
  - A symbol value may be used as an identifier for object properties; this is the data type's only purpose.

<!-- ### Structures -->

---

### Operators

- operator associativity

  - left-to-right
  - right-to-left
    - `x = y = z = 10;`

- operator precedence

  - [ref](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence)

- grouping operator

  - `( )`

- new operator

  - `new ObjConstructro( ... )`
  - `new Obj` is equal to `new Obj()` when no parameters passed.
  - new operator process
    ![Alt](/img/JS-operator-new.png "new operator process")

- member access operator

  - `obj.property_name`

- computed member access operator

  - `obj[computed_property]`
  - `obj["property_name"]`

- function call operator

  - `func( ... )`
  - `( arg => { body_statements; } )( args ) // IIFE`

- increment/decrement operator

  - `operand++, ++operand, operand--, --operand`
  - If used postfix (for example, x--), then it returns the value before decrementing.
  - If used prefix (for example, --x), then it returns the value after decrementing.

- typeof operator

  - `typeof operand`
    - `typeof null // "object"`
  - `typeof( operand )`
  - typeof always returns a string
    - `"undefined", "string", "number", "boolean", "function", "object"`

- void operator

  - `void expression`
  - `void( expression )`
  - The `void` operator returns `undefined` after expression evaluation, used merely to obtain the `undefined` primitive value.

- delete operator

  - `delete obj.property`
  - `delete obj['property']`
  - delete operator removes a property from an object

- await operator

  - `[rv] = await expression;`
    - `var x = await resolveAfter2Seconds(10);`
  - `await` returns the fulfilled value of the promise, or the value itself if it's not a Promise.
  - `await` can only be used inside an async function.

- arithmetic operators

  - `+, -, *, /, %, **`
  - `+operand, -operand`

- string concatenate operators

  - `+`

- bitwise operators

  - `&, |, ^, ~`
    - bitwise AND, OR, XOR, NOT
  - `>>, >>>, <<`
    - bitwise SHIFT_RIGHT_SIGN_FILL, SHIFT_RIGHT_0_FILL, SHIFT_LEFT

- comparison operators

  - `>, >=, <, <=`

- equality operators

  - `operand1 == operand2`
    - `2 == '2' is true`
    - `false == 'false' is false`
  - `operand1 === operand2`
    - `2 === '2' is false`
  - `!=, !==`
  - primitive types compare by value
  - object types compare by memeory slot

- in operator

  - `'property' in Obj`
  - `in` operator returns true if the specified property is in the specified object or its prototype chain.

- instanceof operator

  - `obj instanceof ObjConstructor`
  - `instanceof` operator tests whether the prototype property of a ObjConstructor appears anywhere in the prototype chain of an obj.

- logical operators

  - `&&, ||, !`
    - no xor in JS, xor pattern: `(x && !y || !x && y)`

- conditional ternary operator

  - `condition ? exprIfTrue : exprIfFalse`

- assignment operators

  - `=`
  - `+=, -=, *=, /=, %=, **=`
  - `>>=, <<=, >>>=`
  - `>>>=`
    - SHIFT_RIGHT_0_FILL assignment
  - `&=, ^=, |=,`
    - `^=` bitwise xor

- destructuring assignment operator

  - `[a, b] = [10, 20]`
  - `[a, b, ...rest] = [10, 20, 30, 40, 50]`
  - `[a=5, b=7] = [1]; // array destructure with default value`
  - `[a, b] = [b, a];`
  - `[a, , b] = [1, 2, 3]`

  - `({ a, b } = { a: 10, b: 20 });`
  - `({a: foo, b: bar} = { a: 10, b: 20 }); //foo = 10, bar = 20`
  - `({a: aa = 10, b: bb = 5} = {a: 3}); //aa = 3, bb = 5`
  - `({a, b, ...rest} = {a: 10, b: 20, c: 30, d: 40});`

- yeild operator

  - `[rv] = yield [expression];`
    - `yield` keyword is used to pause and resume a generator function `(function*)`
    - `let iterator = Generator(args); iterator.next().value`
  - `yield* [[expression]];`
    - `yield*` expression is used to delegate to another generator or iterable object.

- comma/sequence operator

  - `expr1, ..., exprn`

- array indexing operator(computed member access operator)

  - `arr[index]`

- rest operator

  ```javascript
  function sum(...args) {
    return args.reduce((accumulator, current) => {
      return (accumulator += current);
    });
  }
  ```

- spread operator

  ```javascript
  const adrian = {
    fullName: "Adrian Oprea",
    occupation: "Software developer",
    age: 31,
    website: "https://oprea.rocks"
  };
  const bill = {
    ...adrian,
    fullName: "Bill Gates",
    website: "https://microsoft.com"
  };
  const numbers1 = [1, 2, 3, 4, 5];
  const numbers2 = [...numbers1, 1, 2, 6, 7, 8];
  // this will be [1, 2, 3, 4, 5, 1, 2, 6, 7, 8]
  ```

- arrow function operator

  - `(param1, param2, …, paramN) => { statements }`
  - `(param1, param2, …, paramN) => expression`

---

### Objects

#### Object Introduction

- Properties
  - data properties
    - value
    - enumerable
    - writable
    - configurable
  - access properties
    - getter
    - setter

#### Object Type

- Object literal

  - `let obj = { [ nameValuePair1[, nameValuePair2[, ...nameValuePairN] ] ] }`
  - `let obj = new Object([value])`

- Object properties

  - `Object​.prototype`

- Object methods

  - `retrunObj = Object.assign(target, ...sources)`

    - copy the values of all enumerable own properties from one or more source objects to a target object. It will return the target object.
    - Properties in the target object will be overwritten by properties in the sources if they have the same key.
    - shallow copy
    - deep copy pattern

      ```javascript
      obj1 = { a: 0, b: { c: 0 } };
      let obj3 = JSON.parse(JSON.stringify(obj1));
      ```

  - `obj = Object.create(proto, [propertiesObject])`

    - creates a new object, using an existing object as the prototype of the newly created object.

  - `obj = Object.defineProperties(obj, props)`

    - defines new or modifies existing properties directly on an object, returning the object

  - `obj = Object.defineProperty(obj, prop, descriptor)`

    - defines a new property directly on an object, or modifies an existing property on an object, and returns the object.
    - descriptor
      - `let descriptor = { enumerable: false, configurable: false, writable: false, value: 'static'}`

  - `descriptor = Object.getOwnPropertyDescriptor(obj, prop)`

    - descriptor is an object with some of the following attributes
      - `value, writable, get, set, configurable, enumerable`

  - `descriptorsObj = Object.getOwnPropertyDescriptors(obj)`

  - `propsStringArr = Object.getOwnPropertyNames(obj)`

  - `symbolPropsArr = Object.getOwnPropertySymbols(obj)`

  - `bool = obj.hasOwnProperty(prop)`

  - `bool = obj.propertyIsEnumerable(prop)`

  - `string = obj.toLocaleString()`

  - `string = obj.toString()`

  - `primitiveValue = object.valueOf()`

  - `protoObj = Object.getPrototypeOf(obj)`

  - `newObj = Object.setPrototypeOf(obj, prototype)`

  - `bool = prototypeObj.isPrototypeOf(object)`

  - `bool = Object.is(value1, value2);`

    - more strict equality check then `===`
    - `Object.is(-0, 0); //return false`
    - `-0 === 0 ; //return true`

  - `bool = Object.isExtensible()`

  - `unextendObj = Object.preventExtensions(obj)`

  - `sealObj = Object.seal(obj)`

  - `bool = Object.isSealed(obj)`

  - `bool = Object.isFrozen(obj)`

  - `frozenObj = Object.freeze(obj)`

    - Nothing can be added to or removed from the properties set of a frozen object
    - shallow freeze
    - deep freeze pattern

      ```javascript
      function deepFreeze(object) {
        // Retrieve the property names defined on object
        var propNames = Object.getOwnPropertyNames(object);

        // Freeze properties before freezing self

        for (let name of propNames) {
          let value = object[name];

          object[name] =
            value && typeof value === "object" ? deepFreeze(value) : value;
        }

        return Object.freeze(object);
      }
      ```

  - `propsStringArr = Object.keys(obj)`

  - `propsValueArr = Object.values(obj)`

  - `let keyValueArray = Object.entries(obj)`

    ```javascript
    // iterate through key-value gracefully
    const obj = { a: 5, b: 7, c: 9 };
    for (const [key, value] of Object.entries(obj)) {
      console.log(`${key} ${value}`); // "a 5", "b 7", "c 9"
    }

    // Or, using array extras
    Object.entries(obj).forEach(([key, value]) => {
      console.log(`${key} ${value}`); // "a 5", "b 7", "c 9"
    });

    //Converting to a Map
    const map = new Map(Object.entries(obj));
    ```

  - `obj = Object.fromEntries(iterable);`

    - iterable argument is expected to be an object that implements an @@iterator method, that returns an iterator object, that produces a two element array-like object.

#### Built-in Object: Date

- Date Object

  - `moment` module recommanded.

  ```javascript
  //constructor
  new Date();
  new Date(value);
  new Date(dateString);
  new Date(year, monthIndex [, day [, hours [, minutes [, seconds [, milliseconds]]]]]);

  //examples
  var today = new Date();
  var birthday = new Date('December 17, 1995 03:24:00');
  var birthday = new Date('1995-12-17T03:24:00');
  var birthday = new Date(1995, 11, 17);
  var birthday = new Date(1995, 11, 17, 3, 24, 0);

  //raw timing pattern
  var start = Date.now();
  doSomethingForALongTime();
  var end = Date.now();
  var elapsed = end - start; // elapsed time in milliseconds
  ```

- [Date MDN Ref](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

#### Built-in Object: Math

- Math Object

  - not a function constructor
  - no instance can be created from Math.

  ```javascript
  //properties
  Math.E; //Euler's constant and the base of natural logarithms, approximately 2.718.
  Math.LN2; //Natural logarithm of 2, approximately 0.693.
  Math.LN10; //Natural logarithm of 10, approximately 2.303.
  Math.LOG2E; //Base 2 logarithm of E, approximately 1.443.
  Math.LOG10E; //Base 10 logarithm of E, approximately 0.434.
  Math.PI; //Ratio of the circumference of a circle to its diameter, approximately 3.14159.
  Math.SQRT1_2; //Square root of 1/2; equivalently, 1 over the square root of 2, approximately 0.707.
  Math.SQRT2; //Square root of 2, approximately 1.414.

  //methods - basic arithmatic
  Math.abs(x);
  Math.sign(x);
  Math.max([x[, y[, …]]]);
  Math.min([x[, y[, …]]]);
  Math.sqrt(x);
  Math.hypot([x[, y[, …]]]); //Returns the square root of the sum of squares of its arguments.
  Math.cbrt(x); //Returns the cube root of a number.
  Math.clz32(x); //Returns the number of leading zeroes of a 32-bit integer.

  Math.ceil();
  Math.floor(x);
  Math.fround(x);// returns the nearest 32-bit single precision float representation
  Math.round(x);
  Math.trunc(x);

  Math.log(x);
  Math.log1p(x); //Returns the natural logarithm (loge, also ln) of 1 + x for a number x.
  Math.log10(x); //Returns the base 10 logarithm of a number.
  Math.log2(x); //Returns the base 2 logarithm of a number.

  Math.imul(x, y); //Returns the result of a 32-bit integer multiplication.
  Math.pow(x, y);
  Math.exp(x);
  Math.expm1(x); //Returns exp(x) -1.

  //methods - trigonometric
  Math.acos(x);
  Math.acosh(x);
  Math.cos(x);
  Math.cosh(x);
  Math.asin(x);
  Math.asinh(x);
  Math.sin(x);
  Math.sinh(x);
  Math.atan(x);
  Math.atanh(x);
  Math.atan2(y, x);
  Math.tan(x);
  Math.tanh(x);

  //methods - random
  Math.random(); //Returns a pseudo-random number [0, 1).
  ```

- [Math MDN Ref](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)

#### Built-in Object: Array

- Array Object

  - shallow copy
  - deep copy
  - chaining array methods

```javascript
//Constructor
var arr = [];
var fruits = ["Apple", "Banana", "Peach", "Durian"];
var fruits = new Array("Apple", "Banana", "Peach", "Durian");
var fruits = new Array(4);

//properties
arr.length;

// constructor like
Array.from(arrayLike[, mapFn[, thisArg]]);
Array.of(element0[, element1[, ...[, elementN]]]);

// array type testing
Array.isArray(arr);

// stringify
var str = arr.toString();
var str = arr.join([separator]);//null, undefined elements treated as empty string

// array operation: shallow-copy
var new_array = old_array.concat([value1[, value2[, ...[, valueN]]]]);
var new_array = arr.reverse();

// array operation: in-place
arr.copyWithin(target[, start[, end]]);
arr.fill(value[, start[, end]])
var popedItem = arr.pop(); // undeinfed if arr is empty.
arr.push(element1[, ...[, elementN]]);
var firstItem = arr.shift();
arr.unshift(element1[, ...[, elementN]]); //push to head of arr

// array operation: slicing and dicing
var new_array = arr.slice([begin[, end]]); // [begin, end), shallow-copy
var arrDeletedItems = arr.splice(start[, deleteCount[, insertItem1[, insertItem2[, ...]]]]);

// array access operation: inidexing
var first = fruits[0];
var last = fruits[fruits.length - 1];
var some = fruits["2"];
var index = arr.indexOf(searchElement[, fromIndex]); // -1 if not found
var index = arr.lastIndexOf(searchElement[, fromIndex]); // -1 if not found
var bool = arr.includes(valueToFind[, fromIndex]);

//functor-like
//iterator
var iterator = arr.entries();
iterator.next().value; // [index, elem]

var iterator = arr.keys();
iterator.next().value; // index

var iterator = arr.values();
iterator.next().value; // elem

//finder
var elem = arr.find(callback(element[, index[, array]])[, thisArg]);//undefined if not found
var index = arr.findIndex(callback(element[, index[, array]])[, thisArg]);//-1 if not found

//sorter
arr.sort([compareFunction]); // sorted in-place

// tester
var bool = arr.every(callback(element[, index[, array]])[, thisArg]);
var bool = arr.some(callback(element[, index[, array]])[, thisArg]);

//filter
var newArray = arr.filter(callback(element[, index[, array]])[, thisArg]);

//flatter
var newArray = arr.flat(depth);//default depth = 1
  //recursive flatten deep
    function flatten(array) {
      var flattend = [];
      (function flat(array) {
        array.forEach( el => {
          if (Array.isArray(el)) flat(el);
          else flattend.push(el);
        });
      })(array);
      return flattend;
    }
  //map first, then flat at depth=1
  var new_array = arr.flatMap(function callback(currentValue[, index[, array]]) {
    // return element for new_array
  }[, thisArg])

  //mapper
  var new_array = arr.map(function callback(currentValue[, index[, array]]) {
    // Return element for new_array
  }[, thisArg])

  arr.forEach(function callback(currentValue [, index [, array]]) {
    //your iterator
  }[, thisArg]); // return undefined

  //reducer
  var value = arr.reduce(callback(accumulator, currentValue[, index[, array]])[, initialValue]);
  var value = arr.reduceRight(callback(accumulator, currentValue[, index[, array]])[, initialValue])

  // functor patterns:
  // ranger
  const range = n => Array.from(new Array(n).keys());
  const rangeRandom = (n, max_val) => {
  return Array.from(new Array(n).keys()).map(val =>
    Math.ceil(Math.random() * max_val)
  );
  };
  var arr10 = range(10);
  var arr8 = rangeRandom(8, 100);

  //max/min number array
  const maxElement = arr =>
    arr.reduceRight((acc, cur) => (acc = Math.max(acc, cur)), arr[0]);

  const minElement = arr =>
    arr.reduceRight((acc, cur) => (acc = Math.min(acc, cur)), arr[0]);
  //concateArr
  const concateArr = (...args) => Array.from(args).flat();
  //typeof all args
  const typeofAll = (...args) => Array.from(args).map(elem => typeof elem);
```

- [Array MDN Ref](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)

---

<!-- ### Architectures -->

#### Built-in Object: JSON

- `JSONstring = JSON.stringify(value[, replacer[, space]])`

  ```javascript
  JSON.stringify({
    x: 5,
    y: 6,
    toJSON() {
      return this.x + this.y;
    }
  });

  let foo = {
    foundation: "Mozilla",
    model: "box",
    week: 45,
    transport: "car",
    month: 7
  };

  //replacer as function
  function replacer(key, value) {
    // Filtering out properties
    if (typeof value === "string") {
      return undefined;
    }
    return value;
  }
  JSON.stringify(foo, replacer); // '{"week":45,"month":7}'

  //replacer as array
  JSON.stringify(foo, ["week", "month"]); // '{"week":45,"month":7}', only keep "week" and "month" propertie

  //space for indentation
  JSON.stringify({ uno: 1, dos: 2 }, null, "\t");
  // returns the string:
  // '{
  //     "uno": 1,
  //     "dos": 2
  // }'

  //obj toJSON() method
  //If object defined toJSON() method, it will be called to  customizes JSON stringification behavior.
  //JSON.stringify() calls toJSON with one parameter:
  var obj = {
    data: "data",
    toJSON(key) {
      if (key) return `Now I am a nested object under key '${key}'`;
      else return this;
    }
  };

  // pass empty string "" to toJSON() for directly called on obj
  JSON.stringify(obj);
  // '{"data":"data"}'

  //pass property name of obj for obj is a property value
  JSON.stringify({ obj });
  // '{"obj":"Now I am a nested object under key 'obj'"}'

  //pass index number's string of obj for obj in an array
  JSON.stringify([obj]);
  // '["Now I am a nested object under key '0'"]'
  ```

- `obj = JSON.parse(text[, reviver])`

  ```javascript
  //reviver
  JSON.parse(
    '{"p": 5}',
    (key, value) =>
      typeof value === "number"
        ? value * 2 // return value * 2 for numbers
        : value // return everything else unchanged
  );

  // { p: 10 }

  //reviver called from inner to outer for nested obj.
  //property name, the property value will passed as arguments.
  //If the reviver function returns undefined, the property is deleted
  //If the reviver retruns, property is redefined to be the return value.
  ```

#### Built-in Object: RegExp

- Introduction

  - text manipulating
  - String Object: match(), matchAll(), replace(), search(), split()
  - greedy match

- RegExp literal

  - `var re = /ab+c/;`
  - `var re = /pattern/flags;`

- RegExp constructor and new operator

  - `var re = new RegExp('ab+c');`
  - `var re = new RegExp(pattern[, flags]);`
  - `var re = RegExp(pattern[, flags]);`

- RegExp properties

  - `regExpObj.lastIndex`
  - `regExpObj.source`
  - `regExpObj.flages`
  - `regExpObj.global; //'g' flag`
    - if set, find all matches.
    - if not set, find the first match.
  - `regExpObj.ignoreCase; //'i' flag`
    - if set, ignore case-sensitivity
    - if not set, case sensitive for matching
  - `regExpObj.multiline; //'m' flag`
    - if set, `^` and `$` match the start and end of the whole string.
    - if not set, `^` and `$` match the start and end of each line.
  - `regExpObj.sticky; //'y' flag`
  - `regExpObj.unicode; //'u' flag`
  - `regExpObj.dotAll; // 's' flag`
    - if set, `.` matches newline character(`\n`) as well.
    - if not set, `.` will not match newline character.

- RegExp methods

  - `matchArrLike = regexObj.exec(str)`
  - `bool = regexObj.test(str)`

- Patterns

  ```javascript
  let re = /abc/;

  // Simple pattern
  re = /abc/; //matching "abc"

  // Escaping
  // special chars: \ / [ ] { } ( ) ? + . * | = ! :  ^ $
  re = /ab\*c/; //matching "ab*c"
  re = new RegExp("a\\*bc"); //matching "ab*c", escaping twice
  re = /\/example\/[a-z]+/; //matching "/example/(LOWERCASESTRING)"

  //Meta characters
  // .        a single character, exclude newlines if 's' flag is not set.
  // \0       a null char
  // \t       a tab char
  // \v       a vertical tab
  // \r       a carriage return
  // \n       a newline char
  // \uXXXX   a unicode char

  // Char Classes
  // \s       a whitespace, like [\t\r\n]
  // \S       a non-whitespace character
  // \d       a digit, like [0-9]
  // \D       a non-digit, like [^0-9]
  // \w       a world character, like [a-zA-Z0-9_]
  // \W       a non-world character, like [^a-zA-Z0-9_]

  // Bracket and Ranges
  // [...]    Any one character between the brackets, Brackets escapes special chars implicitly, but exclude '-, ^, \, ], '.
  // [^...]   Any one character not between the brackets.
  // [0-9]    one digit char
  // [a-z]    one lowercase char
  // [A-Z]    one uppercase char
  // [a-Z]    one char case-insensitive
  // [^ ]      matches any non-empty char
  let re = /[^]/g;
  re.exec("!@\t\b\ndf\0");

  // Quantifier
  // p+       p repeated 1 or more
  // p*       p repeated 0 or more
  // .*       matches any sequence of char, except newline if flag 's' is not set.
  // p?       p repeated atmost 1
  // p{N}     p repeated N
  // p{L,U}   p repeated between [L,U]
  // p{L,}    p repeated at least L

  // Grouping
  // (p)      p grouped by "()"
  // (p)+     quantifier can be apllied to grouping pattern, retrun value arry will record group values.
  // (p)*
  // (p)?
  // (p){N}
  // (p){L, U}
  // (p){L, }

  // named group
  const re = /(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/;

  //Assertion
  //Positive lookahead, Negative lookahead
  const re = /foo(?=bar)/;
  const re = /foo(?!bar)/;

  //Positive lookbehind, Negative lookbehind
  const re = /(?<=foo)bar/;
  const re = /(?<!not )foo/;

  // Boundaries
  // p$       lines end with p
  // ^p       lines begin with p
  // \b       a word boundary
  // \B       not a word boundary

  //Greedniess and Laziness
  let re = /<p>.*<\/p>/g;
  let re = /<p>.*?<\/p>/g;

  //regexObj.exec(str) returns matchedArrLike
  var re = /quick\s(brown).+?(jumps)/gi;
  var result = re.exec("The Quick Brown Fox Jumps Over The Lazy Dog");
  console.log(result);
  // ["Quick Brown Fox Jumps", "Brown", "Jumps", index: 4, input: "The Quick Brown Fox Jumps Over The Lazy Dog", groups: undefined]
  // 0: "Quick Brown Fox Jumps"
  // 1: "Brown"
  // 2: "Jumps"
  // groups: undefined
  // index: 4
  // input: "The Quick Brown Fox Jumps Over The Lazy Dog"
  // length: 3
  // **proto**: Array(0)
  ```

#### Built-in Object: Map

- Introduction

  - Map object holds key-value pairs and remembers the original insertion order of the keys.
  - Any value in JS can be used as Map keys or values.

- Map constructor and new operator

  - `let myMap = new Map();`
  - `let map = new Map([iterable])`
    - iterable object's elements should be key-value pairs

- Properties

  - `myMap.size`

- Methods

  - `myMap.clear();`
  - `bool = myMap.delete(key); //false if key not exists`

  - `bool = myMap.has(key);`
  - `value = myMap.get(key); // undefined if key not exists`
  - `newMap = myMap.set(key, value);`

  - `mapIterator = myMap.entries() // [key, value] in iterator`
  - `mapKeyIterator = myMap.keys()`
  - `mapValueIterator = myMap.values()`

  - `myMap.forEach(callback(value[, key[, map]])[, thisArg])`

- Pattern

  ```javascript
  let myMap = new Map();
  myMap.set(0, "zero");
  myMap.set(1, "one");

  for ([key, value] of myMap) {
    console.log(key + " = " + value);
  }

  for (key of myMap.keys()) {
    console.log(key);
  }

  for (value of myMap.values()) {
    console.log(value);
  }

  for ([key, value] of myMap.entries()) {
    console.log(key + " = " + value);
  }
  ```

#### Built-in Object: Set

- Introduction

  - Set object stores unique values of any type, whether primitive values or object references.

- Set constructor and new operator

  - `let mySet = new Set();`
  - `let mySet = new Set([iterable]);`

- Properties

  - `mySet.size`

- Methods

  - `mySet.clear();`
  - `bool = mySet.delete(value); //false if value not exists`

  - `bool = mySet.has(value);`

  - `newSet = mySet.add(value);`

  - `setIterator = mySet.entries() // [value, value] in iterator`
  - `setValueIterator = mySet.values()`

  - `mySet.forEach(callback(value[, value[, set]])[, thisArg])`

- Pattern

  ```javascript
  let mySet = new Set();
  mySet.add(0);
  mySet.add(1);
  mySet.add(5).add("some text"); // chainable

  for (value of mySet) {
    console.log(value);
  }

  for (value of mySet.values()) {
    console.log(value);
  }

  for ([key, value] of mySet.entries()) {
    console.log(key + " = " + value);
  }
  ```

#### Built-in Object: Promise

- Introduction

  - A Promise is an object representing the eventual completion or failure of an asynchronous operation.
  - Promises mainly to solve Callback Hell (heavy nested callback code) and Pyramid of doom Problem.
  - Promises helps to group your asynchronous in a efficient way.
  - Promises States:
    - pending: Initial Case where promise instantiated.
      - A pending promise can either be fulfilled with a value, or rejected with a reason (error).
    - fulfilled: Success Case which means promise resolved.
    - rejected: Failure Case which means promise rejected.

- Promise constructor and new operator

  - `let promise1 = new Promise(executor);`
    - executor is a callback function passed with the arguments resolve and reject.
    - The executor function is executed immediately by the Promise implementation, passing resolve and reject functions (the executor is called before the Promise constructor even returns the created object).
    - The resolve and reject functions, when called, resolve or reject the promise, respectively.
    - The executor normally initiates some asynchronous work, and then, once that completes, either calls the resolve function to resolve the promise or else rejects it if an error occurred.
    - If an error is thrown in the executor function, the promise is rejected.
    - The return value of the executor is ignored.
    - the asynchronous method returns a promise to supply the value at some point in the future.

- Methods

  - `promise = Promise.all(iterable); //aggregating the results of multiple promises.`
  - `promise = Promise.race(iterable); //racing for the first results of multiple promises.`

  - `promise = Promise.reject(reason);`
  - `promise = Promise.resolve(value);`

  - `promise = myPromise.then( (value) => { fulfillment_statements }, (reason) => { rejection_statements });`
  - `promise = myPromise.then(onFulfilled[, onRejected]);`

    - promise handler

  - `promise = myPromise.catch( reason => { rejection_statements; });`
  - `promise = myPromise.catch(onRejected)`

    - error handle for rejected.

  - `promise = myPromise.finally( () => { final_settled_statements; } );`
  - `promise = myPromise.finally(onFinally)`

    - finnally settle for rejected or fullfilled.

- Pattern

  ```javascript
  //hello Promise
  let helloPromise = new Promise((resolve, reject) => {
    setTimeout(() => resolve(" Promise Success!"), 250);
  });

  helloPromise.then(successMessage => console.log("Hello " + successMessage));

  //Async function
  function myAsyncFunction(url) {
    return new Promise((resolve, reject) => {
      const xhr = new XMLHttpRequest();
      xhr.open("GET", url);
      xhr.onload = () => resolve(xhr.responseText);
      xhr.onerror = () => reject(xhr.statusText);
      xhr.send();
    });
  }

  // Promise, async, await
  async function sequence() {
    await Promise.all([promise1(), promise2()]);
    return "done!";
  }

  const makeRequest = () =>
    getJSON().then(data => {
      console.log(data);
      return "done";
    });

  const makeRequest = async () => {
    console.log(await getJSON());
    return "done";
  };

  makeRequest();
  ```

#### Built-in Object: Error

- Introduction

  - `EvalError`
    - an instance representing an error that occurs regarding the global function `eval()`
  - `InternalError`
    - internal error in the JavaScript engine is thrown. E.g. "too much recursion"
  - `RangeError`
    - a numeric variable or parameter is outside of its valid range
  - `ReferenceError`
    - de-referencing an invalid reference
  - `SyntaxError`
    - syntax error that occurs while parsing code in `eval()`
  - `TypeError`
    - variable or parameter is not of a valid type
  - `URIError`
    - `encodeURI()` or `decodeURI()` are passed invalid parameters.

- Error constructor and new operator

  - `error = new Error([message[, fileName[, lineNumber]]])`
  - `const x = Error('I was created using a function call!');`
  - `const y = new Error('I was constructed via the "new" keyword!');`

- Properties

  - `err.message`
  - `err.name`

- Pattern

  ```javascript
  //hello Error
  try {
    foo.bar();
  } catch (e) {
    if (e instanceof EvalError) {
      console.log(e.name + ": " + e.message);
    } else if (e instanceof RangeError) {
      console.log(e.name + ": " + e.message);
    }
    // ... etc
  }

  //customize Error
  class CustomError extends Error {
    constructor(foo = "bar", ...args) {
      super(...args);

      // Maintains proper stack trace for where our error was thrown (only available on V8)
      if (Error.captureStackTrace) {
        Error.captureStackTrace(this, CustomError);
      }

      this.name = "CustomError";
      // Custom debugging information
      this.foo = foo;
      this.date = new Date();
    }
  }

  try {
    throw new CustomError("baz", "bazMessage");
  } catch (e) {
    console.log(e.name); //CustomError
    console.log(e.foo); //baz
    console.log(e.message); //bazMessage
    console.log(e.stack); //stacktrace
  }
  ```

#### Built-in Object: WebAssembly

- TODO

#### Built-in Object: Misc

- GeneratorFunction

  - `generatorFunctor = new GeneratorFunction ([arg1[, arg2[, ...argN]],] functionBody)`

- Generator

  - The Generator object is returned by a generator function and it conforms to both the iterable protocol and the iterator protocol.

  ```javascript
  function* idMaker() {
    var index = 0;
    while (true) yield index++;
  }

  var gen = idMaker(); // "Generator { }"

  console.log(gen.next().value); // 0
  console.log(gen.next().value); // 1
  console.log(gen.next().value); // 2
  ```

- Proxy

  - `var p = new Proxy(target, handler);`

- Reflex

  - built-in object that provides methods for interceptable JavaScript operations.
  - no constructor.

- ArrayBuffer

  - `new ArrayBuffer(length)`

- DataView

  - `new DataView(buffer [, byteOffset [, byteLength]])`

- Intl

  - Intl object is the namespace for the ECMAScript Internationalization API.
  - no constructor

---

### Functions

#### Function Basics

- function is a "subprogram" that can be called by code external (or internal in the case of recursion) to the function.
- function is first-class objects in JavaScript.
- function scope, variables can not access outside function body.
- nested function closure, inner function can use the arguments and variables of the outer function, even outer function returned.

#### Function Type

- function declaration
- function expression
  - anonymous
  - named(function scope only)
- arrow function expression
  - arrow function does not have its own `this`
- Function constructor
- function scope
- nested function closure
- default args
- rest args
- tagged function, aka tagged templates literal
- IIFE

  ```javascript
  //function declaration
  function square(number) {
    return number * number;
  }

  //function expression, anonymous
  const square = function(number) {
    return number * number;
  };

  //function expression, named
  const factorial = function fac(n) {
    return n < 2 ? 1 : n * fac(n - 1);
  };

  //arrow function expression
  const square = number => number * number;

  //Function constructor
  const squre = new Function("number", "return number * number");

  //nested function and closure
  function outside(x) {
    function inside(y) {
      return x + y;
    }
    return inside;
  }
  fn_inside = outside(3); // returns a function
  result = fn_inside(5); // returns 8
  result1 = outside(3)(5); // returns 8

  // default args
  function multiply(a, b = 1) {
    return a * b;
  }
  multiply(5); // 5

  // rest args
  function multiply(multiplier, ...theArgs) {
    return theArgs.map(x => multiplier * x);
  }
  let arr = multiply(2, 1, 2, 3);
  console.log(arr); // [2, 4, 6]

  //tagged function
  const myTag = (textArr, ...placeHolders) =>
    textArr.reduce(
      (acc, elem, index) =>
        acc + elem + (placeHolders[index] ? placeHolders[index] : ""),
      ""
    );
  [name, age] = ["tagged function", 32];
  console.log(myTag`name is ${name}, age is ${age}.`);

  //IIFE
  (msg => console.log(msg))("IIFE called");
  ```

#### Function Usage

- Functional Programming

  - FP princiles

    - be pure
    - explicite return statement
    - no shared state
    - immutable state
    - composable
    - atomic task

  - Pure function

    - no side effects
    - same input results in same output

  - Indempotent

    - nested calls result in same output

  - Imperative vs Declerative

    - what-to-do vs how-to-do-it

  - Immutability

    - not change state, or clone it, change it, then return it.

  - HOF, Higher Order Function and closure

    - a function take another function as argument.

  - Curry

    - refactor multi args function into a pattern, each functions takes one argument at a time.

  - Partical Application

    - refactor multi args function into tow a pattern, one function takes one argument, the other takes rest of args.

  - Memoization

    - cache consistance calculation result for future function call.

  - Compose and Pipe

    - link multiple functions together to process data.
    - compose links from left to right
    - pipe links from right to left

- Pattern

  ```javascript
  //Singleton with IIFE

  //Module with IIFE

  //closure
  const closureDemo = () => {
    let l1 = "Layer 1";
    return () => {
      let l2 = "Layer2";
      return () => {
        let l3 = "Layer3";
        return `Returnning from : ${l3} -> ${l2} -> ${l1}.`;
      };
    };
  };

  closureDemo();
  ```

#### Built-in Functions

- eval()
- uneval()

---

### OOP Prototyping and Class

- prototyping

  - `prototype` property
    - `__proto__` in instance
  - prototype chain
  - `prototype`

    ```javascript
    //constructor function
    const Person = function(name, age, job) {
      this.name = name;
      this.age = age;
      this.job = job;
    };

    //prototyping
    Person.prototype.getProfile = function() {
      return `${this.name} is ${this.age} old, and works as ${this.job}`;
    };

    //instantiation
    let peter = new Person("peter", 30, "accountant");

    console.log(peter.getProfile());
    ```

  - `Object.create()`

    ```javascript
    //prototypig
    const personProto = {
      getProfile() {
        return `${this.name} is ${this.age} old, and works as ${this.job}`;
      }
    };

    //inheritance and instantiation
    let peter = Object.create(personProto, {
      name: { value: "peter" },
      age: { value: 30 },
      job: { value: "accountant" }
    });

    console.log(perter.getProfile());
    ```

- class syntax suger

  ```javascript
  class MyObject {
    //constructor
    constructor(...props){
      this.prop1 = props[0];
      ...;
    }

    //methods
    mothod1(...args){
      function_body_statements;
    }

    //static methods
    static staticMethod(...args){
      static_function_body_statements;
    }
  }

  //instanciation
  let myobj = new MyObject(...props);

  class MyExtendsObject extends MyObject {
    constructor(newprops, ...props) {
      super(...props);
      this.newprop1 = newprops[0];
      ...;
    }

    //methods
    newMethod1(...args){
      function_body_statements;
    }

    //static methods
    static newStaticMethod(...args){
      static_function_body_statements;
    }
  }
  ```

---

### Expressiones

- Conditional Expression

  - `==`
  - `===`
  - undefined variables return false
  - `>, >=, <, <=, !=`
  - ternary conditional expression

- Logical Expression

  - `&&, ||, !`
  - xor pattern: `(x && !y || !x && y)`

- String Concatenation

  - `"String " + myVal + " Another String."`

- Function expression

  - function expression is a lhv:

    ```javascript
      function(...args){
        function_statements;
      }
    ```

---

### Statements

- Declare Variables

  - const
  - var
  - let

- Assignment

  - `=`

- Comment

  - line comment
  - `// LINE`
  - block comment
  - `/* BLOCK */`

- Function Statement

  - `const funName = (...parameters) => { ...codes }`

### Modules

- module: import, exports

```javascript
//export
export default "string literal";
export const func1 = () => {
  func1_body;
};
export const func2 = () => {
  func2_body;
};

//import
import string from "./MODULE_PATH";
import * as NameSpace from "./MODULE_PATH";
import { func1, func2 } from "./MODULE_PATH";
import { func1 as func, func2 } from "./MODULE_PATH";
```

---

### Flow Control

#### Condition Statement

- if
- if-else
- if-else if-else

```javascript
if (a === "other value") {
  do_something;
} else if (a === "another value") {
  do_something;
} else {
  do_something;
}
```

#### Branch Statement

- switch-case-break-default
- switch-case-break
- switch-case
- swotch-case-default

  ```javascript
  switch (x) {
    case 1:
      console.log("one");
      break;
    case 2:
      console.log("two");
      break;
    default:
      console.log("none");
  }
  ```

#### Loop Statement

- control keywords

  - `break`
  - `continue`

- for

  ```javascript
  for (let i = 0; i < 100; i += 1) {
    do_something;
  }
  ```

- for-of

  ```javascript
  let arrLike = Array(100).keys();

  //iterating
  for (value of arrLike) {
    value += 1;
    console.log(value);
  }
  ```

- for-in

  ```javascript
  let objLike = { a: 1, b: 2, c: 3 };

  //enumerating
  for (value in objLike) {
    value += 1;
    console.log(value);
  }
  ```

- do-while

  ```javascript
  do {
    x += 1;
    console.log(x);
  } while (x < 100);
  ```

- while

  ```javascript
  while (x > 90) {
    x -= 1;
    console.log(x);
  }
  ```

---

---

## Web API, AJAX and JSON

### [DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)

#### DOM Tree

- DOM tree: ![Alt](/img/JS-DOM-Tree.png "DOM Tree")

- DOM Interfaces
  - Window
  - Document
    - HTMLDocument
      - HTMLElement
        - Element
          - Node
            - EventTarget

---

#### [window](https://developer.mozilla.org/en-US/docs/Web/API/window)

- properties

  ```javascript
  window.console;
  window.document;
  window.outHeight;
  window.outWidth;
  window.innerHeight;
  window.innerWidth;
  window.location;
  ```

- methods

  ```javascript
  alert("message");
  var confirmation = confirm("Confirm?");
  var message = prompt("Input your message: ");
  console.log("message");

  //close window
  close();
  ```

---

#### [document](https://developer.mozilla.org/en-US/docs/Web/API/document)

- properties and methods

  ```javascript
  //document texts
  document;
  document.head;
  document.body;

  //document infos
  document.domain;
  document.URL;
  document.characterSet;
  document.contentType;

  //document Nodes/Elems Collectors
  document.all;
  document.links;
  document.forms;
  document.images;
  document.scrips;

  //document Elem Selectors
  elem = document.getElementByID("ID");
  elem = document.querySelector("ELEM_TAG");
  elem = document.querySelector("#ID");
  elem = document.querySelector(".CLASS");
  elem = document.querySelector(".CLASS ELEM_TAG");

  //document multi elems selectors
  elems = document.getElementsbyClassName("CLASS");
  elems = document.getElementsbyTagName("ELEM_TAG");
  elems = document.querySelectorAll(SELECTOR);

  //traversing DOM
  //manipulate DOM
  elem = document.createElement("TAG");
  ```

---

#### [elements](https://developer.mozilla.org/en-US/docs/Web/API/Element)

- events refer:

  - [MDN events ref](https://developer.mozilla.org/en-US/docs/Web/Events)

- properties and methods

  ```javascript
  //Node Context
  elem = document.forms[0];

  //Elem properties
  elem.id;
  elem.method;
  elem.action;
  elem.classList;
  elem.className;
  elem.style;

  //Attributes
  elem.getAttribute("ATTRI");
  elem.setAttribute("ATTRI", VALUE);
  elem.hasAttribute("ATTRI");
  elem.removeAttribute("ATTRI");

  //traversing
  elem.nodeType;
  elem.parentNode;
  elem.parentElement;
  elem.previousSibling;
  elem.previousElementSibling;
  elem.nextElementSibling;
  elem.childNodes;
  elem.children;
  elem.firstElementChild;
  elem.lastChild;
  elem.lastElementChild;

  //manipulating
  elem.appendChild(NODE);
  elem.replaceChild(newNode, oldNode);
  elem.remove();
  elem.removeChild(childNode);
  ```

---

#### [event, eventListener, eventTarget](https://developer.mozilla.org/en-US/docs/Web/API/Event)

- event bubbling

  - events will pass from target element up to DOM tree root.

- event delegation

  - set event handler in parent element instead of target element
  - scenarios:
    - too many target elements to handle
    - target element does not yet exist when DOM loaded.

- properteis and methods

  ```javascript
  //EventListener
  EventListener.handleEvent();

  //Event
  Event.type;
  e.target;
  e.preventDefault();
  e.stopPropagation();

  //EventTarget
  elem.addEventListener("TYPE", HANDLER);
  ```

---

### Ajax

- AJAX: Asynchronous JavaScript And XML
- `XMLHttpRequest` object to communicate with servers
  - [XHR Ref](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- supports JSON, XML, HTML, and text files

- practices

  ```javascript
  //readyState:
  // 0: request not initialized
  // 1: server connection established
  // 2: request received
  // 3: processing request
  // 4: request finished and response is ready

  //status
  // HTTP Message: https://www.w3schools.com/tags/ref_httpmessages.asp

  //local txt
  function loadTxt() {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (this.readyState == 4 && this.status == 200) {
        document.getElementById("demo").innerHTML = this.responseText;
      }
    };
    xhttp.open("GET", "ajax_info.txt", true);
    xhttp.send();
  }

  //local JSON
  function loadJSON() {
    // Create the object
    const xhr = new XMLHttpRequest();

    // Open the connection
    xhr.open("GET", "employees.json", true);

    // Execute the function
    xhr.onload = function() {
      if (this.status === 200) {
        // Get the response as an Object
        const employees = JSON.parse(this.responseText);

        let output = "";
        employees.forEach(function(employee) {
          output += `<ul>
                          <li>ID: ${employee.id}</li>
                          <li>Name: ${employee.name}</li>
                          <li>Company: ${employee.company}</li>
                          <li>Job: ${employee.job}</li>
                        </ul>`;
        });

        document.getElementById("employees").innerHTML = output;
      }
    };

    // Send the request
    xhr.send();
  }

  //RESTful API
  function loadPosts() {
    // Create the object
    const xhr = new XMLHttpRequest();

    // Open the connection
    xhr.open("GET", "https://jsonplaceholder.typicode.com/posts", true);

    // Execute the function
    xhr.onload = function() {
      if (this.status === 200) {
        const response = JSON.parse(this.responseText);

        // print the contents
        let output = "";

        response.forEach(function(post) {
          output += `
                          <h3>${post.title}</h3>
                          <p>${post.body}</p>
                      `;
        });
        document.querySelector("#result").innerHTML = output;
      }
    };

    // Send the request
    xhr.send();
  }
  ```

---

### Web APIs

#### Performance

- `performance.now()`
- `performance.toJSON();`

  ```javascript
  let js;
  js = window.performance.toJSON();
  console.log("json = " + JSON.stringify(js));

  let t0 = performance.now();
  doSomething();
  var t1 = performance.now();
  console.log("Call to doSomething took " + (t1 - t0) / 1000 + " seconds.");
  ```

#### WindowOrWorkerGlobalScope

- `setTimeOut()`
- `clearTimeOut()`
- `setInterval()`
- `clearInterval()`

  ```javascript
  timeoutID = scope.setTimeout(function[, delay, param1, param2, ...]);
  timeoutID = scope.setTimeout(function[, delay]);
  scope.clearTimeout(timeoutID);

  intervalID = scope.setInterval(func, delay[, param1, param2, ...]);
  scope.clearInterval(intervalID)
  ```

- `fetch()`

  - provides an interface for fetching resources (including across the network).
  - Async API with Promise-based.
  - Interfaces: `Body, Headers, Response, Request`
  - `fetchResponsePromise = fetch(resource, init);`

  - practices

    ```javascript
    //fetch api
    fetch(url)
      .then(function(response) {
        return response.json();
      })
      .then(function(names) {
        let html = "<h2>Generated Names</h2>";
        html += '<ul class="list">';
        names.forEach(function(name) {
          html += `
                  <li>${name.name}</li>
            `;
        });
        html += "</ul>";

        document.querySelector("#result").innerHTML = html;
      })
      .catch(function(error) {
        console.log(error);
      });

    //async fetch
    async function getPosts(url) {
      const response = await fetch(url);

      const data = await response.json();

      return data;
    }

    getPosts("URL").then(dataHandler);
    ```

---

---

## Design Patterns

### Function as Module

- Singleton

  ```javascript
  //encapsulate statements in a IIFE
  const Singleton = (function () {
    let privateVar = ...;
    function privateFunc() {};

    return {
      property1: 'property',
      method1(...args) {},
      method2(...args) {}
    }
  }());
  ```

- Module Encapsulation

  ```javascript
  //encapsulate statements in a IIFE
  const Walker = () => {
    let deviation = 0;
    let stepList = [];

    function walking(step, direction = 1) {
      deviation += step * direction;
      stepList.push(step * direction);
    }

    function getDeviation() {
      return deviation;
    }

    function getSteps() {
      return stepList.length;
    }

    return {
      right: function(n) {
        walking(n);
        return getDeviation();
      },
      left: function(n) {
        walking(n, -1);
        return getDeviation();
      },
      steps: function() {
        return getSteps();
      }
    };
  };

  const walker1 = Walker();
  const walker2 = Walker();
  ```

### Function as Class

---

---

## Best Practice

### Install and Initialize

#### Internal Script

- HelloWorld.html

```html
...
<body>
  ...
  <script>
    // JAVASCRIPT HERE
  </script>
</body>
...
```

#### External Script

- HelloWorld.html

  ```html
  ...
  <body>
    ...
    <script src="src/helloWorld.js"></script>
    ...
  </body>
  ...
  ```

- helloWorld.js

  ```javascript
  ...
  console.log("hello world!")
  ...
  ```

#### Script Loading

- scripts reffering DOM elements should be loaded after HTML elements parsed by browser.
  - internal scripting:
    - `document.addEventListener("DOMContentLoaded", function() { ... });`
  - external scripting:
    - `<script src="script.js" async></script>`
  - put script element right at the bottom of the body (e.g. just before the `</body>` tag)
- defer vs async diractive

  - `defer` loads script by order
  - `async` cannot granty orders

    ```javascript
    //async diractive
    <script async src="js/vendor/jquery.js" />
    <script async src="js/script2.js"></script>

    //defer diractive
    <script defer src="js/vendor/jquery.js" />
    <script defer src="js/script2.js"></script>
    ```

---

### Good Parts and Bad Pars of JS

#### Bad Parts

```javascript
// return object, left curley braces result in silent runtime error.
return;
{
  name: value;
}

// with object, execution unpredictable
with (obj) {
  foo = bar;
}

// ==, implicity type coercion
0 == ""; //ture
0 == "0"; //ture
"" == "0"; //ture
```

---

---

## Misc

### JavaScript Standards

- ECMA: European Computer Manufacturers Association
- [ECMA-262](https://github.com/tc39/ecma262)
- [ECMA-404 The JSON Data Interchange Syntax](https://ecma-international.org/publications/standards/Ecma-404.htm)
- [ECMA-402](https://ecma-international.org/publications/standards/Ecma-402.htm)

---

### Online Resources

- [Mozilla Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
- [W3School JavaScript Tutorial](https://www.w3schools.com/js/)
- [JavaScript Resources](https://www.javascript.com/resources)

---

### DevOps

- VS code extensions
  - Beautify
  - Prettier
  - Bracket Pair Colorizer 2
  - HTML Snippets
  - JavaScript (ES6) code snippets
  - Live Server
  - ESLint

---

---
