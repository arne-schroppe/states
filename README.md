# States
A small language to enumerate all the states a system can be in

## Usage
```
Usage: states ([SOURCE] | [-f|--file FILE]) [-F|--filters FILTERS]
  Print all combinations of states

Available options:
  SOURCE                   Use definition in SOURCE
  -f,--file FILE           Read definitions from FILE
  -F,--filters FILTERS     Apply additional filters
  -h,--help                Show this help text

```

## Syntax
Strings: `a b c d`

_output:_
```
a b c d
```


Tuples: `(a, b)`

_output:_
```
(a, b)
```

Variants: `a | b`

_output:_
```
a
b
```

Variants can have data: `a (x | y) | b`

_output:_
```
a x
a y
b
```

Variants and tuples can be combined: `(a | b, c | d)`

_output:_
```
(a, c)
(a, d)
(b, c)
(b, d)
```

Three filter operations are currently supported: `remove`, `only` and `highlight`.

You can also specify filters on the command line with the `-F` or `--filters` option.

If you need to use special characters in your symbols, you can use either single or double quotes (either works).
