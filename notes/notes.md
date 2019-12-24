# States tool

## Purpose
A tool that helps me think about the various states a system can be in

## What does it do
I write down information about the possible states in a dsl
(This notation should already help me think about the various states)
The tool can then print out all the possible states in a long list

## Notation
The syntax is based on algebraic data types. The basic type is a symbol
(maybe also strings?). The higher order types are the usual product and
sum types. 

All of: `a, b`
Alternative: `a | b`
Hierarchy/specialisation: `a b`

Parentheses for structure: `a (b | d)`, `((a, b), c)`

- What's the difference between `a b1 b2` and `a (b1, b2)`?
- Should we disallow the first form?


## Syntax examples
```

type Existence =
    does_not_exist
  | exists (has_result | does_not_have_result)

type Event (EventAge t) = event (t, existence)

type EventAge = old | new

type system = (Event old, Event new)

filter no_new_result_without_old_result =
  remove system (event (old, exists does_not_have_result), event (new, exists has_result))

```

Minimal example:
```
(event_a|event_b, other_event_a|other_event_b)

becomes:
(event_a, other_event_a)
(event_b, other_event_a)
(event_a, other_event_b)
(event_b, other_event_b)
```

## Syntax
```
expr = tuple | variant

variant = ident ( '|' expr )*
tuple = '(' expr (',' expr)* ')'
ident = <string>
```

## Combinations
```
symbol -> just the symbol
tuple -> multiplication of each element
variant -> sum of each option
```


## Brainstorming

old event exists has result, new event exists has result:
old event exists has result, new event exists does not have result:
old event exists has result, new event does not exist has result:
old event exists has result, new event does not exist does not have result:
old event exists does not have result, new event exists has result:
old event exists does not have result, new event exists does not have result:
old event exists does not have result, new event does not exist has result:
old event exists does not have result, new event does not exist does not have result:
old event does not exist has result, new event exists has result:
old event does not exist has result, new event exists does not have result:
old event does not exist has result, new event does not exist has result:
old event does not exist has result, new event does not exist does not have result:
old event does not exist does not have result, new event exists has result:
old event does not exist does not have result, new event exists does not have result:
old event does not exist does not have result, new event does not exist has result:
old event does not exist does not have result, new event does not exist does not have result:


### Meta
Should prevent the illegal states
Can this be accurately modeled with algebraic types alone? yes
(only figured this out after going to lunch and making a strong cup of tea. Before that my mind was stuck)

```
event:
  event existence

existence:
| exists result
| does_not_exist

result:
| has_result
| no_result

(event old, event new)
```

or:

```
event a = (a, exists (result | no_result) | does_not_exist); (event old, event new)
```

should probably be string based:

```
event a = "{a} event {"exists and" ("has result" | "does not have result") | "does not exist"}"; "{event "old"}, {event "new"}"

or (but more complicated)

"{("old", "new") "{_0} event {"exists and" ("has result" | "does not have result") | "does not exist"}"}"

```

auto-stringify symbols? e.g. `does_not_exist` becomes "does not exist"?

we could translate this into C# by using classes as sum types
```
class EventState {
  class Exists;
  class DoesNotExist;
}

class Exists {
  class HasResult;
  class HasNoResult;
}

etc
```
however, since all classes in C# implicitly also inhabit the type `null`, we don't need this for this case. It is simply
what we have now!

It's actually interesting that in all languages I know, types can only be named, but sub-expressions can be used inline
(e.g. `2 * (3 + 4)` is valid but `type x = (a, (b | c))` isn't. Or maybe OCaml can do this?!)

What if you could tell the system what states are impossible and it refines the logic for you?


Also add filters (via pattern matching?)
Work with symbols or strings
Maximum recursion depth

types: Symbol, string
Higher order types: Sum and product types
Filters (patterns)

pattern: `(event _ (exists has_no_result), event _ (exists has_result))`
Pattern should not have to be this specific. We need a notation that allows for very generic patterns with few characters.
e.g. `@ (exists has_no_result), @ (exists has_result)` would filter this out on all levels

needs scoping (we might only apply certain patterns in certain contexts)

Three structural elements (the terms aren't very accurate I'm afraid):
Union `,`
Alternative `|`
Hierarchy `juxtaposition` (with extra structural elements)
(precedence?)

```
a, b, c | d, e (f, g)
```

`,` isn't a real union though, because the order matters. An artefact of linear written text.

Maybe there shouldn't be a comma-separated tuple but only symbols with child elements, e.g.
```
event a b
```


Reintegrate states into a new state! So needs "dynamic" typing, since things can diverge from structure (???)

Also consider time! How do we model that? Must model parallelism

Mutually dependent state (e.g. receive event before/after)
Need additional markers to signify what depends on what if there are multiple dependencies
```
mut arrival =
  arrived_first | arrived_second

mut arrival 'a =
  arrived_first 'a | arrived_second 'a

event ex = some_event (arrival :backend)
event ey = some_other_event (arrival :backend)

# or

event_x 'a = some_event (arrival 'a)
event_y 'a = some_other_event (arrival 'a)

```
Does this make sense? And how do we model multiple dependencies, what would those even be?

Example for multiple dependencies:
"If the left sensor is triggered first, the green light turns on. If the right sensor
is triggered first, the red light turns on"

```
light name = light name on|off
sensor position = sensor position triggered|not_triggered

dep sensor_system (sensor left triggered) -> light green on
dep sensor_system (sensor right triggered) -> light red on

light_system = 
  system (light red)@lr (light green)@lg (sensor left)@sl (sensor right)@sr, 
    dep sensor_system(lr lg sl sr)

```
I feel that now we're moving towards a turing complete language to make this work :/
Maybe it's enough for now to simply filter out states that make no sense?
E.g. just filter out (sensor left triggered, light red on)
Could even have an interactive mode where you mark the illegal states and the
system tries to infer a common pattern.

Advanved filtering: All states that contain a certain state or a certain sequence

Name: Case
Actually probably not a good name on the command line :/
cases?
Or just: state


Is text even the best medium for this?

Should at least have a repl

```
[[a, b], [c, d]]

[[a, c], [a, d], [b, c], [c, d]]

```

Patterns and filters
a filter can remove results or allow only certain results
consists of a keyword and a pattern.
patterns match against values, not expressions (obviously):
```
(_, old _ does_not_exist)
```

filter syntax
```
let Event = event exists (has_result | has_no_result) | does_not_exist;

(old Event, new Event) {
  remove %(_ event exists has_no_result, _ event exists has_result)
}

# or

let Event = event exists (has_result | has_no_result) | does_not_exist {
  remove %(event exists has_result)
};

(old Event, new Event)
```

should we check if patterns are even making sense? probably not, just
try to match them

Can also have named events:
```
let Event = event exists (has_result | has_no_result) | does_not_exist;

(old Event, new Event) {
  newer_results_only = remove [_ event exists has_no_result, _ event exists has_result]
}
```
Could be useful for enabling/disabling certain filters

