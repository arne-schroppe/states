# States
A small tool to help think about all the states a system can be in.

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

## Example
Imagine a railroad grade crossing. There can be a train approaching or not. If a train is approaching the red lights can be activated or not (in case of failure). There might also be a car on the tracks. We can get all the states of the system in the following way:

```
states "(train not_approaching | approaching (red_light_flashing | red_light_not_flashing), car on_tracks | not_on_tracks)"
```

With the output:

```
(train not_approaching, car on_tracks)
(train not_approaching, car not_on_tracks)
(train approaching red_light_flashing, car on_tracks)
(train approaching red_light_flashing, car not_on_tracks)
(train approaching red_light_not_flashing, car on_tracks)
(train approaching red_light_not_flashing, car not_on_tracks)
```

To make things more readable, variables can be used
```
states << EOF

# Definition of Train and Car
let Train = train not_approaching | approaching (red_light_flashing | red_light_not_flashing);
let Car = car on_tracks | not_on_tracks;

(Train, Car)

EOF
```
Note that variable names must begin with uppercase letters and that a variable 
binding must end with a semicolon.


The results can be further refined in a filter block:

```
states << EOF

let Train = train not_approaching | approaching (red_light_flashing | red_light_not_flashing);
let Car = car on_tracks | not_on_tracks;

(Train, Car) [
  remove (train approaching red_light_flashing, car not_on_tracks),

  # The underscore in the following pattern matches any value
  highlight (_, car on_tracks)
]

EOF
```

Three filter operations are currently supported: `remove`, `only` and `highlight`


You can also specify filters on the command line with the `-F` or `--filters` option
```
states "(train not_approaching | approaching (red_light_flashing | red_light_not_flashing), car on_tracks | not_on_tracks)" -F "only (_, car on_tracks), highlight (_ _ red_light_flashing, _)"
```
