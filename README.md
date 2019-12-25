# States
A small tool to help think about all the states a system can be in.

## Usage
Either provide the state definition as the first command line argument or call the tool
without command line arguments to read from stdin.

## Example
Imagine a railroad grade crossing. There can be a train approaching or not. If a train is approaching the red lights can be activated or not (in case of failure). There might also be a car on the tracks. We can get all the states of the system in the following way:

```
states “(train not_approaching | approaching (red_light_flashing | red_light_not_flashing), car on_tracks | not_on_tracks)“
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


Unwanted states can be filtered out in a filter block:

```
states << EOF

let Train = train not_approaching | approaching (red_light_flashing | red_light_not_flashing);
let Car = car on_tracks | not_on_tracks;

(Train, Car) [
  # All values that match the following patterns will be removed from the result set
  remove (_ approaching red_light_flashing, _ not_on_tracks),
  remove (train _, car on_tracks)
]

EOF
```
(This is a silly example because it filters out almost everything, but you should get the idea)

The only filter operation that is supported for now is `remove`. Others might follow later.

