# States
A small tool to help think about all the states a system can be in.

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

