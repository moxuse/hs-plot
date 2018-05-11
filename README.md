# hs-plot

Software for Livecoding to manipulate XY Plotter on ghci repl. providing interface for SmartLaserMini. Using SmartLaserMini modified as XY Plotter which has pen holder.

## Boot

Booting repl with script (Boot.ghci) below

```
ghci -XOverloadedStrings -ghci-script ./Boot.ghci -i./GCode
```

## Stroke ant Trail

```
type Stroke = [Trail]

type Trail = (Double, Double)
```

```Trail``` is construct form twe Double values. first is Axis-X and seccond is Y.

## Plot with Plotter to send GCode to the backend

Use ```stream``` (```g1``` is assigned at first look Boot.ghci) function which has a argument type of ```[Stroke]```, array of Stroke type.


```
g1 :: [Stroke] -> IO ()
```

generate GCodeMap and post g-code to SmartLaser's backend.

## Functions

There are sevaral functions to generate Stroke and modifing shape of them.

### generate

```
generate
```

```
sinX
```

```
randomize
```

### example code

generate Stroke data from two vectors

```
l = sinX 1.2 $ generate 2
g1 $ l:[]
```

```
prev $ map (\x -> translate' (x, 3) $ generate 10) [0..3]
```



