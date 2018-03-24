# h-plotter

Software for Livecoding to manipulate XY Plotter on ghci repl. providing interface for SmartLaserMini. Using SmartLaserMini modified as XY Plotter which has pen holder.

## Boot

Booting repl with script (Boot.ghci) below

```
ghci -XOverloadedStrings -ghci-script ./Boot.ghci -i./GCode
```

## Stroke data

```
Stroke  
 = S {name :: String, from :: V2 Double, to :: V2 Double}
```


## function

```
g1 :: [Stroke] -> IO ()
```

generate GCodeMap and post g-code to SmartLaser's backend.

```
lineto :: V2 Double -> V2 Double -> Stroke
```

generate Stroke data from two vectors

```
g1 $ map (\m lineto -> (V2 m 1) (V2 1 m)) [0..2]
```