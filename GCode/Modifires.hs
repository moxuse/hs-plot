
module Modifires where

-- ptSin :: Double -> Stroke -> Stroke
ptSin rad xs = map (\ x -> (cos (fst x) * rad, (sin (snd x)) * rad)) xs

-- translate' :: (Double, Double) -> Stroke -> Stroke
translate' t ls = map (\ l -> ((fst l) + (fst t), (snd l) + (snd t))) ls

-- scale' :: (Double, Double) -> Stroke -> Stroke
scale' t ls = map (\ l -> ((fst l) * (fst t), (snd l) * (snd t))) ls

-- sinX :: Double -> Stroke -> Stroke
sin' freq ls = map (\ tr -> ( sin((snd tr) * (fst freq)) + (fst (fst tr)) , sin((snd tr) * (snd freq)) + (snd (fst tr)) ) ) (zip ls (map fromIntegral [0.. (length ls)-1]))

cos' freq ls = map (\ tr -> ( cos((snd tr) * (fst freq)) + (fst (fst tr)) , cos((snd tr) * (snd freq)) + (snd (fst tr)) ) ) (zip ls (map fromIntegral [0.. (length ls)-1]))

-- cos freq ls = map (\ tr -> (cos ((fst l) * freq), (snd l))) ls

-- sinY :: Double -> Stroke -> Stroke


-- sine tr x y = map (\ t -> sin(fst (fst t) * (snd t)) ) [(x, i) | x <- tr, i = ]
--  in JS
-- sin = (stroke, [x, y]) => {
--   let i = 0;
--   return stroke.map((tr) => {
--     const v = [Math.sin(i * x) + tr[0], Math.sin(i * y) + tr[1]];
--     i++;
--     return v;
--   })
-- }
