import Sound.Tidal.Context
import Sound.Tidal.Bjorklund
import Sound.Tidal.Utils
import Sound.Tidal.Chords
import Sound.Tidal.UI
import Sound.Tidal.Transition
import Sound.Tidal.Utils
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

{-

let myScales = [("scalea", [0,5,7,12]),
                ("scaleb", [0,5,10])
               ]
    scale sp p = (\n scaleName -> noteInScale (fromMaybe [0] $ lookup scaleName myScales) n) <$> p <*> sp
      where octave s x = x `div` length s
            noteInScale s x = (s !!! x) + fromIntegral (12 * octave s x)

-}


-- helper for karp
toScale' :: Num a => Int -> [a] -> Pattern Int -> Pattern a
toScale' o [] = const silence
toScale' o s = fmap noteInScale
 where octave x = x `div` length s
       noteInScale x = (s !!! x) + fromIntegral (o * octave x)
----
-- cScale accepts an OSC message that represents a Pattern of scales as
-- well as a numeric Pattern and feeds the numeric pattern through the scales
---- [[String]]: An array of scale Patterns
---- [String]: OSC route
---- Pattern [Double]: Numeric (arp) Pattern
_cScale :: [[String]] -> String -> Pattern [Double]
_cScale ds s = fmap (catMaybes . map readM) $ _cX f ds s
  where f a (VI v) = [Event a a (parseList $ show v)]
        f a (VF v) = [Event a a (parseList $ show v)]
        f a (VS v) = [Event a a (parseList v)]
        parseList v = map (T.unpack . T.strip . filterBrac) $ T.splitOn "," $ T.pack v
        filterBrac = T.filter (\x -> x /= '[' && x /= ']')
        readM :: String -> Maybe Double
        readM = readMaybe
cScale :: [Double] -> String -> Pattern [Double]
cScale d = _cScale [map show d]
----
-- d1 $ overlay (s "[jstkick*4,[~ jstsn:4]*2,[~ hh]*4]" * hold 0.15 * gain 1.1 * release 0.01) $ fast 2 $ n (tParam toScale (cScale [0] "notes") (run 4)) |+ s "[plucklead,[~ donk]]" |+ legato 4 # orbit 1
----
-- a combination of chop and loopAt (convenince method)
chl :: Pattern Int -> Pattern Time -> ControlPattern -> ControlPattern
chl s c p = chop s $ loopAt c $ p
----
-- converts a Pattern to a list of values
patToList :: Pattern a -> [a]
patToList a = eventValue <$> (queryArc a (Arc 0 1))
----
-------------------------------------------------------------------------------
----
-- gets a parallel pattern from a MIDI keyboard connected to SuperCollider
keys :: Pattern Double
keys = cP "[]" "notes"
-- chooses a single key from a parallel pattern from a MIDI keyboard connected to SuperCollider
chkey :: Pattern Int -> Pattern Double
chkey s = mono $ rot s $ cP "[]" "notes"
-- arpeggiates through a chords sent from a MIDI keyboard connected to SuperCollider according to a given Pattern
karp :: Pattern Int -> Pattern Double
karp p = tParam toScale' (cScale [0] "notes") p
-- first key in a parallel chord from keys
key :: Pattern Double
key = chkey 0
-- second key in a parallel chord from keys
key' :: Pattern Double
key' = chkey 1
-- third key in a parallel chord from keys
key'' :: Pattern Double
key'' = chkey 2
-- fourth key in a parallel chord from keys
key''' :: Pattern Double
key''' = chkey 3
----
-------------------------------------------------------------------------------
----
-- structures a pattern in terms of a euclidian rhythm, but makes it an isorhythm at
-- rhythmic level s instead of dividing a cycle by the number of slots
isoe :: Pattern Int -> Pattern Int -> Pattern Time -> Pattern a -> Pattern a
isoe n d s p = slow ((fmap toTime d)/s) $ euclid n d $ p
-- like isoe but with an optional offset of o*(1/s) divisions of a cycle
isoe' :: Pattern Int -> Pattern Int -> Time -> Pattern Rational -> Pattern a -> Pattern a
isoe' n d o s p = (~>) (fmap ((/) o) s) $ slow ((fmap toTime d)/(fmap toTime s)) $ euclid n d $ p
----
-------------------------------------------------------------------------------
----
-- solo an element of a pattern according to sound name
solo' :: Pattern String -> ControlPattern -> ControlPattern
solo' p p' = unfix (const $ s "") (s p) $ p'
-- solo multiple elements of a pattern according to sound names
solos :: [Pattern String] -> ControlPattern -> Pattern ControlMap
solos ps p' = if length ps>0 then stack [solo' (head ps) p', (solos (tail ps) p')] else s ""
----
-------------------------------------------------------------------------------
----
-- send pattern to sidechain bus, causing it to "pump" according to the default bus
pump :: Pattern ControlMap -> Pattern ControlMap
pump p = p * gain 0.95 # orbit 1
----
-------------------------------------------------------------------------------
----
-- sweep a highpass up over c cycles
swuph :: Pattern Time -> ControlPattern
swuph c = hcutoff (range 50 4000 $ slow c $ saw)
-- sweep a highpass down over c cycles
swdownh :: Pattern Time -> ControlPattern
swdownh c = hcutoff (range 4000 50 $ slow c $ saw)
-- sweep a lowpass up over c cycles
swupl :: Pattern Time -> ControlPattern
swupl c = cutoff (range 50 4000 $ slow c $ saw)
-- sweep a lowpass down over c cycles
swdownl :: Pattern Time -> ControlPattern
swdownl c = cutoff (range 4000 50 $ slow c $ saw)
----
-------------------------------------------------------------------------------
----
-- unison detune
unison :: Pattern ControlMap -> Pattern ControlMap
unison = (|+ note "[0.05,-0.05]")
-- more unison detune
unison' :: Pattern ControlMap -> Pattern ControlMap
unison' = (|+ note "[0.1,-0.1]")
-- more more unison detune
unison'' :: Pattern ControlMap -> Pattern ControlMap
unison'' = (|+ note "[0.15,-0.15]")
-- more more more unison detune
unison''' :: Pattern ControlMap -> Pattern ControlMap
unison''' = (|+ note "[0.2,-0.2]")
----
-- invert a value up
octceil :: (Ord p, Num p) => p -> p -> p -> p
octceil by upper noteval =
  if noteval>upper then octceil by upper (noteval - by) else noteval
-- invert a value down
octfloor :: (Ord p, Num p) => p -> p -> p -> p
octfloor by lower noteval =
  if noteval<lower then octfloor by lower (noteval + by) else noteval
-- invert a pattern to sit in a given range as if it were a pattern of chords or notes
invert :: Double -> Double -> Pattern Double -> Pattern Double
invert low high p = fmap ((octfloor 12 low).(octceil 12 high)) $ p
----
pmap param numf stringf p = (Map.mapWithKey (\k v -> if k == param then (applyFIS numf (round.numf.fromIntegral) stringf v) else v)) <$> p
----
ninvert l h p = pmap "note" ((octfloor 12 l).(octceil 12 h)) id p
-------------------------------------------------------------------------------
----
-- swap between two different pattern modifiers according to a euclidian rhythm at a given rate r
efx :: Int -> Int -> Pattern Time -> (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
efx n d r e e' p = fast r $ ifp (\x -> (bjorklund (n,d))!!!x) ((slow r).(e)) ((slow r).(e')) $ p
-- swap between two patterns according to a euclidian rhythm at a given rate r
epat :: Int -> Int -> Pattern Time -> Pattern ControlMap -> Pattern ControlMap -> Pattern ControlMap
epat n d r p p' = fast r $ ifp (\x -> (bjorklund (n,d))!!!x) (const $ slow r $ p) (const $ slow r $ p') $ s ""
----
-------------------------------------------------------------------------------
----
-- convert a pattern of integrals to generic numerics
fimap :: (Integral a, Num b) => [a] -> [b]
fimap = (map fromIntegral)
----
-------------------------------------------------------------------------------
----
-- retrigger on a particular section of a pattern
focus :: Time -> Time -> Pattern a -> Pattern a
focus a b p = slow (listToPat [(b-a)]) $ zoom (toRational $ a, toRational $ b) $ p
-- like focus but takes a tuple
focus' :: (Time, Time) -> Pattern a -> Pattern a
focus' (a,b) p = slow (pure $ b-a) $ zoom (toRational $ a, toRational $ b) p
-- accepts a list of time offsets and a list of divisions of a cycle and re-arranges
-- a Pattern accordingly
linger' :: Pattern Time -> Pattern Time -> Pattern d -> Pattern d
linger' = tParam2 (\m n p -> _fast (1/n) $ zoomArc (Arc m (m+n)) p)
----
-------------------------------------------------------------------------------
----
-- makes a euclidian distribution as a list of 1s and 0s
en :: Num b => Int -> Int -> [b]
en n d = fimap $ fmap (\x -> if x then 1 else 0) $ bjorklund (n,d)
-- replaces every 1 in a euclidian distribution list of
-- 1s and 0s with a value from list l, cycling through
seqe :: [Int] -> [Int] -> [Int]
seqe l el = fimap $ if length el<1 then [] else (head l * (min 1 $ head el)):(seqe (drop (head el) (cycle l)) (tail el))
----
-------------------------------------------------------------------------------
----
hack :: Pattern Int -> Int -> Pattern a -> Pattern a
hack ipat n pat = innerJoin $ (\i -> _fast nT $ repeatCycles n $ pats !! i) <$> ipat
                  where
                    pats = map (\i -> zoom (fromIntegral i / nT, fromIntegral (i+1) / nT) pat) [0 .. (n-1)]
                    nT :: Time
                    nT = fromIntegral n
----
-- makes a heirarchical euclidian disribution as a list of Bools
distrib'' :: [Int] -> [Bool]
distrib'' xs = (foldr distrib' (replicate (last xs) True) (reverse $ layers xs))
                where
                  distrib' :: [Bool] -> [Bool] -> [Bool]
                  distrib' [] _ = []
                  distrib' (_:a) [] = False : distrib' a []
                  distrib' (True:a) (x:b) = x : distrib' a b
                  distrib' (False:a) b = False : distrib' a b
                  layers = map bjorklund . (zip<*>tail)
                  boolsToPat a b' = flip const <$> filterValues (== True) (fastFromList a) <*> b'
----
-- makes a heirarchical euclidian disribution as a list of 1s and 0s
eheir :: Num c => [Int] -> [c]
eheir xs = if length xs > 2 then (zipWith (\x y -> x + y) cur (eheir (tail xs))) else cur
              where cur = map (\x -> if x then 1 else 0) (distrib'' xs)
----
-------------------------------------------------------------------------------
----
-- cseq helper
cseq' :: (Eq a, Num a) => [a] -> a -> [a]
cseq' l acc = if length l < 1
                then []
                else [new] ++ (cseq' rest newacc)
              where cur = head l
                    rest = tail l
                    new = if cur /= 0 then cur else acc
                    newacc = if cur /= 0 then cur+1 else acc+1
----
-- takes a list and integrates from the last non-zero
-- value - non-zero values reset the counter
cseq :: (Integral a, Num b) => [a] -> [b]
cseq l = fimap $ fmap (\x -> x - 1) $ cseq' l 0
-- builds an ascending/descending numeric sequence from a euclidian
-- rhythm and a list of reset-values using cseq, seqe, and en
enseq :: Num b => Int -> Int -> [Int] -> [b]
enseq n d s = cseq $ seqe s $ en n d
-- like enseq, but outputs a Pattern
--enseqp :: Num b => Int -> Int -> [Int] -> [b]
--enseqp n d s = fastFromList $ cseq $ seqe s $ en n d
-- like enseq, but outputs a Pattern and has an optional divisor
ecount' :: Fractional a => Int -> Int -> a -> [Int] -> Pattern a
ecount' n d divs offsets = fastFromList $ ((/ divs) <$> (enseq n d offsets))
-- like ecount, but automatically divides the pattern by the number of euclidian steps
ecount :: (Fractional a1, Integral a2) => Int -> a2 -> [Int] -> Pattern a1
ecount n d offsets = ecount' n (fromIntegral $ d) (fromIntegral $ d) offsets
----
-------------------------------------------------------------------------------
----
-- accepts a number of divisions, a sample, and a sequence, and hacks up the pattern accordingly
-- (sequences on the beat)
slitch :: Integral a => a -> Pattern ControlMap -> [Double] -> Pattern ControlMap
slitch sdivs sample newseq = slow (spd/divs) $ loopAt 1 $ begin ((fastFromList $ newseq)/divs') # sample # legato 1 # unit "c" * speed 1
                        where divs = fromIntegral sdivs
                              divs' = fromIntegral sdivs
                              spd = fromIntegral (length newseq)
----
-- accepts a number of divisions, a sample, and a sequence, and hacks up the pattern accordingly
-- stretches/squashes into a single cycle
slitch' :: Integral a => a -> Pattern ControlMap -> [Double] -> Pattern ControlMap
slitch' sdivs sample newseq = slow (divs/spd) $ (slitch sdivs sample newseq) * speed (spd'/divs')
                        where divs = fromIntegral sdivs
                              divs' = fromIntegral sdivs
                              spd = fromIntegral (length newseq)
                              spd' = fromIntegral (length newseq)
----
whenmodr :: [Pattern Time] -> [Int] -> [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmodr speeds numerators denominators modifier pattern
    | done = modifiedpattern
    | otherwise =  whenmodr rests restn restd modifier modifiedpattern
    where modifiedpattern = outside speed (whenmod numerator denominator (modifier)) $ pattern
          numerator = (head numerators)
          denominator = (head denominators)
          speed = (head speeds)
          done = (null $ tail speeds) && (null $ tail numerators) && (null $ tail denominators)
          restn = if null (tail numerators) then [numerator] else (tail numerators)
          restd = if null (tail denominators) then [denominator] else (tail denominators)
          rests = if null (tail speeds) then [speed] else (tail speeds)
----
whenmods' speeds numerators denominators modifier pattern
    | done = modifiedpattern
    | otherwise =  whenmods' rests restn restd modifier modifiedpattern
    where modifiedpattern = outside speed (whenmod numerator denominator ((fast speed).(modifier))) $ pattern
          numerator = (head numerators)
          denominator = (head denominators)
          speed = (head speeds)
          done = (null $ tail speeds) && (null $ tail numerators) && (null $ tail denominators)
          restn = if null (tail numerators) then [numerator] else (tail numerators)
          restd = if null (tail denominators) then [denominator] else (tail denominators)
          rests = if null (tail speeds) then [speed] else (tail speeds)
----
whenmods speeds numerators denominators newpattern pattern =
  whenmods' speeds numerators denominators (const newpattern) pattern
----
-------------------------------------------------------------------------------
----
rip = ((stut 4 0.7 (-1/32)).(zoom ((1/4),(3/4))))
rip' n = ((stut 4 0.7 (-1/n)).(zoom ((1/4),(3/4))))
----
stutdown t d s p = if t>0 then stack [stutdown (t-1) d s (((* gain 0.9).(+ hcutoff 100).(* speed s).((1/d) <~)) $ p), p] else p
stutup t d s p = if t>0 then stack [stutup (t-1) d s (((* gain 0.9).(+ hcutoff 100).(* speed s).((1/d) ~>)) $ p), p] else p
----
ripdown d = stutdown 4 d 2
ripup d = stutup 4 d 2
----
ripd = ripdown 32
ripu = ripup 32
----
seqslice divs n d l s = slitch' divs s (enseq n d l)
----
-------------------------------------------------------------------------------
----
saturate amount = superimpose ((# shape amount).(# hcutoff 6500).(|*| gain 0.9))
----
-------------------------------------------------------------------------------
----
cycles2minutes cps ncycles = ((1/cps)*ncycles)/60
----
minutes2cycles cps minutes = (minutes*60)/(1/cps)
----
-------------------------------------------------------------------------------
----
integrate' x l =
  if (length l) > 0
    then [runningSum] ++ integrate' runningSum (tail l)
    else []
  where runningSum = x + (head l)
----
integrate = integrate' 0
----
-------------------------------------------------------------------------------
----
matchedIndices' i f l =
  if (length l) > 0
    then matchedIndex ++ (matchedIndices' (i+1) f (tail l))
    else []
  where matchedIndex = if (f $ head l) then [i] else []
----
matchedIndices = matchedIndices' 0
----
{-
replaceIndices' i r il l =
  if (min (length l) (length il)) > 0
    then [newVal] ++ (replaceIndices' (i+1) r il (tail l))
    else []
  where newVal = if (elem i il) then r else (head l)

replaceIndices = replaceIndices' 0
-}
----
convertBools t f = map (\x -> if x then t else f)
----
makeSeqPattern' :: Num a => a -> a -> [a] -> [t] -> [(a, a, t)]
makeSeqPattern' rsum unit l p =
  if (length l) > 0
    then [(rsum*unit, ((head l) + rsum)*unit, (head p))] ++ (makeSeqPattern' (rsum + (head l)) unit (tail l) (tail p))
    else []
----
makeSeqPattern unit l p = makeSeqPattern' 0 unit l p
----
seqhelper i unit f l p =
  if (length l) > 0
    then [(toRational (i*unit),toRational (i*unit*(head l)),f (head l) p)] ++ (seqhelper (i+(head l)) unit f (tail l) p)
    else []
----
applyn times func p = if times > 0 then (applyn (times-1) func (func p)) else p
----


----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------


-- panics
---- ghost
gst n = ((* gain 0.8).(* speed 1.05).(* release 0.15).(+ hcutoff 100).((1/n) <~))
---- rhythms
risefall c = stack [chl 32 c $ s "f1fall" # hcutoff 8500, chop 32 $ loopAt c $ s "noisebuild" # hcutoff 12000]
housep = stack [s "kick*4" # orbit 1, s "f1snare:1(2,4,1)" # orbit 1, s "sonhh:1(4,8,1)" * gain 0.9 # hcutoff 10000]
dnbp = stack [n "[0 ~ ~ ~ ~ 0 ~ ~]" |+ s "kick" # orbit 1, s "f1snare:0(2,4,1)" # orbit 1, n "[1 1 2 1 1 1 1 2]" |+ s "sonhh" * release 0.4 * gain 0.9 # hcutoff 10000]
gabbap = stack [saturate 0.992 $ s "<f1kick*4>" # hold 0.2 # release 0.1 # legato 1, n ("[~ 2]*4") |+ s "sonhh" # hcutoff 9000 * gain 0.8]
---- fills
trapfill = "808sd:4*<8 32 8 16 8>*4" |* speed (range 2 4 saw) * release 0.15
kksfill =  s "[kick kick f1snare]*16%3" # hcutoff (range 150 250 saw)
tomfill = s "drumtraks:11*16" # hcutoff 100 # cutoff 12000 * release 0.5 * release "[0.1 0.2 0.3 0.4]*4" + n "[0 1]*<8 4 2>*2" + note ("[3 2 1]*16%3"*1.5)
clapfill = s "[~ [f1clap:1]*2]*<2 1 2>*2" * gain 0.9 # hcutoff 800
fclapfill = s "[808:3*16]" |+ cutoff (rangex 10 7000 $ saw) * release 0.07 * speed 1
kickfill = superimpose (gst 16) $ superimpose (gst 8) $ "kick*8" + hcutoff 100
gabbafill = saturate 0.992 $ s "f1kick*<8 16>*5" # hold 0.2 # release 0.1 # legato 1

-----------
-----------
-----------
