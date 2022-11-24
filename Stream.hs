{-# LANGUAGE FlexibleContexts, Safe #-}

module Stream where

import Control.Applicative ( liftA2 )
import Control.Comonad ( (=>>), Comonad(duplicate, extract) )
import Data.List ( intercalate )

-- The Stream datatype.
infixr 5 :<
data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
  show xs
    = concat ["{", shownItems, ",...}"]
      where
        items      = streamTake 10 xs
        shownItems = tail $ init $ show items

-- Stream creation functions.
streamOf :: a -> Stream a
streamOf x = x :< streamOf x

streamWithStart :: [a] -> a -> Stream a
streamWithStart xs x = foldr (:<) (streamOf x) xs

cycleStreamOf :: [a] -> Stream a
cycleStreamOf [] = error "Empty list."
cycleStreamOf xs
  = go xs
    where
      go []       = go xs
      go (x : xs) = x :< go xs

iterateAsStream :: (a -> a) -> a -> Stream a
iterateAsStream f x = x :< iterateAsStream f (f x)

scanStream :: (b -> a -> b) -> b -> Stream a -> Stream b
scanStream f i (x :< xs) = i :< scanStream f (f i x) xs

-- Some useful stream utilities.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :< xs) = f x :< streamMap f xs

streamIterateMap :: (a -> a) -> Stream a -> Stream a
streamIterateMap f xxs = sim f xxs
  where
    sim g (x :< xs) = g x :< sim (f . g) xs

streamFilter :: (a -> Bool) -> Stream a -> Stream a
streamFilter p (x :< xs)
  | p x       = x :< streamFilter p xs
  | otherwise = streamFilter p xs

streamTake :: Int -> Stream a -> [a]
streamTake 0 _         = []
streamTake i (x :< xs) = x : streamTake (pred i) xs

streamHead :: Stream a -> a
streamHead (x :< xs) = x

streamTail :: Stream a -> Stream a
streamTail (_ :< xs) = xs

streamTails :: Stream a -> Stream (Stream a)
streamTails xs = xs :< streamTails (streamTail xs)

infixl 9 !!!
(!!!) :: Stream a -> Int -> a
(x :< _) !!! 0 = x
(_ :< s) !!! n = s !!! (n - 1)

-- A useful view of a stream of streams.
diagStream :: Stream (Stream a) -> Stream a
diagStream (s :< ss) = streamHead s :< diagStream (fmap streamTail ss)

-- Streams are monadic...
instance Functor Stream where
  fmap = streamMap

instance Applicative Stream where
  pure = streamOf
  (f :< fs) <*> (x :< xs)
    = f x :< (fs <*> xs)

instance Monad Stream where
  return    = pure
  s >>= sgf = diagStream (fmap sgf s)

instance Comonad Stream where
  extract   = streamHead
  duplicate = streamTails

-- With this, we can build a stream from any other comonad.
comonadToStream :: Comonad c => (c a -> a) -> c a -> Stream (c a)
comonadToStream f = iterateAsStream (=>> f)

-------------------------------------------------------------------------------
-- UP: STREAM | DOWN: TAPE
-------------------------------------------------------------------------------

-- The class of all things (turing) tape-like.
class TapeLike t where
  -- Law: fwd . rev == id == rev . fwd
  fwd       :: t a -> t a
  rev       :: t a -> t a

  -- Law: (writeHead x) . readHead != x (if x is not already at the tape head).
  -- Law: readHead . (writeHead x) == x
  readHead  :: t a -> a
  writeHead :: a -> t a -> t a

-- The tape data type... a stream of streams with a central view.
-- Essentially a seekable stream.
-- Field names given _ prefixes for use with makeLens.
data Tape a = Tape
  { _leftT  :: Stream a
  , _headT  :: a
  , _rightT :: Stream a
  }

instance TapeLike Tape where
  fwd       = next
  rev       = prev
  readHead  = readTape
  writeHead = writeTape

instance Show a => Show (Tape a) where
  show (Tape l c r)
    = intercalate " | " [rShowStream l', show c, show r]
      where
        l' = cycleStreamOf $ reverse $ streamTake 10 l

        rShowStream xs
          = concat ["{...,", shownItems, "}"]
            where
              items      = streamTake 10 xs
              shownItems = tail $ init $ show items

-- Tape creation function.
tapeOf :: a -> Tape a
tapeOf a = Tape (streamOf a) a (streamOf a)

tapeWithStart :: a -> [a] -> a -> [a] -> a -> Tape a
tapeWithStart lc ls c rs rc
  = Tape (streamWithStart ls lc) c (streamWithStart rs rc)

tapeFromFoldable :: Foldable f => f a -> a -> Tape a
tapeFromFoldable xs rest
  = writeTape' xs (tapeOf rest)

cycleTapeOf :: [a] -> Tape a
cycleTapeOf [] = error "Empty list."
cycleTapeOf xs
  = Tape l c r -- The created tape has its head at the head of the list.
    where
      (c : cs) = xs
      xs'      = cs ++ [c]
      l        = cycleStreamOf (reverse xs)
      r        = cycleStreamOf xs'

iterateAsTape :: (a -> a) -> (a -> a) -> a -> Tape a
iterateAsTape lf rf x
  = Tape (iterateAsStream lf (lf x)) x (iterateAsStream rf (rf x))

-- Immutably read / write to a tape at its head.
readTape :: Tape a -> a
readTape (Tape _ c _) = c

writeTape :: a -> Tape a -> Tape a
writeTape c (Tape l _ r) = Tape l c r

-- Write a list of objects to a tape, then optionally rewind to the first
-- insertion point.
writeTape' :: Foldable f => f a -> Tape a -> Tape a
writeTape' xs t
  = let t' = foldl (\ t x -> next $ writeTape x t) t xs in
    shiftTape (- (length xs)) t'

-- Move the tape head around.
next :: Tape a -> Tape a
next (Tape l c (r :< r')) = Tape (c :< l) r r'

prev :: Tape a -> Tape a
prev (Tape (l :< l') c r) = Tape l' l (c :< r)

shiftTape :: (Ord n, Num n, Enum n) => n -> Tape a -> Tape a
-- Positive numbers indicate "next"-ing the tape n times.
-- Negative numbers indicate "prev"-ing the tape (abs n) times.
shiftTape n tape
  | n > 0     = callN n next tape
  | n < 0     = callN (abs n) prev tape
  | otherwise = tape

infixl 9 >!<
(>!<) :: Tape a -> Int -> a
t >!< ix = readTape (shiftTape ix t)

callN :: (Eq n, Num n, Enum n) => n -> (a -> a) -> a -> a
callN 0 _ x = x
callN n f x = callN (pred n) f (f x)

-- Utility functions for tapes.
tapeMap :: (a -> b) -> Tape a -> Tape b
tapeMap f (Tape l c r) = Tape (fmap f l) (f c) (fmap f r)

tapeIterateMap :: (a -> a) -> (a -> a) -> (a -> a) -> Tape a -> Tape a
tapeIterateMap lf cf rf (Tape l c r) = Tape (streamIterateMap lf l) (cf c) (streamIterateMap rf r)

-- Map the left, head and right parts with different functions.
tapePartMap :: (a -> b) -> (a -> b) -> (a -> b) -> Tape a -> Tape b
tapePartMap fl fc fr (Tape l c r) = Tape (fmap fl l) (fc c) (fmap fr r)

-- Diagonals are also a useful view of tapes of tapes.
diagTape :: Tape (Tape a) -> Tape a
diagTape (Tape lt ct rt)
  = Tape (fmap readTape lt') (readTape ct) (fmap readTape rt')
    where
      lt' = streamIterateMap prev lt
      rt' = streamIterateMap next rt

-- The "inverse" of diagTape can also be defined.
-- It is not strictly an inverse as diagTape is lossy.
diagTape' :: Tape a -> Tape (Tape a)
diagTape' = iterateAsTape prev next

-- Tapes are Monadic...
instance Functor Tape where
  fmap = tapeMap

instance Applicative Tape where
  pure = tapeOf
  (Tape lf f rf) <*> (Tape lx x rx)
    = Tape (lf <*> lx) (f x) (rf <*> rx)

instance Monad Tape where
  return    = pure
  t >>= tgf = diagTape (fmap tgf t)

instance Comonad Tape where
  extract   = readTape
  duplicate = diagTape'

-- And same as Stream, we can now construct tapes from any comonad.
comonadToTape :: Comonad c => (c a -> a) -> (c a -> a) -> c a -> Tape (c a)
comonadToTape lf rf = iterateAsTape (=>> lf) (=>> rf)

-------------------------------------------------------------------------------
-- UP: TAPE | DOWN: TWODIMTAPE
-------------------------------------------------------------------------------

-- An infinite 2D field of data.
-- Essentially a (Tape (Tape *)).
data TwoDimTape a = TwoDimTape
  { _left2T  :: Stream (Tape a)
  , _head2T  :: Tape a
  , _right2T :: Stream (Tape a)
  }

-- Wrappers for different axes of movement.
newtype LR2DTape a = LR2DTape (TwoDimTape a)
newtype UD2DTape a = UD2DTape (TwoDimTape a)

instance TapeLike LR2DTape where
  fwd (LR2DTape t)         = LR2DTape $ right t
  rev (LR2DTape t)         = LR2DTape $ left t
  readHead (LR2DTape t)    = read2dTape t
  writeHead x (LR2DTape t) = LR2DTape $ write2dTape x t

instance TapeLike UD2DTape where
  fwd (UD2DTape t)         = UD2DTape $ down t
  rev (UD2DTape t)         = UD2DTape $ up t
  readHead (UD2DTape t)    = read2dTape t
  writeHead x (UD2DTape t) = UD2DTape $ write2dTape x t

instance Show a => Show (TwoDimTape a) where
  show (TwoDimTape ut ct rt)
    = intercalate "\n" $ concat [us, [""], cs, [""], ds]
      where
        us = reverse $ streamTake 10 $ fmap showRawTape ut
        cs = [showRawTape ct]
        ds = streamTake 10 $ fmap showRawTape rt

        showRawTape (Tape l' c' r')
          = unwords [us, " ", cs, " ", ds]
            where
              us = unwords $ map show $ reverse $ streamTake 10 l'
              cs = show c'
              ds = unwords $ map show $ streamTake 10 r'

-- Two-dimensional tape creation functions.
twoDimTapeOf :: a -> TwoDimTape a
twoDimTapeOf x = let Tape u c d = diagTape' (tapeOf x) in
  TwoDimTape u c d

cycleTwoDimTapeOf :: [a] -> TwoDimTape a
cycleTwoDimTapeOf [] = error "Empty list."
cycleTwoDimTapeOf xs = let Tape ut ct dt = diagTape' (cycleTapeOf xs) in
  TwoDimTape ut ct dt

twoDimTapeFromFoldable :: Foldable f => f (f a) -> a -> TwoDimTape a
twoDimTapeFromFoldable xxs rest = writeGrid xxs (twoDimTapeOf rest)

iterateAs2dTape :: (a -> a) -> (a -> a) -> (a -> a)
                   -> (a -> a) -> a -> TwoDimTape a
iterateAs2dTape uf lf rf df x
  = TwoDimTape ut (iterateAsTape lf rf x) dt
    where
      ut = iterateAsStream (fmap uf) $ uf <$> iterateAsTape lf rf x
      dt = iterateAsStream (fmap df) $ df <$> iterateAsTape lf rf x

-- Movement inside a 2D tape.
left :: TwoDimTape a -> TwoDimTape a
left (TwoDimTape ut ct dt)
  = TwoDimTape (fmap prev ut) (prev ct) (fmap prev dt)

right :: TwoDimTape a -> TwoDimTape a
right (TwoDimTape ut ct dt)
  = TwoDimTape (fmap next ut) (next ct) (fmap next dt)

up :: TwoDimTape a -> TwoDimTape a
up (TwoDimTape (ut :< ut') ct dt)
  = TwoDimTape ut' ut (ct :< dt)

down :: TwoDimTape a -> TwoDimTape a
down (TwoDimTape ut ct (dt :< dt'))
  = TwoDimTape (ct :< ut) dt dt'

-- +X = Right, +Y = Down...
move2d :: (Ord n, Num n, Enum n) => (n, n) -> TwoDimTape a -> TwoDimTape a
move2d (dx, dy)
  = my . mx
    where
      mx = if dx < 0 then callN (abs dx) left else callN dx right
      my = if dy < 0 then callN (abs dy) up else callN dy down

-- Reading and writing.
read2dTape :: TwoDimTape a -> a
read2dTape (TwoDimTape _ ct _) = readTape ct

write2dTape :: a -> TwoDimTape a -> TwoDimTape a
write2dTape x t@(TwoDimTape _ ct _) = t { _head2T = writeTape x ct }

write2dTape' :: Tape a -> TwoDimTape a -> TwoDimTape a
write2dTape' t tdt = tdt { _head2T = t }

write2dTape'' :: Foldable f => f a -> TwoDimTape a -> TwoDimTape a
write2dTape'' xs t@(TwoDimTape _ ct _) = t { _head2T = writeTape' xs ct }

writeGrid :: Foldable f => f (f a) -> TwoDimTape a -> TwoDimTape a
writeGrid xxs t
  = let t' = foldl (\ t xs -> down $ write2dTape'' xs t) t xxs in
    move2d (0, - (length xxs)) t'

-- Utility functions for 2D tapes.
twoDimTapeMap :: (a -> b) -> TwoDimTape a -> TwoDimTape b
twoDimTapeMap f (TwoDimTape ut ct dt)
  = TwoDimTape (fmap (fmap f) ut) (fmap f ct) (fmap (fmap f) dt)

twoDimTapePartMap :: (a -> b) -> (a -> b) -> (a -> b) -> (a -> b) -> (a -> b)
                     -> TwoDimTape a -> TwoDimTape b
twoDimTapePartMap uf lf cf rf df (TwoDimTape ut ct dt)
  = TwoDimTape
    (fmap (tapePartMap lf uf rf) ut)
    (tapePartMap lf cf rf ct)
    (fmap (tapePartMap lf df rf) dt)

-- Just like the 1D tape, diagonalisation provides a good view of nested 2D
-- tapes. It is still lossy.
diag2dTape :: TwoDimTape (TwoDimTape a) -> TwoDimTape a
diag2dTape (TwoDimTape ut ct dt)
  = TwoDimTape (fmap (fmap read2dTape) ut') (fmap read2dTape ct') (fmap (fmap read2dTape) dt')
    where
      ct' = tapeIterateMap left id right ct

      ut' = streamIterateMap (tapePartMap (left . up) up (right . up) . prev) ut
      dt' = streamIterateMap (tapePartMap (left . down) down (right . down) . next) dt

-- And its "inverse".
diag2dTape' :: TwoDimTape a -> TwoDimTape (TwoDimTape a)
diag2dTape' = iterateAs2dTape up left right down

-- 2D tapes are Monadic... (Oh god.)
instance Functor TwoDimTape where
  fmap = twoDimTapeMap

instance Applicative TwoDimTape where
  pure = twoDimTapeOf
  (TwoDimTape uf cf df) <*> (TwoDimTape ux cx dx)
    = TwoDimTape (liftA2 (<*>) uf ux) (cf <*> cx) (liftA2 (<*>) df dx)

instance Monad TwoDimTape where
  return        = pure
  tdt >>= tdtgf = diag2dTape (fmap tdtgf tdt)

instance Comonad TwoDimTape where
  extract   = read2dTape
  duplicate = diag2dTape'

-- And finally, we can construct a 2D tape from any comonad.
comonadTo2dTape :: Comonad c => (c a -> a) -> (c a -> a) -> (c a -> a)
                                -> (c a -> a) -> c a -> TwoDimTape (c a)
comonadTo2dTape uf lf rf df = iterateAs2dTape (=>> uf) (=>> lf) (=>> rf) (=>> df)

-- 3D tapes and beyond are left as an exercise for the reader.
