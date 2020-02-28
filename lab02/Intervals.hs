module Intervals where

data Endpoint a
  = MIN
  | E a
  | MAX
  deriving (Eq, Ord) -- Ord lets us use min/max

data Interval a
  = Empty
  | Range (Endpoint a) (Endpoint a)
  deriving (Eq, Ord) -- Ord is for normalizeIS

instance (Show a) => Show (Interval a) where
  show Empty                     = "Empty"
  show (Range MIN MAX)           = "All"
  show (Range MIN (E end))       = "<" ++ show end
  show (Range (E start) MAX)     = ">=" ++ show start
  show (Range (E start) (E end)) = show start ++ "<=_<" ++ show end

instance (Read a) => Read (Interval a) where
  -- Read is a little less straightforward because it uses parsers,
  -- which we'll learn about later. Just replace the undefineds below
  -- with what you want to return and everything will work.
  readsPrec _ "Empty"        = [(Empty, "")]
  readsPrec _ "All"          = [(Range MIN MAX, "")]
  readsPrec _ ('>':'=':next) = [(Range (E $ read next) MAX, "")]
  readsPrec _ ('<':next)     = [(Range MIN (E $ read next), "")]
  readsPrec _ str            =
    -- Don't worry about this case. It is a bit clunky. We will learn
    -- a better option later in the course.
    case reads str of
      [] -> error "error parsing interval"
      (start, '<':'=':'_':'<':rest):_ ->
        case reads rest of
          [] -> error "error parsing interval"
          (end,_):_ -> [(Range (E start) (E end), "")]

-- All functions that return an interval should sanitize it.
sanitizeInterval :: Ord a => Interval a -> Interval a
sanitizeInterval int@(Range start end)
  | end <= start = Empty
  | otherwise    = int
sanitizeInterval int = int

intersectIntervals :: Ord a => Interval a -> Interval a -> Interval a
intersectIntervals Empty _ = Empty
intersectIntervals _ Empty = Empty
intersectIntervals (Range start1 end1) (Range start2 end2) =
  sanitizeInterval $ Range (max start1 start2) (min end1 end2)

---- Interval Sets ----

-- An interval set might have intervals that overlap or touch. Don't
-- worry about simplifying these cases in the following functions.
-- That is handled just before displaying by normalizeIS

type IntervalSet a = [Interval a]

toIS :: Interval a -> IntervalSet a
toIS = (:[])

emptyIS :: IntervalSet a
emptyIS = toIS Empty

allIS :: IntervalSet a
allIS = toIS $ Range MIN MAX

intersectISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a
intersectISI intls intl = map (intersectIntervals intl) intls

complementInterval :: Ord a => Interval a -> IntervalSet a
complementInterval Empty             = [Range MIN MAX]
complementInterval (Range MIN MAX)   = [Empty]
complementInterval (Range MIN end)   = [Range end MAX]
complementInterval (Range start MAX) = [Range MIN start]
complementInterval (Range start end) = [Range MIN start, Range end MAX]

-- An interval minus an interval must return an interval set because
-- the second could cut a hold in the middle of the first.
-- Big Hint: Use complements and intersetions.
differenceIntervals :: Ord a => Interval a -> Interval a -> IntervalSet a
differenceIntervals Empty _     = [Empty]
differenceIntervals itvl Empty  = [itvl]
differenceIntervals itvl1 itvl2 = intersectISI (complementInterval itvl2) itvl1

-- interval set minus an interval
differenceISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a
differenceISI [] _ = []
differenceISI (itvl:itvls) sub =
  union (differenceIntervals itvl sub) (differenceISI itvls sub)

---- Helpers for interval sets ----

intersection :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
intersection [] _      = []
intersection (a:as) bs = union (intersection bs as) (intersectISI bs a)

union :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
union set1 set2 = foldr (:) set1 set2

difference :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
difference a []     = a
difference a (b:bs) = difference (differenceISI a b) bs

---- Queries on interval sets ----

intersectAll :: Ord a => [IntervalSet a] -> IntervalSet a
intersectAll []            = emptyIS
intersectAll (first:rest)  = foldr intersection first rest

unionAll :: Ord a => [IntervalSet a] -> IntervalSet a
unionAll []                = emptyIS
unionAll (first:rest)      = foldr union first rest

-- Subtract from the first interval set all the remaining interval
-- sets.
differenceAll :: Ord a => [IntervalSet a] -> IntervalSet a
differenceAll []           = emptyIS
differenceAll (first:rest) = foldr (intersection . difference first) first rest

---- Boolean Helpers ----

isEmpty :: Eq a => IntervalSet a -> Bool
isEmpty = null . filter (/= Empty)

-- Hint: areDisjoint and isSubset are simpler than areEqual. Use what
-- you have already defined.

-- two interval sets are disjoint if they do not overlap
areDisjoint :: Ord a => IntervalSet a -> IntervalSet a -> Bool
areDisjoint a b = isEmpty $ intersection a b

isSubset :: Ord a => IntervalSet a -> IntervalSet a -> Bool
isSubset a b = isEmpty $ difference a b

areEqual :: Ord a => IntervalSet a -> IntervalSet a -> Bool
areEqual is1 is2 =
    is1 `isSubset` is2 &&
    is2 `isSubset` is1

---- Boolean Queries ----

areAllDisjoint :: Ord a => [IntervalSet a] -> Bool
areAllDisjoint [] = True
areAllDisjoint (first:rest) = foldr ((&&) . areDisjoint first) True rest && areAllDisjoint rest

areAllEqual :: Ord a => [IntervalSet a] -> Bool
areAllEqual [] = True
areAllEqual [first] = True
areAllEqual (first:rest) = areEqual first (head rest) && (areAllEqual rest)
