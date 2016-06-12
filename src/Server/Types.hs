module Server.Types where



-- | Basically a percentage, where %50 is identity
newtype Vote = Vote
  { getVote :: Float
  } deriving (Eq)

makeVote :: Float -> Maybe Vote
makeVote x
  | x > 1     = Nothing
  | x < 0     = Nothing
  | otherwise = Just (Vote x)

-- | Over average
instance Monoid Vote where
  mempty = Vote 0.5
  mappend (Vote x) (Vote y) =
    Vote $ (x + y) / 2
