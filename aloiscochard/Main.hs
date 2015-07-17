import qualified Calculus0 as Q0
import qualified Calculus1 as Q1
import qualified Calculus2 as Q2
import qualified Calculus3 as Q3
import qualified Calculus4 as Q4

main = do
  print $ Q0.run    -- | 21 + 21
  print $ Q1.run 42 -- | x = (x + ((21 + 21) - 2))
  print $ Q2.run    -- | 21 + 21
  print $ Q3.run 42 -- | x = (x + ((21 + 21) - 2))
  print $ Q4.run 42 -- | let y = 21 in (x + (y + y))
