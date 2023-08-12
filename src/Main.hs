-- | main function is application of a status
--   function inbetween the IO layer and the code
module Main where
-- where the magic happens...
import Prelude()
import UPrelude
--import Sign ( checkStatus )
--import Prog.Init ( runProg )
--import Vulk ( runVulk )
-- | runs vulkan in the continuation monad, after
--   initializing state and env, using a status function
main ∷ IO ()
main = print "blop"--runProg checkStatus runVulk
