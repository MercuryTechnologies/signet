import qualified SignetTest
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain SignetTest.testTree
