module Main where

import Questao1
import Questao2

main :: IO ()
main = do
  let str1 = "haskell is a lot of fun"
  let str2 = "for writers a random sentence can help them get their creative juices flowing since the topic of the sentence is completely unknown it forces the writer to be creative when the sentence appears there are a number of different ways a writer can use the random sentence for creativity the most common way to use the sentence is to begin a story another option is to include it somewhere in the story a much more difficult challenge is to use it to end a story in any of these cases it forces the writer to think creatively since they have no idea what sentence will appear from the tool"
  let test2 = (decode 3 . encode 3 $ str2) == (crack str2)
  let test1 = (decode 3 . encode 3 $ str1) == (crack str1)
  putStrLn . show $ test1
  putStrLn . show $ test2
  putStrLn "isTaut"
  let a = Var 'A'
  let b = Var 'B'
  let c = Var 'C'
  let prop = ((a :->: b) :&: (b :->: c)) :->: (a :->: c)
  let prop2 = (a :&: b) :->: a
  putStr "((A -> B) ^ (B -> C) -> (A -> C)) é tautologia? "
  putStrLn . show $ isTaut prop
  putStr "(A ^ B) -> A é tautologia? "
  putStrLn . show $ isTaut prop2