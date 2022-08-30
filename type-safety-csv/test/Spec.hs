import Test.Tasty
import Test.Tasty.HUnit

import Lib
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy.UTF8 (fromString) -- from utf8-string
import Data.Word (Word8)



main = defaultMain tests

csv = fromString "Nome,Endereco\nLucas,Rua 1\nJoao,Rua 2\nMarcos,Rua 3"

lucasRow = ("Lucas" :> "Rua 1" :> VNil)
headers = ("Nome" :> "Endereco" :> VNil)
firstColumn = ("Lucas" :> "Joao" :> "Marcos" :> VNil)

test1Return = (NameIndexed headers (lucasRow :> ("Joao" :> "Rua 2" :> VNil):> ("Marcos" :> "Rua 3" :> VNil):> VNil))

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "decode"
      (assertEqual  "can decode a valid csv" (Just test1Return) (Lib.decode Header sNat2 sNat3 csv))
  , testCase "getRow"
      (assertEqual "can get a valid row" lucasRow (getRow cidx0 test1Return))
  , testCase "getColumn"
      (assertEqual "can get a valid column" firstColumn (getColumnByIndex cidx0 test1Return))
  , testCase "getHeaders"
      (assertEqual "can get the header from a header type" headers (getHeaders test1Return))
  ]


