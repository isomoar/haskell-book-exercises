module HttpStuff where

import           Data.ByteString.Lazy hiding (map)
import           Network.Wreq


urls :: [String]
urls = [ "http://package.elm-lang.org/packages/elm-lang/html/latest"
        , "http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Keyed"
       ]


mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls


traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
