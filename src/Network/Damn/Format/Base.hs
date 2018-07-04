module Network.Damn.Format.Base
    ( module Network.Damn.Format.Base
    , module Network.Damn.Tablumps
    , Text
    ) where

import Data.Text
import Network.Damn.Tablumps

type Formatter m = Either Text Lump -> m
