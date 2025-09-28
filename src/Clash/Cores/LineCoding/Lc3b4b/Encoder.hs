{- |
  Copyright   :  (C) 2025, Jasper Vinkenvleugel <j.t.vinkenvleugel@proton.me>
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  5b/6b encoding look-up table
-}
module Clash.Cores.LineCoding.Lc3b4b.Encoder where

import Clash.Prelude

encoderLut :: [Maybe (Bool, BitVector 4)]
encoderLut =
  [ Just (False, 0b0000) -- D.x.0
  , Just (False, 0b0000) -- D.x.1
  , Just (False, 0b0000) -- D.x.2
  , Just (False, 0b0000) -- D.x.3
  , Just (False, 0b0000) -- D.x.4
  , Just (False, 0b0000) -- D.x.5
  , Just (False, 0b0000) -- D.x.6
  , Just (False, 0b0000) -- D.x.P7
  , Just (False, 0b0000) -- D.x.0
  , Just (False, 0b0000) -- D.x.1
  , Just (False, 0b0000) -- D.x.2
  , Just (False, 0b0000) -- D.x.3
  , Just (False, 0b0000) -- D.x.4
  , Just (False, 0b0000) -- D.x.5
  , Just (False, 0b0000) -- D.x.6
  , Just (False, 0b0000) -- D.x.P7
  , Nothing -- D.x.0
  , Nothing -- D.x.1
  , Nothing -- D.x.2
  , Nothing -- D.x.3
  , Nothing -- D.x.4
  , Nothing -- D.x.5
  , Nothing -- D.x.6
  , Just (False, 0b0000) -- D.x.A7
  , Nothing -- D.x.0
  , Nothing -- D.x.1
  , Nothing -- D.x.2
  , Nothing -- D.x.3
  , Nothing -- D.x.4
  , Nothing -- D.x.5
  , Nothing -- D.x.6
  , Just (False, 0b0000) -- D.x.A7
  , Just (False, 0b0000) -- K.x.0
  , Just (False, 0b0000) -- K.x.1
  , Just (False, 0b0000) -- K.x.2
  , Just (False, 0b0000) -- K.x.3
  , Just (False, 0b0000) -- K.x.4
  , Just (False, 0b0000) -- K.x.5
  , Just (False, 0b0000) -- K.x.6
  , Just (False, 0b0000) -- K.x.7
  , Just (False, 0b0000) -- K.x.0
  , Just (False, 0b0000) -- K.x.1
  , Just (False, 0b0000) -- K.x.2
  , Just (False, 0b0000) -- K.x.3
  , Just (False, 0b0000) -- K.x.4
  , Just (False, 0b0000) -- K.x.5
  , Just (False, 0b0000) -- K.x.6
  , Just (False, 0b0000) -- K.x.7
  , Nothing -- K.x.0
  , Nothing -- K.x.1
  , Nothing -- K.x.2
  , Nothing -- K.x.3
  , Nothing -- K.x.4
  , Nothing -- K.x.5
  , Nothing -- K.x.6
  , Nothing -- K.x.7
  , Nothing -- K.x.0
  , Nothing -- K.x.1
  , Nothing -- K.x.2
  , Nothing -- K.x.3
  , Nothing -- K.x.4
  , Nothing -- K.x.5
  , Nothing -- K.x.6
  , Nothing -- K.x.7
  ]
