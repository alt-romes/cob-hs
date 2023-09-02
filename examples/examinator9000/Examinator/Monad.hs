module Examinator.Monad
  ( ask
  ) where

import Prettyprinter as PP
import Prettyprinter.Render.Terminal as PP
import Control.Monad.Reader
import System.Console.ANSI
import Cob

-- | An examinator action which serves to examinate a given exam, usually
-- returning a boolean to indicate whether the examination was successful.
newtype Examinator a = Examinator { examinate :: ReaderT Int Cob a
                                  -- ^ Reads the depth to nest questions
                                  }


-- todo: hide cursor

--------------------------------------------------------------------------------
----- Terminal input -----------------------------------------------------------

bold :: Doc AnsiStyle -> Doc AnsiStyle
bold = annotate PP.bold

-- | An empty examination, to put at the end of directives that don't require
-- any further action
done :: Examinator ()
done = return ()
