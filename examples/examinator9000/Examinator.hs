{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Examinator where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Examinator.Monad
import Cob



-- | The type 'Test's, which have a name and a collection of 'Prompt's that may
-- or may not be already answered/satisfied.
-- data Test = Test String [(Prompt, Bool)]

-- | The type of exam evaluation prompts.
-- It's constructed with a problem statement and a Cob action (written using
-- the Cob DSL) that checks whether the problem has been successfully
-- completed.
-- SUBSUMED by Examinator monad, which has questions and validation built-in.
-- data Prompt = Prompt String (Examinator Bool)

-- test_insert :: Test
-- test_insert = Test "Test 1" $ map (,True)
--                             [ Prompt "Insert an item to the _" (return True)
--                             , Prompt "Input the exact word \"cat\"" (expect "cat")
--                             ]

-- test_delete :: Test
-- test_delete = Test "Test 2" $ map (,False)
--                             [ Prompt "Delete the item with Id X from _" (return False)
--                             , Prompt "Then delete the item with Id X from _" (return False)
--                             ]

type Test = Examinator Bool

tests :: [Test]
tests =
  [ test2
  ]

-- test1 :: Test
-- test1 = do
--   directive ("Aceda à platforma COB usando o username e password" <> bold "demo") do
--     directive "Aceda ao menu Back-End para ver a solução de demonstração World Data" done
--     question "Que definições tem a solução de demonstração?" do
--       deflist :: [String] <- readAnswer


test2 :: Test
test2 = do
  say (bold "Criar Registos:")
  directive "Crie um novo registo na solução de demonstração, na definição Country Trip Ideas

