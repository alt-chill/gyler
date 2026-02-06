{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.Task (
    TaskView(..),
    Task(..),

    StateOf(..),

    pattern IDOnlyTask
) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable(..))
import Data.Serialize (Serialize(..), Get)

import Gyler.Serialize (deriveIDSerializable)

import GHC.Natural (Natural)

---------------------------------------------------
-- Task representation type

data TaskView = IDOnly
--              | Regular
--              | Full
              deriving (Show, Eq, Generic)

instance Hashable  TaskView
instance Serialize TaskView

-- There are two ways to retrieve information about a task:
-- - Via `gyle task ls`
-- - Via `gyle task show TASK_ID`
--
-- In the first case, we can parse the output in at least two ways:
-- - Retrieve only the IDs of the user's tasks
-- - Parse all available information
--
-- The type `Task 'IDOnly`  represents the case `gyle task ls` with only task IDs parsed.
-- The type `Task 'Regular` represents the case `gyle task ls` with all printed information parsed.
-- The type `Task 'Full`    represents the case `gyle task show TASK_ID`.

-- TEMP: currently `Task 'IDOnly` is only one implemented type.
type family StateOf (t :: TaskView) where
    StateOf 'IDOnly  = ()
--    StateOf 'Regular = State

data Task (t :: TaskView) = Task {
    _id     :: !Natural,
    _state  :: !(StateOf t)
}

---------------------------------------------------
-- Pattern Synonyms

pattern IDOnlyTask :: Natural -> Task 'IDOnly
pattern IDOnlyTask n = Task n ()

---------------------------------------------------
-- Typeclass Instances

deriving instance (Show (StateOf t)) => Show (Task t)
deriving instance (Eq   (StateOf t)) => Eq   (Task t)
deriving instance Generic (Task t)

instance (Hashable  (StateOf t)) => Hashable  (Task t)

---------------------------------------------------
-- Serialize
--
$(deriveIDSerializable [t| Task 'IDOnly |])
