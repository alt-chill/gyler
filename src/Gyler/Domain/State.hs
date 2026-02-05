{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.State (
    State,
    StatesSet
) where

import Gyler.Domain.RuntimeValidated.Template (mkRvNonEmptyText)

$(mkRvNonEmptyText "State" "StatesSet")
