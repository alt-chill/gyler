{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.PushableBranch (
    PushableBranch,
    PushableBranchesSet
) where

import Gyler.Domain.RuntimeValidated.Template (mkRvNonEmptyText)

$(mkRvNonEmptyText "PushableBranch" "PushableBranchesSet")
