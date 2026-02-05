{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.Branch (
    Branch,
    BranchesSet
) where

import Gyler.Domain.RuntimeValidated.Template (mkRvNonEmptyText)

$(mkRvNonEmptyText "Branch" "BranchesSet")
