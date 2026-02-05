{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Gyler.Domain.Maintainer (
    Maintainer,
    MaintainersSet
) where

-- |
-- Module: Gyler.Domain.Maintainer
-- Description: A RuntimeValidated datatype containing maintainers recognized by girar.
--
-- Use RuntimeValidated `mkValidate` to create a new value.
-- Use `fetch` with `MaintainersQuery` to obtain a `MaintainersSet`.
--
-- See: `Gyler.FetchSpec.MaintainersQuery`

import Gyler.Domain.RuntimeValidated.Template (mkRvNonEmptyText)

$(mkRvNonEmptyText "Maintainer" "MaintainersSet")
