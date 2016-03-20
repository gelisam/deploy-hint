module Hint.Conversions (
      typeToString, kindToString, moduleToString, isSucceeded
) where

import qualified Hint.GHC as GHC

import Hint.Base
import qualified Hint.Compat as Compat

-- --------- Types / Kinds -----------------------

typeToString :: MonadInterpreter m => GHC.Type -> m String
typeToString t
 = do -- Unqualify necessary types
      -- (i.e., do not expose internals)
      unqual <- runGhc GHC.getPrintUnqual
      withDynFlags $ \df ->
        return $ GHC.showSDocForUser df unqual (Compat.pprType t)

kindToString :: MonadInterpreter m => Compat.Kind -> m String
kindToString (Compat.Kind k)
 = withDynFlags $ \df ->
     return $ GHC.showSDoc df (Compat.pprType k)

-- ---------------- Modules --------------------------

moduleToString :: GHC.Module -> String
moduleToString = GHC.moduleNameString . GHC.moduleName

-- ---------------- Misc -----------------------------

isSucceeded :: GHC.SuccessFlag -> Bool
isSucceeded GHC.Succeeded = True
isSucceeded GHC.Failed    = False
