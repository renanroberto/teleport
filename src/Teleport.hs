module Teleport where

import Prelude hiding (lookup)
import Data.Map.Lazy (Map, toList, insert, delete, lookup)

type Label = String
type Portals = Map Label FilePath


listPortals :: Portals -> String
listPortals = (unlines . map transform . toList)
  where transform :: (String, String) -> String
        transform (key, value) = "[" ++ key ++ "] " ++ value

addPortal :: Label -> FilePath -> Portals -> Portals
addPortal = insert

removePortal :: Label -> Portals -> Portals
removePortal = delete

goToPortal :: Label -> Portals -> Maybe FilePath
goToPortal = lookup
