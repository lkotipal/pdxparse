diff --git a/src/HOI4/Handlers.hs b/src/HOI4/Handlers.hs
index d3d909e..4c09ab8 100644
--- a/src/HOI4/Handlers.hs
+++ b/src/HOI4/Handlers.hs
@@ -173,7 +173,7 @@ import qualified Data.Trie as Tr
 import qualified Text.PrettyPrint.Leijen.Text as PP
 
 import Data.List (foldl', intersperse)
-import Data.Maybe (isJust, isNothing, fromMaybe)
+import Data.Maybe
 
 import Control.Applicative (liftA2)
 import Control.Arrow (first)
@@ -4158,14 +4158,14 @@ foldCompound "addBuildingConstruction" "BuildingConstruction" "bc"
             "bunker" -> MsgAddBuildingConstruction (iconText "land fort")
             "supply_node" -> MsgAddBuildingConstruction (iconText "supply hub")
             _ -> MsgAddBuildingConstruction (iconText _type)
-         ) buildingLoc _level "" "check files if bunker" 
+         ) buildingLoc _level "" "check files if bunker"
     |]
 -}
 --------------------------------------------------------------
 
 data HOI4ABCProv
-    = HOI4ABCProvSimple Double  -- province = key
-    | HOI4ABCProvMult GenericScript
+    = HOI4ABCProvSimple [Double]  -- province = key
+    | HOI4ABCProvAll { prov_all_provinces :: Bool, prov_limit_to_border :: Bool }
             -- province = { id = key id = key }
             -- province = { all_provinces = yes	limit_to_border = yes}
     deriving Show
@@ -4175,7 +4175,7 @@ data HOI4ABCLevel
     | HOI4ABCLevelVariable Text
     deriving Show
 
-data HOI4AddBC = HOI4AddBC{ 
+data HOI4AddBC = HOI4AddBC{
       addbc_type :: Text
     , addbc_level :: Maybe HOI4ABCLevel
     , addbc_instantbuild :: Bool
@@ -4186,7 +4186,7 @@ newABC :: HOI4AddBC
 newABC = HOI4AddBC "" Nothing False Nothing
 
 addBuildingConstruction :: forall g m. (HOI4Info g, Monad m) => StatementHandler g m
-addBuildingConstruction stmt@[pdx| %_ = @scr |] = 
+addBuildingConstruction stmt@[pdx| %_ = @scr |] =
 --    let leveltype = foldl' levelCheck HOI4AddBC scr in
     msgToPP =<< pp_abc (foldl' addLine newABC scr)
     where
@@ -4194,22 +4194,25 @@ addBuildingConstruction stmt@[pdx| %_ = @scr |] =
 --        levelCheck lvl [pdx| level = !levelint |] = lvl HOI4ABCLevelSimple
 --        levelCheck lvl [pdx| level = $levelvar |] = lvl HOI4ABCLevelVariable
 --        levelcheck lvl stmt = lvl
-        
+
         addLine :: HOI4AddBC -> GenericStatement -> HOI4AddBC
         addLine abc [pdx| type = $build |] = abc { addbc_type = build }
-        addLine abc stmt@[pdx| level = %rhs |] = 
+        addLine abc stmt@[pdx| level = %rhs |] =
             case rhs of
                 (floatRhs -> Just amount) -> abc { addbc_level = Just (HOI4ABCLevelSimple amount) }
                 GenericRhs amount [] -> abc { addbc_level = Just (HOI4ABCLevelVariable amount) }
-                _ -> (trace $ "Unknown leveltype in add_building_construction: " ++ show stmt) $ abc 
+                _ -> (trace $ "Unknown leveltype in add_building_construction: " ++ show stmt) $ abc
         addLine abc [pdx| instant_build = yes |] = abc { addbc_instantbuild = True } --default is no an doesn't exist
         addLine abc [pdx| province = %rhs |] =
             case rhs of
-                (floatRhs -> Just id) -> abc { addbc_province = Just (HOI4ABCProvSimple id) }
---                CompoundRhs provs -> do
---                    provinces <- abcProv provs
---                    return $ abc { addbc_province = Just (HOI4ABCProvMult provinces) }
-                _ -> (trace $ "Unknown provincetype in add_building_construction: ") $ abc
+                (floatRhs -> Just id) -> abc { addbc_province = Just (HOI4ABCProvSimple [id]) }
+                CompoundRhs provs
+                    | Just ids <- forM provs $ \case { [pdx| id = %i |] -> floatRhs i; _ -> Nothing }
+                        -> abc { addbc_province = Just (HOI4ABCProvSimple ids ) }
+                    | all_provinces <- fromMaybe False $ listToMaybe [ b == "yes" | [pdx| all_provinces = $b |] <- provs ]
+                    , limit_to_border <- fromMaybe False $ listToMaybe [ b == "yes" | [pdx| limit_to_border = $b |] <- provs ]
+                        -> abc { addbc_province = Just (HOI4ABCProvAll all_provinces limit_to_border) }
+                _ -> trace ("Unknown provincetype in add_building_construction: " ++ show rhs) $ abc
 
         addLine abc stmt = (trace $ "Unknown in add_building_construction: " ++ show stmt) $ abc
 
@@ -4224,7 +4227,7 @@ addBuildingConstruction stmt@[pdx| %_ = @scr |] =
                     HOI4ABCLevelVariable amount -> 0
                 variable = case amountvar of
                     HOI4ABCLevelVariable amount -> amount
-                    HOI4ABCLevelSimple amount -> ""                   
+                    HOI4ABCLevelSimple amount -> ""
             return $ MsgAddBuildingConstruction (iconText (T.toLower buildingLoc)) buildingLoc amount variable provform
         pp_abc abc = return $ (trace $ "Not handled in caddBuildingConstruction: abc=" ++ show abc ++ " stmt=" ++ show stmt) $ preMessage stmt
 addBuildingConstruction stmt = (trace $ "Not handled in addBuildingConstruction: " ++ show stmt) $ preStatement stmt
