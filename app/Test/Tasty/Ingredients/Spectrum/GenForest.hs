module Test.Tasty.Ingredients.Spectrum.GenForest (genForest) where


import Test.Tasty.Ingredients.Spectrum.Types
import Test.Tasty.Ingredients.Spectrum.ParseCSV



import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Graph as Graph
import Data.Graph (Tree(..), Forest)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Function (on)
    

-- | This function orders a List of Labels (from a spectrum.csv) into multiple trees. 
--   This is done by looking if one label contains another (based on source-code spans) 
--   and then using this contains relation to build trees around nodes that are not contained by anything (roots). 
genForest :: [Label] -> Forest Label
genForest all_nodes = map toTreeF roots_and_children
  where nodeSet :: Map String (Set Label)
        nodeSet = Map.fromAscList $
                       map (\g@(h:_) -> (loc_name h, Set.fromList g)) $
                       L.groupBy ((==) `on` loc_name) $
                       L.sortBy (compare `on` loc_name) $  all_nodes
        containedBy n = (n,ns'')
            where ns' = Set.delete n (nodeSet Map.! (loc_name n))
                  ns'' = Set.filter (insideHpcPos (loc_pos n) . loc_pos) ns'
        contained :: [(Label, Set Label)]
        contained = map containedBy all_nodes
        roots :: [Label]
        roots = map fst $ filter (Set.null . snd) contained
        roots_and_children = 
                    map (\r -> (r, map (\(c,s) -> (c, Set.delete r s)) $
                                            filter (Set.member r . snd) contained)) roots

        toTreeF (r, []) = Node r []
        toTreeF (r,children) = Node r $ map toTreeF r_and_c
            where rs = map fst $ filter (Set.null . snd) children
                  r_and_c = 
                        map (\r -> (r, map (\(c,s) -> (c, Set.delete r s)) $
                            filter (Set.member r . snd) children)) rs
                
            

