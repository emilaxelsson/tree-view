module Data.Tree.View
    ( showTree
    , drawTree
    , Behavior (..)
    , NodeInfo (..)
    , htmlTree
    , writeHtmlTree
    ) where



import Control.Monad.State
import Data.Traversable (traverse)
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree
import System.IO



indentInit :: [String] -> [String]
indentInit [] = []
indentInit (s:ss) = (" ├╴" ++ s) : map (" │ " ++) ss

indentLast :: [String] -> [String]
indentLast [] = []
indentLast (s:ss) = (" └╴" ++ s) : map ("   " ++) ss

indentChildren :: [[String]] -> [[String]]
indentChildren [] = []
indentChildren ns = map indentInit (init ns) ++ [indentLast (last ns)]

appLast :: [String] -> String -> [String]
appLast ss s = init ss ++ [last ss ++ s]

showTree' :: Tree String -> [String]
showTree' (Node n ns) = n : concat (indentChildren (map showTree' ns))

-- | Show a 'Tree' using Unicode art
showTree :: Tree String -> String
showTree = unlines . showTree'

-- | Draw a 'Tree' on the terminal using Unicode art
--
-- Example:
--
-- > *Data.Tree.View> drawTree $ Node "Add" [Node "Sub" [Node "3" [], Node "Mul" [Node "1" [], Node "2" []]], Node "4" []]
-- > Add
-- >  ├╴Sub
-- >  │  ├╴3
-- >  │  └╴Mul
-- >  │     ├╴1
-- >  │     └╴2
-- >  └╴4
drawTree :: Tree String -> IO ()
drawTree = putStrLn . showTree

enumTree :: Tree a -> Tree (a,Int)
enumTree = flip evalState 0 . traverse count
  where
    count :: a -> State Int (a,Int)
    count a = do
      i <- get; put (i+1)
      return (a,i)

data Behavior
    = Fixed  -- ^ Non-collapsible
    | InitiallyCollapsed
    | InitiallyExpanded

-- | A tree node
data NodeInfo = NodeInfo
    { nodeBehavior :: Behavior
    , nodeName     :: String  -- ^ Node name (to be displayed in the HTML tree view)
    , nodeInfo     :: String  -- ^ Additional information (to be displayed when hovering the mouse over
                              --   the node). This field may contain line breaks.
    }

htmlNode :: (NodeInfo, Int) -> String
htmlNode (n,i) = concat
    [ "<span id=\"node"
    , show i
    , "\" class=\"node "
    , mode
    , "\""
    , onclick
    , "title=\""
    , nodeInfo n
    , "\""
    , ">"
    , nodeName n
    , "</span>"
    ]
  where
    mode = case nodeBehavior n of
      Fixed              -> "fixed"
      InitiallyCollapsed -> "interactive collapsed"
      InitiallyExpanded  -> "interactive expanded"
    onclick = case nodeBehavior n of
      Fixed -> " "
      _     -> " onclick=\"toggle(event)\" "

showTreeHtml' :: Tree (NodeInfo, Int) -> [String]
showTreeHtml' (Node n []) = [htmlNode n]
showTreeHtml' (Node n ns)
    =  [  htmlNode n ++ "<span id=\"children_node" ++ show (snd n)
       ++ "\" class=" ++ display ++ ">"
       ]
    ++ appLast (concat (indentChildren (map showTreeHtml' ns))) "</span>"
  where
    display = case nodeBehavior $ fst n of
      InitiallyCollapsed -> show "hidden"
      _                  -> show "shown"

-- | Convert a 'Tree' to HTML with foldable nodes
htmlTree :: Tree NodeInfo -> String
htmlTree tree = unlines $ lines template1 ++ showTreeHtml' (enumTree tree) ++ lines template2

-- | Convert a 'Tree' to an HTML file with foldable nodes
writeHtmlTree :: FilePath -> Tree NodeInfo -> IO ()
writeHtmlTree file tree = do
    h <- openFile file WriteMode
    hSetEncoding h utf8
    hPutStr h $ htmlTree $ tree
    hClose h

template1 =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
  \<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\
  \\n\
  \<head>\n\
  \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\
  \  <title>Tree view</title>\n\
  \  <style type=\"text/css\">\n\
  \    .node {\n\
  \      font-weight: bold;\n\
  \      cursor:      pointer;\n\
  \    }\n\
  \    .interactive:hover {\n\
  \        background-color: #CCC;\n\
  \    }\n\
  \    .collapsed {\n\
  \      color: grey;\n\
  \    }\n\
  \    .expanded {\n\
  \      color: blue;\n\
  \    }\n\
  \    .fixed {\n\
  \      color: black;\n\
  \    }\n\
  \    .shown {\n\
  \      display: inline;\n\
  \    }\n\
  \    .hidden {\n\
  \      display: none;\n\
  \    }\n\
  \  </style>\n\
  \  <script type=\"text/javascript\">\n\
  \    function toggle(e) {\n\
  \      var node     = e.srcElement == undefined ? e.target : e.srcElement;\n\
  \      var id       = node.getAttribute(\"id\");\n\
  \      var children = document.getElementById(\"children_\" + id),\n\
  \          cstyle   = window.getComputedStyle(children),\n\
  \          cdispay  = cstyle.getPropertyValue(\"display\");\n\
  \      if (cdispay == \"inline\") {\n\
  \        document.getElementById(\"children_\" + id).className = \"hidden\";\n\
  \        document.getElementById(id).className = \"node collapsed\";\n\
  \      } else {\n\
  \        document.getElementById(\"children_\" + id).className = \"shown\";\n\
  \        document.getElementById(id).className = \"node expanded\";\n\
  \      }\n\
  \    }\n\
  \  </script>\n\
  \</head>\n\
  \\n\
  \<body>\n\
  \<pre>\n"

template2 =
  "</pre>\n\
  \</body>\n\
  \\n\
  \</html>\n"

