module Data.Tree.View
    ( showTree
    , drawTree
    , NodeInfo (..)
    , htmlTree
    , writeHtmlTree
    ) where



import Control.Monad.State
import Data.Traversable (traverse)
import Data.Tree (Tree (..))
import qualified Data.Tree as Tree



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

-- | A tree node
data NodeInfo = NodeInfo
    { nodeName :: String  -- ^ Node name (to be displayed in the HTML tree view)
    , nodeInfo :: String  -- ^ Additional information (to be displayed when hovering the mouse over
                          --   the node). This field may contain line breaks.
    }

htmlNode :: (NodeInfo, Int) -> String
htmlNode (n,i)
    =  "<span id=\"node"
    ++ show i
    ++ "\" class=\"node\" onclick=\"toggle(event)\" "
    ++ "title=\""
    ++ nodeInfo n
    ++ "\""
    ++ ">"
    ++ nodeName n
    ++ "</span>"

showTreeHtml' :: Tree (NodeInfo, Int) -> [String]
showTreeHtml' (Node n []) = [htmlNode n]
showTreeHtml' (Node n ns)
    =  [htmlNode n ++ "<span id=\"children_node" ++ show (snd n) ++ "\" class=\"children\">"]
    ++ appLast (concat (indentChildren (map showTreeHtml' ns))) "</span>"

-- | Convert a 'Tree' to HTML with foldable nodes
htmlTree :: Tree NodeInfo -> String
htmlTree tree = unlines $ lines template1 ++ showTreeHtml' (enumTree tree) ++ lines template2

-- | Convert a 'Tree' to an HTML file with foldable nodes
writeHtmlTree :: FilePath -> Tree NodeInfo -> IO ()
writeHtmlTree file = writeFile file . htmlTree

template1 =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\
  \<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\
  \\n\
  \<head>\n\
  \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\
  \  <title>Tree view</title>\n\
  \  <style type=\"text/css\">\n\
  \    body {\n\
  \      font-family: monospace;\n\
  \      font-size:   12px;\n\
  \    }\n\
  \    .node {\n\
  \      color:       blue;\n\
  \      font-weight: bold;\n\
  \      cursor:      pointer;\n\
  \    }\n\
  \    .node:hover {\n\
  \        background-color: #CCC;\n\
  \    }\n\
  \    .children {\n\
  \      display: inline;\n\
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
  \        document.getElementById(\"children_\" + id).style.display = \"none\";\n\
  \        document.getElementById(id).style.color = \"gray\";\n\
  \      } else {\n\
  \        document.getElementById(\"children_\" + id).style.display = \"inline\";\n\
  \        document.getElementById(id).style.color = \"blue\";\n\
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

