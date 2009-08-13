{--
Module VXMLTree 
written by Hisham Benotman & Abdussalam Alawini
As a part of a project for the class
Open Source Software Development , Summer 2009 , Portland State University
--}


module VXMLTree(
   all2dot   -- :: IO [()]   Converts all vxml files in the current directory to dot files , 
              --              you can use Graphviz software to convert a dot file to an image
  ,file2dot  -- :: String -> IO ()   Gets a vxml file name and converts it to a dot file
) where



import Treedot2
import Converter
import Proc2
import IOActions

import Text.XML.Light
import Text.XML.Light.Types 
import Text.XML.Light.Input
import Text.XML.Light.Proc

-- VXMLTree is used to represent a vxml file , the current tree can represent menus anf forms

data VXMLTree = VXMLFile String [VXMLTree] -- File "file name" (menu , form ...)
              | Fil String             -- Fil "file name"
              | Menu VXMLTree VXMLTree -- Menu Prompt Options 
              | Form VXMLTree VXMLTree VXMLTree -- Form Prompt Fields Filled
	      | Prompt VXMLTree        -- Prompt ActPrompt
              | ActPrompt String       -- ActPrompt "Actual prompt here"
              | Options [VXMLTree]     -- Options  [Option]
              | Option String VXMLTree -- Option "option name" File
              | Fields [VXMLTree]      -- Fields [Field]
              | Field String VXMLTree VXMLTree -- Field "field name" Grammar
              | Grammar [VXMLTree]     -- Grammar [Item]
              | Item String            -- Item "grammar item name"
              | Filled VXMLTree        -- Filled File 
	      | Noting deriving Show   
	 

-- Tree and LabeledTree classes are used by TreeDot module to generate dot files that describe trees
-- dot files can then be converted to image files containing the wanted trees

instance Tree (VXMLTree) where
   subtrees (VXMLFile x c)   = c    -- subtrees function gives the sub trees of each VXMLTree node
   subtrees (Fil x)   = []
   subtrees (Menu x o)   = [x,o]
   subtrees (Form x o f)   = [x,o,f]
   subtrees (Prompt x)   = [x]
   subtrees (ActPrompt x)   = []
   subtrees (Options x)   = x
   subtrees (Option x y)   = [y]
   subtrees (Fields x)   = x
   subtrees (Field x g t)   = [g,t]
   subtrees (Grammar x)   = x
   subtrees (Item x)   = []
   subtrees (Filled x)   = [x]
   subtrees  Noting = []

instance LabeledTree (VXMLTree) where
   label (VXMLFile x c)   = x   -- label function gives the label for each VXMLTree node
   label (Fil x)   = x
   label (Menu x o)   = "Menu"
   label (Form x o f)   = "Form"
   label (Prompt x)   = "Prompt"
   label (ActPrompt x)   = x
   label (Options x)   = "Options"
   label (Option x y)   = x
   label (Fields x)   = "Fields"
   label (Field x g t)   = x
   label (Grammar x)   = "Grammar"
   label (Item x)   = x
   label (Filled x)   = "Filled"
   label (Noting)   = ""


-- Example: VXMLTree 
tree2 = (Prompt (ActPrompt "you went right"))
tree3 = (Prompt (ActPrompt "you went left"))
m1 = Menu (Prompt (ActPrompt "Hello there")) (Options [Option "left" tree3,Option "right" tree2])
tree1 = VXMLFile "hello.vxml" [m1]


-- A function that converts all vxml files in the current directory to dot files
-- You can use the command 
-- !dot -Tpng filename.dot > filename.png		   
-- to convert any dot file to png (image) file

all2dot :: IO [()]
all2dot
  = getCurrentDirectory
    >>= getFiles
    >>= inIO (filter (isSuffixOf "vxml")) >>= mapM file2dot 

-- A function to convert a single vxml file to a dot file
-- You can use the command 
-- !dot -Tpng filename.dot > filename.png		   
-- to convert the dot file to png (image) file

file2dot :: String -> IO ()
file2dot src = do x <- vxml2Tree src
                  toDot x ((getOnlyFileName src) ++ ".dot")

 
-- A function that reads a file and returns the equivalent VXMLTree	

vxml2Tree :: String -> IO VXMLTree    
vxml2Tree src = do x <- readFile src
    	           return (VXMLFile src (convertVxmlToTree $ findVxmlElem $ parseXML x))


convertVxmlToTree :: Element -> [VXMLTree]
convertVxmlToTree (Element {elContent=cs}) = convertMainElements2Tree $ onlyElems cs



{--
This function gets the elements that we have got from the vxml element and convert them to VXMLTree nodes
The function will focus on converting menus and forms
--}
convertMainElements2Tree        :: [Element] -> [VXMLTree] 
convertMainElements2Tree []     = []
convertMainElements2Tree (x:xs) = if getElemName x == "menu" 
                                  then ([convertMenu2Tree x] ++ convertMainElements2Tree xs)
                                  else if getElemName x == "form" 
                                           then ([convertForm2Tree x] ++ convertMainElements2Tree xs)
                                           else [] ++ convertMainElements2Tree xs  


-- Converting a menu to a XMLTree
convertMenu2Tree :: Element -> VXMLTree
convertMenu2Tree elem = Menu (Prompt (ActPrompt (convertPrompts2Str $ findChildren (mkQName "prompt") elem))) 
                             (Options (convertChoices2Options $ findChildren (mkQName "choice") elem))  

--A function to convert all prompts in a menu to a string
convertPrompts2Str        :: [Element] -> String
convertPrompts2Str []     = ""
convertPrompts2Str (x:xs) = (getTagText x ++ convertPrompts2Str xs)


--A function to extract the text content from a single tag
getTagText :: Element -> String 
getTagText prmpt = ignoreNewLine $ strContent prmpt 

-- Convert choices in a menu to a list of option nodes
convertChoices2Options        :: [Element] -> [VXMLTree]
convertChoices2Options []     = []
convertChoices2Options (x:xs) = [Option (getTagText x) 
                                        (Fil (toString $ findAttr (mkQName "next") x))
                                 ] ++ convertChoices2Options xs

-- Convert a form to a Form node
convertForm2Tree :: Element -> VXMLTree
convertForm2Tree elem = (Form 
                           (Prompt (ActPrompt (convertPrompts2Str $ findChildren (mkQName "prompt") elem)))
                           (Fields (convertFields2Tree $ findChildren (mkQName "field") elem)) 
                           (Filled (Fil (getFilledNext elem)))
                        )


convertFields2Tree        :: [Element] -> [VXMLTree]
convertFields2Tree []     = []
convertFields2Tree (x:xs) = [convertField2Tree x] ++ (convertFields2Tree xs)

convertField2Tree :: Element -> VXMLTree 
convertField2Tree field = Field (toString $ findAttr (mkQName "name") field)
                                (Prompt (ActPrompt (convertPrompts2Str $ findChildren (mkQName "prompt") field)))  
                                (Grammar (strings2Items $ getGrammar field))  

strings2Items :: [String] -> [VXMLTree]
strings2Items [] = []
strings2Items (x:xs) = (Item x:strings2Items xs)


-- Extract the next attribute from the goto tag inside a filled tag
getFilledNext :: Element -> String
getFilledNext elem = 
    if (length $ findChildren (mkQName "filled") elem) == 0 
       then ""
       else if (length $ findChildren (mkQName "goto") $ head $ findChildren (mkQName "filled") elem) == 0 
               then ""
               else toString $ findAttr (mkQName "next") $ 
                    head $ findChildren (mkQName "goto") $ 
                    head $ findChildren (mkQName "filled") elem    



