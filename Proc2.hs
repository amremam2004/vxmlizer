module Proc2 where

import Text.XML.Light
import Text.XML.Light.Types 

empLst = []

--A function that extract the file name from a file path
--for example , "menu1.vxml" results in "menu1"
  --            "c:\dir1\menu1.vxml" results in "menu1" 

getOnlyFileName :: String -> String
getOnlyFileName link = reverse $ getOnlyFileName2 (break (=='.') $ reverse link) 
getOnlyFileName2 (x,"") = getOnlyFileName3 $ break (`elem` "/\\") x
getOnlyFileName2 (x,y) = getOnlyFileName3 $ break (`elem` "/\\") $ tail y
getOnlyFileName3 (x,"") = x
getOnlyFileName3 (x,y) = x

--This function gets a list of Content which represents the vxml file Contents
--and only returns the vxml element "vxml tag and what is inside it"
findVxmlElem :: [Content] -> Element
findVxmlElem [] = blank_element
findVxmlElem (Elem x:xs) = if doesElementEqual x "vxml" 
                                then x 
    			        else findVxmlElem xs 
findVxmlElem (_:xs) = findVxmlElem xs


getGrammar :: Element -> [String]
getGrammar field = if (length $ findChildren (mkQName "grammar") field) == 0
                        then [""]
			else getItems $ head $ findChildren (mkQName "grammar") field
                     
getItems grammar = if (length $ findChildren (mkQName "rule") grammar) == 0
                        then [""]
                        else map (ignoreDoubleSpace.ignoreNewLine.strContent) $ findChildren (mkQName "item") $ 
                             head $ findChildren (mkQName "one-of") $ 
	                     getWantedRule (getRootName grammar) $ 
		             findChildren (mkQName "rule") grammar

getRootName grammar = toString $ findAttr (mkQName "root") grammar

getWantedRule :: String -> [Element] -> Element
getWantedRule _ [] = blank_element
getWantedRule ruleName (x:xs) = if (toString $ findAttr (mkQName "id") x) == ruleName
                                      then x
				      else getWantedRule ruleName xs


toString :: Maybe String -> String
toString Nothing = ""
toString (Just x) = x


--This function is used to add content(s) (Element,Text,..) to an excisting element.
--The idea of the function is the same as add_attrs in the import "Text.XML.Light" library.

add_cont 	:: Content -> Element -> Element
add_cont cs e = add_conts [cs] e

add_conts 	 :: [Content] -> Element -> Element
add_conts cs e = e { elContent =cs ++ elContent e}


mkContent       :: String -> Content
mkContent eName = (Elem (mkElem eName))

--mkElem is a function that creates an XML element, 
--and since VXML is based on XML, so the same data type is used here by "Text.XML.Light.Types" library.

mkElem       :: String -> Element
mkElem eName = Element { 
			   elName = mkQName eName
 		         , elAttribs  = empLst
		 	 , elContent = empLst
			 , elLine    = Nothing
	 		}	

mkAttr    			      :: (String,String) -> Attr
mkAttr strings = head $ mkAttrs [strings]

--mkAttrs is a function that creates attributes 
--that are used by the element to specify the special attributes that each element has. 

mkAttrs    			      :: [(String,String)] -> [Attr]		
mkAttrs [] 			      = empLst
mkAttrs ((attr_Key,attr_Val):attrs) = [Attr {attrKey=(mkQName attr_Key),attrVal=attr_Val}] ++ mkAttrs attrs


mkQName       :: String -> QName
mkQName qName = QName {
    		        qName    = qName
	               ,qURI    = Nothing
	               ,qPrefix = Nothing
		                   } 
				   
-- A function that compares a string with a QName
doesQnameEqual :: QName -> String -> Bool 
doesQnameEqual (QName {qName=x}) name = x == name

-- A function that compares a string with an Element name
doesElementEqual :: Element -> String -> Bool
doesElementEqual (Element {elName =x}) name = doesQnameEqual x name

elementsToContents :: [Element] -> [Content]
elementsToContents [] = []
elementsToContents (x:xs) = ((Elem x):elementsToContents xs)

contentsToElement :: [Content] -> String -> Element
contentsToElement contents elementName = Element {
                                                   elName = mkQName elementName
						  ,elAttribs =[]
						  ,elContent= contents
						  ,elLine = Nothing}
						  
elementToElement :: Element -> String -> Element
elementToElement element elementName = 
                                Element {
                                          elName = mkQName elementName
   				         ,elAttribs =[]
					 ,elContent= [Elem element]
					 ,elLine = Nothing}
					 
-- CData is an imported data type form light xml types. 
-- Which are the text content of any xml file. 
-- This function simplifies the creation of this type to the user.

mkCData  	       :: CDataKind -> String -> CData
mkCData cdKind cdata =  CData {
             		 	  cdVerbatim = cdKind
	                	 ,cdData     = cdata
	                         ,cdLine     = Nothing
       	                } 

getElemName :: Element -> String
getElemName (Element {elName = QName {qName =x}}) = x

ignoreNewLine :: String -> String
ignoreNewLine [] = []
ignoreNewLine (x:xs) = if x == '\n'
                        then ignoreNewLine xs
                        else (x:ignoreNewLine xs) 

ignoreDoubleSpace :: String -> String
ignoreDoubleSpace [] = []
ignoreDoubleSpace (x:(y:ys)) = if (x == ' ' && y == ' ') 
                                  then ignoreDoubleSpace (y:ys)
                                  else (x:(ignoreDoubleSpace (y:ys)))
ignoreDoubleSpace (x:xs) = (x:ignoreDoubleSpace xs)


					 

