

Module Converter
written by Hisham Benotman & Abdussalam Alawini
As a part of a project for the class
Open Source Software Development , Summer 2009 , Portland State University


> module Converter(
>     convertAll -- :: IO [()]	 Convert all vxml files in the current directory
>    ,parseAndConvertVxml  -- :: String -> String -> IO ()  converts one vxml file , gets the source and target names
>     ) where

> import IOActions
> import Proc2
> import HTMLCreator

> import Text.XML.Light
> import Text.XML.Light.Types 
> import Text.XML.Light.Output
> import Text.XML.Light.Input

> htmlHeader = "<html>\n"
> htmltail = "\n</html>"

> genHTML 		   :: Element -> String -> IO ()
> genHTML filecnt filename  = return (htmlHeader++(ppElement filecnt)++htmltail) >>= writeFile (filename ++ ".html")

For understanding purposes , a function that reads a vxml file and shows
the corresponding data structure which is a list of Content
@param 
  rr: vxml file name
  
> parseAndPrint rr = do x <- readFile rr
>    	                print (parseXML x)


For understanding purposes , a function that reads a vxml file and shows
only the vxml element
@param 
  rr: vxml file name
  
> parseAndPrintVxml rr = do x <- readFile rr
>    	                    print $ findVxmlElem $ parseXML x 

====================================================
The main function in this program
This function reads a vxml file and convert it to HTML file
@param 
  src: Source file, vxml file name
  dst: Destination file , HTML file

> parseAndConvertVxml :: String -> String -> IO ()  
> parseAndConvertVxml src dst = do x <- readFile src
>    	                           genHTML (convertVxmlToHTML $ findVxmlElem $ parseXML x) dst 


convertAll converts all vxml files in the current direcotory
                                    
> convertAll :: IO [()]				    
> convertAll
>  = getCurrentDirectory
>    >>= getFiles
>    >>= inIO (filter (isSuffixOf "vxml")) >>= mapM parseAndConvertMultipleVxml

> parseAndConvertMultipleVxml src = do x <- readFile src
>    	                               genHTML (convertVxmlToHTML $ findVxmlElem $ parseXML x) (getOnlyFileName src)

====================================================

A function that gets vxml element and convert all its Contents to HTML Element

> convertVxmlToHTML :: Element -> Element
> convertVxmlToHTML (Element {elContent=cs}) = wrapInHTML $ convertMainElements $ onlyElems cs

This function gets the elements that we have got from the vxml element and convert them to HTML Contents
The function will focus on converting 
  menus  --> Done
  Forms  --> Not started

> convertMainElements        :: [Element] -> [Content] 
> convertMainElements []     = []
> convertMainElements (x:xs) = if getElemName x == "menu" 
>                                  then ((convertMenu x) ++ convertMainElements xs)
>                                  else if getElemName x == "form" 
>                                           then ((convertForm x) ++ convertMainElements xs)
>                                           else [] ++ convertMainElements xs  


Creates an HTML element from the given list of Content
 
> wrapInHTML :: [Content] -> Element
> wrapInHTML elements = elementToElement (wrapInBody elements) "html"

Creates a body element from the given list of Content

> wrapInBody :: [Content] -> Element
> wrapInBody contents = contentsToElement contents "body" 

This function converts a menu element to an HTML menu
a prompt is converted to text title , choices are converted to links

> convertMenu :: Element -> [Content]
> convertMenu elem = [rows2TableCont $ (convertPrompts $ findChildren (mkQName "prompt") elem) 
>                                     ++ (convertChoices $ findChildren (mkQName "choice") elem)]  

A function to convert all prompts in a menu to a list of Text Contents

> convertPrompts        :: [Element] -> [Content]
> convertPrompts []     = []
> convertPrompts (x:xs) = (convertPrompt x) ++ (convertPrompts xs)

A function to convert a single prompt to a text Content

> convertPrompt :: Element -> [Content] 
> convertPrompt prmpt = (getAudioContent prmpt) ++ [stringToHTML $ strContent prmpt] 

> getAudioContent :: Element -> [Content]
> getAudioContent prmpt = if (length $ findChildren (mkQName "audio") prmpt) == 0
>                             then []
>			      else [newLineContent] ++
>			           [mkAudioCont 
>			                 (toString $ findAttr (mkQName "src") $ head $ findChildren (mkQName "audio") prmpt)
>			  	    ]

----------------------------------------------------------------------
A function to convert all choices in a menu to a list of Text links

> newLineContent = (Text (mkCData CDataRaw "</br>"))

> convertChoices        :: [Element] -> [Content]
> convertChoices []     = []
> convertChoices (x:xs) = [convertChoice x] ++ convertChoices xs

A function to convert a single choice to a button tag (link) content

> convertChoice :: Element -> Content 
> convertChoice choice = Elem (add_cont 
>                                (Elem $ add_attr 
>                                           (mkAttr ("onclick","window.location.href=\"" ++ 
>					                       (convertLink2Html $ findAttr (mkQName "next") choice) ++ 
>						               "\""))  
>  				            (mkButtonElem (strContent choice)))  
>				 mkFormElem 
>			        )

 convertChoice choice = Elem (add_cont ( Elem $ add_attr (mkAttr ("onclick","window.location.href='" ++ (convertLink2Html $ findAttr (mkQName "next") choice) ++ "'"))  (mkButtonElem (strContent choice)))  mkFormElem )

> convertLink2Html :: Maybe String -> String
> convertLink2Html link = attachHTMLExt $ getOnlyFileName $ toString link

==================================================================================================================
If you want to convert a choice to a link instead of a button use this function

> convertChoice2Link :: Element -> Content 
> convertChoice2Link choice = setHref (getAElement choice) (findAttr (mkQName "next") choice)

Function that gets a choice Element and returns 'a' (link) Element

> getAElement :: Element -> Element 
> getAElement choice = add_cont (Text (mkCData CDataText (strContent choice))) (mkElem "a")

gets A Element and and the converted choice link and sets 
the a element href attribute so the a element will link to the converted html file

> setHref :: Element -> Maybe String -> Content
> setHref elem attr = 
>     if attr == Nothing 
>        then (Elem elem)
>	 else (Elem (add_attr (mkAttr ("href",attachHTMLExt $ getOnlyFileName $ toString attr)) elem))

==================================================================================================================


> attachHTMLExt :: String -> String
> attachHTMLExt [] = []
> attachHTMLExt name = name ++ ".html"

> convertForm :: Element -> [Content]
> convertForm elem = [ rows2TableCont $
>                      (convertPrompts $ findChildren (mkQName "prompt") elem)
>                      ++ [Elem (add_cont (conts2TableCont $ convertFields $ findChildren (mkQName "field") elem) (mkElem "form"))]
>                      ++ getSubmitButton elem ] 

> getSubmitButton elem = 
>    if (length $ findChildren (mkQName "filled") elem) == 0 
>       then []
>       else if (length $ findChildren (mkQName "goto") $ head $ findChildren (mkQName "filled") elem) == 0 
>	        then []
>               else    [(Elem (add_cont 
>                                (Elem $ add_attrs 
>                                           (mkAttrs [("onclick","window.location.href=\"" ++ 
>					                       (convertLink2Html $ findAttr (mkQName "next") $ 
>                                                                                            head $ findChildren (mkQName "goto") $ 
>                                                                                            head $ findChildren (mkQName "filled") elem) 
>						                ++ "\"")
>                                                    ,("align","center")])  
>  				            (mkButtonElem "submit"))  
>				 mkFormElem 
>			        ))]


> convertFields        :: [Element] -> [[Content]]
> convertFields []     = []
> convertFields (x:xs) = convertField x ++ (convertFields xs)


A function to convert a single prompt to a text Content

> convertField :: Element -> [[Content]] 
> convertField field =[convertPrompts $ findChildren (mkQName "prompt") field
>                       ,[stringToHTML $ toString $ findAttr (mkQName "name") field] ++ [getDataField $ getGrammar field]]


> getDataField :: [String] -> Content
> getDataField [""] = mkTextBoxCont ""
> getDataField xs = mkComboBoxCont xs






