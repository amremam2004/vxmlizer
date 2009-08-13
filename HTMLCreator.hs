-- Module HTMLCreator is a tool to generate HTML elements in 
-- Text.XML.Light library format
-- 

module HTMLCreator where 

import Proc2
import Text.XML.Light
import Text.XML.Light.Types 

stringToHTML :: String -> Content
stringToHTML str = Text (mkCData CDataRaw str)

mkTextBoxElem :: String -> Element
mkTextBoxElem name = (add_attr (mkAttr ("type","text"))) $ (add_attr (mkAttr ("name",name))) $ mkElem "input" 

mkTextBoxCont :: String -> Content
mkTextBoxCont name= (Elem (mkTextBoxElem name))

mkComboBoxElem :: [String] -> Element
mkComboBoxElem options = add_conts (getOptions options) (mkElem "select")

mkComboBoxCont :: [String] -> Content
mkComboBoxCont options = Elem (mkComboBoxElem options)

mkButtonElem :: String -> Element
mkButtonElem value = (add_attr (mkAttr ("type","button"))) $ (add_attr (mkAttr ("value",value))) $ mkElem "input" 

mkButtonCont :: String -> Content
mkButtonCont value= (Elem (mkButtonElem value))

mkFormElem :: Element
mkFormElem = mkElem "form" 

mkFormCont :: Content
mkFormCont = (Elem (mkElem "form"))

getOptions :: [String] -> [Content]
getOptions [] = []
getOptions (x:xs) =((Elem (add_attr (mkAttr ("value",x)) (add_cont (stringToHTML x) (mkElem "option")))) : getOptions xs)

rows2TableCont :: [Content] -> Content
rows2TableCont xs = Elem (rows2TableElem xs)

rows2TableElem :: [Content] -> Element
rows2TableElem xs = add_attrs (mkAttrs [("border","1"),("align","center")]) 
                              (add_conts (rows2TableElem2 xs) (mkElem "table"))
rows2TableElem2 [] = []
rows2TableElem2 (x:xs) = (Elem (add_cont (Elem (add_cont x (add_attrs (mkAttrs [("align","center")]) $ 
                                                                       mkElem "td"))) 
                                         (mkElem "tr"))
			  ): rows2TableElem2 xs 


conts2TableCont :: [[Content]] -> Content
conts2TableCont xs = Elem (conts2TableElem xs)
			  
conts2TableElem :: [[Content]] -> Element
conts2TableElem xs = add_attrs (mkAttrs [("align","center")]) 
                               (add_conts (getRows xs) (mkElem "table"))

getRows [] = []
getRows (x:xs) = (Elem (add_conts (getCols x) (mkElem "tr"))): getRows xs 			      

getCols [] = []
getCols (x:xs) = (Elem (add_cont x (mkElem "td"))) : getCols xs

mkAudioElem :: String -> Element
mkAudioElem src = add_attrs (mkAttrs [("SRC",src),
                                     ("type","audio/x-pn-realaudio-plugin"),
                                     ("CONTROLS","ControlPanel,StatusBar"),
				     ("HEIGHT","60"),
				     ("WIDTH","275"),
				     ("AUTOSTART","false"),
				     ("style","border: 1px double #000000; ")]) 
                           (mkElem "embed")

mkAudioCont :: String -> Content
mkAudioCont src = Elem (mkAudioElem src)			   
  
