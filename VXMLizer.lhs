> {-# LANGUAGE RecordPuns #-}

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Project Describtion															   		
-------------------									
VXMLizer is a module that consists of three main components
1) Convertor from VXML to HTML
2) Convertor from VXML to Tree
3) VXML Generator
				   	
VXML is A markup language
used for the development of voice applications. 
Using only a traditional Web infrastructure, 
you can create applications that are accessible over the telephone.			   																	   
and then visualize these applications by converting them to a graphical user interface (HTML) or
to graphical trees (dot files and then to png files)

                DEVELOPED BY 
ABDUSSALAM ALWINI    &     HISHAM BENOTMAN
alawini@gmail.com          hrbmail@yahoo.com 
           PORTLAND STATE UNIVERSITY
																   
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> module Vxmlizer (mkQName                  -- mkQName   :: String -> QName
>		     ,mkAttrs               -- mkAttrs   :: [(String,String)] -> [Attr]		
>		     ,mkCData               -- mkCData   :: CDataKind -> String -> CData
> 		     ,add_conts             -- add_conts :: [Content] -> Element -> Element
>                    ,genVXML               -- genVXML   :: Element -> String -> IO ()
>                    ,mkPrompt              -- mkPrompt  :: String -> Element
> 		     ,mkBlock               -- mkBlock   :: Strihg -> Element
> 		     ,mkGoto                -- mkGoto    :: String -> Element
> 		     ,mkValue               -- mkValue   :: String -> Element
> 		     ,mkfill                -- mkfill    :: Content -> Element
>		     ,mkAudio               -- mkAudio   :: String -> Element
> 		     ,mkBreak               -- mkBreak   :: BreakStrength -> Element
> 		     ,mkEmph                -- mkEmph    :: EmphasisLevel -> String -> Element
>	 	     ,mkCatch               -- mkCatch   :: CatchEvents -> String -> Element
> 		     ,mkChoices             -- mkChoices :: [Choices] -> [Content]
> 		     ,mkMenu                -- mkMenu    :: Element -> [Content] ->  Element
> 		     ,mkFields              -- mkFields  :: [Fields] -> [Element]
>	 	     ,mkForm                -- mkForm    :: String -> Element -> [Element] ->  Element
>		     ,mkGrammar             -- mkGrammar :: [Content] -> Element
> 		     ,mkRules               -- mkRules   :: [Rules] -> [Content]
>                    ,m_convertAll          -- m_convertAll :: IO [()] 
>                    ,m_parseAndConvertVxml -- m_parseAndConvertVxml :: String -> String -> IO ()
>                    ,m_all2dot             -- m_all2dot :: IO [()]
>                    ,m_file2dot            -- m_file2dot :: String -> IO () 
> ) where 

> import IOActions
> import Proc2

> import Text.XML.Light
> import Text.XML.Light.Types 
> import Text.XML.Light.Output
> import Text.XML.Light.Input

> import Converter -- VXML to HTML convertor
> import VXMLTree  -- VXML to Tree convertor


==============================================================================
============= (1) Converotr from VXML to HTML =================================
==============================================================================

Convert all vxml files in the current directory to HTML

> m_convertAll :: IO [()]
> m_convertAll = convertAll 

converts one vxml file to HTML , gets the source and target names

> m_parseAndConvertVxml :: String -> String -> IO ()
> m_parseAndConvertVxml src dst = parseAndConvertVxml src dst




==============================================================================
============= (2) Converotr from VXML to tree ================================
==============================================================================

these functions convert vxml files to 'dot' files which can be converted to png (image) files that contain the graphical trees


Converts all vxml files in the current directory to dot files , 
you can use Graphviz software to convert a dot file to an image
Use the command 
!dot -Tpng filename.dot > filename.png		   
to convert any dot file to png (image) file in linux terminal

> m_all2dot :: IO [()] 
> m_all2dot = all2dot  
                     
Gets a vxml file name and converts it to a dot file
you can use Graphviz software to convert a dot file to an image
Use the command 
!dot -Tpng filename.dot > filename.png		   
to convert any dot file to png (image) file in linux terminal

> m_file2dot :: String -> IO () 		     
> m_file2dot src = file2dot src 




==============================================================================
============================ (3) VXML Generator ==============================
==============================================================================
VXML Generator is a collection of Haskell functions that 
generate ".vxml" files. 
all of the following functions are for VXML Generation
after the definition of these functions you will find two examples of using VXML generation functions


Choices data type is used by <menu> element to determine 
the user choices in that specific menu, it has the choice
that the user will have to say, which is mentioned in the menu prompt, 
and the link to the page that the user will be navigated to when he/she speakes the choice name.

> data Choices = Choice {
>	                   chName :: String	
>       	          ,chLink :: String
>                } deriving Show

Fields data type is used by <form> element to specify user input. 
Voice forms are similar to web form, except they can accept voice as proper data entry. 
Each field will have: name, prompt to identify what user should say,
filled to identify what the system response will be when the user fill this field, 
and finally the grammar.

> data Fields = Field {
>	                  fName  :: String
>       	         ,fPrmt  :: String
>			 ,filled :: Element
>			 ,grammar:: Element
>                     } deriving Show

Rules data type is used by <grammar> element, 
that is used to help the Voice Recognition system to identify what the user can say. 
Grammar is an important element in VXML, since no field can accept data without it.

> data Rules = Rule {
>	                  item     :: String
>       	         ,varName  :: String
>			 ,varVal   :: String
>                     } deriving Show 

CatchEvent data type is used by <catch> element, 
which is very similar to exception handling used in other languages.
The event that VXML handles are: NoInput when the user don't speak anything,
NoMatch when the user input is not specified in the grammar, 
and help when the user says "help".

> data CatchEvents = NoInput | NoMatch | Help

BreakStrength data type is used by <break> element, 
which is used to make a delay in system prompts, 
to identify the break strength (durration).

> data BreakStrength = STrong | Weak | Medium | None

EmphasisLevel is used by <emphasis> element, 
which is used to emphasis a word or a sentese, 
to identify the strength of the emphasised phrase.

> data EmphasisLevel = Strong | Reduced

> xmlHeader  = "<?xml version=\"1.0\"?>\n" 
> vxmlHeader = let hdElem = mkElem "vxml" 
>		   hdAttrs  = mkAttrs [("version","2.0")] 
>  	        in add_attrs hdAttrs hdElem

> space=" "

> empAttr = (empLst,empLst)

mkQname is a function that creates the qualified name data type, 
which is imported from "Text.XML.Light.Types", 
and used by all elements and attributes.


 mkConts [] = empLst
 mkConts ((contType,cont):conts) = [contType cont] ++ mkConts conts

This is the final functions that gothers all the elements 
created as one parenet element and uses ppElement from "Text.XML.Light.Output"
to convert the parent element along with all its content to string,
this string then written to a file using writeFile from "IOActions" Library

> genVXML 		   :: Element -> String -> IO ()
> genVXML filecnt filename  = return (xmlHeader++(ppElement (add_cont (Elem filecnt) vxmlHeader))) >>= writeFile (filename ++ ".vxml")

Create prompts that the system will speak to the user to identify user options or that he/she has.

> mkPrompt      :: String -> Element
> mkPrompt []   = blank_element 
> mkPrompt prmt = let prmtElem = mkElem "prompt" 
> 		      prmtConts= [Text (mkCData CDataText prmt)]
>		  in add_conts prmtConts prmtElem

Block is an element used by the form elemenet to contain the prompt, 
since syntcically prompts can not be a child of a form element.

> mkBlock = mkElem "block" 

goto element is used to navigate internal or external of vxml file.

> mkGoto      :: String -> Element
> mkGoto dest = let gotoElem = mkElem "goto" 
>		    gotoAttrs= mkAttrs [("next",dest)] 
>  	        in add_attrs gotoAttrs gotoElem

value is an element used to get the value of the specified experision

> mkValue exprVal =let valElem = mkElem "value" 
>		       valAttrs= mkAttrs [("expr",exprVal)] 
>  	           in add_attrs valAttrs valElem

When the user fill a form field, by voice entry, 
then fill element should contain the appropriate actions 
(elements) as a response to user entry.
This function faciliate the creation of fill element.

> mkfill      :: Content -> Element
> mkfill conts = add_cont conts (mkElem "filled")
		   
audio element is used to play a saved .wav file, 
like hold or transfer music or real human recorded sound.

> mkAudio      :: String -> Element
> mkAudio source = let audioElem = mkElem "audio" 
>		       audioAttrs= mkAttrs [("src",source)] 
>		   in add_attrs audioAttrs audioElem
 
break is an element used to make a delay for a durration specified by BreakStrength.

> mkBreak       :: BreakStrength -> Element
> mkBreak None = mkElem "break" 
> mkBreak value=case value of
>			 STrong -> mkElemBreak "strong"
>			 Weak ->   mkElemBreak "weak"
>			 Medium -> mkElemBreak "medium"
> 		   where mkElemBreak breakVal   = let breakElem = mkElem "break" 
>	 				  	      breakAttrs = mkAttrs [("strength",breakVal)]
>						  in add_attrs breakAttrs breakElem

emphasis is an element used to emphasis a phrase of words or sentese.

> mkEmph           :: EmphasisLevel -> String -> Element
> mkEmph value txt =case value of
>					 Strong ->  mkElemEmph "strong"
>	 				 Reduced -> mkElemEmph "reduced"
> 			 	    where  mkElemEmph emphVal = let emphElem = mkElem "emphasis" 
>								    emphAttrs= mkAttrs [("level",emphVal)]
>								    emphConts= [Text (mkCData CDataText txt)] 
>								in add_conts emphConts (add_attrs emphAttrs emphElem) 												

catch is an event handler element that handles event specified by CatchEvent (help, noinput, nomatch).

> mkCatch     	     :: CatchEvents -> String -> Element
> mkCatch event prmt = case event of
>			 Help ->   mkElemCtch "help"
>			 NoInput-> mkElemCtch "noinput"
>			 NoMatch-> mkElemCtch "nomatch"
>        where  mkElemCtch evenVal = let ctchElem = mkElem "catch" 
>					 ctchAttrs= mkAttrs [("event",evenVal)]
>					 ctchConts= [Elem (mkPrompt prmt)] 
>				     in add_conts ctchConts (add_attrs ctchAttrs ctchElem)

choice is a child element to menu element, 
used to specify the user chioces and links associated to it.

> mkChoices				   ::[Choices] -> [Content]
> mkChoices  []				   = empLst
> mkChoices ((Choice {chName, chLink}):t) = let choElem = mkElem "choice" 
>						choAttrs= mkAttrs [("next",chLink) ]
>						choConts= [Text (mkCData CDataText chName)]
>					    in [Elem (add_conts choConts (add_attrs choAttrs choElem))] ++ mkChoices t

menu element is used to present a user with multiple choices that he can navigate throug.

> mkMenu	      :: Element -> [Content] ->  Element
> mkMenu prmt choices =  let mnuElem = add_conts choices (mkElem "menu")
>			     mnuConts = [Elem prmt]
>			 in add_conts mnuConts mnuElem
>			     

Field is a child element to form, and used to accept user spoken data entry.

> mkFields				   ::[Fields] -> [Element]
> mkFields  []				   = empLst
> mkFields ((Field { fName,fPrmt,filled,grammar}):flds) = let fldAttrs= mkAttrs [("name",fName)]
>					       	    	      fldElem = add_attrs fldAttrs (mkElem "field")
> 					                      fldConts= [Elem grammar,Elem (mkPrompt fPrmt),Elem filled] 
> 							  in [add_conts fldConts fldElem] ++ mkFields flds

form is an element that is used as webforms to accept user entries.

> mkForm	       :: String -> Element -> [Element] ->  Element
> mkForm id prmt fields= let frmElem = add_conts (map Elem fields) (mkElem  "form")
>			     frmAttrs= mkAttrs [("id",id)]
>			     frmConts= [Elem  (add_conts [Elem  prmt] mkBlock)]
>		         in add_conts frmConts (add_attrs frmAttrs frmElem)

grammar is a key element to all spoken data entry elements,
where it specifies what the user is expected to say so that the voice recoginition system 
can handle use input easily.

> mkGrammar  :: [Content] -> Element
> mkGrammar rules = let grammElem = add_conts rules (mkElem "grammar")
>			grammAttrs= mkAttrs [("xml:lang","en-US"),("root","TOPLEVEL"), ("mode","voice")]			
>		    in  add_attrs grammAttrs grammElem

rule is a child element of grammar, each rule specify a special grammar to a specific field. 
Each rule is a parent to another element called <oneof> 
which identify single entry that the user may say to fill this field and match this rule.
CData is used to present the response of each <oneof> element.

> mkRules  :: [Rules] -> [Content]
> mkRules rules = let rleAttr   = mkAttrs [("id","TOPLEVEL"), ("scope","public")]
>		      rleElem   = add_attrs rleAttr (mkElem "rule")
>		      rleOneof  = mkElem "one-of"
>		      itemCont  = map Elem (mkItems rules)
>	          in [Elem (add_conts [Elem (add_conts itemCont rleOneof)] rleElem)]
>    where  mkItems [] = empLst
>           mkItems ((Rule { item,varName, varVal}):rls) = let itemConts = [Text (mkCData CDataText item)]
>	 						       tagConts  = [Text (mkCData CDataVerbatim (varName++ space ++ "\"" ++ varVal++ "\""))]
>							       tagElem   =add_conts  tagConts (mkElem "tag") 
>							       itemElem  = add_conts (itemConts++[Elem tagElem]) (mkElem "item")
>  	 			 		           in [itemElem]++mkItems rls

Test 1
------
Here we generate a VXML file with a menu that has two choices.

Here's how it works.

----------------------
System:
Welcome to the first Haskell generated V X M L file. 
Please say test to test a generated file, or say real to see a real application.

User: 
test => system will navigates the user to test2
real => system will navigates the user to a previously developed game using vxml.

End of Program
---------------------



The Output VXML file:



======================================================================================================

> prmt = "Welcome to the first Haskell generated V X M L file. Please say test to test a generated file. or say real to see a real application"
> myChoices = [Choice { chName="test", chLink ="http://web.pdx.edu/~alawini/test.vxml"},Choice { chName="real", chLink ="http://web.pdx.edu/~alawini/hmsreal.vxml"}]		
> myVXML1 = let mnuAudio  = mkAudio "http://web.pdx.edu/~alawini/mill/intro.wav"
>		mnuPrmt   = add_conts [Elem mnuAudio] (mkPrompt prmt)
>		chs       = mkChoices myChoices
>	        rls1      = mkRules myRules 
>	        mnuGrammar= mkGrammar rls1
>	     in mkMenu mnuPrmt chs


====================================================================================================



Test 2
------

Here we create a vxml file that has a form with single field. Here's how it works

-----------------------------------
System:
This is a test to the vxml file.
Please say Testing. If the test passed successfully, then you will hear test passed

User:
test ,testing, test 1 2 all are accepted entries.

The Output VXML file:



======================================================================================================


> myRules   = [Rule {item="testing",varName= "test", varVal="passed"},Rule {item="testing one two",varName= "test", varVal="passed"},Rule {item="test",varName= "test", varVal="passed"}]
> myVXML2 = let myFields  = [ Field {fName="test",fPrmt="Please say Testing. If the test passed successfully, then you will hear test passed",filled = fill, grammar= grammar}]
>	        audio     = mkAudio "http://web.pdx.edu/~alawini/mill/intro.wav"
>	        frmPrmt   = add_conts [Elem audio] (mkPrompt "This is a test to the vxml file.")
>	        nomtch    = mkCatch NoMatch "Sorry, I did not get that"
>	        noinpt    = mkCatch NoInput "I am sorry. but I did not hear anything. Please try again."
>	        helpct    = mkCatch Help "Please choose one of the options listed"
>	        fill      = mkfill (Elem (mkValue "test$.interpretation")) 
>	        flds      = mkFields myFields 
>	        rls       = mkRules myRules
>	        grammar   = mkGrammar rls
>	     in add_conts [Elem nomtch,Elem noinpt, Elem helpct] (mkForm "MainMenu"  frmPrmt flds) 


====================================================================================================


