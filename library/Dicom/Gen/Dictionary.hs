module DICOM.Gen.Dictionary (module DICOM.Gen.Dictionary) where

import qualified DICOM.Dictionary as D
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.ParserCombinators.Parsec hiding( spaces)
import Data.Word

import System.IO

outf::String
outf = "dicom.dic"

inf::String
inf = "part06.xml"

-- TODO Add logic to write out the dictionary elements
main :: IO ()
main = do
         f <- openBinaryFile "part06.xml" ReadMode
         c <- hGetContents f
         case  xmlParse' "name" c of
             Left x' -> putStrLn x'
             Right y ->  do
                          let ch =  (take 10 $ getElements $ getContent y)
                          print ch

parseDict::FilePath -> String -> IO (Either String D.Dictionary)
parseDict fp n = do
  f <- openBinaryFile fp ReadMode
  c <- hGetContents f 
  case xmlParse' n c of
      Left x' -> return (Left x')
      Right y -> return (Right $ getDictionary $ getContent y )  


getDictionary::Content Posn -> D.Dictionary
getDictionary d = D.Dictionary $  getElements d

--converts the document from the file into a content tree
getContent::Document Posn ->Content Posn
getContent (Document _ _ e _) = CElem e noPos

-------- Filters ----------------
book::CFilter Posn
book = tag "book"

--get the chapters
chapters::CFilter Posn
chapters = tag "chapter" `o`  children `o` book

--find chapter6
chapter6 :: CFilter Posn
chapter6 = attrval (N "xml:id", AttValue [Left "chapter_6"])  `o` chapters

--find the element table
elementTable::CFilter Posn
elementTable = attrval(N "xml:id", AttValue [Left "table_6-1"]) `o`
               tag "table" `o` children `o` chapter6

--find the element rows from the chapter 6 table
elementRows ::CFilter Posn
elementRows = tag "tr" `o` children `o` tag "tbody"`o` children  `o` elementTable

--get all the elements
getElements::Content Posn -> [D.DictionaryElement]
getElements c = map buildElement (elementRows c) 

--The children of the td element contain the values of the dictionary elements
tdFilter::CFilter Posn
tdFilter = tag "para" `o`children `o` tag "td" `o` children 

--Filter for finding the elements that are retired.  Retired elements are in italics
--The italics are attributes of the "emphasis" tag
emphasisFilter::CFilter Posn
emphasisFilter = tag "emphasis" `o` children 

--Builds the dictionary element
--The values are attributes of the "para" tag, unless the element is retired
--Then the values are attributes of the "emphasis" tag
buildElement::Content Posn -> D.DictionaryElement
buildElement c = buildDictElement $ map parseElement (tdFilter c) 
  where parseElement x' = let e = emphasisFilter x'
                          in if null e then unpackval x' else unpackval $ head e

--Parses out the individual element values
buildDictElement::[String] -> D.DictionaryElement
buildDictElement xs  =
                     D.DictionaryElement (parseTag (head xs)) (xs!!1) (xs!!2) (xs!!3)
                         (xs!!4) (xs!!5)  

--Convert dicom tag in string form (0000,0000) into a tuple
parseTag::String -> (Word16,Word16)
parseTag s = case parse tagParser "tagparser" s of
                Left _ -> (0,0)
                Right p -> p

--Parses out the values from the tag strings
tagParser :: Parser (Word16,Word16)
tagParser = do
  _<-char '('
  group <- many1 digit
  _ <-char ','
  element' <- many1 digit
  _ <-char ')'
  return  (read  group::Word16,  read element'::Word16)

--unpack the content that contains the value
unpackval::Content Posn -> String
unpackval (CElem (Elem _ _ (CString _ x' _ :_) ) _) = x'
unpackval (CString _ x'  _)                         = x'
unpackval (CRef _ _ )                               = ""
unpackval (CMisc _ _ )                              = ""
unpackval (CElem (Elem _ _ (CElem _ _  : _ ) ) _)   = ""
unpackval (CElem ( Elem _ _ []) _)                  = ""
unpackval (CElem (Elem _ _ (CRef _ _ : _)) _)       = ""
unpackval (CElem (Elem _ _ (CMisc _ _ : _)) _)      = ""
