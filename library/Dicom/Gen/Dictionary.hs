module DICOM.Gen.Dictionary (module DICOM.Gen.Dictionary) where

import qualified DICOM.Dictionary as D
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.ParserCombinators.Parsec hiding( spaces)
import System.IO

main :: IO ()
main = do
         (inf,outf)<-fix2Args
         if inf == "-" || outf == "-"
         then error "An input file or output file name was not provided"
         else
             do
               hinf <- openBinaryFile inf ReadMode
               houtf <- openBinaryFile outf WriteMode
               c <- hGetContents hinf
               case  xmlParse' "ParseDicomChapter6" c of
                  Left x' -> putStrLn x'
                  Right y ->  do
                                let d = getDictionary $ getContent y
                                hPutStr houtf  (show d)
               hClose hinf
               hClose houtf


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
                     D.DictionaryElement{
                                          D.dicomGroup = fst $ parseTag (head xs)
                                        , D.dicomElement = snd $ parseTag (head xs)
                                        , D.elementName = xs!!1
                                        , D.elementKeyWord = xs!!2
                                        , D.vr = xs!!3
                                        , D.vm = xs!!4
                                        , D.elementVersion = xs!!5
                                       }
--Convert dicom tag in string form (0000,0000) into a tuple
parseTag::String -> (String,String)
parseTag s = case parse tagParser "tagparser" s of
                Left _ -> ("","")
                Right p -> p

--Parses out the values from the tag strings
tagParser :: Parser (String,String)
tagParser = do
  _<-char '('
  group <- many1 anyChar
  _ <-char ','
  element' <- many1 anyChar
  _ <-char ')'
  return  (group,  element')

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

