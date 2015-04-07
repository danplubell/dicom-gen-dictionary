module DICOM.Gen.Dictionary (module DICOM.Gen.Dictionary) where

import qualified DICOM.Dictionary as D
import DICOM.Gen.Utilities
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.ParserCombinators.Parsec hiding( spaces)
import System.IO
--import Control.Exception

main :: IO ()
main = do
  (inf1,inf2, outf)<-fix3Args
  if inf1 == "-" || inf2 == "-" ||outf == "-"
  then error "An input file or output file name was not provided"
  else
    do
      de1 <- withBinaryFile inf1 ReadMode parseDictionaryFile
      de2 <- withBinaryFile inf2 ReadMode parseDictionaryFile
      withBinaryFile outf WriteMode (\handle -> hPutStr handle (show $ D.Dictionary $ de1 ++ de2))
         
{-
      houtf <- openBinaryFile outf WriteMode
      hinf1 <- openBinaryFile inf1 ReadMode
      hinf2 <- openBinaryFile inf2 ReadMode
      de1 <- parseDictionaryFile hinf1
      de2 <- parseDictionaryFile hinf2
      hPutStr houtf (show $ D.Dictionary $ de1 ++ de2)
      hClose houtf
      hClose hinf1
      hClose hinf2
-}
--Parse a DocBook xml file into a list of elements
--Part 6 contains 3 tables that need to be parsed
--Part 7 contains 1 table that nees to be parsed
parseDictionaryFile::Handle->IO [D.DictionaryElement]
parseDictionaryFile h  = do
  c1 <- hGetContents h
  case  xmlParse' "ParseDicomDictionary" c1 of
    Left x' -> error $  "An error occurred while parsing: " ++ x' ++ "[" ++ show h ++ "]"
    Right y -> do 
      let content' = getContent y
      let part = unpackval $ head (getPart content')
      case part of  
        "PS3.6" -> return $ getElements part "chapter_6" "table_6-1" content' --parse the three chapters and concatenate them
                         ++ getElements part "chapter_7" "table_7-1" content'
                         ++ getElements part "chapter 8" "table_8-1" content'
        "PS3.7" -> return $ getElements part "chapter_E" "table_E.1-1" content'
        _       -> error $ "Unknown part: " ++ part 
                            
                
getContent::Document Posn ->Content Posn
getContent (Document _ _ e _) = CElem e noPos

--Filter for getting the part name
getPart ::CFilter Posn
getPart  = tag "title" `o` children `o` tag "book"  

--Creates a filter for the chapters of each part                     
getChapterFilter::String->String->String->CFilter Posn
getChapterFilter part chapter table =
  case part of
    "PS3.7" ->
       tag "tr"
       `o` children
       `o` tag "tbody"
       `o` children
       `o` attrval ( N "xml:id", AttValue [Left table])
       `o` tag "table"
       `o` children
       `o` attrval ( N "xml:id", AttValue [Left "sect_E.1"])
       `o` tag "section"
       `o` children
       `o` attrval (N "xml:id", AttValue [Left chapter])
       `o` tag "chapter"
       `o` children
       `o` tag "book"
    "PS3.6" -> tag "tr"
       `o` children
       `o` tag "tbody"
       `o` children
       `o` attrval ( N "xml:id", AttValue [Left table])
       `o` tag "table"
       `o` children
       `o` attrval (N "xml:id", AttValue [Left chapter])
       `o` tag "chapter"
       `o` children
       `o` tag "book"
    _       ->  error $  "Unknown part: " ++ part            
 
--get all the elements
getElements::String->String -> String -> Content Posn -> [D.DictionaryElement]
getElements part chapter table c = map buildElement (( getChapterFilter part chapter table) c)

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
  group <- many1 alphaNum
  _ <-char ','
  element' <- many1 alphaNum
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

