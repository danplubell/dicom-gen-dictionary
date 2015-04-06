module DICOM.Gen.Dictionary (module DICOM.Gen.Dictionary) where

import qualified DICOM.Dictionary as D
import DICOM.Gen.Utilities
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.ParserCombinators.Parsec hiding( spaces)
import System.IO

main :: IO ()
main = do
         (inf1,inf2, outf)<-fix3Args
         if inf1 == "-" || inf2 == "-" ||outf == "-"
         then error "An input file or output file name was not provided"
         else
             do
                houtf <- openBinaryFile outf WriteMode
                
                de1 <- parseDictionaryFile inf1
                de2 <- parseDictionaryFile inf2
                hPutStr houtf (show $ D.Dictionary $ de1 ++ de2)
                hClose houtf

parseDictionaryFile::FilePath->IO [D.DictionaryElement]
parseDictionaryFile fp  = do
                            hinf <- openBinaryFile fp ReadMode
                            c1 <- hGetContents hinf
                            case  xmlParse' "ParseDicomDictionary" c1 of
                               Left x' -> do hClose hinf
                                             error $  "An error occurred while parsing: " ++ x' ++ "[" ++ fp ++ "]"
                               Right y -> do hClose hinf
                                             let content' = getContent y
                                             let part = unpackval $ head (getPart content')
                                             return $ getElements (getChapter part) (getTable part)  content'
                            
                
getContent::Document Posn ->Content Posn
getContent (Document _ _ e _) = CElem e noPos

getPart ::CFilter Posn
getPart  = tag "title" `o` children `o` tag "book"  

getChapter::String -> String
getChapter part = case part of
                       "PS3.7" -> "chapter_E"
                       "PS3.6" -> "chapter_6" 
                       _  -> error "Unknown part: " ++ part
  
getTable::String -> String
getTable part = case part of
                     "PS3.7"   -> "table_E.1-1"
                     "PS3.6"   -> "table_6-1"
                     _ -> error "Unknown part: " ++ part

-------- Filters ----------------
--book::CFilter Posn
--book = tag "book"

--get the chapters
--chapters::CFilter Posn
--chapters = tag "chapter" `o`  children `o` book

--find the specific chapter
--chapterFilter  :: String ->CFilter Posn
--chapterFilter chapter  = attrval (N "xml:id", AttValue [Left chapter])  `o` chapters

--find the element table
--elementTable::String-> String -> CFilter Posn
--elementTable chapter tablename = attrval( N "xml:id", AttValue [Left tablename]) `o`
--               tag "table" `o` children `o` chapterFilter chapter

--find the element rows from the chapter table
elementRows ::String -> String -> CFilter Posn
elementRows chapter tablename = tag "tr"
                                `o` children
                                `o` tag "tbody"
                                `o` children
                                `o` attrval ( N "xml:id", AttValue [Left tablename])
                                `o` tag "table"
                                `o` children
                                `o` attrval (N "xml:id", AttValue [Left chapter])
                                `o` tag "chapter"
                                `o` children
                                `o` tag "book"
                                --book
                                --chapters
                                --elementTable chapter tablename

--get all the elements
getElements::String -> String -> Content Posn -> [D.DictionaryElement]
getElements chapter tablename c = map buildElement (elementRows chapter tablename c)

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

