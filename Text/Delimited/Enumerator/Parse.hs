{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides both a native Haskell solution for parsing XML
-- documents into a stream of events, and a set of parser combinators for
-- dealing with a stream of events.
--
-- The important thing to know about the combinators is that they do /not/ work
-- on the fully-powered 'Event' datatype; rather, this module defines an
-- 'SEvent' datatype which only deals with tags, attributes and content. For
-- most uses, this is sufficient. If you need to parse doctypes, instructions
-- or contents, you will not be able to use the combinators.
--
-- As a simple example, if you have the following XML file:
--
-- > <?xml version="1.0" encoding="utf-8"?>
-- > <people>
-- >     <person age="25">Michael</person>
-- >     <person age="2">Eliezer</person>
-- > </people>
--
-- Then this code:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Text.XML.Enumerator.Parse
-- > import Data.Text.Lazy (Text, unpack)
-- > 
-- > data Person = Person { age :: Int, name :: Text }
-- >     deriving Show
-- > 
-- > parsePerson = tag' "person" (requireAttr "age") $ \age -> do
-- >     name <- content'
-- >     return $ Person (read $ unpack age) name
-- > 
-- > parsePeople = tag'' "people" $ many parsePerson
-- > 
-- > main = parseFile_ "people.xml" (const Nothing) $ force "people required" parsePeople
--
-- will produce:
--
-- > [Person {age = 25, name = "Michael"},Person {age = 2, name = "Eliezer"}]
module Text.Delimited.Enumerator.Parse
    ( -- * Parsing tabular files
    --   parseBytes
    -- , parseText
    -- , detectUtf
    --   -- * Simplified events
    -- , SEvent (..)
    -- , simplify
    -- , SAttr
    -- , parseFile
    -- , parseFile_
    --   -- * SEvent parsing
    -- , tag
    -- , tag'
    -- , tag''
    -- , content
    -- , content'
    --   -- * Attribute parsing
    -- , AttrParser
    -- , requireAttr
    -- , optionalAttr
    -- , requireAttrRaw
    -- , optionalAttrRaw
    -- , ignoreAttrs
    --   -- * Combinators
    -- , choose
    -- , many
    -- , force
    --   -- * Exceptions
    -- , XmlException (..)
    ) where

import Data.Attoparsec.Text hiding (many)
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text.Enumerator
import Data.XML.Types
import Control.Applicative ((<|>), (<$>))
-- import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Prelude hiding (takeWhile)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Enumerator (Iteratee, Enumeratee, (>>==), Stream (..),
                        checkDone, yield, ($$), joinI, run, throwError)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.Binary as EB
import Control.Monad (unless, ap, liftM, when)
import qualified Data.Text as TS
import Data.List (foldl')
import Control.Applicative (Applicative (..))
import Data.Typeable (Typeable)
import Control.Exception (throwIO, SomeException)
import qualified Control.Exception as Ex
import Data.Enumerator.Binary (enumFile)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)

import Data.ByteString (ByteString)

-- | A record is a series of fields.
-- Each record is located on a separate line, delimited by a line break (CRLF).
type Record = [Field]

-- | A field is a strict ByteString.
type Field = TS.Text

data Token = BeginRecord | RecordField TS.Text | EndRecord deriving (Eq, Show)

data Exception = Exception
    { errorMessage :: String
    , badInput :: Maybe Token
    }
    deriving (Show, Typeable)

instance Ex.Exception Exception

-- -- | Parses a byte stream into 'Event's. This function is implemented fully in
-- -- Haskell using attoparsec-text for parsing. The produced error messages do
-- -- not give line/column information, so you may prefer to stick with the parser
-- -- provided by libxml-enumerator. However, this has the advantage of not
-- -- relying on any C libraries.
-- --
-- -- This relies on 'detectUtf' to determine character encoding, and 'parseText'
-- -- to do the actual parsing.
-- parseBytes :: Monad m => Enumeratee S.ByteString Event m a
-- parseBytes step = joinI $ ET.decode ET.utf8 $$ parseText step

-- | Parses a character stream into 'Event's. This function is implemented
-- fully in Haskell using attoparsec-text for parsing. The produced error
-- messages do not give line/column information, so you may prefer to stick
-- with the parser provided by libxml-enumerator. However, this has the
-- advantage of not relying on any C libraries.
-- parseText :: MonadIO m => Char -> Enumeratee TS.Text Token m a
parseText delim = checkDone $ \k -> k (Chunks [BeginRecord]) >>== loop
    where
        loop = checkDone go
        go k = do
            mtoken <- iterTokens delim
            -- liftIO $ print mtoken
            case mtoken of
                -- []     -> k (Chunks [EndDocument]) >>== return
                [] -> k EOF >>== return
                tokens -> k (Chunks tokens) >>== loop

iterTokens :: Monad m => Char -> Iteratee TS.Text m [Token]
iterTokens delim = iterParser ((endOfInput >> return []) <|> parseTokens delim)

parseTokens :: Char -> Parser [Token]
parseTokens delim = do
    f <- RecordField `fmap` A.takeWhile (\c -> c /= delim && not (isEOL c))
    x <- (char delim >> return [])
            <|> 
         (satisfy isEOL >> return [EndRecord])
            <|>
         (endOfInput >> return [EndRecord])
    return $ [f] ++ x
    where
        isEOL c = c == '\n' || c == '\r'

-- parseToken delim = line
--     where
--         line  = (field `sepBy` sep) `endBy` eol
--         field = A.takeWhile (\c -> c /= delim && not (isEOL c))
--         sep   =             (== delim)
--         eol   = P.skipMany1 isEOL
--         isEOL c = c == '\n' || c == '\r'


-- -- | Grabs the next piece of content if available.
-- content :: Monad m => Iteratee SEvent m (Maybe Text)
-- content = do
--     x <- E.peek
--     case x of
--         Just (SContent t) -> EL.drop 1 >> return (Just t)
--         _ -> return Nothing
-- 
-- -- | Grabs the next piece of content. If none if available, returns 'T.empty'.
-- content' :: Monad m => Iteratee SEvent m Text
-- content' = do
--     x <- content
--     case x of
--         Nothing -> return T.empty
--         Just y -> return y
-- 
-- -- | The most generic way to parse a tag. It takes a predicate for checking if
-- -- this is the correct tag name, an 'AttrParser' for handling attributes, and
-- -- then a parser for dealing with content.
-- --
-- -- This function automatically absorbs its balancing closing tag, and will
-- -- throw an exception if not all of the attributes or child elements are
-- -- consumed. If you want to allow extra attributes, see 'ignoreAttrs'.
-- record :: Monad m
--     => Iteratee TS.Text m a
--     -> Iteratee Token m a
record recordParser = do
    x <- EL.head
    case x of
        Just BeginRecord -> do 
            a <- E.joinI $ embed $$ recordParser
            -- liftIO (print a)
            x' <- EL.head
            case x' of
                Just EndRecord -> return a
                _ -> throwError $ Exception "Expected end of record" x'
        _ -> throwError $ Exception "Unexpected token" x

-- embed :: Monad m => (a -> Maybe b) -> Enumeratee a b m c
-- embed f step | n <= 0 = return step
embed (E.Continue k) = E.continue loop
    where
    	loop (Chunks []) = E.continue loop
    	loop (Chunks xs) =
            -- | len xs <= n = k (Chunks xs) >>== isolate (n - len xs)
            -- | otherwise = let
                -- (s1, s2) = L.genericSplitAt n xs
            let (s1, s2) = span (not . isEndRecord) xs
			in k (Chunks (map recordContent s1)) >>== (\step -> yield step (Chunks (drop 1 s2)))
    	loop EOF = k EOF >>== (\step -> yield step EOF)
    	isEndRecord EndRecord = True
    	isEndRecord _ = False
embed step = return step

recordContent (RecordField t) = t
recordContent _ = TS.empty

-- fields :: Monad m => Iteratee Token m [Field

  -- where
    -- dropWS = do
    --     x <- E.peek
    --     case x of
    --         Just (SContent t)
    --             | T.all isSpace t -> EL.drop 1 >> E.peek
    --         _ -> return x
    -- runAttrParser' p as =
    --     case runAttrParser p as of
    --         Left e -> Left e
    --         Right ([], x) -> Right x
    --         Right (attr, _) -> Left $ UnparsedAttributes attr

-- -- | Get the value of the first parser which returns 'Just'. If none return
-- -- 'Just', returns 'Nothing'.
-- choose :: Monad m
--        => [Iteratee SEvent m (Maybe a)]
--        -> Iteratee SEvent m (Maybe a)
-- choose [] = return Nothing
-- choose (i:is) = do
--     x <- i
--     case x of
--         Nothing -> choose is
--         Just a -> return $ Just a
-- 
-- -- | Force an optional parser into a required parser. All of the 'tag'
-- -- functions, 'choose' and 'many' deal with 'Maybe' parsers. Use this when you
-- -- want to finally force something to happen.
-- force :: Monad m
--       => String -- ^ Error message
--       -> Iteratee SEvent m (Maybe a)
--       -> Iteratee SEvent m a
-- force msg i = do
--     x <- i
--     case x of
--         Nothing -> throwError $ XmlException msg Nothing
--         Just a -> return a
-- 
-- -- | Convert a stream of 'Event's into a stream 'SEvent's. The first argument
-- -- is a function to decode character entity references. Some things to note
-- -- about this function:
-- --
-- -- * It drops events for document begin/end, comments, and instructions.
-- --
-- -- * It concatenates all pieces of content together. The output of this
-- -- function is guaranteed to not have two consecutive 'SContent's.
-- --
-- -- * It automatically checks that tag beginnings and endings are well balanced,
-- -- and throws an exception otherwise.
-- --
-- -- * It also throws an exception if your supplied entity function does not know
-- -- how to deal with a character entity.
-- --
-- -- Please also note that you do /not/ need to handle the 5 XML-defined
-- -- character entity references (lt, gt, amp, quot and apos), nor deal with
-- -- numeric entities (decimal and hex).
-- simplify :: Monad m => (Text -> Maybe Text) -> Enumeratee Event SEvent m b
-- simplify renderEntity =
--     loop []
--   where
--     loop stack = E.checkDone $ go stack
--     sattr (Attribute x y) = do
--         y' <- flip mapM y $ \z ->
--             case z of
--                 ContentText t -> return t
--                 ContentEntity t ->
--                     case renderEntity t of
--                         Just t' -> return t'
--                         Nothing -> throwError $ InvalidEntity t
--         return (x, T.concat y')
--     go stack k = do
--         x <- EL.head
--         case x of
--             Nothing -> k EOF >>== return
--             Just EventBeginDocument -> go stack k
--             Just EventEndDocument ->
--                 k EOF >>== return
--             Just EventInstruction{} -> go stack k
--             Just EventDoctype{} -> go stack k
--             Just (EventBeginElement n as) -> do
--                 as' <- mapM sattr as
--                 k (Chunks [SBeginElement n as']) >>== loop (n : stack)
--             Just (EventEndElement n) ->
--                 case stack of
--                     [] -> throwError $ InvalidEndElement n
--                     n':rest
--                         | n == n' -> k (Chunks [SEndElement]) >>== loop rest
--                         | otherwise -> throwError $ InvalidEndElement n
--             Just (EventContent c) -> do
--                 t <- contentToText c
--                 ts <- takeContents $ (:) t
--                 k (Chunks [SContent $ T.concat $ ts []]) >>== loop stack
--             Just EventComment{} -> go stack k
--       where
--         contentToText (ContentEntity e) =
--             case renderEntity e of
--                 Nothing -> throwError $ InvalidEntity e
--                 Just t -> return t
--         contentToText (ContentText t) = return t
--         takeContents front = do
--             x <- E.peek
--             case x of
--                 Nothing -> return front
--                 Just EventBeginElement{} -> return front
--                 Just EventEndElement{} -> return front
--                 Just (EventContent c) -> do
--                     EL.drop 1
--                     t <- contentToText c
--                     takeContents $ front . (:) t
--                 Just EventBeginDocument -> helper
--                 Just EventEndDocument -> helper
--                 Just EventInstruction{} -> helper
--                 Just EventDoctype{} -> helper
--                 Just EventComment{} -> helper
--           where
--             helper = EL.drop 1 >> takeContents front
-- 
-- -- | The same as 'parseFile', but throws any exceptions.
-- parseFile_ :: String -> (Text -> Maybe Text) -> Iteratee SEvent IO a -> IO a
-- parseFile_ fn re p =
--     parseFile fn re p >>= go
--   where
--     go (Left e) = liftIO $ throwIO e
--     go (Right a) = return a
-- 
-- -- | A helper function which reads a file from disk using 'enumFile', detects
-- -- character encoding using 'detectUtf', parses the XML using 'parseBytes',
-- -- converts to an 'SEvent' stream using 'simplify' and then handing off control
-- -- to your supplied parser.
-- parseFile :: String -> (Text -> Maybe Text) -> Iteratee SEvent IO a -> IO (Either SomeException a)
-- parseFile fn re p =
--     run $ enumFile fn     $$ joinI
--         $ parseBytes      $$ joinI
--         $ simplify re     $$ p
-- 
-- data XmlException = XmlException
--     { xmlErrorMessage :: String
--     , xmlBadInput :: Maybe Event
--     }
--                   | InvalidEndElement Name
--                   | InvalidEntity Text
--                   | SXmlException
--     { xmlErrorMessage :: String
--     , sxmlBadInput :: Maybe SEvent
--     }
--                   | UnparsedAttributes [SAttr]
--     deriving (Show, Typeable)
-- instance Exception XmlException
-- 
-- -- | A monad for parsing attributes. By default, it requires you to deal with
-- -- all attributes present on an element, and will throw an exception if there
-- -- are unhandled attributes. Use the 'requireAttr', 'optionalAttr' et al
-- -- functions for handling an attribute, and 'ignoreAttrs' if you would like to
-- -- skip the rest of the attributes on an element.
-- newtype AttrParser a = AttrParser { runAttrParser :: [SAttr] -> Either XmlException ([SAttr], a) }
-- 
-- instance Monad AttrParser where
--     return a = AttrParser $ \as -> Right (as, a)
--     (AttrParser f) >>= g = AttrParser $ \as ->
--         case f as of
--             Left e -> Left e
--             Right (as', f') -> runAttrParser (g f') as'
-- instance Functor AttrParser where
--     fmap = liftM
-- instance Applicative AttrParser where
--     pure = return
--     (<*>) = ap
-- 
-- optionalAttrRaw :: (SAttr -> Maybe b) -> AttrParser (Maybe b)
-- optionalAttrRaw f =
--     AttrParser $ go id
--   where
--     go front [] = Right (front [], Nothing)
--     go front (a:as) =
--         case f a of
--             Nothing -> go (front . (:) a) as
--             Just b -> Right (front as, Just b)
-- 
-- requireAttrRaw :: String -> (SAttr -> Maybe b) -> AttrParser b
-- requireAttrRaw msg f = do
--     x <- optionalAttrRaw f
--     case x of
--         Just b -> return b
--         Nothing -> AttrParser $ const $ Left $ XmlException msg Nothing
-- 
-- -- | Require that a certain attribute be present and return its value.
-- requireAttr :: Name -> AttrParser Text
-- requireAttr n = requireAttrRaw
--     ("Missing attribute: " ++ show n)
--     (\(x, y) -> if x == n then Just y else Nothing)
-- 
-- -- | Return the value for an attribute if present.
-- optionalAttr :: Name -> AttrParser (Maybe Text)
-- optionalAttr n = optionalAttrRaw
--     (\(x, y) -> if x == n then Just y else Nothing)
-- 
-- -- | Skip the remaining attributes on an element. Since this will clear the
-- -- list of attributes, you must call this /after/ any calls to 'requireAttr',
-- -- 'optionalAttr', etc.
-- ignoreAttrs :: AttrParser ()
-- ignoreAttrs = AttrParser $ \_ -> Right ([], ())
-- 
-- -- | Keep parsing elements as long as the parser returns 'Just'.
-- many :: Monad m => Iteratee SEvent m (Maybe a) -> Iteratee SEvent m [a]
-- many i =
--     go id
--   where
--     go front = do
--         x <- i
--         case x of
--             Nothing -> return $ front []
--             Just y -> go $ front . (:) y
