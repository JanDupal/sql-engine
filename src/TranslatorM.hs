module TranslatorM where

type Translator = Either TranslationError

data TranslationError =
    UnknownIdentifier (Maybe String) String
  | UnknownTable String
  | AmbiguousIdentifier (Maybe String) String
  deriving (Eq, Show)
