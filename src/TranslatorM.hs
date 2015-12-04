module TranslatorM where

type Translator = Either TranslationError

data TranslationError =
    UnknownIdentifier (Maybe String) String
  | UnknownTable String
  | AmbigiousIdentifier (Maybe String) String
  deriving (Eq, Show)
