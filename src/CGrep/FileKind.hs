module CGrep.FileKind (
    FileKind(..)
) where


data FileKind = KindText | KindConfig | KindLanguage | KindData | KindMarkup | KindScript
    deriving (Eq, Ord, Enum, Bounded)


instance Show FileKind where
    show KindText     = "Text"
    show KindConfig   = "Config"
    show KindLanguage = "Language"
    show KindData     = "Data"
    show KindMarkup   = "Markup"
    show KindScript   = "Script"

instance Read FileKind where
    readsPrec _ "Text"     = [(KindText, "")]
    readsPrec _ "Config"   = [(KindConfig, "")]
    readsPrec _ "Language" = [(KindLanguage, "")]
    readsPrec _ "Data"     = [(KindData, "")]
    readsPrec _ "Markup"   = [(KindMarkup, "")]
    readsPrec _ "Script"   = [(KindScript, "")]
    readsPrec _ _          = []
