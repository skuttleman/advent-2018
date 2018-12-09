module Utils.Regex ((#=)) where
import Text.Regex.TDFA


(#=) :: String -> String -> [String]
(#=) s1 s2 = matches (s1 =~ s2)


matches :: ( String, String, String, [String] ) -> [String]
matches ( _, _, _, matches ) = matches
