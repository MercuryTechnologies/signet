module Signet.Unstable.Extra.Maybe where

note :: e -> Maybe a -> Either e a
note = flip maybe Right . Left
