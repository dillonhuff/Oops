module Utils(extractRight) where

extractRight (Right a) = a
extractRight (Left a) = error "Bad left appears in extractRight"
