module Utils.Fns (call) where


call :: a -> (a -> b) -> b
call x f = f x
