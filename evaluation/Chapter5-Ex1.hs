-- Chapter 5, Exercise 1
-- Instead of treating any non-false value as true,
-- change the definition of if so that the predicate accepts
-- only Bool values and throws an error on any others.

eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         Bool True  -> eval conseq
         otherwise  -> throwError $ TypeMismatch "bool" pref

