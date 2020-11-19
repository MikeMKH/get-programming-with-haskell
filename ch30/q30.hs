allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func val = val >>= return . func

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp func val = func >>= (\f -> val >>= return . f)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just val) func = func val