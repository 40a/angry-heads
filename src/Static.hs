module Static (makeWithStatic) where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (
    CachingStrategy (PublicStaticCaching),
    addBase, hasPrefix, hasSuffix, initCaching, only,
    staticPolicy, staticPolicy', (<|>), (>->)
    )


makeWithStatic :: Bool -> IO Middleware
makeWithStatic dontCache
    | dontCache =
        return $ staticPolicy policy

    | otherwise =
        initCaching PublicStaticCaching
        >>= return . flip staticPolicy' policy
  where
    policy =
        ( only [("", "index.html")]
          <|> hasPrefix "css/"
          <|> hasPrefix "js/"
          <|> hasSuffix ".html"
          <|> hasPrefix "img/"
        ) >-> addBase "static"
