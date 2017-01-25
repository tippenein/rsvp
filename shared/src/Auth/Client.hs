{-# LANGUAGE ScopedTypeVariables #-}

module Auth.Client where

-- import Servant.API
-- import Servant.Auth.Token.API
-- import Servant.Reflex

-- import Protolude
-- (authSigninMethod
--   :<|> authSigninGetCodeMethod
--   :<|> authSigninPostCodeMethod
--   :<|> authTouchMethod
--   :<|> authTokenInfoMethod
--   :<|> authSignoutMethod
--   :<|> authSignupMethod
--   :<|> authUsersMethod
--   :<|> authGetUserMethod
--   :<|> authPatchUserMethod
--   :<|> authPutUserMethod
--   :<|> authDeleteUserMethod
--   :<|> authRestoreMethod
--   :<|> authGetSingleUseCodes
--   :<|> authGetGroupMethod
--   :<|> authPostGroupMethod
--   :<|> authPutGroupMethod
--   :<|> authPatchGroupMethod
--   :<|> authDeleteGroupMethod
--   :<|> authGroupsMethod) = client (Proxy :: Proxy AuthApi)
--                                  (Proxy :: Proxy m)
--                                  (constDyn (BasePath "/"))
