{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Recetas where

import Import
import Database.Persist.Sql (toSqlKey)
import Data.Aeson

data I = I
  { iId :: Maybe Int64
  , iNombre :: Text
  , iCantidad :: Double
  , iUnidad :: Text
  } deriving (Generic, Show)

instance FromJSON I where
  parseJSON = withObject "I" $ \o -> do
    iId <- o .:? "id"
    iNombre <- o .: "nombre"
    iCantidad  <- o .: "cantidad"
    iUnidad  <- o .: "unidad"
    return I{..}

instance ToJSON I where
  toJSON I{..} = object [
    "id" .= iId,
    "nombre" .= iNombre,
    "cantidad"  .= iCantidad,
    "unidad" .= iUnidad]

data P = P
  { pId :: Maybe Int64
  , pNumero :: Int
  , pTexto :: Text
  } deriving (Generic, Show)

instance FromJSON P where
  parseJSON = withObject "P" $ \o -> do
    pId <- o .:? "id"
    pNumero <- o .: "numero"
    pTexto  <- o .: "texto"
    return P{..}

instance ToJSON P where
  toJSON P{..} = object [
    "id" .= pId,
    "numero" .= pNumero,
    "texto"  .= pTexto]

data R = R
  { rId :: Maybe Int64
  , rNombre :: Text
  , rIngredientes :: [I]
  , rPasos :: [P]
  } deriving (Generic, Show)

instance FromJSON R where
  parseJSON = withObject "R" $ \o -> do
    rId <- o .:? "id"
    rNombre <- o .: "nombre"
    rIngredientes <- o .: "ingredientes"
    rPasos  <- o .: "pasos"
    return R{..}

instance ToJSON R where
  toJSON R{..} = object [
    "id" .= rId,
    "nombre" .= rNombre,
    "ingredientes"  .= rIngredientes,
    "pasos" .= rPasos]

ingredienteJson :: Entity Ingrediente -> Value
ingredienteJson (Entity _ i) =
  object [ "nombre" .= ingredienteNombre i
         , "cantidad" .= ingredienteCantidad i
         , "unidad" .= ingredienteUnidad i
         ]

pasoJson :: Entity Paso -> Value
pasoJson (Entity _ p) =
  object [ "numero" .= pasoNumero p
         , "texto" .= pasoTexto p
         ]

recetaJson :: Entity Receta -> Handler Value
recetaJson (Entity rk r) = do
  let rNombre = recetaNombre r
  ingredientes <- runDB $ selectList [IngredienteRecetaId ==. rk] [Asc IngredienteNombre]
  let ingredientesList = map ingredienteJson ingredientes
  pasos <- runDB $ selectList [PasoRecetaId ==. rk] [Asc PasoNumero]
  let pasosList = map pasoJson pasos
  return $ object [ "id" .= rk
                  , "nombre" .= rNombre
                  , "ingredientes" .= ingredientesList
                  , "pasos" .= pasosList
                  ]

getRecetasR :: Handler Value
getRecetasR = do
  recetas <- runDB $ selectList [] [Asc RecetaNombre]
  l <- mapM recetaJson recetas
  return $ Array $ fromList l

postRecetasR :: Handler ()
postRecetasR = do
  msg <- requireJsonBody :: Handler R
  _ <- runDB $ do
    rId <- insert $ Receta {recetaNombre = rNombre msg}
    mapM (\i->insert $ Ingrediente
              { ingredienteNombre = iNombre i
              , ingredienteCantidad = iCantidad i
              , ingredienteUnidad = iUnidad i
              , ingredienteRecetaId = rId}) (rIngredientes msg)
    mapM (\p->insert $ Paso
           { pasoNumero = pNumero p
           , pasoTexto = pTexto p
           , pasoRecetaId = rId}) (rPasos msg)
  sendResponseStatus status201 ("Created" :: Text)

getRecetaR :: Int64 -> Handler Value
getRecetaR n = do
  let recetaId = toSqlKey n :: Key Receta
  receta <- runDB $ get recetaId
  case receta of
    (Just r) -> recetaJson (Entity recetaId r)
    Nothing -> return $ object []

deleteRecetaR :: Int64 -> Handler ()
deleteRecetaR n = do
  let recetaId = toSqlKey n :: Key Receta
  _ <- runDB $ do
    deleteWhere [IngredienteRecetaId ==. recetaId]
    deleteWhere [PasoRecetaId ==. recetaId]
    delete recetaId
  sendResponseStatus status200 ("Deleted" :: Text)
