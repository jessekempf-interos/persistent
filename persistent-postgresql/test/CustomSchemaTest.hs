{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings #-}

module CustomSchemaTest where

import           PgInit

share [mkPersist sqlSettings, mkMigrate "schemaMigrate"] [persistLowerCase|
Table1 schema=custom
  someField Text
  deriving Eq Show

-- Ensure we can fkey from schema public -> schema custom
Table2
  t1ref Table1Id

-- Ensure we can fkey from schema custom -> schema public
-- Table3 schema=custom
--   t2ref Table2Id
|]

specs :: Spec
specs = do
  describe "using PostgreSQL schemas" $ do
    it "generates a usable migration" $ runConnAssert $
      void $ runMigration schemaMigrate

    it "inserts and retrieves data correctly" $ runConnAssert $ do
      void $ runMigrationSilent schemaMigrate

      void $ insertMany
        [ Table1 "foo"
        , Table1 "bar"
        ]

      tab1 <- map entityVal <$> selectList [] []

      liftIO $ tab1 `shouldBe` [Table1 "foo", Table1 "bar"]

    it "updates data correctly" $ runConnAssert $ do
      void $ runMigrationSilent schemaMigrate

      k <- insert (Table1 "foo")

      update k [Table1SomeField =. "bar"]

      tab1 <- map entityVal <$> selectList [] []

      liftIO $ tab1 `shouldBe` [Table1 "bar"]

    it "deletes data correctly" $ runConnAssert $ do
      void $ runMigrationSilent schemaMigrate

      void $ insertMany
        [ Table1 "foo"
        , Table1 "bar"
        ]

      deleteWhere [Table1SomeField ==. "bar"]

      tab1 <- map entityVal <$> selectList [] []

      liftIO $ tab1 `shouldBe` [Table1 "foo"]