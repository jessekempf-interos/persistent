{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

-- | Intended for creating new backends.
module Database.Persist.Sql.Internal
    ( mkColumns
    , defaultAttribute
    , BackendSpecificOverrides(..)
    , emptyBackendSpecificOverrides
    ) where

import Data.Char (isSpace)
import Data.Monoid (mappend, mconcat)
import Data.Text (Text)
import qualified Data.Text as T

import Database.Persist.Quasi
import Database.Persist.Sql.Types
import Database.Persist.Types
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

-- | Record of functions to override the default behavior in 'mkColumns'.
-- It is recommended you initialize this with 'emptyBackendSpecificOverrides' and override the default values,
-- so that as new fields are added, your code still compiles.
--
-- @since 2.11
data BackendSpecificOverrides = BackendSpecificOverrides
    { backendSpecificForeignKeyName :: Maybe (DBName -> DBName -> DBName)
    }

-- | Creates an empty 'BackendSpecificOverrides' (i.e. use the default behavior; no overrides)
--
-- @since 2.11
emptyBackendSpecificOverrides :: BackendSpecificOverrides
emptyBackendSpecificOverrides = BackendSpecificOverrides Nothing

defaultAttribute :: [Attr] -> Maybe Text
defaultAttribute =
    listToMaybe
    . mapMaybe (T.stripPrefix "default=")

-- | Create the list of columns for the given entity.
mkColumns
    :: [EntityDef]
    -> EntityDef
    -> BackendSpecificOverrides
    -> ([Column], [UniqueDef], [ForeignDef])
mkColumns allDefs t overrides =
    (cols, entityUniques t, entityForeigns t)
  where
    cols :: [Column]
    cols = map goId idCol <> map go (entityFields t)

    idCol :: [FieldDef]
    idCol = case entityPrimary t of
        Just _ -> []
        Nothing -> [entityId t]

    goId fd =
        Column
            { cName = fieldDB fd
            , cNull = False
            , cSqlType = fieldSqlType fd
            , cDefault =
                case defaultAttribute $ fieldAttrs fd of
                    Nothing ->
                        -- So this is not necessarily a problem...
                        -- because you can use eg `inserKey` to insert
                        -- a value into the database without ever asking
                        -- for a default attribute.
                        Nothing
                        -- But we need to be able to say "Hey, if this is
                        -- an *auto generated ID column*, then I need to
                        -- specify that it has the default serial picking
                        -- behavior for whatever SQL backend this is using.
                        -- Because naturally MySQL, Postgres, MSSQL, etc
                        -- all do ths differently, sigh.
                    Just def ->
                        --
                        Just def

            , cDefaultConstraintName =  Nothing
            , cMaxLen = maxLen $ fieldAttrs fd
            , cReference = ref (fieldDB fd) (fieldReference fd) (fieldAttrs fd)
            }

    tableName :: DBName
    tableName = entityDB t

    go :: FieldDef -> Column
    go fd =
        Column
            { cName = fieldDB fd
            , cNull = nullable (fieldAttrs fd) /= NotNullable || entitySum t
            , cSqlType = fieldSqlType fd
            , cDefault = defaultAttribute $ fieldAttrs fd
            , cDefaultConstraintName =  Nothing
            , cMaxLen = maxLen $ fieldAttrs fd
            , cReference = ref (fieldDB fd) (fieldReference fd) (fieldAttrs fd)
            }

    maxLen :: [Attr] -> Maybe Integer
    maxLen [] = Nothing
    maxLen (a:as)
        | Just d <- T.stripPrefix "maxlen=" a =
            case reads (T.unpack d) of
              [(i, s)] | all isSpace s -> Just i
              _ -> error $ "Could not parse maxlen field with value " ++
                           show d ++ " on " ++ show tableName
        | otherwise = maxLen as

    refNameFn = fromMaybe refName (backendSpecificForeignKeyName overrides)

    ref :: DBName
        -> ReferenceDef
        -> [Attr]
        -> Maybe (DBName, DBName) -- table name, constraint name
    ref c fe []
        | ForeignRef f _ <- fe =
            Just (resolveTableName allDefs f, refNameFn tableName c)
        | otherwise = Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c fe (a:as)
        | Just x <- T.stripPrefix "reference=" a = do
            constraintName <- snd <$> (ref c fe as)
            pure (DBName x, constraintName)
        | Just x <- T.stripPrefix "constraint=" a = do
            tableName <- fst <$> (ref c fe as)
            pure (tableName, DBName x)
    ref c x (_:as) = ref c x as

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    DBName $ Data.Monoid.mconcat [table, "_", column, "_fkey"]

resolveTableName :: [EntityDef] -> HaskellName -> DBName
resolveTableName [] (HaskellName hn) = error $ "Table not found: " `Data.Monoid.mappend` T.unpack hn
resolveTableName (e:es) hn
    | entityHaskell e == hn = entityDB e
    | otherwise = resolveTableName es hn
