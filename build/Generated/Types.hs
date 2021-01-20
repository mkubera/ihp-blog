-- This file is auto generated and will be overriden regulary. Please edit `Application/Schema.sql` to change the Types
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, TypeFamilies, DataKinds, TypeOperators, UndecidableInstances, ConstraintKinds, StandaloneDeriving  #-}
module Generated.Types where

import IHP.HaskellSupport
import IHP.ModelSupport
import CorePrelude hiding (id)
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Time.Calendar
import qualified Data.List as List
import qualified Data.ByteString as ByteString 
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField hiding (Field)
import qualified IHP.Controller.Param
import GHC.TypeLits
import Data.UUID (UUID)
import Data.Default
import qualified IHP.QueryBuilder as QueryBuilder
import qualified Data.Proxy
import GHC.Records
import Data.Data
import qualified Data.String.Conversions
import qualified Data.Text.Encoding
import qualified Data.Aeson
import Database.PostgreSQL.Simple.Types (Query (Query), Binary ( .. ))
import qualified Database.PostgreSQL.Simple.Types
import IHP.Job.Types
import IHP.Job.Queue ()




data Author'  = Author {id :: (Id' "authors"), name :: Text, postsCount :: Int, tags :: (Maybe [Text]), createdAt :: UTCTime, updatedAt :: UTCTime, meta :: MetaBag} deriving (Eq, Show)
instance InputValue Author where inputValue = IHP.ModelSupport.recordToInputValue
type Author = Author' 

instance FromRow Author where
    fromRow = do
        id <- field
        name <- field
        postsCount <- field
        tags <- field
        createdAt <- field
        updatedAt <- field
        pure $ Author id name postsCount tags createdAt updatedAt def

type instance GetTableName (Author' ) = "authors"
type instance GetModelByTableName "authors" = Author
type instance GetModelName (Author' ) = "Author"

type instance PrimaryKey "authors" = UUID

instance QueryBuilder.FilterPrimaryKey "authors" where
    filterWhereId id builder =
        builder |> QueryBuilder.filterWhere (#id, id)
    {-# INLINE filterWhereId #-}

instance CanCreate Author where
    create :: (?modelContext :: ModelContext) => Author -> IO Author
    create model = do
        List.head <$> withDatabaseConnection \databaseConnection -> Database.PostgreSQL.Simple.query databaseConnection "INSERT INTO authors (id, name, posts_count, tags, created_at, updated_at) VALUES (?, ?, ?, ? :: TEXT[], ?, ?) RETURNING *" ((fieldWithDefault #id model, get #name model, fieldWithDefault #postsCount model, get #tags model, fieldWithDefault #createdAt model, fieldWithDefault #updatedAt model))
    createMany models = do
        withDatabaseConnection \databaseConnection -> Database.PostgreSQL.Simple.query databaseConnection (Query $ "INSERT INTO authors (id, name, posts_count, tags, created_at, updated_at) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ?, ?, ? :: TEXT[], ?, ?)") models)) <> " RETURNING *") (List.concat $ List.map (\model -> [toField (fieldWithDefault #id model), toField (get #name model), toField (fieldWithDefault #postsCount model), toField (get #tags model), toField (fieldWithDefault #createdAt model), toField (fieldWithDefault #updatedAt model)]) models)

instance CanUpdate Author where
    updateRecord model = do
        List.head <$> withDatabaseConnection \databaseConnection -> Database.PostgreSQL.Simple.query databaseConnection "UPDATE authors SET id = ?, name = ?, posts_count = ?, tags = ?, created_at = ?, updated_at = ? WHERE id = ? RETURNING *" ((fieldWithUpdate #id model, fieldWithUpdate #name model, fieldWithUpdate #postsCount model, fieldWithUpdate #tags model, fieldWithUpdate #createdAt model, fieldWithUpdate #updatedAt model, get #id model))

instance Record Author where
    {-# INLINE newRecord #-}
    newRecord = Author def def def def def def  def

instance SetField "id" (Author' ) (Id' "authors") where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author newValue name postsCount tags createdAt updatedAt (meta { touchedFields = "id" : touchedFields meta })
instance SetField "name" (Author' ) Text where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author id newValue postsCount tags createdAt updatedAt (meta { touchedFields = "name" : touchedFields meta })
instance SetField "postsCount" (Author' ) Int where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author id name newValue tags createdAt updatedAt (meta { touchedFields = "postsCount" : touchedFields meta })
instance SetField "tags" (Author' ) (Maybe [Text]) where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author id name postsCount newValue createdAt updatedAt (meta { touchedFields = "tags" : touchedFields meta })
instance SetField "createdAt" (Author' ) UTCTime where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author id name postsCount tags newValue updatedAt (meta { touchedFields = "createdAt" : touchedFields meta })
instance SetField "updatedAt" (Author' ) UTCTime where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author id name postsCount tags createdAt newValue (meta { touchedFields = "updatedAt" : touchedFields meta })
instance SetField "meta" (Author' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (Author id name postsCount tags createdAt updatedAt meta) =
        Author id name postsCount tags createdAt updatedAt newValue
instance UpdateField "id" (Author' ) (Author' ) (Id' "authors") (Id' "authors") where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author newValue name postsCount tags createdAt updatedAt (meta { touchedFields = "id" : touchedFields meta })
instance UpdateField "name" (Author' ) (Author' ) Text Text where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author id newValue postsCount tags createdAt updatedAt (meta { touchedFields = "name" : touchedFields meta })
instance UpdateField "postsCount" (Author' ) (Author' ) Int Int where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author id name newValue tags createdAt updatedAt (meta { touchedFields = "postsCount" : touchedFields meta })
instance UpdateField "tags" (Author' ) (Author' ) (Maybe [Text]) (Maybe [Text]) where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author id name postsCount newValue createdAt updatedAt (meta { touchedFields = "tags" : touchedFields meta })
instance UpdateField "createdAt" (Author' ) (Author' ) UTCTime UTCTime where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author id name postsCount tags newValue updatedAt (meta { touchedFields = "createdAt" : touchedFields meta })
instance UpdateField "updatedAt" (Author' ) (Author' ) UTCTime UTCTime where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author id name postsCount tags createdAt newValue (meta { touchedFields = "updatedAt" : touchedFields meta })
instance UpdateField "meta" (Author' ) (Author' ) MetaBag MetaBag where
    {-# INLINE updateField #-}
    updateField newValue (Author id name postsCount tags createdAt updatedAt meta) = Author id name postsCount tags createdAt updatedAt newValue


data Post'  = Post {id :: (Id' "posts"), title :: Text, body :: Text, createdAt :: UTCTime, meta :: MetaBag} deriving (Eq, Show)
instance InputValue Post where inputValue = IHP.ModelSupport.recordToInputValue
type Post = Post' 

instance FromRow Post where
    fromRow = do
        id <- field
        title <- field
        body <- field
        createdAt <- field
        pure $ Post id title body createdAt def

type instance GetTableName (Post' ) = "posts"
type instance GetModelByTableName "posts" = Post
type instance GetModelName (Post' ) = "Post"

type instance PrimaryKey "posts" = UUID

instance QueryBuilder.FilterPrimaryKey "posts" where
    filterWhereId id builder =
        builder |> QueryBuilder.filterWhere (#id, id)
    {-# INLINE filterWhereId #-}

instance CanCreate Post where
    create :: (?modelContext :: ModelContext) => Post -> IO Post
    create model = do
        List.head <$> withDatabaseConnection \databaseConnection -> Database.PostgreSQL.Simple.query databaseConnection "INSERT INTO posts (id, title, body, created_at) VALUES (?, ?, ?, ?) RETURNING *" ((fieldWithDefault #id model, get #title model, get #body model, fieldWithDefault #createdAt model))
    createMany models = do
        withDatabaseConnection \databaseConnection -> Database.PostgreSQL.Simple.query databaseConnection (Query $ "INSERT INTO posts (id, title, body, created_at) VALUES " <> (ByteString.intercalate ", " (List.map (\_ -> "(?, ?, ?, ?)") models)) <> " RETURNING *") (List.concat $ List.map (\model -> [toField (fieldWithDefault #id model), toField (get #title model), toField (get #body model), toField (fieldWithDefault #createdAt model)]) models)

instance CanUpdate Post where
    updateRecord model = do
        List.head <$> withDatabaseConnection \databaseConnection -> Database.PostgreSQL.Simple.query databaseConnection "UPDATE posts SET id = ?, title = ?, body = ?, created_at = ? WHERE id = ? RETURNING *" ((fieldWithUpdate #id model, fieldWithUpdate #title model, fieldWithUpdate #body model, fieldWithUpdate #createdAt model, get #id model))

instance Record Post where
    {-# INLINE newRecord #-}
    newRecord = Post def def def def  def

instance SetField "id" (Post' ) (Id' "posts") where
    {-# INLINE setField #-}
    setField newValue (Post id title body createdAt meta) =
        Post newValue title body createdAt (meta { touchedFields = "id" : touchedFields meta })
instance SetField "title" (Post' ) Text where
    {-# INLINE setField #-}
    setField newValue (Post id title body createdAt meta) =
        Post id newValue body createdAt (meta { touchedFields = "title" : touchedFields meta })
instance SetField "body" (Post' ) Text where
    {-# INLINE setField #-}
    setField newValue (Post id title body createdAt meta) =
        Post id title newValue createdAt (meta { touchedFields = "body" : touchedFields meta })
instance SetField "createdAt" (Post' ) UTCTime where
    {-# INLINE setField #-}
    setField newValue (Post id title body createdAt meta) =
        Post id title body newValue (meta { touchedFields = "createdAt" : touchedFields meta })
instance SetField "meta" (Post' ) MetaBag where
    {-# INLINE setField #-}
    setField newValue (Post id title body createdAt meta) =
        Post id title body createdAt newValue
instance UpdateField "id" (Post' ) (Post' ) (Id' "posts") (Id' "posts") where
    {-# INLINE updateField #-}
    updateField newValue (Post id title body createdAt meta) = Post newValue title body createdAt (meta { touchedFields = "id" : touchedFields meta })
instance UpdateField "title" (Post' ) (Post' ) Text Text where
    {-# INLINE updateField #-}
    updateField newValue (Post id title body createdAt meta) = Post id newValue body createdAt (meta { touchedFields = "title" : touchedFields meta })
instance UpdateField "body" (Post' ) (Post' ) Text Text where
    {-# INLINE updateField #-}
    updateField newValue (Post id title body createdAt meta) = Post id title newValue createdAt (meta { touchedFields = "body" : touchedFields meta })
instance UpdateField "createdAt" (Post' ) (Post' ) UTCTime UTCTime where
    {-# INLINE updateField #-}
    updateField newValue (Post id title body createdAt meta) = Post id title body newValue (meta { touchedFields = "createdAt" : touchedFields meta })
instance UpdateField "meta" (Post' ) (Post' ) MetaBag MetaBag where
    {-# INLINE updateField #-}
    updateField newValue (Post id title body createdAt meta) = Post id title body createdAt newValue

