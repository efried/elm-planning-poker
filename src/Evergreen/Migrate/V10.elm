module Evergreen.Migrate.V10 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Evergreen.V10.Types
import Evergreen.V8.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V8.Types.FrontendModel -> ModelMigration Evergreen.V10.Types.FrontendModel Evergreen.V10.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V8.Types.BackendModel -> ModelMigration Evergreen.V10.Types.BackendModel Evergreen.V10.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V8.Types.FrontendMsg -> MsgMigration Evergreen.V10.Types.FrontendMsg Evergreen.V10.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V8.Types.ToBackend -> MsgMigration Evergreen.V10.Types.ToBackend Evergreen.V10.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V8.Types.BackendMsg -> MsgMigration Evergreen.V10.Types.BackendMsg Evergreen.V10.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V8.Types.ToFrontend -> MsgMigration Evergreen.V10.Types.ToFrontend Evergreen.V10.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_FrontendModel : Evergreen.V8.Types.FrontendModel -> Evergreen.V10.Types.FrontendModel
migrate_Types_FrontendModel old =
    { game = old.game
    , selectedCard = old.selectedCard
    , enteredGameCode = old.enteredGameCode
    , cardOptions = old.cardOptions
    , hideStats = old.hideStats
    , device = old.device
    , key = Nothing
    }


migrate_Types_ToFrontend : Evergreen.V8.Types.ToFrontend -> Evergreen.V10.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V8.Types.NoOpToFrontend ->
            Evergreen.V10.Types.NoOpToFrontend

        Evergreen.V8.Types.GameReceived p0 ->
            Evergreen.V10.Types.GameReceived p0

        Evergreen.V8.Types.GameReset p0 ->
            Evergreen.V10.Types.GameReset p0