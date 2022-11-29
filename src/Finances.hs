{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Finances where
import Data.Kind (Type, Constraint)

type Post :: Type
data Post
  = Directors
  | CEO
  | CFO
  | COO
  | FinAnalityst
  | Accountant
  | Team

type TeamPost :: Type
data TeamPost
  = Producer -- like a teamlead&techlead, responsible to external communications
  | Singer -- senior
  -- testers
  | Manager
  | PR
  | Advertiser
  | SoundEngineer

type TeamCommand :: TeamPost -> Type
data TeamCommand t where
  ProducerCommand
    :: TeamCommand Singer
    -> TeamCommand Manager
    -> TeamCommand Producer
  SingerCommand
    :: TeamCommand Singer
  ManagerCommand
    :: [TeamCommand PR]
    -> [TeamCommand Advertiser]
    -> [TeamCommand SoundEngineer]
    -> TeamCommand Manager
  PRCommand
    :: TeamCommand PR 
  AdvertiserCommand
    :: TeamCommand Advertiser
  SoundEngineerCommand
    :: TeamCommand SoundEngineer

type Command :: Post -> Type
data Command post where
  DirectorsCommand
    :: Command CEO
    -> Command Directors
  CEOCommand
    :: Command CFO
    -> Command COO
    -> Command CEO
  CFOCommand
    :: [Command FinAnalityst]
    -> [Command Accountant]
    -> Command CFO
  COOCommand
    :: [NonChiefCommand]
    -> Command COO
  AnalitystCommand
    :: Command FinAnalityst
  AccountantCommand
    :: Command Accountant
  SingerTeamCommand :: TeamCommand Producer -> Command Team
instance NonChief Team

type NonChiefCommand :: Type
data NonChiefCommand = forall (post :: Post). NonChief post =>
  MkNonChiefCommand (Command post)

type NonChief :: Post -> Constraint
class NonChief post

-- Example company command line
singingCrickets :: Command 'Directors
singingCrickets =
  DirectorsCommand
    ( CEOCommand
        ( CFOCommand
            (replicate 3 AnalitystCommand)
            (replicate 5 AccountantCommand)
        )
        ( COOCommand
            ( replicate 13 
                ( MkNonChiefCommand
                  (SingerTeamCommand
                    ( ProducerCommand
                        SingerCommand
                        ( ManagerCommand
                            (replicate 2 PRCommand)
                            (replicate 5 AdvertiserCommand)
                            [SoundEngineerCommand]
                        )
                    )
                  )
                )
            )
        )
    )


