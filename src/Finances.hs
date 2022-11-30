{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
module Finances where
import Data.Kind (Type, Constraint)
import Finances.Types (USD (MkUSD), Worker (MkWorker, name, outcome, salary))

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

type TeamStaff :: TeamPost -> Type
data TeamStaff t where
  ProducerStaff
    :: Worker
    -> TeamStaff Singer
    -> TeamStaff Manager
    -> TeamStaff Producer
  SingerStaff
    :: Worker
    -> TeamStaff Singer
  ManagerStaff
    :: Worker
    -> [TeamStaff PR]
    -> [TeamStaff Advertiser]
    -> [TeamStaff SoundEngineer]
    -> TeamStaff Manager
  PRStaff
    :: Worker
    -> TeamStaff PR 
  AdvertiserStaff
    :: Worker
    -> TeamStaff Advertiser
  SoundEngineerStaff
    :: Worker
    -> TeamStaff SoundEngineer

asTeamWorker :: TeamStaff post -> Worker
asTeamWorker = undefined

type Staff :: Post -> Type
data Staff post where
  DirectorsStaff
    :: Worker
    -> Staff CEO
    -> Staff Directors
  CEOStaff
    :: Worker
    -> Staff CFO
    -> Staff COO
    -> Staff CEO
  CFOStaff
    :: Worker
    -> [Staff FinAnalityst]
    -> [Staff Accountant]
    -> Staff CFO
  COOStaff
    :: Worker
    -> [NonChiefStaff]
    -> Staff COO
  AnalitystStaff
    :: Worker
    -> Staff FinAnalityst
  AccountantStaff
    :: Worker
    -> Staff Accountant
  SingerTeamStaff
    :: TeamStaff Producer
    -> Staff Team

asWorker :: Staff post -> Worker
asWorker = undefined

instance NonChief Team

type NonChiefStaff :: Type
data NonChiefStaff = forall (post :: Post). NonChief post =>
  MkNonChiefStaff (Staff post)

type NonChief :: Post -> Constraint
class NonChief post

-- Example company Staff line
singingCrickets :: Staff 'Directors
singingCrickets =
  DirectorsStaff
    MkWorker
      { name = "Benjamin Cook"
      , outcome = MkUSD 25_000
      , salary = MkUSD 3_678_900
      }
    ( CEOStaff
        MkWorker
          { name = "Ficjerald Brown"
          , outcome = MkUSD 19_000
          , salary = MkUSD 350_900
          }
        ( CFOStaff
            MkWorker
              { name = "Elsa Brown"
              , outcome = MkUSD 15_000
              , salary = MkUSD 105_900
              }
            [ AnalitystStaff
                MkWorker
                  { name = "Mila Yovowitch"
                  , outcome = MkUSD 30_000
                  , salary = MkUSD 10_000
                  }
            , AnalitystStaff
                MkWorker
                  { name = "John Martin"
                  , outcome = MkUSD 35_000
                  , salary = MkUSD 12_000
                  }
            ]
            [ AccountantStaff
                MkWorker
                  { name = "Greedy Butch"
                  , outcome = MkUSD 20_000
                  , salary = MkUSD 4_000
                  }
            , AccountantStaff
                MkWorker
                  { name = "Philip Cock"
                  , outcome = MkUSD 66_000
                  , salary = MkUSD 8_000
                  }
            ]
        )
        ( COOStaff
            MkWorker
              { name = "Rodger Stone"
              , outcome = MkUSD 40_000
              , salary = MkUSD 107_000
              }
            ( replicate 13
                ( MkNonChiefStaff
                  ( SingerTeamStaff
                      ( ProducerStaff
                          MkWorker
                            { name = "Jozeppo Bernini"
                            , outcome = MkUSD 70_000
                            , salary = MkUSD 60_000
                            }
                          ( SingerStaff
                              MkWorker
                                { name = "Polly 'Benis' Bankova"
                                , outcome = MkUSD 127_246_889
                                , salary = MkUSD 78_000
                                }
                          )
                          ( ManagerStaff
                              MkWorker
                                { name = "Ginat Ball"
                                , outcome = MkUSD 30_000
                                , salary = MkUSD 12_000
                                }
                              [ PRStaff
                                  MkWorker
                                    { name = "Rob Smith"
                                    , outcome = MkUSD 78_980
                                    , salary = MkUSD 9_000
                                    }
                              , PRStaff
                                  MkWorker
                                    { name = "Morris Crock"
                                    , outcome = MkUSD 45_890
                                    , salary = MkUSD 6_800
                                    }
                              ]
                              [ AdvertiserStaff
                                  MkWorker
                                    { name = "Morris Crock"
                                    , outcome = MkUSD 45_890
                                    , salary = MkUSD 6_800
                                    }
                              , AdvertiserStaff
                                  MkWorker
                                    { name = "Helen Crock"
                                    , outcome = MkUSD 20_000
                                    , salary = MkUSD 3_800
                                    }
                              , AdvertiserStaff
                                  MkWorker
                                    { name = "Dmitriy Begulya"
                                    , outcome = MkUSD 55_000
                                    , salary = MkUSD 8_000
                                    }
                              , AdvertiserStaff
                                  MkWorker
                                    { name = "Jesus Christ"
                                    , outcome = MkUSD 2_000
                                    , salary = MkUSD 300
                                    }
                              ]
                              [ SoundEngineerStaff
                                  MkWorker
                                    { name = "Listen Ya"
                                    , outcome = MkUSD 80_000
                                    , salary = MkUSD 7_000
                                    }
                              ]
                          )
                      )
                  )
                )
            )
        )
    )


