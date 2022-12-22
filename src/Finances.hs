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
import Finances.Types (USD (MkUSD), Worker (MkWorker, workerName, workerOutcome, workerSalary))

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
      { workerName = "Benjamin Cook"
      , workerOutcome = MkUSD 25_000
      , workerSalary = MkUSD 3_678_900
      }
    ( CEOStaff
        MkWorker
          { workerName = "Ficjerald Brown"
          , workerOutcome = MkUSD 19_000
          , workerSalary = MkUSD 350_900
          }
        ( CFOStaff
            MkWorker
              { workerName = "Elsa Brown"
              , workerOutcome = MkUSD 15_000
              , workerSalary = MkUSD 105_900
              }
            [ AnalitystStaff
                MkWorker
                  { workerName = "Mila Yovowitch"
                  , workerOutcome = MkUSD 30_000
                  , workerSalary = MkUSD 10_000
                  }
            , AnalitystStaff
                MkWorker
                  { workerName = "John Martin"
                  , workerOutcome = MkUSD 35_000
                  , workerSalary = MkUSD 12_000
                  }
            ]
            [ AccountantStaff
                MkWorker
                  { workerName = "Greedy Butch"
                  , workerOutcome = MkUSD 20_000
                  , workerSalary = MkUSD 4_000
                  }
            , AccountantStaff
                MkWorker
                  { workerName = "Philip Cock"
                  , workerOutcome = MkUSD 66_000
                  , workerSalary = MkUSD 8_000
                  }
            ]
        )
        ( COOStaff
            MkWorker
              { workerName = "Rodger Stone"
              , workerOutcome = MkUSD 40_000
              , workerSalary = MkUSD 107_000
              }
            ( replicate 13
                ( MkNonChiefStaff
                  ( SingerTeamStaff
                      ( ProducerStaff
                          MkWorker
                            { workerName = "Jozeppo Bernini"
                            , workerOutcome = MkUSD 70_000
                            , workerSalary = MkUSD 60_000
                            }
                          ( SingerStaff
                              MkWorker
                                { workerName = "Polly 'Benis' Bankova"
                                , workerOutcome = MkUSD 127_246_889
                                , workerSalary = MkUSD 78_000
                                }
                          )
                          ( ManagerStaff
                              MkWorker
                                { workerName = "Ginat Ball"
                                , workerOutcome = MkUSD 30_000
                                , workerSalary = MkUSD 12_000
                                }
                              [ PRStaff
                                  MkWorker
                                    { workerName = "Rob Smith"
                                    , workerOutcome = MkUSD 78_980
                                    , workerSalary = MkUSD 9_000
                                    }
                              , PRStaff
                                  MkWorker
                                    { workerName = "Morris Crock"
                                    , workerOutcome = MkUSD 45_890
                                    , workerSalary = MkUSD 6_800
                                    }
                              ]
                              [ AdvertiserStaff
                                  MkWorker
                                    { workerName = "Morris Crock"
                                    , workerOutcome = MkUSD 45_890
                                    , workerSalary = MkUSD 6_800
                                    }
                              , AdvertiserStaff
                                  MkWorker
                                    { workerName = "Helen Crock"
                                    , workerOutcome = MkUSD 20_000
                                    , workerSalary = MkUSD 3_800
                                    }
                              , AdvertiserStaff
                                  MkWorker
                                    { workerName = "Dmitriy Begulya"
                                    , workerOutcome = MkUSD 55_000
                                    , workerSalary = MkUSD 8_000
                                    }
                              , AdvertiserStaff
                                  MkWorker
                                    { workerName = "Jesus Christ"
                                    , workerOutcome = MkUSD 2_000
                                    , workerSalary = MkUSD 300
                                    }
                              ]
                              [ SoundEngineerStaff
                                  MkWorker
                                    { workerName = "Listen Ya"
                                    , workerOutcome = MkUSD 80_000
                                    , workerSalary = MkUSD 7_000
                                    }
                              ]
                          )
                      )
                  )
                )
            )
        )
    )


