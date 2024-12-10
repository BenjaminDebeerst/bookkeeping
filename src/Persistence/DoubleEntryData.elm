module Persistence.DoubleEntryData exposing (..)

import Time.Date exposing (Date)


type alias BankStatement =
    { id : Int
    , line : String
    }


type AccountKind
    = Income
    | Bank
    | Init
    | Expense


type Account
    = BaseAccount AccountKind
    | CustomAccount
        { id : Int
        , label : String
        , parent : Account
        }


type Origin
    = BankStatement Int
    | AccountInit Int


type alias BookEntry =
    { id : Int
    , date : Date
    , creditor : Account
    , debitor : Account
    , amount : Int
    , label : String
    , origin : Origin
    }
