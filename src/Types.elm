module Types exposing (..)


type alias Mode =
    { id : Int
    , mtype : ModeType
    , title : String
    , states : List State
    }


type alias State =
    { id : Int
    , phase : Phase
    , teamID : Int
    }


type ModeType
    = Bo3
    | Bo1


type alias Map =
    { id : Int
    , imgurl : String
    , title : String
    , status : Maybe Phase
    , inPlay : Bool
    }


type alias Team =
    { id : Int
    , name : String
    }


type Phase
    = Pick
    | Ban
    | Rand
