module Model exposing (..)

import Html exposing (b, div, p, text)
import Model.Date as Date
import Model.Event as Event exposing (Event)
import Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected)
import Model.Interval as Interval
import Model.PersonalDetails exposing (DetailWithName, PersonalDetails)
import Model.Repo exposing (Repo)


type alias Model =
    { personalDetails : PersonalDetails
    , events : List Event
    , selectedEventCategories : SelectedEventCategories
    , repos : List Repo
    }


academicEvents : List Event
academicEvents =
    [ { title = "Academic event 1"
      , interval = Interval.withDurationYears (Date.onlyYear 2017) 4
      , description = p [] [ text "I graduated ",  text "from the" , text " National College Silvania", text " in Zalău." ]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Academic event 2"
      , interval = Interval.withDurationYears (Date.onlyYear 2021) 4
      , description = p [] [text "I started my studies at the Technical University of Cluj-Napoca, Faculty of Automation and Computers."]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


workEvents : List Event
workEvents =
    [ { title = "Work event 1"
      , interval = Interval.withDurationMonths 2023 Date.Jun 3
      , description = text "Internship"
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Work event 2"
      , interval = Interval.open (Date.full 2023 Date.Sep)
      , description = text "Junior position"
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


projectEvens : List Event
projectEvens =
    [ { title = "Personal project 1"
      , interval = Interval.oneYear 2022
      , description = text "Single-Cycle MIPS Processor"
      , category = Project
      , url = Just "https://github.com/MatisOana6/MIPS"
      , tags = []
      , important = False
      }
    , { title = "Personal project 2"
      , interval = Interval.oneYear 2022
      , description = text "Programming Techniques Assignments"
      , category = Project
      , url = Just "https://github.com/MatisOana6/Programming-Techniques"
      , tags = []
      , important = False
      }
    , { title = "Personal project 3"
      , interval = Interval.oneYear 2023
      , description = text "Law Firm Database"
      , category = Project
      , url = Just "https://github.com/MatisOana6/Data-Bases/blob/main/Colocviu_Matis_Oana_Antonia/Colocviu_partial.txt"
      , tags = []
      , important = False
      }
    , { title = "Personal project 4"
      , interval = Interval.oneYear 2023
      , description = text "Operating Systems Assignments - Linux"
      , category = Project
      , url = Just "https://github.com/MatisOana6/Operating-Systems"
      , tags = []
      , important = False
      }
    ]


personalDetails : PersonalDetails
personalDetails =
    { name = "Matiș Oana-Antonia"
    , intro = " I study Computer Science at Technical Univeristy of Cluj-Napoca. I am an enthusiastic student with a passion for continuous learning and a collaborative spirit. Eager to delve into diverse areas of technology, I find joy in acquiring new skills and exploring emerging trends. Thriving in team environments, I actively seek opportunities to contribute to group projects and discover fresh capabilities through collective efforts."
    , contacts = [ DetailWithName "Email" "matis.oana@yahoo.com"
                 , DetailWithName  "Phone" "0787 464 449"]
    , socials = [ DetailWithName "Github" "https://github.com/MatisOana6"
                , DetailWithName "Linkedin" "https://www.linkedin.com/in/oana-matis-107729296/" 
                , DetailWithName "Instagram" "https://www.instagram.com/oana_matis6/"]    
    }


initModel : Model
initModel =
    { personalDetails = personalDetails
    , events = Event.sortByInterval <| academicEvents ++ workEvents ++ projectEvens
    , selectedEventCategories = allSelected
    , repos = [
          { name = "MIPS Processor"
          , description = Just "Single-Cycle MIPS Processor"
          , url = "https://github.com/MatisOana6/MIPS"
          , pushedAt = "2023-11-18"  -- Replace with the actual date
          , stars = 92
          }
        , { name = "Operating Systems"
          , description = Just "Operating Systems Assignments"
          , url = "https://github.com/MatisOana6/Operating-Systems"
          , pushedAt = "2023-08-04"  -- Replace with the actual date
          , stars = 90
          }
          , { name = "Data-Bases"
          , description = Just "Law Firm Data-Base"
          , url = "https://github.com/MatisOana6/Data-Bases"
          , pushedAt = "2023-10-15"  -- Replace with the actual date
          , stars = 100
          }]
    }
