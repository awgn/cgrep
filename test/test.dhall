{- This is an example Dhall configuration file

   Can you spot the mistake?

   Fix the typo, then move onto the "Definitions"
   example
-}

let port : Natural = 8080
let host : Text = "localhost"
let enabled : Bool = if True then True else False

in  { home       = "/home/bill"
    , privateKey = "/home/bill/.ssh/id_ed25519"
    , publicKey  = "/home/blil/.ssh/id_ed25519.pub"
    , serverPort = port
    , serverHost = host
    }
