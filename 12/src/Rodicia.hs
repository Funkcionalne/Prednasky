matka :: String -> Maybe String
matka "Anna" = Just "Eva"
matka "Betka" = Just "Hanka"
matka "Danka" = Just "Iveta"
matka "Eva" = Just "Renata"
matka "Hanka" = Just "Michaela"
matka "Iveta" = Just "Svetlana"
matka "Adam" = Just "Danka"
matka "Boris" = Just "Eva"
matka "Dusan" = Just "Hanka"
matka "Emil" = Just "Iveta"
matka "Gusto" = Just "Maria"
matka "Ivan" = Just "Renata"
matka _ = Nothing

otec :: String -> Maybe String
otec "Anna" = Just "Dusan"
otec "Betka" = Just "Emil"
otec "Danka" = Just "Gusto"
otec "Eva" = Just "Ivan"
otec "Hanka" = Just "Martin"
otec "Iveta" = Just "Peter"
otec "Adam" = Just "Emil"
otec "Boris" = Just "Gusto"
otec "Dusan" = Just "Ivan"
otec "Emil" = Just "Peter"
otec "Gusto" = Just "Rado"
otec "Ivan" = Just "Zigmund"
otec _ = Nothing

-- Vrati to iste co matka, iba zacne prefixom "Lady "
lady_matka :: String -> Maybe String
lady_matka _ = Nothing

-- Vrati to iste co otec, iba zacne prefixom "Sir "
sir_otec :: String -> Maybe String
sir_otec _ = Nothing

-- Vrati otca otcovho otca
praotec :: String -> Maybe String
praotec _ = Nothing

-- Pre k=1 matka, pre k=2 babka, pre k=3 prababka ...
k_matka :: Int -> String -> Maybe String
k_matka k _ = Nothing

-- Vrati rodicov
rodicia :: String -> [String]
rodicia _ = []

-- Vrati prarodicov
prarodicia :: String -> [String]
prarodicia _ = []

-- Vrati rodicov v k-tej generacii
k_rodicia :: Int ->String -> [String]
k_rodicia k _ = []

-- Vrati vsetkych znamych predkov
predkovia :: String -> [String]
predkovia _ = []
