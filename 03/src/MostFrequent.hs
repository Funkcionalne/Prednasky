module MostFrequent where

import Data.Char (toLower)
import Data.List (sort,words)
import Data.List.Split

type FreqTable = [(Int,String)]

-- najdi najcastejsie vyskytujucie sa slovo v Stringu, priklad-- mostFrequent hamlet

chunks    :: [String] -> [[String]]
chunks [] = []
chunks xs@(w:_) = takeWhile (==w) xs: chunks (dropWhile (==w) xs)

chunkLengths  :: [[String]] -> FreqTable
chunkLengths xs = map (\chunk -> (length chunk, head chunk)) xs

splitWords  = filter(/= "") . splitOneOf " .,;!@#$%^&*()'"

mostFrequent :: String -> String
mostFrequent ws = snd $ last $ sort $ chunkLengths $ chunks $ sort $ splitWords $ map toLower ws

-- funkcionalna verzia 
mostFrequent' = snd .last . sort . chunkLengths . chunks . sort . splitWords . map toLower

-- zatvorkova verzia pre rodenych Lispistov
mostFrequent'' ws = snd (
                      last (
                        sort (
                          chunkLengths ( 
                            chunks ( 
                              sort ( 
                                splitWords ( 
                                  map toLower ws
                      )))))))
                          
hamlet = "There was this king sitting in his garden all alane " ++
  "When his brother in his ear poured a wee bit of henbane. " ++
  "He stole his brother's crown and his money and his widow. " ++
  "But the dead king walked and got his son and said Hey listen, kiddo! " ++
  " " ++
  "I've been killed, and it's your duty then to take revenge on Claudius. " ++
  "Kill him quick and clean and show the nation what a fraud he is. "  ++
  "The boy said Right, I'll do it. But I'll have to play it crafty. " ++
  "So no one will suspect me, I'll kid on that I'm a dafty."  ++
  " " ++
  "Then with all except Horatio, cuz he counts him as a friend, " ++
  "Hamlet, that's the boy, puts on he's round the bend. " ++
  "But because he was not ready for obligatory killing, " ++
  "He tried to make the king think he was tuppence off a shilling. " ++
  " " ++
  "Got a rise out of Polonius, treats poor Ophelia vile, " ++
  "Told Rosencrantz and Guildenstern Denmark's a bloody jail. " ++
  "Then a troupe of travelling actors, like Seven Eighty-four, " ++
  "Arrived to do a special one night gig in Elsinore. " ++
  " " ++
      "Hamlet! Hamlet! Loved his mommy! " ++
      "Hamlet! Hamlet! Acting barmy! " ++
      "Hamlet! Hamlet! Hesitatin', " ++
      "Wonders if the ghost's a cheat " ++
      "And that is why he's waitin'. " ++
  " " ++
  "Then Hamlet wrote a scene for the players to enact " ++
  "While Horatio and he watched to see if Claudius cracked. " ++
  "The play was called The Mousetrap (not the one that's running now), " ++
  "And sure enough, the king walked out before the final bow. " ++
  " " ++
  "So Hamlet's got the proof that Claudius gave his dad the dose. " ++
  "The only problem being, now, that Claudius knows he knows. " ++
  "So while Hamlet tells his mother her new husband's not a fit one, " ++
  "Uncle Claude puts out a contract with the English king as hit man. " ++
  " " ++
  "And when Hamlet killed Polonius, the concealed corpus delecti, " ++
  "Was the king's excuse to send him for an English hempen necktie. " ++
  "With Rosencrantz and Guildenstern to make sure he got there, " ++
  "Hamlet jumped the boat and put the finger on that pair. " ++
  " " ++
  "Meanwhile Leartes heard his dad had been stabbed through the arras. " ++
  "He came running back to Elsinore, toot-sweet, hotfoot from Paris. " ++
  "And Ophelia, with her dad killed by the man she wished to marry, " ++
  "After saying it with flowers, she committed hari-cari. " ++
  " " ++
      "Hamlet! Hamlet! Nae messin'! " ++
      "Hamlet! Hamlet! Learned his lesson! " ++
      "Hamlet! Hamlet! Yorick's crust " ++
      "Convinced him that men, good or bad, " ++
      "At last must come to dust. " ++
  " " ++
  "Then Leartes lost his cool and was demanding retribution. " ++
  "The king said Keep your head, and I'll provide you a solution."  ++
  "He arranged a sword fight for the interested parties, " ++
  "With a blunted sword for Hamlet, and a sharpened sword for Leartes. " ++
  " " ++
  "To make things double sure, the old belt and braces line, " ++
  "He fixed a poison sword tip, and a poison cup of wine. " ++
  "The poison sword got Hamlet, but Leartes went and muffed it, " ++
  "'Cause he got stabbed himself, and he confessed before he snuffed it. " ++
  " " ++
  "Then Hamlet's mommy drank the wine, and as her face turned blue, " ++
  "Hamlet said I think the king's a baddie through and through."  ++
  "Incestuous, murderous, damned Dane, he said to be precise, " ++
  "And made up for hesitating once by killing Claudius twice, " ++
  " " ++
  "For he stabbed him with the sword and forced the wine between his lips. " ++
  "He cried The rest is silence, and cashed in all his chips. " ++
  "They fired a volley over him that shook the topmost rafter. " ++
  "And Fortinbras, knee-deep in Danes, lived happily ever after. " ++
  " " ++
      "Hamlet! Hamlet! Oh so gory! " ++
      "Hamlet! Hamlet! End of story! " ++
      "Hamlet! Hamlet! I'm away! " ++
      "If you think this is boring, " ++
      "You should read the bloody play. "
