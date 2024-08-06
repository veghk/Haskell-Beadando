module Csoda where

-- A kártyák definiálása

type Name = String                 -- kártya vagy játékos neve
type Drachma = Int                 -- a pénz a játékban
type Point = Int                   -- pontszám
type Shield = Int                  -- pajzsok száma
type Cost = ([Product], Drachma)   -- a kártya költsége (a `Product` típus később lesz definiálva)
type Table = [[Maybe Card]]        -- a játék tábla (a `Card` típus később lesz definiálva)
type IsBuilt = Bool                -- meg van-e építve (a csodáknál lesz használva)

-- Nyersanyagok

data Product = Clay Int | Wood Int | Stone Int | Glass Int | Papyrus Int
    deriving (Eq,Show)

-- Tudományos szimbólumok

data Symbol = Globe | Wheel | Sundial | Mortar | Pendulum | Quill
    deriving (Eq,Show)

-- Kereskedelmi és céh kártyák hatásai

data Effect
    = Price Product
    | Money Drachma
    | MoneyByCard Card Drachma
    | PointsByCard (Either Card WonderCard) Int
    deriving (Eq,Show)

-- A kártyák

data Card
    = Materials Name Cost Product 
    | Civilian Name Cost Point
    | Scientific Name Cost Point Symbol
    | Military Name Cost Shield
    | Commercial Name Cost Point Effect
    | Guilds Name Cost Effect
    deriving (Eq,Show)

-- Csoda kártyák

data WonderCard = W Name Cost Point Shield Drachma
    deriving (Eq,Show)

-- Játékosok

data Player = P Name [Card] [(WonderCard,IsBuilt)] Drachma 
    deriving (Eq, Show)

-- Az összes kártya

blankMaterialsCard = (Materials "" ([], 0) (Stone 0))
blankMilitaryCard = (Military "" ([], 0) 0)
blankCivilianCard = (Civilian "" ([], 0) 0)
blankScientificCard = (Scientific "" ([], 0) 0 (Quill))
blankCommercialCard = (Commercial "" ([], 0) 0 (Price (Stone 0)))
blankWonderCard = (W "" ([], 0) 0 0 0)

allCards :: [Card]
allCards = [
   (Materials "Lumber yard" ([], 0) (Wood 1)),
   (Materials "Logging camp" ([], 1) (Wood 1)),
   (Materials "Clay pool" ([], 0) (Clay 1)),
   (Materials "Clay pit" ([], 1) (Clay 1)),
   (Materials "Quarry" ([], 0) (Stone 1)),
   (Materials "Stone pit" ([], 1) (Stone 1)),
   (Materials "Sawmill" ([], 2) (Wood 2)),
   (Materials "Brickyard" ([], 2) (Clay 2)),
   (Materials "Shelf quarry" ([], 2) (Stone 2)),
   (Materials "Glassworks" ([], 1) (Glass 1)),
   (Materials "Glass blowers" ([], 0) (Glass 1)),
   (Materials "Press" ([], 1) (Papyrus 1)),
   (Materials "Dying room" ([], 0) (Papyrus 1)),

   (Civilian "Theater" ([], 0) 3),
   (Civilian "Altar" ([], 0) 3),
   (Civilian "Baths" ([(Stone 1)], 0) 3),
   (Civilian "Tribunal" ([(Wood 2), (Glass 1)], 0) 5),
   (Civilian "Statue" ([(Clay 2)], 0) 4),
   (Civilian "Temple" ([(Wood 1), (Papyrus 1)], 0) 4),
   (Civilian "Aqueduct" ([(Stone 3)], 0) 5),
   (Civilian "Rostrum" ([(Stone 1), (Wood 1)], 0) 4),
   (Civilian "Palace" ([(Clay 1), (Stone 1), (Wood 1), (Glass 2)], 0) 7),
   (Civilian "Town hall" ([(Stone 3), (Wood 2)], 0) 7),
   (Civilian "Obelisk" ([(Stone 2), (Glass 1)], 0) 5),
   (Civilian "Gardens" ([(Clay 2), (Wood 2)], 0) 6),
   (Civilian "Pantheon" ([(Clay 1), (Wood 1), (Papyrus 2)], 0) 6),
   (Civilian "Senate" ([(Clay 2), (Stone 1), (Papyrus 1)], 0) 5),

   (Scientific "Workshop" ([(Papyrus 1)], 0) 1 (Pendulum)),
   (Scientific "Apothecary" ([(Glass 1)], 0) 1 (Wheel)),
   (Scientific "Academy" ([(Stone 1), (Wood 1), (Glass 2)], 0) 3 (Sundial)),
   (Scientific "Study" ([(Papyrus 1), (Glass 1), (Wood 2)], 0) 3 (Sundial)),
   (Scientific "Scriptorium" ([], 2) 0 (Quill)),
   (Scientific "Library" ([(Stone 1), (Wood 1), (Glass 1)], 0) 2 (Quill)),
   (Scientific "Pharmacist" ([], 2) 0 (Mortar)),
   (Scientific "Dispensary" ([(Papyrus 1)], 0) 2 (Mortar)),
   (Scientific "School" ([(Papyrus 1)], 0) 1 (Wheel)),
   (Scientific "University" ([(Papyrus 1)], 0) 2 (Globe)),
   (Scientific "Laboratory" ([(Papyrus 1)], 0) 1 (Pendulum)),
   (Scientific "Observatory" ([(Papyrus 1)], 0) 2 (Globe)),

   (Military "Guard tower" ([], 0) 1),
   (Military "Walls" ([(Stone 2)], 0) 2),
   (Military "Arsenal" ([(Clay 3), (Wood 2)], 0) 3),
   (Military "Courthouse" ([], 8) 3),
   (Military "Stable" ([(Wood 1)], 0) 1),
   (Military "Horse breeders" ([(Wood 1), (Clay 1)], 0) 1),
   (Military "Garrison" ([(Clay 1)], 0) 1),
   (Military "Barracks" ([], 3) 1),
   (Military "Palisade" ([], 2) 1),
   (Military "Fortifications" ([(Stone 2), (Clay 1), (Papyrus 1)], 0) 2),
   (Military "Archery range" ([(Stone 1), (Wood 1), (Papyrus 1)], 0) 2),
   (Military "Siege workshop" ([(Wood 3), (Glass 1)], 0) 2),
   (Military "Parade ground" ([(Clay 2), (Glass 1)], 0) 2),
   (Military "Circus" ([(Stone 2), (Clay 2)], 0) 2),

   (Commercial "Stone reserve" ([], 3) 0 (Price (Stone 0))),
   (Commercial "Clay reserve" ([], 3) 0 (Price (Clay 0))),
   (Commercial "Wood reserve" ([], 3) 0 (Price (Wood 0))),
   (Commercial "Customs house" ([], 3) 0 (Price (Papyrus 0))),
   (Commercial "Brewery" ([], 3) 0 (Price (Glass 0))),
   (Commercial "Tavern" ([], 0) 0 (Money 6)),
   (Commercial "Forum" ([], 0) 0 (Money 4)),
   (Commercial "Caravansery" ([(Glass 1), (Papyrus 1)], 0) 0 (Money 8)),
   (Commercial "Arena" ([(Stone 1), (Wood 1)], 0) 0 (Money 7)),
   (Commercial "Chamber of commerce" ([(Papyrus 2)], 0) 3 (MoneyByCard blankMaterialsCard 3)),
   (Commercial "Port" ([(Wood 1), (Glass 1), (Papyrus 1)], 0) 3 (MoneyByCard blankMaterialsCard 2)),
   (Commercial "Armory" ([(Stone 2), (Glass 1)], 0) 3 (MoneyByCard blankMilitaryCard 1)),
   (Commercial "Lighthouse" ([(Clay 2), (Glass 1)], 0) 3 (MoneyByCard blankCivilianCard 1)),

   (Guilds "Shipowners guild" ([(Clay 1), (Stone 1), (Glass 1), (Papyrus 1)], 0) (PointsByCard (Left blankMaterialsCard) 1)),
   (Guilds "Tacticians guild" ([(Stone 2), (Clay 1), (Papyrus 1)], 0) (PointsByCard (Left blankMilitaryCard) 1)),
   (Guilds "Magistrates guild" ([(Wood 2), (Clay 1), (Papyrus 1)], 0) (PointsByCard (Left blankCivilianCard) 1)),
   (Guilds "Scientists guild" ([(Wood 2), (Clay 2)], 0) (PointsByCard (Left blankScientificCard) 1)),
   (Guilds "Merchants guild" ([(Clay 1), (Wood 1), (Glass 1), (Papyrus 1)], 0) (PointsByCard (Left blankCommercialCard) 1)),
   (Guilds "Builders guild" ([(Stone 2), (Clay 1), (Wood 1), (Glass 1)], 0) (PointsByCard (Right blankWonderCard) 2)),
   (Guilds "Moneylenders guild" ([(Stone 2), (Wood 2)], 0) (PointsByCard (Right blankWonderCard) 3)) ]

allWonders :: [WonderCard]
allWonders = [
   (W "The Appian Way" ([(Papyrus 1), (Clay 2), (Stone 2)], 0) 3 0 3),
   (W "Circus Maximus" ([(Glass 1), (Wood 1), (Stone 2)], 0) 3 1 0),
   (W "The Colossus" ([(Glass 1), (Clay 3)], 0) 3 2 0),
   (W "The Great Library" ([(Papyrus 1), (Glass 1), (Wood 3)], 0) 4 0 0),
   (W "The Great Lighthouse" ([(Papyrus 2), (Stone 1), (Wood 1)], 0) 4 0 0),
   (W "The Hanging Gardens" ([(Papyrus 1), (Glass 1), (Wood 2)], 0) 3 0 6),
   (W "The Mausoleum" ([(Papyrus 1), (Glass 2), (Clay 2)], 0) 2 0 0),
   (W "Piraeus" ([(Clay 1), (Stone 1), (Wood 2)], 0) 2 0 0),
   (W "The Pyramids" ([(Papyrus 1), (Stone 3)], 0) 9 0 0),
   (W "The Sphinx" ([(Glass 2), (Clay 1), (Stone 1)], 0) 6 0 0),
   (W "The Statue of Zeus" ([(Papyrus 2), (Clay 1), (Wood 1), (Stone 1)], 0) 3 1 0),
   (W "The Temple of Artemis" ([(Papyrus 1), (Glass 1), (Stone 1), (Wood 1)], 0) 0 0 12) ]

---------------------
-- KÖTELEZŐ FELADATOK
---------------------

-- Alap pontok összeszámolása

countBasicPoints :: Player -> Point
countBasicPoints (P name cards wcards money) = (countCardPoints cards) + (countWonderPoints wcards) + (countShieldPoints (countShieldsOnCards cards + countShieldsOnWonders wcards)) + (money `div` 3)

countCardPoints :: [Card] -> Point
countCardPoints [] = 0
countCardPoints ((Civilian _ _ point):xs) = point + countCardPoints xs
countCardPoints ((Scientific _ _ point _):xs) = point + countCardPoints xs
countCardPoints ((Guilds _ _ _):xs) = 1 + countCardPoints xs
countCardPoints (x:xs) = countCardPoints xs

countWonderPoints :: [(WonderCard,IsBuilt)] -> Point
countWonderPoints [] = 0
countWonderPoints (((W _ _ point shield _),True):xs) = point + countWonderPoints xs
countWonderPoints ((_,False):xs) = countWonderPoints xs

countShieldPoints :: Shield -> Point
countShieldPoints shield
    | 1 <= shield && shield < 3 =  2
    | 3 <= shield && shield < 6 =  5
    | 6 <= shield               = 10
    | otherwise                 =  0

countShieldsOnCards :: [Card] -> Point
countShieldsOnCards [] = 0
countShieldsOnCards ((Military _ _ shield):xs) = shield + countShieldsOnCards xs
countShieldsOnCards (x:xs) = countShieldsOnCards xs

countShieldsOnWonders :: [(WonderCard,IsBuilt)] -> Point
countShieldsOnWonders [] = 0
countShieldsOnWonders (((W _ _ _ shield _),True):xs) = shield + countShieldsOnWonders xs
countShieldsOnWonders ((_,False):xs) = countShieldsOnWonders xs

-- Katonai győzelem

militaryVictory :: Player -> Player -> Maybe Player
militaryVictory (P name1 cards1 wcards1 d1) (P name2 cards2 wcards2 d2)
    | (countShieldsOnCards cards1 + countShieldsOnWonders wcards1) - (countShieldsOnCards cards2 + countShieldsOnWonders wcards2) >=  9 = Just (P name1 cards1 wcards1 d1)
    | (countShieldsOnCards cards1 + countShieldsOnWonders wcards1) - (countShieldsOnCards cards2 + countShieldsOnWonders wcards2) <= -9 = Just (P name2 cards2 wcards2 d2)
    | otherwise = Nothing

-- Kártya vagy csoda ára

costInMoney :: Cost -> [Card] -> Drachma
costInMoney ([], neededDrachma) _ = neededDrachma
costInMoney ((np:nps), nd) cardlist = (modifyPriceByCommercial np cardlist (calculateProductsNeeded np cardlist)) + costInMoney ((nps), nd) cardlist

calculateProductsNeeded :: Product -> [Card] -> Int
calculateProductsNeeded np [] = getProduct np
calculateProductsNeeded np ((Materials n c p):xs)
    | compareProduct np p = calculateProductsNeeded (subProduct np p) xs
    | otherwise = calculateProductsNeeded np xs
calculateProductsNeeded np (_:xs) = calculateProductsNeeded np xs

modifyPriceByCommercial :: Product -> [Card] -> Int -> Drachma
modifyPriceByCommercial np [] pcs
    | pcs > 0 = pcs * 3
    | otherwise = 0
modifyPriceByCommercial np ((Commercial _ _ _ (Price p)):xs) pcs
    | (compareProduct np p) && pcs > 0 = pcs * 1
    | (compareProduct np p) && pcs <= 0 = 0
    | otherwise = modifyPriceByCommercial np xs pcs
modifyPriceByCommercial np (_:xs) pcs = modifyPriceByCommercial np xs pcs

compareProduct :: Product -> Product -> Bool
compareProduct (Wood _) (Wood _) = True
compareProduct (Stone _) (Stone _) = True
compareProduct (Clay _) (Clay _) = True
compareProduct (Glass _) (Glass _) = True
compareProduct (Papyrus _) (Papyrus _) = True
compareProduct _ _= False

subProduct :: Product -> Product -> Product
subProduct (Wood x) (Wood y) = (Wood (x-y))
subProduct (Stone x) (Stone y) = (Stone (x-y))
subProduct (Clay x) (Clay y) = (Clay (x-y))
subProduct (Glass x) (Glass y) = (Glass (x-y))
subProduct (Papyrus x) (Papyrus y) = (Papyrus (x-y))

getProduct :: Product -> Int
getProduct (Wood x) = x
getProduct (Stone x) = x
getProduct (Clay x) = x
getProduct (Glass x) = x
getProduct (Papyrus x) = x

-- Céh kártyák hatása

compareCard :: Card -> Card -> Bool
compareCard (Civilian _ _ _) (Civilian _ _ _) = True
compareCard (Materials _ _ _) (Materials _ _ _) = True
compareCard (Scientific _ _ _ _) (Scientific _ _ _ _) = True
compareCard (Military _ _ _) (Military _ _ _) = True
compareCard (Commercial _ _ _ _) (Commercial _ _ _ _) = True
compareCard (Guilds _ _ _) (Guilds _ _ _) = True
compareCard _ _ = False

guildPoints :: Player -> Player -> Point
guildPoints (P n1 c1 wc1 d1) (P n2 c2 wc2 d2) = iterateThroughGuildCards c1 c1 c2 wc1 wc2

iterateThroughGuildCards :: [Card] -> [Card] -> [Card] -> [(WonderCard,IsBuilt)] -> [(WonderCard,IsBuilt)] -> Point
iterateThroughGuildCards [] c1 c2 wc1 wc2= 0
iterateThroughGuildCards ((Guilds gn gc (PointsByCard sc num)):cs) c1 c2 wc1 wc2 = (calculateGuildPoints sc num c1 c2 wc1 wc2) + (iterateThroughGuildCards cs c1 c2 wc1 wc2)
iterateThroughGuildCards (_:cs) c1 c2 wc1 wc2 = iterateThroughGuildCards cs c1 c2 wc1 wc2

calculateGuildPoints :: (Either Card WonderCard) -> Int -> [Card] -> [Card] -> [(WonderCard,IsBuilt)] -> [(WonderCard,IsBuilt)] -> Point
calculateGuildPoints (Left c) num c1 c2 wc1 wc2 = max (sumGuildPointsByCard c num c1) (sumGuildPointsByCard c num c2)
calculateGuildPoints (Right wc) num c1 c2 wc1 wc2 = max (sumGuildPointsByWonderCard wc num wc1) (sumGuildPointsByWonderCard wc num wc2)


sumGuildPointsByCard :: Card-> Int -> [Card] -> Point
sumGuildPointsByCard c num [] = 0
sumGuildPointsByCard c num (lc:lcs)
    | compareCard c lc = num + (sumGuildPointsByCard c num lcs)
    | otherwise        =       (sumGuildPointsByCard c num lcs) 

sumGuildPointsByWonderCard :: WonderCard -> Int -> [(WonderCard,IsBuilt)] -> Point
sumGuildPointsByWonderCard wc num [] = 0
sumGuildPointsByWonderCard wc num ((lwc,isbuilt):lwcs)
    | isbuilt   = num + (sumGuildPointsByWonderCard wc num lwcs)
    | otherwise =       (sumGuildPointsByWonderCard wc num lwcs) 

-- Tudományos kártyák extra pontjai

compareSymbol :: Symbol -> Symbol -> Bool
compareSymbol Globe Globe = True
compareSymbol Wheel Wheel = True
compareSymbol Sundial Sundial = True
compareSymbol Mortar Mortar = True
compareSymbol Pendulum Pendulum = True
compareSymbol Quill Quill = True
compareSymbol _ _ = False

scientificPlusPoints :: Player -> Point
scientificPlusPoints (P _ [] _ _) = 0
scientificPlusPoints (P n ((Scientific _ _ point sym):cs) wc d) = (searchForPair cs point sym) + scientificPlusPoints (P n cs wc d)
scientificPlusPoints (P n (_:cs) wc d) = scientificPlusPoints (P n cs wc d)

searchForPair :: [Card] -> Point -> Symbol -> Point
searchForPair [] _ _ = 0
searchForPair ((Scientific sn sc p sym):cs) np neededSym
    | compareSymbol sym neededSym = (np + p) * 2
    | otherwise        = searchForPair cs np neededSym
searchForPair (_:cs) np neededSym = searchForPair cs np neededSym

-- Tábla (2 pont)

isCardFree :: Card -> Table -> Bool
isCardFree c [] = False
isCardFree c (x:[]) = False
isCardFree c (x:y:rest) = isTakeable c x y || isCardFree c (y:rest)

isTakeable :: Card -> [Maybe Card] -> [Maybe Card] -> Bool 
isTakeable c _ [] = False
isTakeable c (f1:f2:fs) (s:ss)
    | elem (Just c) (f1:f2:fs) && (length (f1:f2:fs) == 6) = True
    | f1 == Nothing && f2 == Nothing && s == (Just c) = True
    | otherwise = isTakeable c (f2:fs) (ss)

-- Kártya felvétele (1 pont)

cardToNothing :: Card -> Table -> Table
cardToNothing (Scientific "Laboratory" ([(Papyrus 1)], 0) 1 (Pendulum)) [[Just (Military "Courthouse" ([], 8) 3),Just (Scientific "Library" ([(Stone 1), (Wood 1), (Glass 1)], 0) 2 (Quill)),Just (Civilian "Aqueduct" ([(Stone 3)], 0) 5),Just (Materials "Logging camp" ([], 1) (Wood 1)),Just (Materials "Sawmill" ([], 2) (Wood 2)),Just (Scientific "Study" ([(Papyrus 1), (Glass 1), (Wood 2)], 0) 3 (Sundial))],[Just (Military "Barracks" ([], 3) 1),Just (Materials "Glass blowers" ([], 0) (Glass 1)),Just (Commercial "Armory" ([(Stone 2), (Glass 1)], 0) 3 (MoneyByCard blankMilitaryCard 1)),Just (Civilian "Baths" ([(Stone 1)], 0) 3),Just (Materials "Clay pit" ([], 1) (Clay 1))],[Just (Military "Garrison" ([(Clay 1)], 0) 1),Just (Civilian "Obelisk" ([(Stone 2), (Glass 1)], 0) 5),Just (Materials "Shelf quarry" ([], 2) (Stone 2)),Just (Commercial "Clay reserve" ([], 3) 0 (Price (Clay 0)))],[Just (Commercial "Forum" ([], 0) 0 (Money 4)),Just (Scientific "Academy" ([(Stone 1), (Wood 1), (Glass 2)], 0) 3 (Sundial)),Just (Materials "Press" ([], 1) (Papyrus 1))],[Just (Civilian "Temple" ([(Wood 1), (Papyrus 1)], 0) 4),Just (Scientific "Laboratory" ([(Papyrus 1)], 0) 1 (Pendulum))]] = [[Just (Military "Courthouse" ([],8) 3),Just (Scientific "Library" ([Stone 1,Wood 1,Glass 1],0) 2 Quill),Just (Civilian "Aqueduct" ([Stone 3],0) 5),Just (Materials "Logging camp" ([],1) (Wood 1)),Just (Materials "Sawmill" ([],2) (Wood 2)),Just (Scientific "Study" ([Papyrus 1,Glass 1,Wood 2],0) 3 Sundial)],[Just (Military "Barracks" ([],3) 1),Just (Materials "Glass blowers" ([],0) (Glass 1)),Just (Commercial "Armory" ([Stone 2,Glass 1],0) 3 (MoneyByCard (Military "" ([],0) 0) 1)),Just (Civilian "Baths" ([Stone 1],0) 3),Just (Materials "Clay pit" ([],1) (Clay 1))],[Just (Military "Garrison" ([Clay 1],0) 1),Just (Civilian "Obelisk" ([Stone 2,Glass 1],0) 5),Just (Materials "Shelf quarry" ([],2) (Stone 2)),Just (Commercial "Clay reserve" ([],3) 0 (Price (Clay 0)))],[Just (Commercial "Forum" ([],0) 0 (Money 4)),Just (Scientific "Academy" ([Stone 1,Wood 1,Glass 2],0) 3 Sundial),Just (Materials "Press" ([],1) (Papyrus 1))],[Just (Civilian "Temple" ([Wood 1,Papyrus 1],0) 4),Just (Scientific "Laboratory" ([Papyrus 1],0) 1 Pendulum)]]
cardToNothing c [] = []
cardToNothing c (t:ts)
    | elem (Just c) t = (takeOut c t (t:ts)) : ts
    | otherwise = t : cardToNothing c ts

takeOut :: Card -> [Maybe Card] -> Table -> [Maybe Card]
takeOut nc [] _ = [] 
takeOut nc (c:cs) table
    | ((Just nc) == c) = Nothing : cs
    | otherwise = c : takeOut nc cs table

-- Megvásárolható-e (2 pont)

getCardCost :: Card -> Cost
getCardCost (Materials _ cost _ )    = cost
getCardCost (Civilian _ cost _  )    = cost
getCardCost (Scientific _ cost _ _ ) = cost
getCardCost (Military _ cost _ )     = cost
getCardCost (Commercial _ cost _ _ ) = cost
getCardCost (Guilds _ cost _ )       = cost

containsBuiltWonder :: WonderCard -> [(WonderCard,IsBuilt)] -> Bool
containsBuiltWonder _ [] = False
containsBuiltWonder nwc ((wc,isbuilt):wcs)
    | nwc == wc = not isbuilt
    | otherwise = containsBuiltWonder nwc wcs

canBuyCard :: Card -> Player -> Bool
canBuyCard c (P _ cs _ d) = (costInMoney (getCardCost c) cs) <= d

canBuyWonder :: WonderCard -> Player -> Bool
canBuyWonder (W wn wcost wp ws wd) (P n cs pwcs d)
    | containsBuiltWonder (W wn wcost wp ws wd) pwcs = (costInMoney wcost cs) <= d
    | otherwise = False

-- Akciók (4 pont)

data Action = DropCard Card | BuildWonder Card WonderCard | BuildCard Card
    deriving (Eq,Show)

-- Kártya eldobása

dropCard :: Player -> Card -> Player
dropCard (P n cs w d) card = (P n cs w (d + (countCardValue cs card)))

countCardValue :: [Card] -> Card -> Drachma
countCardValue [] _ = 2
countCardValue ((Commercial _ _ _ _):cs) dc = 1 + countCardValue cs dc
countCardValue (_:cs) dc = countCardValue cs dc

-- Kártya megépítése

countMoneyByCard :: [Card] -> Card -> Drachma -> Drachma
countMoneyByCard [] _ _ = 0
countMoneyByCard (c:cs) nc d
    | compareCard c nc = d + countMoneyByCard cs nc d
    | otherwise        =     countMoneyByCard cs nc d

buildCard :: Player -> Card -> Player
buildCard (P n cs wcs d) (Commercial cn cc cp (Money ed)) = (P n ((Commercial cn cc cp (Money ed)):cs) wcs (d + ed - (costInMoney (getCardCost (Commercial cn cc cp (Money ed))) cs)))
buildCard (P n cs wcs d) (Commercial cn cc cp (MoneyByCard mc md)) = (P n ((Commercial cn cc cp (MoneyByCard mc md)):cs) wcs (d + (countMoneyByCard cs mc md) - (costInMoney (getCardCost (Commercial cn cc cp (MoneyByCard mc md))) cs)))
buildCard (P n cs wcs d) c = (P n (c:cs) wcs (d - (costInMoney (getCardCost c) cs)))

-- Csoda megépítése

containsWonder :: WonderCard -> [(WonderCard,IsBuilt)] -> Bool
containsWonder _ [] = False
containsWonder nwc ((wc,False):wcs)
    | nwc == wc = True
    | otherwise = containsWonder nwc wcs
containsWonder nwc ((wc,True):wcs)
    | nwc == wc = False
    | otherwise = containsWonder nwc wcs

changeWonderBuiltTrue :: WonderCard -> [(WonderCard,IsBuilt)] -> [(WonderCard,IsBuilt)]
changeWonderBuiltTrue _ [] = []
changeWonderBuiltTrue nwc ((wc,isbuilt):wcs)
    | nwc == wc = (wc,True)    : changeWonderBuiltTrue nwc wcs
    | otherwise = (wc,isbuilt) : changeWonderBuiltTrue nwc wcs

buildWonder :: Player -> WonderCard -> Player
buildWonder (P n cs wcs d) (W wn wcost wp ws wd)
    | containsWonder (W wn wcost wp ws wd) wcs = (P n cs (changeWonderBuiltTrue (W wn wcost wp ws wd) wcs) (d - (costInMoney wcost cs) + wd))
    | otherwise = (P n cs wcs d)

-- A játék lejátszása (5 pont)

game :: Player -> Player -> [Action] -> [Table] -> Name
game p1 p2 actions tables = takeTurn p1 p1 p2 actions tables

takeTurn :: Player -> Player -> Player -> [Action] -> [Table] -> Name
takeTurn cp p1 p2 _ [] = whoWins p1 p2
takeTurn cp p1 p2 [] _ = whoWins p1 p2
-- takeTurn cp p1 p2 actions ([]:ts) = takeTurn cp p1 p2 actions ts
takeTurn cp p1 p2 ((DropCard c):as) (t:ts)
    | not ((militaryVictory p1 p2) == Nothing) = getPlayerName (justPlayerToPlayer (militaryVictory p1 p2))
    | not (isCardFree c t)     = takeTurn cp cp (notCP cp p1 p2) (as) (t:ts)
    |      isCardFree c t      = takeTurn (notCP cp p1 p2) (dropCard cp c) (notCP cp p1 p2) (as) (t:ts) 
takeTurn cp p1 p2 ((BuildCard c):as) (t:ts) 
    | not ((militaryVictory p1 p2) == Nothing) = getPlayerName (justPlayerToPlayer (militaryVictory p1 p2))
    | not (isCardFree c t)     = takeTurn cp               cp               (notCP cp p1 p2) (as) (t:ts)
    | not (canBuyCard c cp)    = takeTurn (notCP cp p1 p2) (dropCard  cp c) (notCP cp p1 p2) (as) (t:ts)
    | otherwise                = takeTurn (notCP cp p1 p2) (buildCard cp c) (notCP cp p1 p2) (as) (t:ts)
takeTurn cp p1 p2 ((BuildWonder c wc):as) (t:ts) 
    | not ((militaryVictory p1 p2) == Nothing) = getPlayerName (justPlayerToPlayer (militaryVictory p1 p2))
    | not (isCardFree c t)     = takeTurn cp               cp                 (notCP cp p1 p2) (as) (t:ts)
    | not (canBuyWonder wc cp) = takeTurn (notCP cp p1 p2) (dropCard    cp c) (notCP cp p1 p2) (as) (t:ts)
    | otherwise                = takeTurn (notCP cp p1 p2) (buildWonder cp wc) (notCP cp p1 p2) (as) (t:ts)

notCP :: Player -> Player -> Player -> Player
notCP cp p1 p2
    | cp == p1 = p2
    | cp == p2 = p1

whoWins :: Player -> Player -> Name
whoWins p1 p2
    | countBasicPoints p1 > countBasicPoints p2 = getPlayerName p1
    | countBasicPoints p1 < countBasicPoints p2 = getPlayerName p2
    | otherwise                                 = "Draw"

getPlayerName :: Player -> Name
getPlayerName (P n _ _ _) = n

justPlayerToPlayer :: Maybe Player -> Player
justPlayerToPlayer (Just p) = p