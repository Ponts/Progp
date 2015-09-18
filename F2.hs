-- August Falk Pontus Brink

module F2 where

import Data.List
-- Datatyper
data MolSeq = MolSeq {seqName :: String, seqSequence :: String, seqType :: String}
data Profile= Profile {matrix :: [[(Char, Int)]], profSeqType :: String, nrOfSequences :: Int, name :: String} 

-- Tar två strängar och avgör om det är en protein eller dna
-- Returnar MolSeq protein eller dna
string2seq :: String -> String -> MolSeq
string2seq namn sekvens = if isProtein sekvens
                then MolSeq namn sekvens "Protein" -- är Protein
                else MolSeq namn sekvens "DNA"

molseqs2profile :: String -> [MolSeq] -> Profile
molseqs2profile name mols = Profile (makeProfileMatrix mols) (seqType (head mols)) (length mols) name


nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

makeProfileMatrix :: [MolSeq] -> [[(Char, Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    defaults = 
      if (t == "DNA") then
        zip nucleotides (replicate (length nucleotides) 0) -- Rad (i) Skapar en lista av tuples med bokstav respektive nolla
      else 
        zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii) -||- samma sak fast med karaktärer för Protein
    strs = map seqSequence sl                              -- Rad (iii) strs är en lista som håller alla sekvenserna
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
               (transpose strs)                            -- Rad (iv) transpose strs är gör om strängarna i strs så att den första strängen i listan är den första bokstaven i varje sträng från strs
<<<<<<< HEAD
               											   -- första map gör det inom första parentesen på transpose strs
               											   -- Sort sorterar listan transpose strs, group gör att varje bokstav har en egen sträng. ex "AABB" blir "AA" "BB" 
               											   -- head x tar första bokstaven från en sekvens, length x tar längden på den sekvensen. Så man får en tuple där det står bokstaven och hur ofta den förekommer i första strängen, dvs första platsen tå listan är transponerad.
-- Sort och group körs på en sekvens (i första map), så den sorterar och delar in i strängar där strängens längd är antalet gånger bokstaven förekommer.
-- Sedan körs head x length x på dessa grupper, så man får en tupel med vilken bokstav det är och hur ofta den förekommer. Tillslut har man en dubbellista med tuppels
-- där den första listan av tuppels i listan motsvarar första bokstaven i sekvenserna. tex [[(A,1),(B,2)]] så har vi 
=======
                                                           -- första map gör det inom första parentesen på transpose strs
                                                           -- head x tar första 
>>>>>>> origin/master
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1) -- lägger till tex C,0 ifall det inte förekommer

-- Kollar om det är en DNA.
isProtein :: String -> Bool
<<<<<<< HEAD
isProtein [] = False 				-- Kommer vi hit är det en DNA
=======
isProtein [] = False                -- Kommer vi hit är det en Protein
>>>>>>> origin/master
isProtein (c : rest) 
                | elem c proteinChars = True    -- kommer vi hit är det en protein
                | otherwise = isProtein(rest)   -- Är en char, kolla nästa

-- Chars som finns i Protein
proteinChars = ['R','N','D','E','Q','H','I','L','K','M','F','P','S','W','Y','V']

-- Kollar längden på en sekvens
seqLength :: MolSeq -> Int
seqLength m = length (seqSequence m)

seqDistance :: MolSeq -> MolSeq -> Double
seqDistance m1 m2 = case molTypes m1 m2 of 
                        ("Protein", "Protein")  -> calcProtein m1 m2
                        ("DNA", "DNA")  -> calcDNA m1 m2
                        _               -> error "Invalid types"
    
    
calcDNA :: MolSeq -> MolSeq -> Double
calcDNA m1 m2 = if hammingDist m1 m2 > 0.74
            then 3.3
            else -(3/4)*log(1-(4*hammingDist m1 m2)/3)
            
calcProtein :: MolSeq -> MolSeq -> Double
calcProtein m1 m2 = if hammingDist m1 m2 > 0.94
            then 3.7
            else -(19/20)*log(1-(20*hammingDist m1 m2)/19)
        
-- Tar in två sekvenser
-- Returnerar en Tuple med två Strings som säger om sekvenserna är DNA eller Protein				   
molTypes :: MolSeq -> MolSeq -> (String,String)
molTypes m1 m2 = (seqType m1, seqType m2)

-- Beränkar det normaliserade Hamming-avståndet mellan två sekvenser		
hammingDist :: MolSeq -> MolSeq -> Double
hammingDist m1 m2 = countDiff (seqSequence m1) (seqSequence m2) / fromIntegral (length (seqSequence m1))

-- Beränkar antaler element som skiljer sig mellan två strängar
countDiff :: String -> String -> Double
countDiff [] [] = 0.0 -- Strängarna är samma längd så detta funkar 
countDiff s1 s2
                    | head s1 /= head s2 = 1.0 + countDiff (tail s1) (tail s2)
                    | otherwise = countDiff (tail s1) (tail s2)
